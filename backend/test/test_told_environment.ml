open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf
module Fake_host = Test_host_reconnect.Fake_host

(* Environment changes not yet told to the model are derived from the
   session's [told_environment] entry, so they reach the model with the next
   message wherever the session went in between. *)

(* The notes the latest request's last user message starts with. *)
let print_told t requests ~text =
  let request : Provider.Request.t = Queue.last_exn requests in
  match
    List.find_map (List.rev request.messages) ~f:(function
      | Message.User u -> Some u
      | _ -> None)
  with
  | None -> print_endline "no user message"
  | Some u ->
    let notes =
      Option.value_map
        (Injected.of_message ~origin:u.origin u.text)
        ~default:[]
        ~f:(fun (i : Injected.t) -> i.notes)
    in
    assert (String.equal u.text (Injected.prefix_notes notes text));
    printf "%s: " text;
    (match notes with
     | [] -> print_endline "nothing told"
     | notes ->
       print_endline
         (mask
            t
            (String.concat ~sep:"; " (List.map notes ~f:Injected.Note.title))));
    Queue.clear requests
;;

let with_agents f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(Queue.enqueue requests)
      (List.init 20 ~f:(fun _ -> Reply.text "ok"))
  in
  let make ?session () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~cwd:t.dir
      ()
  in
  (* A backend restart: a new agent over the session as saved. *)
  let restart agent =
    make
      ~session:(Or_error.ok_exn (Session.load (Agent.state agent).session_path))
      ()
  in
  let send agent text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent;
    print_told t requests ~text
  in
  Core_unix.mkdir_p (Filename.concat t.dir "sub");
  f t ~make ~restart ~send
;;

let gpt = Option.value_exn (Model.find "openai/gpt-5.4")

let%expect_test
    "pending changes survive switching sessions in place (the TUI's) and a \
     restart, and are told once"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let agent = make () in
  send agent "one";
  let first = (Agent.state agent).session_path in
  Or_error.ok_exn (Agent.set_cwd agent ~path:"sub");
  Agent.set_model agent gpt;
  Agent.set_mode agent ~plan:true;
  Agent.new_session agent;
  send agent "elsewhere";
  Or_error.ok_exn (Agent.switch_session agent ~path:first);
  send agent "two";
  send agent "three";
  [%expect
    {|
    one: nothing told
    elsewhere: nothing told
    two: Working directory is now $DIR/sub; Tools changed: apply_patch, web_search added; edit removed; Plan mode on: the model was told to plan, not change files
    three: nothing told
    |}];
  (* Changed, then the backend restarts before the next message. *)
  Or_error.ok_exn (Agent.set_cwd agent ~path:t.dir);
  Agent.set_mode agent ~plan:false;
  let agent = restart agent in
  send agent "four";
  let agent = restart agent in
  send agent "five";
  [%expect
    {|
    four: Working directory is now $DIR; Plan mode off: the model was told it can change files
    five: nothing told
    |}]
;;

let%expect_test "changes that cancel out tell nothing, across a restart too" =
  with_agents
  @@ fun _t ~make ~restart ~send ->
  let agent = make () in
  send agent "one";
  let model = (Agent.state agent).model in
  Or_error.ok_exn (Agent.set_cwd agent ~path:"sub");
  Agent.set_model agent gpt;
  Agent.set_mode agent ~plan:true;
  let agent = restart agent in
  Or_error.ok_exn (Agent.set_cwd agent ~path:"..");
  Agent.set_model agent model;
  Agent.set_mode agent ~plan:false;
  send agent "two";
  (* Partly: only the net change is told. *)
  Agent.set_model agent gpt;
  Agent.set_model
    agent
    (Option.value_exn (Model.find "anthropic/claude-opus-4-6"));
  let agent = restart agent in
  send agent "three";
  [%expect
    {|
    one: nothing told
    two: nothing told
    three: Tools changed: web_search added
    |}]
;;

let%expect_test
    "a session from before the told environment was recorded tells changes \
     made after loading it"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let old =
    Session.create ~dir:(Filename.concat t.dir "sessions") ~cwd:t.dir ()
  in
  List.iter
    [ Session.set_system_prompt old ~text:"system"
    ; Session.append_message old (Message.user "one")
    ; Session.append_message old (Message.user "more")
    ]
    ~f:(fun (_ : Session.Entry.t) -> ());
  let agent =
    make ~session:(Or_error.ok_exn (Session.load (Session.path old))) ()
  in
  send agent "two";
  Or_error.ok_exn (Agent.set_cwd agent ~path:"sub");
  let agent = restart agent in
  send agent "three";
  [%expect
    {|
    two: nothing told
    three: Working directory is now $DIR/sub
    |}]
;;

let%expect_test
    "rewinding past the message that told a change tells it again; a fork \
     carries what is pending"
  =
  with_agents
  @@ fun _t ~make ~restart:_ ~send ->
  let agent = make () in
  send agent "one";
  let rewind_to = List.last_exn (Session.active_path (Agent.session agent)) in
  Or_error.ok_exn (Agent.set_cwd agent ~path:"sub");
  send agent "two";
  Or_error.ok_exn (Agent.rewind agent ~to_:rewind_to.id);
  send agent "two again";
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.fork agent ());
  send agent "forked";
  [%expect
    {|
    one: nothing told
    two: Working directory is now $DIR/sub
    two again: Working directory is now $DIR/sub
    forked: Plan mode on: the model was told to plan, not change files
    |}]
;;

let%expect_test
    "over RPC: a host change, then another session, the first evicted, then \
     back: the model is told with the next message"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(Queue.enqueue requests)
      (List.init 10 ~f:(fun _ -> Reply.text "ok"))
  in
  let _, h = Test_rpc.make_server t ~sw ~provider in
  let _laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
  in
  let call ?(params = "{}") meth =
    let reply =
      Rpc_server.handle
        h.server
        h.client
        (Json.of_string
           (sprintf {|{"id": "r", "method": %S, "params": %s}|} meth params))
    in
    match Json.member "error" reply with
    | Some error -> printf "%s: %s\n" meth (Json.to_string error)
    | None -> printf "%s: ok\n" meth
  in
  let send text =
    call ~params:(sprintf {|{"text": %S}|} text) "prompt";
    Agent.wait_idle (Test_rpc.current h);
    print_told t requests ~text
  in
  call "new_session";
  let first = Test_rpc.current h in
  send "one";
  call ~params:{|{"host": "host-laptop"}|} "set_active_host";
  call ~params:{|{"plan_mode": true}|} "set_mode";
  call "new_session";
  send "elsewhere";
  Test_host_reconnect.show_sessions h;
  call
    ~params:(sprintf {|{"path": %S}|} (Agent.state first).session_path)
    "switch_session";
  printf "reloaded: %b\n" (not (phys_equal (Test_rpc.current h) first));
  send "two";
  send "three";
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    new_session: ok
    prompt: ok
    one: nothing told
    set_active_host: ok
    set_mode: ok
    new_session: ok
    prompt: ok
    elsewhere: nothing told
    session "one" live=false
    session "elsewhere" live=true
    switch_session: ok
    reloaded: true
    prompt: ok
    two: Tools now run on laptop; Working directory is now /home/me; Plan mode on: the model was told to plan, not change files
    prompt: ok
    three: nothing told
    |}]
;;
