open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let info t = User_info.create ~home:t.dir
let set info text = print_s [%sexp (User_info.set info text : unit Or_error.t)]

let show t info =
  printf
    "text: %S, file: %s\n"
    (User_info.get info)
    (match In_channel.read_all (User_info.path ~home:t.dir) with
     | data -> sprintf "%S" data
     | exception Sys_error _ -> "none")
;;

let%expect_test "set, get, clear; an edit by hand counts; the limit" =
  with_sandbox
  @@ fun t ->
  let info = info t in
  User_info.subscribe info ~f:(fun () -> print_endline "(changed)");
  show t info;
  set info "  I'm Lloyd.\nI write OCaml.\n\n";
  show t info;
  [%expect
    {|
    text: "", file: none
    (changed)
    (Ok ())
    text: "I'm Lloyd.\nI write OCaml.", file: "I'm Lloyd.\nI write OCaml.\n"
    |}];
  Out_channel.write_all
    (User_info.path ~home:t.dir)
    ~data:"\nEdited in an editor.\n\n";
  show t info;
  print_s [%sexp (User_info.text info : string option)];
  [%expect
    {|
    text: "Edited in an editor.", file: "\nEdited in an editor.\n\n"
    ("Edited in an editor.")
    |}];
  set info (String.make (User_info.max_bytes + 1) 'x');
  show t info;
  [%expect
    {|
    (Error
     "the text about you is limited to 4000 bytes (this one has 4001): shorten it, and keep what is only sometimes needed in memory notes")
    text: "Edited in an editor.", file: "\nEdited in an editor.\n\n"
    |}];
  set info (String.make User_info.max_bytes 'x');
  printf "%d bytes\n" (String.length (User_info.get info));
  (* Beyond the limit by hand: cut, at a character. *)
  Out_channel.write_all
    (User_info.path ~home:t.dir)
    ~data:(String.make (User_info.max_bytes - 1) 'x' ^ "é and more");
  printf "%d bytes\n" (String.length (User_info.get info));
  [%expect
    {|
    (changed)
    (Ok ())
    4000 bytes
    3999 bytes
    |}];
  set info "  \n";
  show t info;
  print_s [%sexp (User_info.text info : string option)];
  set info "";
  [%expect
    {|
    (changed)
    (Ok ())
    text: "", file: none
    ()
    (changed)
    (Ok ())
    |}]
;;

let%expect_test "each namespace has its own" =
  with_sandbox
  @@ fun t ->
  let world name =
    Namespace.world
      { Namespace.name; token = name ^ "-token" }
      ~home:t.dir
      ~legacy_auth_file:(t.dir ^/ ".config/prigh/auth.json")
  in
  let alice = world "alice"
  and bob = world "bob"
  and default = world "default" in
  let of_world (w : Namespace.World.t) = User_info.create ~home:w.home in
  Or_error.ok_exn (User_info.set (of_world alice) "I am Alice.");
  Or_error.ok_exn (User_info.set (of_world default) "I am the default user.");
  List.iter [ alice; bob; default ] ~f:(fun w ->
    printf
      "%s: %S\n"
      (mask t (User_info.path ~home:w.home))
      (User_info.get (of_world w)));
  [%expect
    {|
    $DIR/.prigh/namespaces/alice/.prigh/user_info.md: "I am Alice."
    $DIR/.prigh/namespaces/bob/.prigh/user_info.md: ""
    $DIR/.prigh/user_info.md: "I am the default user."
    |}]
;;

let%expect_test "RPC: get and set; every client hears; errors say what to do" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  (* The server's own, under its home. *)
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home:t.dir
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client name =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "user_info_changed") ->
        printf "%s hears %s\n" name (Json.to_string json)
      | _ -> ())
  in
  let a = client "a" in
  let b = client "b" in
  let call client meth params =
    let response =
      Rpc_server.handle
        server
        client
        (`Object [ "id", `Number "1"; "method", `String meth; "params", params ])
    in
    match Json.member "ok" response, Json.member "result" response with
    | Some `True, Some result -> print_endline (Json.to_string result)
    | _ ->
      print_endline
        (Json.to_string (Option.value_exn (Json.member "error" response)))
  in
  call a "hello" (`Object []);
  call b "hello" (`Object []);
  ignore ([%expect.output] : string);
  call a "get_user_info" (`Object []);
  [%expect {| {"text":"","max":4000} |}];
  call a "set_user_info" (`Object [ "text", `String " I'm Lloyd.\n" ]);
  call b "get_user_info" (`Object []);
  [%expect
    {|
    a hears {"type":"event","event":"user_info_changed","text":"I'm Lloyd.","max":4000}
    b hears {"type":"event","event":"user_info_changed","text":"I'm Lloyd.","max":4000}
    {"text":"I'm Lloyd.","max":4000}
    {"text":"I'm Lloyd.","max":4000}
    |}];
  show t (info t);
  [%expect {| text: "I'm Lloyd.", file: "I'm Lloyd.\n" |}];
  call a "set_user_info" (`Object [ "text", `String (String.make 4001 'x') ]);
  call a "set_user_info" (`Object []);
  call a "set_user_info" (`Object [ "text", `Number "1" ]);
  [%expect
    {|
    "the text about you is limited to 4000 bytes (this one has 4001): shorten it, and keep what is only sometimes needed in memory notes"
    "missing param \"text\""
    "param \"text\" must be a string"
    |}];
  call b "set_user_info" (`Object [ "text", `String "" ]);
  [%expect
    {|
    a hears {"type":"event","event":"user_info_changed","text":"","max":4000}
    b hears {"type":"event","event":"user_info_changed","text":"","max":4000}
    {"text":"","max":4000}
    |}]
;;

(* What the latest request tells the model about the user: the system
   prompt's block and the notes its last user message starts with. *)
let print_request requests ~text =
  let request : Provider.Request.t = Queue.last_exn requests in
  let system = Option.value request.system ~default:"" in
  (match String.substr_index system ~pattern:"About the user" with
   | Some i -> printf "system: … %s\n" (String.drop_prefix system i)
   | None -> print_endline "system: nothing about the user");
  (match List.last_exn request.messages with
   | Message.User u ->
     let notes =
       Option.value_map u.origin ~default:[] ~f:(fun (i : Injected.t) ->
         List.filter i.notes ~f:(fun n ->
           match n.kind with
           | User_info _ -> true
           | _ -> false))
     in
     assert (String.is_suffix u.text ~suffix:text);
     (match notes with
      | [] -> printf "%s: nothing told\n" text
      | notes ->
        List.iter notes ~f:(fun n ->
          assert (String.is_substring u.text ~substring:n.text);
          printf "%s: %s\n%s\n" text (Injected.Note.title n) n.text))
   | _ -> print_endline "no user message");
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
  let make ?session ?user_info () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ?user_info
      ~cwd:t.dir
      ()
  in
  (* A backend restart: a new agent over the session as saved. *)
  let restart ?user_info agent =
    make
      ?user_info
      ~session:(Or_error.ok_exn (Session.load (Agent.state agent).session_path))
      ()
  in
  let send agent text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent;
    print_request requests ~text
  in
  f t ~make ~restart ~send
;;

let%expect_test
    "a new session has it in its system prompt and is not told it again; an \
     edit and its removal are told once, with the next message"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let info = info t in
  Or_error.ok_exn (User_info.set info "I'm Lloyd.\nI prefer short answers.");
  let agent = make ~user_info:info () in
  send agent "one";
  send agent "two";
  [%expect
    {|
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    one: nothing told
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    two: nothing told
    |}];
  Or_error.ok_exn (User_info.set info "I'm Lloyd. Be thorough.");
  send agent "three";
  send agent "four";
  [%expect
    {|
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    three: About you: the model was given what you wrote
    [User info: what the user wrote about themselves in prigh's settings, for every session, is now:
    I'm Lloyd. Be thorough.
    It replaces any earlier text about the user.]
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    four: nothing told
    |}];
  (* Changed and changed back: nothing to tell. Removed, then the backend
     restarts before the next message. *)
  Or_error.ok_exn (User_info.set info "something else");
  Or_error.ok_exn (User_info.set info "I'm Lloyd. Be thorough.");
  send agent "five";
  Or_error.ok_exn (User_info.set info "");
  let agent = restart ~user_info:info agent in
  send agent "six";
  let agent = restart ~user_info:info agent in
  send agent "seven";
  [%expect
    {|
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    five: nothing told
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    six: About you: the model was told you removed it
    [User info: the user removed what they wrote about themselves in prigh's settings: any earlier text about the user no longer applies.]
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    I prefer short answers.
    seven: nothing told
    |}]
;;

let%expect_test
    "a session from before it is told the text once, with its next message; \
     without a text it is told nothing"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let info = info t in
  (* As a backend from before the feature: no user info at all. *)
  let agent = make () in
  send agent "one";
  let agent = restart ~user_info:info agent in
  send agent "two";
  [%expect
    {|
    system: nothing about the user
    one: nothing told
    system: nothing about the user
    two: nothing told
    |}];
  Or_error.ok_exn (User_info.set info "I'm Lloyd.");
  let agent = restart ~user_info:info agent in
  send agent "three";
  let agent = restart ~user_info:info agent in
  send agent "four";
  [%expect
    {|
    system: nothing about the user
    three: About you: the model was given what you wrote
    [User info: what the user wrote about themselves in prigh's settings, for every session, is now:
    I'm Lloyd.
    It replaces any earlier text about the user.]
    system: nothing about the user
    four: nothing told
    |}]
;;

let%expect_test
    "written before the first message of an open session: in its system \
     prompt, not also a note"
  =
  with_agents
  @@ fun t ~make ~restart:_ ~send ->
  let info = info t in
  let agent = make ~user_info:info () in
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (User_info.set info "I'm Lloyd.");
  send agent "one";
  [%expect
    {|
    system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    one: nothing told
    |}]
;;

let%expect_test "a subagent's own system prompt has it; the note's wire form" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let info = info t in
  Or_error.ok_exn (User_info.set info "I'm Lloyd.");
  let requests = Queue.create () in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key r -> Queue.enqueue requests (key, r))
      ~main:
        [ Reply.tool_call
            ~id:"c1"
            ~name:"subagent"
            ~arguments:{|{"task":"look around"}|}
            ()
        ; Reply.text "started"
        ; Reply.text "thanks"
        ]
      [ "look around", [ Reply.text "looked" ] ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ [ Tool_subagent.create
               ~provider
               ~user_info:(fun () -> User_info.text info)
               ~current_model:(fun () -> Model.default)
               ~current_thinking:(fun () -> Off)
               ~home:t.dir
               ()
           ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~user_info:info
      ~cwd:t.dir
      ()
  in
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Agent.wait_idle agent;
    Eio.Fiber.yield ()
  done;
  Queue.iter requests ~f:(fun (key, (r : Provider.Request.t)) ->
    if String.equal key "look around"
    then (
      let system = Option.value r.system ~default:"" in
      match String.substr_index system ~pattern:"About the user" with
      | Some i ->
        printf "subagent's system: … %s\n" (String.drop_prefix system i)
      | None -> print_endline "subagent's system: nothing about the user"));
  [%expect
    {|
    subagent's system: … About the user (written by them in prigh's settings; applies to every session):
    I'm Lloyd.
    |}];
  let note =
    { Injected.Note.kind = User_info { cleared = false }
    ; text = System_prompt.user_info_note (Some "I'm Lloyd.")
    }
  in
  print_endline
    (Json.to_string
       (Injected.to_json
          { Injected.empty with notes = [ note ] }
          ~text:(Injected.prefix_notes [ note ] "hello")));
  [%expect
    {| {"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given what you wrote","text":"[User info: what the user wrote about themselves in prigh's settings, for every session, is now:\nI'm Lloyd.\nIt replaces any earlier text about the user.]"}]} |}]
;;
