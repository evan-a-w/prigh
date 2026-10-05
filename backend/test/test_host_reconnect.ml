open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* Sessions keep their tool host across its disconnects: hosts are known by
   the [host_id] they send in [hello], not by their connection. *)

let member_string json name =
  match Json.member name json with
  | Some (`String s) -> s
  | _ -> ""
;;

(* A tool-host client played by hand: it answers its [tool_exec]s from a
   daemon fiber while [live] ([$resolve_dir] resolves against the exec's cwd,
   [$instructions] has no files, anything else prints where it ran). *)
module Fake_host = struct
  type t =
    { client : Rpc_server.Client.t
    ; sent : Json.t Queue.t
    ; mutable live : bool
    ; mutable answer : bool (** false: leave execs pending *)
    }

  let answer_exec (h : Test_rpc.H.t) t ~name json =
    let cwd = member_string json "cwd" in
    let text =
      match member_string json "name" with
      | "$instructions" -> "[]"
      | "$resolve_dir" ->
        let path =
          Option.value_map
            (Json.member "arguments" json)
            ~default:""
            ~f:(fun a -> member_string a "path")
        in
        if Filename.is_absolute path then path else Filename.concat cwd path
      | tool ->
        printf "%s ran %s in %s\n" name tool cwd;
        sprintf "ran on %s in %s" name cwd
    in
    ignore
      (Rpc_server.handle
         h.server
         t.client
         (`Object
             [ "id", `Number "0"
             ; "method", `String "tool_exec_result"
             ; ( "params"
               , `Object
                   [ "exec_id", `String (member_string json "exec_id")
                   ; "text", `String text
                   ] )
             ])
       : Json.t)
  ;;

  (* Prints the hello's ids (or error). *)
  let connect
        ?host_id
        ?session
        ?(tools = true)
        ~sw
        (h : Test_rpc.H.t)
        ~name
        ~cwd
    =
    let sent = Queue.create () in
    let client = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
    let t = { client; sent; live = true; answer = true } in
    let params =
      `Object
        ([ "name", `String name
         ; "cwd", `String cwd
         ; ("tools", if tools then `True else `False)
         ]
         @ Option.value_map host_id ~default:[] ~f:(fun id ->
           [ "host_id", `String id ])
         @ Option.value_map session ~default:[] ~f:(fun agent ->
           [ "session", `String (Session.id (Agent.session agent)) ]))
    in
    let reply =
      Rpc_server.handle
        h.server
        client
        (`Object
            [ "id", `Number "1"; "method", `String "hello"; "params", params ])
    in
    (match Json.member "result" reply with
     | Some result ->
       printf
         "%s hello: client_id=%s host_id=%s\n"
         name
         (member_string result "client_id")
         (member_string result "host_id")
     | None -> printf "%s hello: %s\n" name (member_string reply "error"));
    Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        if not t.live
        then `Stop_daemon
        else (
          (match Queue.peek sent with
           | Some json
             when t.answer
                  && String.equal (member_string json "event") "tool_exec" ->
             ignore (Queue.dequeue_exn sent : Json.t);
             answer_exec h t ~name json
           | Some json
             when not (String.equal (member_string json "event") "tool_exec") ->
             ignore (Queue.dequeue_exn sent : Json.t)
           | _ -> ());
          Eio.Fiber.yield ();
          loop ())
      in
      loop ());
    t
  ;;

  let disconnect (h : Test_rpc.H.t) t =
    t.live <- false;
    Rpc_server.disconnect h.server t.client
  ;;
end

let show_state t agent =
  let state = Agent.state agent in
  print_endline
    (mask_sexp
       t
       [%message
         ""
           ~active_host:(state.active_host : string)
           ~cwd:(state.cwd : string)
           ~hosts:
             (List.map state.hosts ~f:(fun h -> h.id ^ "=" ^ h.cwd)
              : string list)])
;;

(* The notices pushed to the main client since last time. *)
let notices t (h : Test_rpc.H.t) =
  Queue.iter h.sent ~f:(fun json ->
    if String.equal (member_string json "event") "notice"
    then print_endline (mask t ("notice: " ^ member_string json "text")));
  Queue.clear h.sent
;;

let call t (h : Test_rpc.H.t) ?params meth = Test_rpc.call t h ?params meth

(* Runs a prompt to the end, then prints its tool results and whether the
   model was told about an environment change. *)
let prompt t (h : Test_rpc.H.t) agent text =
  let before = List.length (Agent.messages agent) in
  call t h ~params:(sprintf {|{"text": "%s"}|} text) "prompt";
  Agent.wait_idle agent;
  List.iter
    (List.drop (Agent.messages agent) before)
    ~f:(function
      | Message.User { text; _ } ->
        if String.is_prefix text ~prefix:"[Environment"
        then
          print_endline
            (mask t ("model told: " ^ List.hd_exn (String.split_lines text)))
      | Tool_result r -> print_endline (mask t ("tool_result: " ^ r.text))
      | Assistant _ -> ())
;;

let bash id =
  Reply.tool_call ~id ~name:"bash" ~arguments:{|{"command":"pwd"}|} ()
;;

let%expect_test "a session waits for its disconnected host and resumes on it" =
  Test_rpc.with_agent
    [ bash "c1"
    ; Reply.text "done"
    ; bash "c2"
    ; Reply.text "offline"
    ; bash "c3"
    ; Reply.text "back"
    ; bash "c4"
    ; Reply.text "on desk"
    ]
  @@ fun t agent h ->
  Eio.Switch.run
  @@ fun sw ->
  let laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
      ~session:agent
  in
  call t h ~params:{|{"path": "proj"}|} "set_cwd";
  show_state t agent;
  notices t h;
  prompt t h agent "run it";
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    {"type":"response","id":"r1","ok":true,"result":{}}
    ((active_host host-laptop) (cwd /home/me/proj)
     (hosts (backend=$DIR host-laptop=/home/me/proj)))
    notice: tools now run on laptop in /home/me
    {"type":"response","id":"r1","ok":true,"result":{}}
    laptop ran bash in /home/me/proj
    tool_result: ran on laptop in /home/me/proj
    |}];
  (* The host goes away: the session stays on it, even when another host
     attaches to the session. *)
  Fake_host.disconnect h laptop;
  show_state t agent;
  notices t h;
  let desk =
    Fake_host.connect
      ~sw
      h
      ~name:"desk"
      ~host_id:"host-desk"
      ~cwd:"/srv"
      ~session:agent
  in
  show_state t agent;
  notices t h;
  prompt t h agent "offline";
  call t h ~params:{|{"path": "/"}|} "set_cwd";
  call t h ~params:{|{"prefix": ""}|} "list_dirs";
  call t h ~params:{|{"host": "host-laptop"}|} "set_active_host";
  [%expect
    {|
    ((active_host host-laptop) (cwd /home/me/proj) (hosts (backend=$DIR)))
    notice: waiting for tool host "laptop" to reconnect; /host picks another
    desk hello: client_id=client-3 host_id=host-desk
    ((active_host host-laptop) (cwd /home/me/proj)
     (hosts (backend=$DIR host-desk=/srv)))
    {"type":"response","id":"r1","ok":true,"result":{}}
    tool_result: waiting for tool host "laptop" to reconnect; /host picks another
    {"type":"response","id":"r1","ok":false,"error":"waiting for tool host \"laptop\" to reconnect; /host picks another"}
    {"type":"response","id":"r1","ok":false,"error":"waiting for tool host \"laptop\" to reconnect; /host picks another"}
    {"type":"response","id":"r1","ok":false,"error":"waiting for tool host \"laptop\" to reconnect; /host picks another"}
    |}];
  (* It reconnects (attached to a session of its own): the session resumes
     on it in the same directory, without telling the model anything. *)
  let laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
  in
  show_state t agent;
  notices t h;
  prompt t h agent "back";
  [%expect
    {|
    laptop hello: client_id=client-4 host_id=host-laptop
    ((active_host host-laptop) (cwd /home/me/proj)
     (hosts (backend=$DIR host-desk=/srv host-laptop=/home/me/proj)))
    notice: tools run on laptop again in /home/me/proj
    {"type":"response","id":"r1","ok":true,"result":{}}
    laptop ran bash in /home/me/proj
    tool_result: ran on laptop in /home/me/proj
    |}];
  (* Only an explicit choice moves the session off a disconnected host. *)
  Fake_host.disconnect h laptop;
  call t h ~params:{|{"host": "host-desk"}|} "set_active_host";
  show_state t agent;
  notices t h;
  prompt t h agent "on desk";
  let _laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
      ~session:agent
  in
  show_state t agent;
  notices t h;
  ignore desk;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    ((active_host host-desk) (cwd /srv) (hosts (backend=$DIR host-desk=/srv)))
    notice: waiting for tool host "laptop" to reconnect; /host picks another
    notice: tools now run on desk in /srv
    {"type":"response","id":"r1","ok":true,"result":{}}
    desk ran bash in /srv
    model told: [Environment: the tool host is now desk, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]
    tool_result: ran on desk in /srv
    laptop hello: client_id=client-5 host_id=host-laptop
    ((active_host host-desk) (cwd /srv)
     (hosts (backend=$DIR host-desk=/srv host-laptop=/home/me/proj)))
    |}]
;;

let%expect_test
    "a host that was its session's only client resumes it after eviction"
  =
  Test_rpc.with_agent [ Reply.text "hi" ]
  @@ fun t _ h ->
  Eio.Switch.run
  @@ fun sw ->
  let tui =
    Fake_host.connect ~sw h ~name:"tui" ~host_id:"host-tui" ~cwd:"/home/me"
  in
  let session = Rpc_server.agent_of_client h.server tui.client in
  let tui_call meth params =
    print_endline
      (mask
         t
         (Json.to_string
            (Rpc_server.handle
               h.server
               tui.client
               (Json.of_string
                  (sprintf
                     {|{"id": "r", "method": "%s", "params": %s}|}
                     meth
                     params)))))
  in
  tui_call "set_cwd" {|{"path": "proj"}|};
  tui_call "prompt" {|{"text": "hello"}|};
  Agent.wait_idle session;
  show_state t session;
  Fake_host.disconnect h tui;
  (* The session is no longer live; the TUI reconnects to it, with its
     startup directory in hello. *)
  call t h "list_sessions";
  let tui =
    Fake_host.connect
      ~sw
      h
      ~name:"tui"
      ~host_id:"host-tui"
      ~cwd:"/home/me"
      ~session
  in
  let reloaded = Rpc_server.agent_of_client h.server tui.client in
  printf "reloaded: %b\n" (not (phys_equal reloaded session));
  show_state t reloaded;
  [%expect
    {|
    tui hello: client_id=client-2 host_id=host-tui
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    ((active_host host-tui) (cwd /home/me/proj)
     (hosts (backend=$DIR host-tui=/home/me/proj)))
    {"type":"response","id":"r1","ok":true,"result":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"description":null,"cwd":"/home/me/proj","created_at":"<time>","updated_at":"<time>","first_prompt":"hello","message_count":2,"parent":null,"live":false}]}
    tui hello: client_id=client-3 host_id=host-tui
    reloaded: true
    ((active_host host-tui) (cwd /home/me/proj)
     (hosts (backend=/home/me/proj host-tui=/home/me/proj)))
    |}]
;;

let%expect_test "host ids: a newer connection takes the id over; reserved ids" =
  Test_rpc.with_agent
    [ bash "c1"; Reply.text "failed"; bash "c2"; Reply.text "ok" ]
  @@ fun t agent h ->
  Eio.Switch.run
  @@ fun sw ->
  let old =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
      ~session:agent
  in
  old.answer <- false;
  (* A call in flight on the old connection when the host comes back on a
     new one (before the backend noticed the old one died). *)
  call t h ~params:{|{"text": "go"}|} "prompt";
  let rec wait_exec () =
    if
      Queue.exists old.sent ~f:(fun json ->
        String.equal (member_string json "name") "bash")
    then ()
    else (
      (match Queue.peek old.sent with
       | Some json when String.equal (member_string json "name") "$instructions"
         ->
         ignore (Queue.dequeue_exn old.sent : Json.t);
         Fake_host.answer_exec h old ~name:"laptop" json
       | _ -> ());
      Eio.Fiber.yield ();
      wait_exec ())
  in
  wait_exec ();
  let _new =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
  in
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(function
    | Tool_result r -> print_endline ("tool_result: " ^ r.text)
    | _ -> ());
  show_state t agent;
  prompt t h agent "again";
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    {"type":"response","id":"r1","ok":true,"result":{}}
    laptop hello: client_id=client-3 host_id=host-laptop
    tool_result: [tool host reconnected]
    ((active_host host-laptop) (cwd /home/me)
     (hosts (backend=$DIR client-2=/home/me host-laptop=/home/me)))
    {"type":"response","id":"r1","ok":true,"result":{}}
    laptop ran bash in /home/me
    tool_result: ran on laptop in /home/me
    |}];
  (* Ids that would clash with the backend's or a client's own are refused;
     a client without tools keeps its client id. *)
  let _ = Fake_host.connect ~sw h ~name:"x" ~host_id:"backend" ~cwd:"/" in
  let _ = Fake_host.connect ~sw h ~name:"y" ~host_id:"client-1" ~cwd:"/" in
  let _ = Fake_host.connect ~sw h ~name:"z" ~host_id:"" ~cwd:"/" in
  let _ =
    Fake_host.connect
      ~sw
      h
      ~name:"web"
      ~tools:false
      ~host_id:"host-web"
      ~cwd:"/"
  in
  let _ = Fake_host.connect ~sw h ~name:"old" ~cwd:"/" in
  [%expect
    {|
    x hello: param "host_id": "backend" is reserved
    y hello: param "host_id": "client-1" is reserved
    z hello: param "host_id": "" is reserved
    web hello: client_id=client-7 host_id=client-7
    old hello: client_id=client-8 host_id=client-8
    |}]
;;
