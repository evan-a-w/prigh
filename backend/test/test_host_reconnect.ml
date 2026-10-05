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
    ; mutable notices : string list (** received, newest first *)
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
    let t = { client; sent; live = true; answer = true; notices = [] } in
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
             ignore (Queue.dequeue_exn sent : Json.t);
             if String.equal (member_string json "event") "notice"
             then t.notices <- member_string json "text" :: t.notices
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

  (* Prints, then forgets, the notices it received. *)
  let print_notices t ~name =
    Queue.filter_inplace t.sent ~f:(fun json ->
      if String.equal (member_string json "event") "notice"
      then (
        t.notices <- member_string json "text" :: t.notices;
        false)
      else true);
    List.iter (List.rev t.notices) ~f:(fun text ->
      printf "%s notice: %s\n" name text);
    t.notices <- []
  ;;

  let agent (h : Test_rpc.H.t) t = Rpc_server.agent_of_client h.server t.client

  (* Sends a request as this client and prints the response. *)
  let call test (h : Test_rpc.H.t) t ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params)
    in
    print_endline
      (mask test (Json.to_string (Rpc_server.handle h.server t.client request)))
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
let prompt ?via t (h : Test_rpc.H.t) agent text =
  let before = List.length (Agent.messages agent) in
  let params = sprintf {|{"text": "%s"}|} text in
  (match via with
   | None -> call t h ~params "prompt"
   | Some host -> Fake_host.call t h host ~params "prompt");
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

(* Each saved session (by first prompt) and whether it is live. *)
let show_sessions (h : Test_rpc.H.t) =
  match
    Json.member
      "result"
      (Rpc_server.handle
         h.server
         h.client
         (Json.of_string {|{"id": "s", "method": "list_sessions"}|}))
  with
  | Some (`Array sessions) ->
    List.iter (List.rev sessions) ~f:(fun s ->
      printf
        "session %S live=%s\n"
        (member_string s "first_prompt")
        (Json.to_string (Option.value_exn (Json.member "live" s))))
  | _ -> print_endline "no sessions"
;;

(* The [cwd] entries of the session's file: where it ran. *)
let show_cwd_entries t agent =
  List.iter
    (In_channel.read_lines (Session.path (Agent.session agent)))
    ~f:(fun line ->
      match Json.of_string line with
      | `Array [ `String "Entry"; entry ] ->
        (match Json.member "payload" entry with
         | Some (`Array [ `String "Cwd"; cwd ]) ->
           print_endline (mask t ("cwd entry: " ^ Json.to_string cwd))
         | _ -> ())
      | _ -> ())
;;

let%expect_test
    "no backend host: an evicted session comes back on its host, not on the \
     one that is always connected"
  =
  Test_rpc.with_agent
    ~backend_host:false
    [ bash "c1"; Reply.text "on desk"; bash "c2"; Reply.text "still on desk" ]
  @@ fun t _ h ->
  Eio.Switch.run
  @@ fun sw ->
  (* The container's tool host is always connected, before anyone else. *)
  let _container =
    Fake_host.connect
      ~sw
      h
      ~name:"container"
      ~host_id:"container-me"
      ~cwd:"/workspace/me"
  in
  let desk =
    Fake_host.connect ~sw h ~name:"desk" ~host_id:"host-desk" ~cwd:"/home/me"
  in
  let session = Fake_host.agent h desk in
  show_state t session;
  Fake_host.call
    t
    h
    desk
    ~params:{|{"host": "host-desk", "cwd": "/home/me/proj"}|}
    "set_active_host";
  prompt ~via:desk t h session "run it";
  show_state t session;
  [%expect
    {|
    container hello: client_id=client-2 host_id=container-me
    desk hello: client_id=client-3 host_id=host-desk
    ((active_host container-me) (cwd /workspace/me)
     (hosts (container-me=/workspace/me host-desk=/home/me)))
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    desk ran bash in /home/me/proj
    tool_result: ran on desk in /home/me/proj
    ((active_host host-desk) (cwd /home/me/proj)
     (hosts (container-me=/workspace/me host-desk=/home/me/proj)))
    |}];
  (* The desktop TUI goes away: its session has no clients and is evicted. *)
  Fake_host.disconnect h desk;
  show_sessions h;
  [%expect {| session "run it" live=false |}];
  (* It comes back with the same host id and resumes its session. *)
  let desk =
    Fake_host.connect
      ~sw
      h
      ~name:"desk"
      ~host_id:"host-desk"
      ~cwd:"/home/me"
      ~session
  in
  let reloaded = Fake_host.agent h desk in
  printf "reloaded: %b\n" (not (phys_equal reloaded session));
  show_state t reloaded;
  prompt ~via:desk t h reloaded "again";
  show_cwd_entries t reloaded;
  [%expect
    {|
    desk hello: client_id=client-4 host_id=host-desk
    reloaded: true
    ((active_host host-desk) (cwd /home/me/proj)
     (hosts (container-me=/workspace/me host-desk=/home/me/proj)))
    {"type":"response","id":"r","ok":true,"result":{}}
    desk ran bash in /home/me/proj
    tool_result: ran on desk in /home/me/proj
    cwd entry: {"cwd":"/workspace/me","host":{"id":"container-me","name":"container","pinned":false}}
    cwd entry: {"cwd":"/home/me/proj","host":{"id":"host-desk","name":"desk","pinned":true}}
    |}]
;;

let%expect_test
    "no backend host: a session that adopted its host keeps it after eviction"
  =
  Test_rpc.with_agent
    ~backend_host:false
    [ Reply.text "hi"; bash "c1"; Reply.text "ok" ]
  @@ fun t _ h ->
  Eio.Switch.run
  @@ fun sw ->
  (* The container's host is restarting when the desktop connects. *)
  let desk =
    Fake_host.connect ~sw h ~name:"desk" ~host_id:"host-desk" ~cwd:"/home/me"
  in
  let session = Fake_host.agent h desk in
  prompt ~via:desk t h session "hello";
  let _container =
    Fake_host.connect
      ~sw
      h
      ~name:"container"
      ~host_id:"container-me"
      ~cwd:"/workspace/me"
  in
  Fake_host.disconnect h desk;
  let desk =
    Fake_host.connect
      ~sw
      h
      ~name:"desk"
      ~host_id:"host-desk"
      ~cwd:"/home/me"
      ~session
  in
  let reloaded = Fake_host.agent h desk in
  show_state t reloaded;
  prompt ~via:desk t h reloaded "again";
  [%expect
    {|
    desk hello: client_id=client-2 host_id=host-desk
    {"type":"response","id":"r","ok":true,"result":{}}
    container hello: client_id=client-3 host_id=container-me
    desk hello: client_id=client-4 host_id=host-desk
    ((active_host host-desk) (cwd /home/me)
     (hosts (container-me=/workspace/me host-desk=/home/me)))
    {"type":"response","id":"r","ok":true,"result":{}}
    desk ran bash in /home/me
    tool_result: ran on desk in /home/me
    |}]
;;

let%expect_test
    "an evicted session whose host is still away waits for it; only \
     set_active_host moves it"
  =
  Test_rpc.with_agent
    ~backend_host:false
    [ Reply.text "hi"
    ; bash "c1"
    ; Reply.text "waiting"
    ; bash "c2"
    ; Reply.text "on container"
    ]
  @@ fun t _ h ->
  Eio.Switch.run
  @@ fun sw ->
  let _container =
    Fake_host.connect
      ~sw
      h
      ~name:"container"
      ~host_id:"container-me"
      ~cwd:"/workspace/me"
  in
  (* The browser's own new session adopted the container. *)
  notices t h;
  let desk =
    Fake_host.connect ~sw h ~name:"desk" ~host_id:"host-desk" ~cwd:"/home/me"
  in
  let session = Fake_host.agent h desk in
  Fake_host.call t h desk ~params:{|{"host": "host-desk"}|} "set_active_host";
  prompt ~via:desk t h session "hello";
  Fake_host.disconnect h desk;
  (* A browser (no tools) opens it, then a laptop TUI attaches to it. *)
  call
    t
    h
    ~params:(sprintf {|{"path": "%s"}|} (Session.id (Agent.session session)))
    "switch_session";
  let reloaded = Test_rpc.current h in
  show_state t reloaded;
  prompt t h reloaded "go";
  let laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
      ~session:reloaded
  in
  show_state t reloaded;
  notices t h;
  [%expect
    {|
    container hello: client_id=client-2 host_id=container-me
    notice: tools now run on container in /workspace/me
    desk hello: client_id=client-3 host_id=host-desk
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    ((active_host host-desk) (cwd /home/me) (hosts (container-me=/workspace/me)))
    {"type":"response","id":"r1","ok":true,"result":{}}
    tool_result: waiting for tool host "desk" to reconnect; /host picks another
    laptop hello: client_id=client-4 host_id=host-laptop
    ((active_host host-desk) (cwd /home/me)
     (hosts (container-me=/workspace/me host-laptop=/home/me)))
    |}];
  call t h ~params:{|{"host": "container-me"}|} "set_active_host";
  prompt t h reloaded "on container";
  show_state t reloaded;
  ignore laptop;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    container ran bash in /workspace/me
    model told: [Environment: the tool host is now container, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]
    tool_result: ran on container in /workspace/me
    ((active_host container-me) (cwd /workspace/me)
     (hosts (container-me=/workspace/me host-laptop=/home/me)))
    |}]
;;

let%expect_test "a backend restart keeps each session's host" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server ~backend_host replies =
    snd
      (Test_rpc.make_server
         t
         ~sw
         ~backend_host
         ~provider:(Faux_provider.create replies))
  in
  (* Before: the desktop's session on the desktop, pinned; another on the
     backend, pinned there with /host. *)
  let h = server ~backend_host:true [ Reply.text "hi"; Reply.text "hi" ] in
  let desk =
    Fake_host.connect ~sw h ~name:"desk" ~host_id:"host-desk" ~cwd:"/home/me"
  in
  let on_desk = Fake_host.agent h desk in
  Fake_host.call
    t
    h
    desk
    ~params:{|{"host": "host-desk", "cwd": "/home/me/proj"}|}
    "set_active_host";
  prompt ~via:desk t h on_desk "desk work";
  let on_backend = Test_rpc.current h in
  call t h ~params:{|{"host": "backend"}|} "set_active_host";
  prompt t h on_backend "backend work";
  Fake_host.disconnect h desk;
  Rpc_server.shutdown h.server;
  [%expect
    {|
    desk hello: client_id=client-2 host_id=host-desk
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    |}];
  (* After: another host connects first; the desktop comes back to its
     session, and attaching to the other one does not take it over. *)
  let h =
    server
      ~backend_host:true
      [ bash "c1"; Reply.text "desk"; bash "c2"; Reply.text "backend" ]
  in
  let _laptop =
    Fake_host.connect
      ~sw
      h
      ~name:"laptop"
      ~host_id:"host-laptop"
      ~cwd:"/home/me"
  in
  let desk =
    Fake_host.connect
      ~sw
      h
      ~name:"desk"
      ~host_id:"host-desk"
      ~cwd:"/home/me"
      ~session:on_desk
  in
  let on_desk = Fake_host.agent h desk in
  show_state t on_desk;
  prompt ~via:desk t h on_desk "desk again";
  Fake_host.call
    t
    h
    desk
    ~params:(sprintf {|{"path": "%s"}|} (Session.id (Agent.session on_backend)))
    "switch_session";
  let on_backend = Fake_host.agent h desk in
  show_state t on_backend;
  prompt ~via:desk t h on_backend "backend again";
  [%expect
    {|
    laptop hello: client_id=client-2 host_id=host-laptop
    desk hello: client_id=client-3 host_id=host-desk
    ((active_host host-desk) (cwd /home/me/proj)
     (hosts (backend=/home/me/proj host-laptop=/home/me host-desk=/home/me/proj)))
    {"type":"response","id":"r","ok":true,"result":{}}
    desk ran bash in /home/me/proj
    tool_result: ran on desk in /home/me/proj
    {"type":"response","id":"r","ok":true,"result":{}}
    ((active_host backend) (cwd $DIR)
     (hosts (backend=$DIR host-laptop=/home/me host-desk=/home/me)))
    {"type":"response","id":"r","ok":true,"result":{}}
    tool_result: $DIR
    |}]
;;

let%expect_test "two frontends on one machine share its host id" =
  Test_rpc.with_agent
    [ Reply.text "hi"
    ; bash "c1"
    ; Reply.text "on b"
    ; bash "c2"
    ; Reply.text "on a"
    ]
  @@ fun t _ h ->
  Eio.Switch.run
  @@ fun sw ->
  let a =
    Fake_host.connect ~sw h ~name:"tui-a" ~host_id:"host-desk" ~cwd:"/a"
  in
  let session = Fake_host.agent h a in
  prompt ~via:a t h session "hello";
  (* A second TUI on the same machine: the newer connection holds the id; the
     older one is told and carries on as its own client id. *)
  let b =
    Fake_host.connect ~sw h ~name:"tui-b" ~host_id:"host-desk" ~cwd:"/b"
  in
  Fake_host.print_notices a ~name:"tui-a";
  show_state t session;
  prompt ~via:a t h session "run";
  [%expect
    {|
    tui-a hello: client_id=client-2 host_id=host-desk
    {"type":"response","id":"r","ok":true,"result":{}}
    tui-b hello: client_id=client-3 host_id=host-desk
    tui-a notice: tools now run on tui-a in /a
    tui-a notice: a newer connection is tool host "host-desk" now; this one is "client-2" until it disconnects
    ((active_host host-desk) (cwd /a)
     (hosts (backend=$DIR client-2=/a host-desk=/a)))
    {"type":"response","id":"r","ok":true,"result":{}}
    tui-b ran bash in /a
    tool_result: ran on tui-b in /a
    |}];
  (* The newer one quits: the older one takes the id back and the sessions
     on it carry on there. *)
  Fake_host.disconnect h b;
  Fake_host.print_notices a ~name:"tui-a";
  show_state t session;
  prompt ~via:a t h session "again";
  [%expect
    {|
    tui-a notice: this connection is tool host "host-desk" again
    ((active_host host-desk) (cwd /a) (hosts (backend=$DIR host-desk=/a)))
    {"type":"response","id":"r","ok":true,"result":{}}
    tui-a ran bash in /a
    tool_result: ran on tui-a in /a
    |}]
;;

let%expect_test
    "no backend host: after a backend restart the desktop's session is still \
     on the desktop"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server replies =
    snd
      (Test_rpc.make_server
         t
         ~sw
         ~backend_host:false
         ~provider:(Faux_provider.create replies))
  in
  let connect_container h =
    Fake_host.connect
      ~sw
      h
      ~name:"container"
      ~host_id:"container-me"
      ~cwd:"/workspace/me"
  in
  let h = server [ Reply.text "hi" ] in
  let _container = connect_container h in
  let desk =
    Fake_host.connect ~sw h ~name:"desk" ~host_id:"host-desk" ~cwd:"/home/me"
  in
  let session = Fake_host.agent h desk in
  Fake_host.call
    t
    h
    desk
    ~params:{|{"host": "host-desk", "cwd": "/home/me/proj"}|}
    "set_active_host";
  prompt ~via:desk t h session "desk work";
  Rpc_server.shutdown h.server;
  let h = server [ bash "c1"; Reply.text "desk" ] in
  let _container = connect_container h in
  let desk =
    Fake_host.connect
      ~sw
      h
      ~name:"desk"
      ~host_id:"host-desk"
      ~cwd:"/home/me"
      ~session
  in
  let session = Fake_host.agent h desk in
  show_state t session;
  prompt ~via:desk t h session "desk again";
  [%expect
    {|
    container hello: client_id=client-2 host_id=container-me
    desk hello: client_id=client-3 host_id=host-desk
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    container hello: client_id=client-2 host_id=container-me
    desk hello: client_id=client-3 host_id=host-desk
    ((active_host host-desk) (cwd /home/me/proj)
     (hosts (container-me=/workspace/me host-desk=/home/me/proj)))
    {"type":"response","id":"r","ok":true,"result":{}}
    desk ran bash in /home/me/proj
    tool_result: ran on desk in /home/me/proj
    |}]
;;
