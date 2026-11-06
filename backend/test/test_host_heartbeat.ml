open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* Tool hosts that go without closing their connection (a network dropped,
   a laptop asleep, a frozen process): the backend hears nothing more from
   them. Those that send heartbeats are given up on after [host_timeout] of
   silence, measured by a mock clock here. *)

let member_string json name =
  match Json.member name json with
  | Some (`String s) -> s
  | _ -> ""
;;

let real_clock t = Eio.Stdenv.clock t.env

(* Waits (in real time) for [f], else says what it gave up on. *)
let eventually t ~what f =
  let deadline = Eio.Time.now (real_clock t) +. 10. in
  let rec go () =
    if f ()
    then ()
    else if Float.( > ) (Eio.Time.now (real_clock t)) deadline
    then printf "timed out waiting for %s\n" what
    else (
      Eio.Time.sleep (real_clock t) 0.005;
      go ())
  in
  go ()
;;

let wait_idle t agent =
  Eio.Fiber.first
    (fun () -> Agent.wait_idle agent)
    (fun () ->
       Eio.Time.sleep (real_clock t) 10.;
       print_endline "the run hangs")
;;

(* A tool host on a connection of JSON lines served by [serve_lines]; it
   answers [tool_exec]s until it is [frozen], then never says anything
   again. *)
module Line_host = struct
  type t =
    { input : string option Eio.Stream.t
    ; mutable frozen : bool
    ; mutable freeze_on : string option
      (** freezes when sent a [tool_exec] of this tool *)
    ; mutable closed : bool (** the server gave up on the connection *)
    ; mutable responses : string list
    ; mutable execs : string list (** the tools it was sent, newest first *)
    }

  let send t line = if not t.frozen then Eio.Stream.add t.input (Some line)

  let answer t json ~name =
    let tool = member_string json "name" in
    let text =
      if String.equal tool "$instructions" then "[]" else "ran on " ^ name
    in
    send
      t
      (Json.to_string
         (`Object
             [ "id", `String "x"
             ; "method", `String "tool_exec_result"
             ; ( "params"
               , `Object
                   [ "exec_id", `String (member_string json "exec_id")
                   ; "text", `String text
                   ] )
             ]))
  ;;

  let connect ?freeze_on ~sw (h : Test_rpc.H.t) ~session ~name =
    let input = Eio.Stream.create Int.max_value in
    let output = Eio.Stream.create Int.max_value in
    let t =
      { input
      ; frozen = false
      ; freeze_on
      ; closed = false
      ; responses = []
      ; execs = []
      }
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      ignore
        (Rpc_server.serve_lines
           h.server
           ~read_line:(fun () -> Eio.Stream.take input)
           ~write_line:(fun line -> Eio.Stream.add output line)
         : _ option);
      t.closed <- true;
      `Stop_daemon);
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let json = Json.of_string (Eio.Stream.take output) in
        match member_string json "type", member_string json "event" with
        | "response", _ -> t.responses <- member_string json "id" :: t.responses
        | "event", "tool_exec" ->
          let tool = member_string json "name" in
          t.execs <- tool :: t.execs;
          if Option.equal String.equal t.freeze_on (Some tool)
          then t.frozen <- true;
          if not t.frozen then answer t json ~name
        | _ -> ()
      done;
      `Stop_daemon);
    send
      t
      (sprintf
         {|{"id": "hello", "method": "hello", "params": {"name": "%s", "cwd": "/home/me", "tools": true, "host_id": "host-%s", "features": ["heartbeat"], "session": "%s"}}|}
         name
         name
         (Session.id (Agent.session session)));
    t
  ;;

  let answered t id = List.mem t.responses id ~equal:String.equal
end

let show_hosts t agent =
  let state = Agent.state agent in
  print_endline
    (mask_sexp
       t
       [%message
         ""
           ~active_host:(state.active_host : string)
           ~active_host_name:(state.active_host_name : string option)
           ~hosts:(List.map state.hosts ~f:(fun h -> h.name) : string list)])
;;

let tool_results t agent =
  List.iter (Agent.messages agent) ~f:(function
    | Message.Tool_result r -> print_endline (mask t ("tool_result: " ^ r.text))
    | _ -> ())
;;

(* The messages in the session's file, as a new backend would load them. *)
let saved t agent =
  match Session.load (Session.path (Agent.session agent)) with
  | Error e -> print_endline ("not saved: " ^ Error.to_string_hum e)
  | Ok session ->
    List.iter (Session.messages session) ~f:(fun m ->
      print_endline
        (mask
           t
           (match m with
            | Message.User u -> "saved user: " ^ u.text
            | Assistant a ->
              "saved assistant: "
              ^ Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)]
            | Tool_result r -> "saved tool_result: " ^ r.text)))
;;

let bash id =
  Reply.tool_call ~id ~name:"bash" ~arguments:{|{"command":"pwd"}|} ()
;;

let with_server replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio_mock.Clock.make () in
  let agent, h =
    Test_rpc.make_server
      ~host_timeout:30.
      ~heartbeat_clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
      t
      ~sw
      ~provider:(Faux_provider.create replies)
  in
  f t ~sw ~clock agent h
;;

let%expect_test
    "a silent host is given up on: its call fails, the turn ends, it comes back"
  =
  with_server [ bash "c1"; Reply.text "it went"; bash "c2"; Reply.text "back" ]
  @@ fun t ~sw ~clock agent h ->
  let laptop =
    Line_host.connect ~sw h ~session:agent ~name:"laptop" ~freeze_on:"bash"
  in
  eventually t ~what:"hello" (fun () -> Line_host.answered laptop "hello");
  show_hosts t agent;
  [%expect
    {|
    ((active_host host-laptop) (active_host_name (laptop))
     (hosts (<host> laptop)))
    |}];
  (* Heartbeats keep it: silent for less than 30s at a time. *)
  Eio_mock.Clock.set_time clock 20.;
  Line_host.send laptop {|{"id": "hb", "method": "ping"}|};
  eventually t ~what:"pong" (fun () -> Line_host.answered laptop "hb");
  Test_rpc.call t h ~params:{|{"text": "go"}|} "prompt";
  eventually t ~what:"bash" (fun () ->
    List.mem laptop.execs "bash" ~equal:String.equal);
  (* It froze while running the call: at 45s it was last heard 25s ago. *)
  Eio_mock.Clock.set_time clock 45.;
  Eio.Fiber.yield ();
  printf "running=%b closed=%b\n" (Agent.is_running agent) laptop.closed;
  [%expect
    {|
    +mock time is now 20
    {"type":"response","id":"r1","ok":true,"result":{}}
    +mock time is now 45
    running=true closed=false
    |}];
  Eio_mock.Clock.set_time clock 50.;
  wait_idle t agent;
  eventually t ~what:"the connection to close" (fun () -> laptop.closed);
  tool_results t agent;
  show_hosts t agent;
  saved t agent;
  [%expect
    {|
    +mock time is now 50
    prigh: closing the connection of laptop: no reply for 30s
    tool_result: [the tool host "laptop" stopped responding (no reply for 30s), so it was disconnected and this call's result is lost: pick another host with /host, or wait for it to reconnect and run it again]
    ((active_host host-laptop) (active_host_name (laptop)) (hosts (<host>)))
    saved user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    go
    saved assistant: Tool_use
    saved tool_result: [the tool host "laptop" stopped responding (no reply for 30s), so it was disconnected and this call's result is lost: pick another host with /host, or wait for it to reconnect and run it again]
    saved assistant: End_turn
    |}];
  (* It reconnects (as [prigh tool-host] does): the session runs on it again. *)
  let laptop = Line_host.connect ~sw h ~session:agent ~name:"laptop" in
  eventually t ~what:"hello" (fun () -> Line_host.answered laptop "hello");
  show_hosts t agent;
  Test_rpc.call t h ~params:{|{"text": "again"}|} "prompt";
  wait_idle t agent;
  tool_results t agent;
  [%expect
    {|
    ((active_host host-laptop) (active_host_name (laptop))
     (hosts (<host> laptop)))
    {"type":"response","id":"r1","ok":true,"result":{}}
    tool_result: [the tool host "laptop" stopped responding (no reply for 30s), so it was disconnected and this call's result is lost: pick another host with /host, or wait for it to reconnect and run it again]
    tool_result: ran on laptop
    |}]
;;

let%expect_test "hosts that do not send heartbeats are not timed out" =
  with_server []
  @@ fun t ~sw ~clock agent h ->
  let input = Eio.Stream.create Int.max_value in
  let closed = ref false in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    ignore
      (Rpc_server.serve_lines
         h.server
         ~read_line:(fun () -> Eio.Stream.take input)
         ~write_line:ignore
       : _ option);
    closed := true;
    `Stop_daemon);
  Eio.Stream.add
    input
    (Some
       (sprintf
          {|{"id": "hello", "method": "hello", "params": {"name": "old", "cwd": "/", "tools": true, "session": "%s"}}|}
          (Session.id (Agent.session agent))));
  eventually t ~what:"hello" (fun () -> List.length (Agent.hosts agent) = 2);
  Eio_mock.Clock.set_time clock 1000.;
  Eio.Fiber.yield ();
  printf "closed=%b\n" !closed;
  show_hosts t agent;
  [%expect
    {|
    +mock time is now 1000
    closed=false
    ((active_host client-2) (active_host_name (old)) (hosts (<host> old)))
    |}]
;;

(* The user's message is saved before the run asks the host for anything:
   a host that never answers the first question (its instructions files)
   used to keep the session from being written at all, so it was lost
   when the user went to another one. *)
let%expect_test
    "the session is saved while its host does not answer; abort ends the wait"
  =
  with_server [ Reply.text "never" ]
  @@ fun t ~sw ~clock:_ agent h ->
  let laptop =
    Line_host.connect
      ~sw
      h
      ~session:agent
      ~name:"laptop"
      ~freeze_on:"$instructions"
  in
  eventually t ~what:"hello" (fun () -> Line_host.answered laptop "hello");
  Test_rpc.call t h ~params:{|{"text": "remember me"}|} "prompt";
  eventually t ~what:"instructions" (fun () ->
    List.mem laptop.execs "$instructions" ~equal:String.equal);
  printf "running=%b\n" (Agent.is_running agent);
  saved t agent;
  (* Another session, then the listing has this one. *)
  let handle meth =
    Json.to_string
      (Rpc_server.handle
         h.server
         h.client
         (Json.of_string (sprintf {|{"id": "r", "method": "%s"}|} meth)))
  in
  ignore (handle "new_session" : string);
  let listed =
    String.is_substring
      (handle "list_sessions")
      ~substring:(Session.id (Agent.session agent))
  in
  printf "listed=%b\n" listed;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    running=true
    saved user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    remember me
    listed=true
    |}];
  ignore (Agent.abort agent : string list);
  wait_idle t agent;
  printf "running=%b\n" (Agent.is_running agent);
  saved t agent;
  [%expect
    {|
    running=false
    saved user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    remember me
    saved assistant: Aborted
    |}]
;;

(* The other way round: a backend that accepts the connection and then never
   says a word (a frozen process, or one behind a dead network). The tool
   host gives up on it and connects again. *)
let%expect_test "a tool host gives up on a silent backend and reconnects" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:4
      ~reuse_addr:true
      (Eio.Stdenv.net t.env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let heard = Queue.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    while true do
      let flow, _ = Eio.Net.accept ~sw socket in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
        (try
           while true do
             Queue.enqueue
               heard
               (member_string
                  (Json.of_string (Eio.Buf_read.line reader))
                  "method")
           done
         with
         | _ -> ());
        `Stop_daemon)
    done;
    `Stop_daemon);
  let logs = Queue.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    ignore
      (Tool_host.connect
         ~env:t.env
         ~log:(fun line ->
           Queue.enqueue
             logs
             (String.substr_replace_all
                line
                ~pattern:(Int.to_string port)
                ~with_:"PORT"))
         ~initial_backoff:(Time_ns.Span.of_sec 0.01)
         ~heartbeat_interval:0.02
         ~heartbeat_timeout:0.1
         ~backend:
           (Or_error.ok_exn
              (Backend_address.parse (sprintf "127.0.0.1:%d" port)))
         ~token:None
         ~name:"box"
         ~cwd:t.dir
         ()
       : Error.t);
    `Stop_daemon);
  let gave_up () = Queue.count logs ~f:(String.is_prefix ~prefix:"no reply") in
  eventually t ~what:"two give-ups" (fun () -> gave_up () >= 2);
  List.iter (List.take (Queue.to_list logs) 4) ~f:print_endline;
  (* It said hello, then pinged while it waited for the answer. *)
  let methods = Queue.to_list heard in
  printf
    "first: %s; pings: %b\n"
    (List.hd_exn methods)
    (List.mem methods "ping" ~equal:String.equal);
  [%expect
    {|
    no reply from 127.0.0.1:PORT for 0.1s: giving up on the connection
    retrying in 10ms
    no reply from 127.0.0.1:PORT for 0.1s: giving up on the connection
    retrying in 20ms
    first: hello; pings: true
    |}]
;;
