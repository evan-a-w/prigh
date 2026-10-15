open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf
module Fiber = Eio.Fiber

(* How connections hold up when their peers do not: a peer that stops
   reading, a write that fails, a connection that closes with requests in
   flight, the same page connecting again, a socket gone quiet. *)

let make_server t ~sw ~provider =
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
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  Rpc_server.create ~env:t.env ~sw ~login ~sessions_dir ~cwd:t.dir ~new_agent ()
;;

let sleep t seconds = Eio.Time.sleep (Eio.Stdenv.clock t.env) seconds

(* A connection fed from the test, serving in its own fiber. *)
module Conn = struct
  type t =
    { input : string option Eio.Stream.t
    ; responses : Json.t Queue.t
    ; mutable ended : bool
    }

  let start
        ?finish_requests
        ?(write : [ `Record | `Never | `Fail ] = `Record)
        server
        ~sw
    =
    let c =
      { input = Eio.Stream.create 20_000
      ; responses = Queue.create ()
      ; ended = false
      }
    in
    Fiber.fork ~sw (fun () ->
      ignore
        (Rpc_server.serve_lines
           ?finish_requests
           server
           ~read_line:(fun () -> Eio.Stream.take c.input)
           ~write_line:(fun line ->
             match write with
             | `Record ->
               let json = Json.of_string line in
               (match Json.member "type" json with
                | Some (`String "response") -> Queue.enqueue c.responses json
                | _ -> ())
             | `Never -> Fiber.await_cancel ()
             | `Fail -> failwith "connection reset by peer")
         : (Json.t * string) option);
      c.ended <- true);
    c
  ;;

  let send c line = Eio.Stream.add c.input (Some line)
  let close c = Eio.Stream.add c.input None

  let rec response t c id =
    match
      Queue.find c.responses ~f:(fun r ->
        String.equal
          (Json.to_string (Option.value (Json.member "id" r) ~default:`Null))
          (Json.to_string id))
    with
    | Some r -> r
    | None ->
      sleep t 0.01;
      response t c id
  ;;

  let ok t c id =
    let r = response t c (`Number (Int.to_string id)) in
    match Json.member "ok" r with
    | Some `True -> "ok"
    | _ ->
      Option.value_map (Json.member "error" r) ~default:"?" ~f:Json.to_string
  ;;
end

let session_id_of t (c : Conn.t) id =
  match
    Json.member "result" (Conn.response t c (`Number (Int.to_string id)))
  with
  | Some result ->
    (match
       Option.bind (Json.member "state" result) ~f:(Json.member "session_id")
     with
     | Some (`String id) -> id
     | _ -> failwith "no session id")
  | None -> failwith "no result"
;;

let rec wait_until t f =
  if not (f ())
  then (
    sleep t 0.01;
    wait_until t f)
;;

let many_deltas n =
  { Reply.events = List.init n ~f:(fun _ -> Assistant_event.Text_delta "x")
  ; stop_reason = End_turn
  ; usage = Usage.zero
  }
;;

(* A laptop that went to sleep, a phone gone from the network behind a proxy
   that keeps the socket open: nothing it is sent is taken. The session it
   watched keeps running, and so do the others, and the connection ends once
   it is hopelessly behind. *)
let%expect_test "a peer that stops reading holds nobody up" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server =
    make_server t ~sw ~provider:(Faux_provider.create [ many_deltas 3_000 ])
  in
  let watcher = Conn.start server ~sw in
  Conn.send watcher {|{"id": 1, "method": "hello", "params": {}}|};
  let session = session_id_of t watcher 1 in
  let frozen = Conn.start server ~sw ~write:`Never in
  Conn.send
    frozen
    (sprintf
       {|{"id": 1, "method": "hello", "params": {"session": "%s"}}|}
       session);
  Conn.send watcher {|{"id": 2, "method": "prompt", "params": {"text": "go"}}|};
  print_endline (Conn.ok t watcher 2);
  let rec wait_for_the_run id =
    Conn.send watcher (sprintf {|{"id": %d, "method": "get_state"}|} id);
    match
      Option.bind
        (Json.member
           "result"
           (Conn.response t watcher (`Number (Int.to_string id))))
        ~f:(Json.member "running")
    with
    | Some `False -> ()
    | _ ->
      sleep t 0.05;
      wait_for_the_run (id + 1)
  in
  wait_for_the_run 100;
  print_endline "the run finished";
  (* Hopelessly behind: what waits for it is dropped and it ends. *)
  for id = 2 to 10_002 do
    Conn.send frozen (sprintf {|{"id": %d, "method": "ping"}|} id)
  done;
  wait_until t (fun () -> frozen.ended);
  print_endline "the frozen connection ended";
  (* Everyone is served: a rename (heard by every client), another prompt. *)
  Conn.send
    watcher
    {|{"id": 3, "method": "set_session_name", "params": {"name": "renamed"}}|};
  print_endline (Conn.ok t watcher 3);
  let other = Conn.start server ~sw in
  Conn.send other {|{"id": 1, "method": "hello", "params": {}}|};
  print_endline (Conn.ok t other 1);
  Conn.send other {|{"id": 2, "method": "list_sessions"}|};
  print_endline (Conn.ok t other 2);
  Conn.close watcher;
  Conn.close other;
  Rpc_server.shutdown server;
  [%expect
    {|
    ok
    the run finished
    prigh: closing the connection of client-2: the peer is not reading what is sent to it (10001 messages waiting)
    the frozen connection ended
    ok
    ok
    ok
    |}]
;;

let%expect_test "a failed write ends the connection" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~provider:(Faux_provider.create []) in
  let broken = Conn.start server ~sw ~write:`Fail in
  Conn.send broken {|{"id": 1, "method": "ping"}|};
  (* Its reader still waits for input: the failed write alone ends it. *)
  wait_until t (fun () -> broken.ended);
  print_endline "ended";
  let other = Conn.start server ~sw in
  Conn.send other {|{"id": 1, "method": "ping"}|};
  print_endline (Conn.ok t other 1);
  Conn.close other;
  [%expect
    {|
    prigh: closing the connection of client-1: cannot write: (Failure "connection reset by peer")
    ended
    ok
    |}]
;;

let%expect_test "a closed connection's requests are cancelled; stdio's finish" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~provider:(Faux_provider.create []) in
  let started = Time_float.now () in
  let gone = Conn.start server ~sw in
  Conn.send
    gone
    {|{"id": 1, "method": "shell", "params": {"command": "sleep 30"}}|};
  sleep t 0.2;
  Conn.close gone;
  wait_until t (fun () -> gone.ended);
  printf
    "ended without waiting for the command: %b\n"
    Time_float.(Span.( < ) (diff (now ()) started) (Span.of_sec 10.));
  let stdio = Conn.start server ~sw ~finish_requests:true in
  Conn.send
    stdio
    {|{"id": 1, "method": "shell", "params": {"command": "sleep 0.3; echo done"}}|};
  Conn.close stdio;
  wait_until t (fun () -> stdio.ended);
  print_endline (Conn.ok t stdio 1);
  [%expect
    {|
    ended without waiting for the command: true
    ok
    |}]
;;

let%expect_test "a slow request does not hold up the others" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~provider:(Faux_provider.create []) in
  let slow = Conn.start server ~sw in
  let quick = Conn.start server ~sw in
  Conn.send
    slow
    {|{"id": 1, "method": "shell", "params": {"command": "sleep 1"}}|};
  sleep t 0.1;
  Conn.send quick {|{"id": 1, "method": "ping"}|};
  Conn.send slow {|{"id": 2, "method": "get_state"}|};
  print_endline ("other connection's ping: " ^ Conn.ok t quick 1);
  print_endline ("same connection's get_state: " ^ Conn.ok t slow 2);
  printf "the slow one still running: %b\n" (Queue.length slow.responses = 1);
  print_endline ("then the slow one: " ^ Conn.ok t slow 1);
  Conn.close slow;
  Conn.close quick;
  [%expect
    {|
    other connection's ping: ok
    same connection's get_state: ok
    the slow one still running: true
    then the slow one: ok
    |}]
;;

(* A page whose network changed reconnects before the backend notices that
   its older connection is gone: that one ends at once. *)
let%expect_test "a newer connection from the same page takes over" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~provider:(Faux_provider.create []) in
  let first = Conn.start server ~sw in
  Conn.send first {|{"id": 1, "method": "hello", "params": {"tab": "page-1"}}|};
  print_endline (Conn.ok t first 1);
  let elsewhere = Conn.start server ~sw in
  Conn.send
    elsewhere
    {|{"id": 1, "method": "hello", "params": {"tab": "page-2"}}|};
  print_endline (Conn.ok t elsewhere 1);
  let second = Conn.start server ~sw in
  Conn.send second {|{"id": 1, "method": "hello", "params": {"tab": "page-1"}}|};
  print_endline (Conn.ok t second 1);
  wait_until t (fun () -> first.ended);
  printf
    "first ended: %b, second ended: %b, other page ended: %b\n"
    first.ended
    second.ended
    elsewhere.ended;
  Conn.close second;
  Conn.close elsewhere;
  [%expect
    {|
    ok
    ok
    prigh: closing the connection of client-1: a newer connection from the same page took over
    ok
    first ended: true, second ended: false, other page ended: false
    |}]
;;

(* Two pages opening the same saved session at once (its file is parsed
   yielding to other fibers) end up in the same live session. *)
let%expect_test "opening a saved session twice at once loads it once" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~provider:(Faux_provider.create []) in
  let session =
    Session.create ~dir:(Filename.concat t.dir "sessions") ~cwd:t.dir ()
  in
  ignore (Session.append_message session (Message.user "hi") : Session.Entry.t);
  let open_it () =
    let c = Conn.start server ~sw in
    Conn.send
      c
      (sprintf
         {|{"id": 1, "method": "hello", "params": {"session": "%s"}}|}
         (Session.id session));
    c
  in
  let a = open_it () in
  let b = open_it () in
  print_endline (Conn.ok t a 1);
  print_endline (Conn.ok t b 1);
  Conn.send a {|{"id": 2, "method": "list_sessions"}|};
  (match Json.member "result" (Conn.response t a (`Number "2")) with
   | Some (`Array [ listed ]) ->
     print_endline
       (Json.to_string
          (`Object
              [ "live", Option.value (Json.member "live" listed) ~default:`Null
              ; ( "clients"
                , Option.value (Json.member "clients" listed) ~default:`Null )
              ]))
   | _ -> print_endline "unexpected list");
  Conn.close a;
  Conn.close b;
  [%expect
    {|
    ok
    ok
    {"live":true,"clients":2}
    |}]
;;

(* The file of a live session grows with every message: listing sums it up
   from memory rather than parsing it again. *)
let%expect_test "listing sums up live sessions from memory" =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir "sessions" in
  let live = Session.create ~dir ~cwd:t.dir () in
  let saved = Session.create ~dir ~cwd:t.dir () in
  List.iter [ live; saved ] ~f:(fun s ->
    ignore (Session.append_message s (Message.user "hi") : Session.Entry.t));
  let list () =
    Session.list_with
      ~live:(fun path ->
        Option.some_if (String.equal path (Session.path live)) live)
      ~load:(fun path ->
        printf
          "parsed the %s one\n"
          (if String.equal path (Session.path saved) then "saved" else "live");
        Session.load path)
      ~dir
    |> List.map ~f:(fun (s : Session.Summary.t) -> s.message_count)
    |> List.sort ~compare:Int.compare
  in
  print_s [%sexp (list () : int list)];
  ignore (Session.append_message live (Message.user "again") : Session.Entry.t);
  print_s [%sexp (list () : int list)];
  [%expect
    {|
    parsed the saved one
    (1 1)
    (1 2)
    |}]
;;

(* Over a socket pair: the server's end of a WebSocket with a keepalive, the
   other end a peer that answers pings (by reading), or not. *)
let keepalive_with ~peer =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server_end, peer_end = Eio_unix.Net.socketpair_stream ~sw () in
  let ws =
    Websocket.create
      ~reader:(Eio.Buf_read.of_flow server_end ~max_size:(64 * 1024 * 1024))
      ~flow:server_end
      ()
  in
  let shut = ref false in
  let outcome =
    Fiber.first
      (fun () ->
         Websocket.keepalive
           ws
           ~clock:(Eio.Stdenv.clock t.env)
           ~shutdown:(fun () ->
             shut := true;
             Eio.Flow.shutdown server_end `All)
           ~interval:0.05
           ~timeout:0.3
           ~write_timeout:0.5
           ();
         "the peer looked gone")
      (fun () ->
         Fiber.first
           (fun () -> Eio.Time.sleep (Eio.Stdenv.clock t.env) 1.5)
           (fun () ->
              Fiber.both
                (fun () -> peer ws peer_end)
                (* The server reads, as its connection does. *)
                (fun () -> ignore (Websocket.read ws : _ option));
              Fiber.await_cancel ());
         "still open")
  in
  printf "%s (shut down: %b)\n" outcome !shut
;;

let%expect_test "a WebSocket peer that answers pings stays" =
  keepalive_with ~peer:(fun _ peer_end ->
    let client =
      Websocket.create
        ~role:`Client
        ~reader:(Eio.Buf_read.of_flow peer_end ~max_size:1_000_000)
        ~flow:peer_end
        ()
    in
    (* [read] answers the pings. *)
    Fiber.first
      (fun () -> ignore (Websocket.read client : _ option))
      (fun () -> Fiber.await_cancel ()));
  [%expect {| still open (shut down: false) |}]
;;

let%expect_test "a silent WebSocket peer is dropped" =
  keepalive_with ~peer:(fun _ _ -> Fiber.await_cancel ());
  [%expect {| the peer looked gone (shut down: true) |}]
;;

let%expect_test "a WebSocket write stuck on a peer that does not read is ended" =
  keepalive_with ~peer:(fun ws _ ->
    (* Far more than the socket buffers hold. *)
    Websocket.send_text ws (String.make (32 * 1024 * 1024) 'x');
    print_endline "the stuck write returned";
    Fiber.await_cancel ());
  [%expect
    {|
    the stuck write returned
    the peer looked gone (shut down: true)
    |}]
;;

let%expect_test "an outbox writes in order, and what waits when it finishes" =
  with_sandbox
  @@ fun _ ->
  let outbox = Outbox.create ~max_lines:3 () in
  List.iter [ "a"; "b" ] ~f:(Outbox.send outbox);
  Outbox.finish outbox;
  Outbox.send outbox "after finish";
  Outbox.run outbox ~write:print_endline;
  let full = Outbox.create ~max_lines:3 () in
  List.iter [ "1"; "2"; "3"; "4"; "5" ] ~f:(Outbox.send full);
  printf
    "%s; waiting: %d\n"
    (Option.value (Eio.Promise.peek (Outbox.failed full)) ~default:"not failed")
    (Outbox.waiting full);
  Outbox.run full ~write:print_endline;
  [%expect
    {|
    a
    b
    the peer is not reading what is sent to it (4 messages waiting); waiting: 0
    |}]
;;
