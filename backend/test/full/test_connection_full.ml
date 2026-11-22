open! Core
open! Prigh
open Tool_test_helpers
open! Test_connection

let%expect_test "a slow peer under a long streamed reply keeps up" =
  slow_peer_keeps_up ~takes_deltas:false;
  [%expect
    {|
    ok
    ((ended false) (updates 2) (text_length 10000) (text_is_the_reply true)
     (thinking_length 2000) (last_partial_has_the_whole_text (true))
     (pieces ((1 2000) (2001 12000))) (final_content (12057)))
    |}];
  slow_peer_keeps_up ~takes_deltas:true;
  [%expect
    {|
    ok
    ((ended false) (updates 2) (text_length 10000) (text_is_the_reply true)
     (thinking_length 2000) (last_partial_has_the_whole_text ())
     (pieces ((1 2000) (2001 12000))) (final_content (12057)))
    |}]
;;

(* A frontend that attaches to the session while a reply streams (a second
   tab, a reconnect) gets the message so far with its first update, then
   deltas; so does one that asks for it ([resync]: it found pieces
   missing). The others keep getting deltas. *)

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
