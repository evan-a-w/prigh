open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
open Prigh_client
open Prigh_protocol
module Tool_host = Prigh_client_unix.Tool_host

let event line =
  match Server_message.of_line line with
  | Ok (Event e) -> e
  | _ -> raise_s [%message "not an event" line]
;;

let connected_client () =
  let transport, backend = Transport.In_memory.create () in
  let client =
    Client.create ~connect:(fun () -> Deferred.Or_error.return transport)
  in
  let%map () = Deferred.Or_error.ok_exn (Client.connect client) in
  client, Transport.In_memory.Backend.requests backend
;;

let print_next pipe =
  let%map line = Pipe.read_exn pipe in
  print_endline line
;;

let%expect_test "terminal events go to the worker, its lines become requests" =
  let%bind client, requests = connected_client () in
  let worker, fake_worker = Transport.In_memory.create () in
  let spawns = ref 0 in
  let host =
    Tool_host.create ~client ~spawn:(fun () ->
      incr spawns;
      Deferred.Or_error.return worker)
  in
  let to_worker = Transport.In_memory.Backend.requests fake_worker in
  List.iter
    ~f:(fun line -> Tool_host.handle host (event line))
    [ {|{"type":"event","event":"terminal_open","host":"client-1","term_id":"t1","key":"sess-1","cwd":"/work","cols":80,"rows":24}|}
    ; {|{"type":"event","event":"terminal_frame","host":"client-1","term_id":"t1","kind":"binary","data":"bHMK"}|}
    ; {|{"type":"event","event":"terminal_frame","host":"client-1","term_id":"t1","kind":"text","data":"{\"type\":\"resize\"}"}|}
    ; {|{"type":"event","event":"terminal_close","host":"client-1","term_id":"t1"}|}
    ; {|{"type":"event","event":"notice","text":"ignored"}|}
    ];
  let%bind () = print_next to_worker in
  let%bind () = print_next to_worker in
  let%bind () = print_next to_worker in
  let%bind () = print_next to_worker in
  [%expect
    {|
    {"type":"terminal_open","term_id":"t1","key":"sess-1","cwd":"/work","cols":80,"rows":24}
    {"type":"terminal_frame","term_id":"t1","kind":"binary","data":"bHMK"}
    {"type":"terminal_frame","term_id":"t1","kind":"text","data":"{\"type\":\"resize\"}"}
    {"type":"terminal_close","term_id":"t1"}
    |}];
  print_s [%message (!spawns : int)];
  [%expect {| (!spawns 1) |}];
  List.iter
    ~f:(Transport.In_memory.Backend.send fake_worker)
    [ {|{"type":"terminal_frame","term_id":"t1","kind":"binary","data":"aGkK"}|}
    ; {|{"type":"terminal_frame","term_id":"t1","kind":"text","data":"{\"type\":\"pong\"}"}|}
    ; {|{"type":"terminal_closed","term_id":"t1"}|}
    ; {|{"type":"terminal_frame","kind":"binary","data":"no term id"}|}
    ; {|{"type":"result","exec_id":"e1","text":"done","is_error":false}|}
    ];
  let%bind () = print_next requests in
  let%bind () = print_next requests in
  let%bind () = print_next requests in
  let%bind () = print_next requests in
  [%expect
    {|
    {"id":1,"method":"terminal_frame","params":{"term_id":"t1","kind":"binary","data":"aGkK"}}
    {"id":2,"method":"terminal_frame","params":{"term_id":"t1","kind":"text","data":"{\"type\":\"pong\"}"}}
    {"id":3,"method":"terminal_closed","params":{"term_id":"t1"}}
    {"id":4,"method":"tool_exec_result","params":{"exec_id":"e1","text":"done","is_error":false}}
    |}];
  Tool_host.close host;
  Client.close client
;;

let%expect_test "a worker that cannot start closes the terminal and fails the \
                 tool call"
  =
  let%bind client, requests = connected_client () in
  let host =
    Tool_host.create ~client ~spawn:(fun () ->
      Deferred.Or_error.error_string "no such file")
  in
  Tool_host.handle
    host
    (event
       {|{"type":"event","event":"terminal_open","host":"client-1","term_id":"t2","key":"sess-1","cwd":"/work","cols":80,"rows":24}|});
  Tool_host.handle
    host
    (event
       {|{"type":"event","event":"tool_exec","host":"client-1","exec_id":"e2","call_id":"c2","name":"bash","arguments":{"command":"ls"},"cwd":"/work"}|});
  let%bind () = print_next requests in
  let%bind () = print_next requests in
  [%expect
    {|
    {"id":1,"method":"terminal_closed","params":{"term_id":"t2"}}
    {"id":2,"method":"tool_exec_result","params":{"exec_id":"e2","text":"cannot start tool host: no such file","is_error":true}}
    |}];
  Client.close client
;;

let%expect_test
    "a result's images are forwarded verbatim, and only when there are some"
  =
  let%bind client, requests = connected_client () in
  let worker, fake_worker = Transport.In_memory.create () in
  let host =
    Tool_host.create ~client ~spawn:(fun () -> Deferred.Or_error.return worker)
  in
  Tool_host.handle
    host
    (event
       {|{"type":"event","event":"tool_exec","host":"client-1","exec_id":"e1","call_id":"c1","name":"read","arguments":{"path":"shot.png"},"cwd":"/work"}|});
  let%bind () = print_next (Transport.In_memory.Backend.requests fake_worker) in
  List.iter
    ~f:(Transport.In_memory.Backend.send fake_worker)
    [ {|{"type":"result","exec_id":"e1","text":"Read image file [image/png, 1x1]","is_error":false,"images":[{"mime_type":"image/png","data":"iVBORw0KGgo="}]}|}
    ; {|{"type":"result","exec_id":"e2","text":"no images","is_error":false,"images":[]}|}
    ];
  let%bind () = print_next requests in
  let%bind () = print_next requests in
  [%expect
    {|
    {"type":"exec","exec_id":"e1","name":"read","arguments":{"path":"shot.png"},"cwd":"/work"}
    {"id":1,"method":"tool_exec_result","params":{"exec_id":"e1","text":"Read image file [image/png, 1x1]","is_error":false,"images":[{"mime_type":"image/png","data":"iVBORw0KGgo="}]}}
    {"id":2,"method":"tool_exec_result","params":{"exec_id":"e2","text":"no images","is_error":false}}
    |}];
  Tool_host.close host;
  Client.close client
;;
