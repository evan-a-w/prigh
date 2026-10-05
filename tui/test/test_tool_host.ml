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

let%expect_test "the hello carries one host id, also when reconnecting" =
  let pairs = List.init 2 ~f:(fun _ -> Transport.In_memory.create ()) in
  let transports = Queue.of_list (List.map pairs ~f:fst) in
  let client =
    Client.create ~connect:(fun () ->
      Deferred.Or_error.return (Queue.dequeue_exn transports))
  in
  let hello =
    Prigh_ui_term.Term_app.connection_hello
      [ "name", `String "laptop" ]
      ~local_tools:(Some "prigh")
  in
  let host_id = ref None in
  (* Connects (again), answers the hello and prints it. *)
  let attempt (_, backend) ~session =
    let reply =
      Prigh_ui_term.Term_app.reconnect
        client
        ~hello
        ~delay_ms:0
        ~session
        ~as_user:None
    in
    let%bind line =
      Pipe.read_exn (Transport.In_memory.Backend.requests backend)
    in
    let request = Or_error.ok_exn (Json.parse line) in
    let field json name = Option.value_exn (Jsonaf.member name json) in
    let id =
      match field (field request "params") "host_id" with
      | `String id -> id
      | _ -> assert false
    in
    if Option.is_none !host_id then host_id := Some id;
    Transport.In_memory.Backend.send
      backend
      (sprintf
         {|{"type":"response","id":%s,"ok":true,"result":{"client_id":"c","host_id":"%s"}}|}
         (Jsonaf.to_string (field request "id"))
         id);
    let%map reply in
    print_endline
      (String.substr_replace_all
         line
         ~pattern:(Option.value_exn !host_id)
         ~with_:"<host-id>");
    print_s [%sexp (Result.is_ok reply : bool)]
  in
  let%bind () = attempt (List.nth_exn pairs 0) ~session:None in
  Transport.In_memory.Backend.close (snd (List.nth_exn pairs 0));
  let%bind () = attempt (List.nth_exn pairs 1) ~session:(Some "s1") in
  print_endline
    (String.map (Option.value_exn !host_id) ~f:(fun c ->
       if Char.is_hex_digit c then 'x' else c));
  [%expect
    {|
    {"id":1,"method":"hello","params":{"name":"laptop","tools":true,"host_id":"<host-id>"}}
    true
    {"id":2,"method":"hello","params":{"name":"laptop","tools":true,"host_id":"<host-id>","session":"s1"}}
    true
    host-xxxxxxxxxxxxxxxx
    |}];
  return ()
;;
