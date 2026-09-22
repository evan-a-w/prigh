open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
open Prigh_client

let%expect_test "call/response correlation, events, errors, close" =
  let transport, backend = Transport.In_memory.create () in
  let client =
    Client.create ~connect:(fun () -> Deferred.Or_error.return transport)
  in
  let%bind not_yet = Client.call client "ping" [] in
  print_s [%sexp (not_yet : Prigh_protocol.Json.t Or_error.t)];
  [%expect {| (Error "not connected") |}];
  let%bind () = Deferred.Or_error.ok_exn (Client.connect client) in
  let requests = Transport.In_memory.Backend.requests backend in
  let incoming = Client.incoming client in
  let a = Client.call client "ping" [] in
  let b = Client.call client "set_model" [ "model", `String "x" ] in
  let%bind r1 = Pipe.read_exn requests in
  let%bind r2 = Pipe.read_exn requests in
  print_endline r1;
  print_endline r2;
  [%expect
    {|
    {"id":1,"method":"ping","params":{}}
    {"id":2,"method":"set_model","params":{"model":"x"}}
    |}];
  (* Out-of-order replies are matched by id. *)
  Transport.In_memory.Backend.send
    backend
    {|{"type":"response","id":2,"ok":false,"error":"unknown model"}|};
  Transport.In_memory.Backend.send
    backend
    {|{"type":"event","event":"notice","text":"hello"}|};
  Transport.In_memory.Backend.send
    backend
    {|{"type":"response","id":1,"ok":true,"result":"pong"}|};
  Transport.In_memory.Backend.send backend {|garbage|};
  Transport.In_memory.Backend.send
    backend
    {|{"type":"response","id":99,"ok":true,"result":null}|};
  let%bind a and b in
  print_s
    [%message
      (a : Prigh_protocol.Json.t Or_error.t)
        (b : Prigh_protocol.Json.t Or_error.t)];
  let%bind e1 = Pipe.read_exn incoming in
  let%bind e2 = Pipe.read_exn incoming in
  let%bind e3 = Pipe.read_exn incoming in
  print_s [%sexp (e1 : Client.Incoming.t)];
  print_s [%sexp (e2 : Client.Incoming.t)];
  print_s [%sexp (e3 : Client.Incoming.t)];
  [%expect
    {|
    ((a (Ok    pong))
     (b (Error "unknown model")))
    (Event (Notice hello))
    (Protocol_error "json: unexpected character: 'g'")
    (Protocol_error "response for unknown request id 99")
    |}];
  (* The backend going away fails pending calls and delivers [Closed]; the
     incoming pipe stays open for a reconnection until [close]. *)
  let c = Client.call client "get_state" [] in
  Transport.In_memory.Backend.close backend;
  let%bind c in
  let%bind e4 = Pipe.read_exn incoming in
  print_s
    [%message
      (c : Prigh_protocol.Json.t Or_error.t)
        (e4 : Client.Incoming.t)
        (Client.is_connected client : bool)];
  [%expect
    {|
    ((c (Error "backend closed"))
     (e4                           Closed)
     ("Client.is_connected client" false))
    |}];
  let%bind () = Client.close client in
  let%bind rest = Pipe.read_all incoming in
  print_s [%sexp (Queue.length rest : int)];
  [%expect {| 0 |}];
  return ()
;;

let%expect_test "reconnect: a fresh transport per attempt, shared attempts, \
                 failures reported"
  =
  let backends = Queue.create () in
  let attempts = ref 0 in
  let fail_next = ref false in
  let connect () =
    incr attempts;
    if !fail_next
    then (
      fail_next := false;
      Deferred.Or_error.error_string "connection refused")
    else (
      let transport, backend = Transport.In_memory.create () in
      Queue.enqueue backends backend;
      Deferred.Or_error.return transport)
  in
  let client = Client.create ~connect in
  let incoming = Client.incoming client in
  let%bind () = Deferred.Or_error.ok_exn (Client.connect client) in
  let%bind () = Deferred.Or_error.ok_exn (Client.connect client) in
  printf "attempts=%d connected=%b\n" !attempts (Client.is_connected client);
  [%expect {| attempts=1 connected=true |}];
  let first = Queue.dequeue_exn backends in
  Transport.In_memory.Backend.close first;
  let%bind closed = Pipe.read_exn incoming in
  print_s [%sexp (closed : Client.Incoming.t)];
  [%expect {| Closed |}];
  fail_next := true;
  let%bind failed = Client.connect client in
  print_s [%sexp (failed : unit Or_error.t)];
  [%expect {| (Error "connection refused") |}];
  (* Two concurrent attempts share one connection. *)
  let a = Client.connect client in
  let b = Client.connect client in
  let%bind a and b in
  printf
    "attempts=%d a=%s b=%s connected=%b\n"
    !attempts
    (Sexp.to_string [%sexp (a : unit Or_error.t)])
    (Sexp.to_string [%sexp (b : unit Or_error.t)])
    (Client.is_connected client);
  [%expect {| attempts=3 a=(Ok()) b=(Ok()) connected=true |}];
  let second = Queue.dequeue_exn backends in
  let reply = Client.call client "ping" [] in
  let%bind request =
    Pipe.read_exn (Transport.In_memory.Backend.requests second)
  in
  print_endline request;
  Transport.In_memory.Backend.send
    second
    {|{"type":"response","id":1,"ok":true,"result":"pong"}|};
  let%bind reply in
  print_s [%sexp (reply : Prigh_protocol.Json.t Or_error.t)];
  [%expect {|
    {"id":1,"method":"ping","params":{}}
    (Ok pong)
    |}];
  let%bind () = Client.close client in
  return ()
;;
