open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
open Prigh_client

let%expect_test "call/response correlation, events, errors, close" =
  let transport, backend = Transport.In_memory.create () in
  let client = Client.create transport in
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
  (* Closing fails pending calls and ends the incoming pipe. *)
  let c = Client.call client "get_state" [] in
  Transport.In_memory.Backend.close backend;
  let%bind c in
  let%bind e4 = Pipe.read_exn incoming in
  let%bind rest = Pipe.read_all incoming in
  print_s
    [%message
      (c : Prigh_protocol.Json.t Or_error.t)
        (e4 : Client.Incoming.t)
        (Queue.length rest : int)];
  [%expect
    {|
    ((c (Error "backend closed"))
     (e4                  Closed)
     ("Queue.length rest" 0))
    |}];
  return ()
;;
