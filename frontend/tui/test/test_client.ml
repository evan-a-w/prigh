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
  let%bind a
  and b in
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
    (Protocol_error "invalid JSON at byte 0: unexpected character")
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

let%expect_test
    "reconnect: a fresh transport per attempt, shared attempts, failures \
     reported"
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
  let%bind a
  and b in
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
  [%expect
    {|
    {"id":1,"method":"ping","params":{}}
    (Ok pong)
    |}];
  let%bind () = Client.close client in
  return ()
;;

let%expect_test
    "disconnect abandons an attempt that hangs: the next one is fresh, and \
     what the old one opens late is closed"
  =
  let hung = Ivar.create () in
  let backends = Queue.create () in
  let attempts = ref 0 in
  let connect () =
    incr attempts;
    if !attempts = 1
    then Ivar.read hung
    else (
      let transport, backend = Transport.In_memory.create () in
      Queue.enqueue backends backend;
      Deferred.Or_error.return transport)
  in
  let client = Client.create ~connect in
  let first = Client.connect client in
  let joined = Client.connect client in
  printf "attempts=%d\n" !attempts;
  [%expect {| attempts=1 |}];
  (* The page gives up on it (its timeout) and tries again. *)
  Client.disconnect client;
  let%bind second = Client.connect client in
  printf
    "attempts=%d second=%s connected=%b\n"
    !attempts
    (Sexp.to_string [%sexp (second : unit Or_error.t)])
    (Client.is_connected client);
  [%expect {| attempts=2 second=(Ok()) connected=true |}];
  (* The hung attempt connects after all: its transport is closed, the
     client keeps the second. *)
  let late, late_backend = Transport.In_memory.create () in
  Ivar.fill_exn hung (Ok late);
  let%bind first
  and joined
  and () = late.closed in
  print_s
    [%message
      (first : unit Or_error.t)
        (joined : unit Or_error.t)
        (Client.is_connected client : bool)];
  [%expect
    {|
    ((first  (Error abandoned))
     (joined (Error abandoned))
     ("Client.is_connected client" true))
    |}];
  ignore (late_backend : Transport.In_memory.Backend.t);
  let reply = Client.call client "ping" [] in
  let second_backend = Queue.dequeue_exn backends in
  let%bind request =
    Pipe.read_exn (Transport.In_memory.Backend.requests second_backend)
  in
  print_endline request;
  Transport.In_memory.Backend.send
    second_backend
    {|{"type":"response","id":1,"ok":true,"result":"pong"}|};
  let%bind reply in
  print_s [%sexp (reply : Prigh_protocol.Json.t Or_error.t)];
  [%expect
    {|
    {"id":1,"method":"ping","params":{}}
    (Ok pong)
    |}];
  let%bind () = Client.close client in
  return ()
;;

let%expect_test
    "disconnect detaches synchronously so an immediate reconnect opens a fresh \
     transport"
  =
  let backends = Queue.create () in
  let attempts = ref 0 in
  let connect () =
    incr attempts;
    let transport, backend = Transport.In_memory.create () in
    Queue.enqueue backends backend;
    Deferred.Or_error.return transport
  in
  let client = Client.create ~connect in
  let incoming = Client.incoming client in
  let%bind () = Deferred.Or_error.ok_exn (Client.connect client) in
  let in_flight = Client.call client "ping" [] in
  Client.disconnect client;
  printf
    "after disconnect: connected=%b waiting=%d\n"
    (Client.is_connected client)
    (Client.waiting client);
  (* An immediate reconnect must open a new transport rather than reusing the
     closing one. *)
  let%bind reconnected = Client.connect client in
  let%bind in_flight in
  let%bind closed = Pipe.read_exn incoming in
  printf
    "attempts=%d reconnected=%s in_flight=%s closed=%s\n"
    !attempts
    (Sexp.to_string [%sexp (reconnected : unit Or_error.t)])
    (Sexp.to_string [%sexp (in_flight : Prigh_protocol.Json.t Or_error.t)])
    (Sexp.to_string [%sexp (closed : Client.Incoming.t)]);
  (* A call on the new transport must not be failed or followed by a stale
     Closed once the old transport's reader finishes. *)
  let _first = Queue.dequeue_exn backends in
  let second = Queue.dequeue_exn backends in
  let reply = Client.call client "ping" [] in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  printf
    "after yield: connected=%b waiting=%d incoming_empty=%b\n"
    (Client.is_connected client)
    (Client.waiting client)
    (Pipe.is_empty incoming);
  let%bind request =
    Pipe.read_exn (Transport.In_memory.Backend.requests second)
  in
  print_endline request;
  Transport.In_memory.Backend.send
    second
    {|{"type":"response","id":2,"ok":true,"result":"pong"}|};
  let%bind reply in
  print_s [%sexp (reply : Prigh_protocol.Json.t Or_error.t)];
  [%expect
    {|
    after disconnect: connected=false waiting=0
    attempts=2 reconnected=(Ok()) in_flight=(Error"backend closed") closed=Closed
    after yield: connected=true waiting=1 incoming_empty=true
    {"id":2,"method":"ping","params":{}}
    (Ok pong)
    |}];
  let%bind () = Client.close client in
  return ()
;;

let%expect_test "close abandons an in-flight connect attempt" =
  let hung = Ivar.create () in
  let client = Client.create ~connect:(fun () -> Ivar.read hung) in
  let first = Client.connect client in
  let%bind () = Client.close client in
  let late, _late_backend = Transport.In_memory.create () in
  Ivar.fill_exn hung (Ok late);
  let%bind first
  and () = late.closed in
  printf
    "first=%s connected=%b\n"
    (Sexp.to_string [%sexp (first : unit Or_error.t)])
    (Client.is_connected client);
  [%expect {| first=(Error abandoned) connected=false |}];
  return ()
;;

let%expect_test "send_line failure fails the call and does not leak pending" =
  let lines_r, lines_w = Pipe.create () in
  let stderr_r, stderr_w = Pipe.create () in
  let closed = Ivar.create () in
  let calls = ref 0 in
  let transport : Transport.t =
    { send_line =
        (fun _ ->
          incr calls;
          if !calls = 1
          then Or_error.error_string "websocket not open"
          else failwith "send threw")
    ; lines = lines_r
    ; stderr_lines = stderr_r
    ; close =
        (fun () ->
          Pipe.close lines_w;
          Pipe.close stderr_w;
          Ivar.fill_if_empty closed ())
    ; closed = Ivar.read closed
    }
  in
  let client =
    Client.create ~connect:(fun () -> Deferred.Or_error.return transport)
  in
  let%bind () = Deferred.Or_error.ok_exn (Client.connect client) in
  let%bind r1 = Client.call client "ping" [] in
  printf
    "r1=%s waiting=%d\n"
    (Sexp.to_string [%sexp (r1 : Prigh_protocol.Json.t Or_error.t)])
    (Client.waiting client);
  let%bind r2 = Client.call client "ping" [] in
  printf
    "r2_is_error=%b waiting=%d\n"
    (Or_error.is_error r2)
    (Client.waiting client);
  [%expect
    {|
    r1=(Error"websocket not open") waiting=0
    r2_is_error=true waiting=0
    |}];
  let%bind () = Client.close client in
  return ()
;;
