open! Core
open! Prigh
open Eio.Std
module Http = Http_client

(* A keep-alive HTTP/1.1 server (optionally over TLS) that counts the
   connections it accepts. [handler] gets the request's index on its
   connection (from 1) and its request line. *)
module Server = struct
  module Action = struct
    type t =
      | Text of string (** with a Content-Length, kept open *)
      | Text_then_close of string (** says [Connection: close] *)
      | Text_then_drop of string (** closes without saying so *)
      | Drop (** closes without answering *)
      | Chunked of string * unit Promise.t * string
      (** the first part, then the rest once the promise resolves *)
  end

  type t =
    { port : int
    ; mutable accepts : int
    ; mutable requests : string list
    ; mutable dropped : int (** connections it closed unasked *)
    ; drop : Eio.Condition.t
    }

  let read_request reader =
    match Eio.Buf_read.line reader with
    | exception End_of_file -> None
    | request_line ->
      let rec headers len =
        match Eio.Buf_read.line reader with
        | "" -> len
        | line ->
          (match String.lsplit2 line ~on:':' with
           | Some (k, v) when String.Caseless.equal k "content-length" ->
             headers (Int.of_string (String.strip v))
           | _ -> headers len)
      in
      let len = headers 0 in
      ignore (Eio.Buf_read.take len reader : string);
      Some request_line
  ;;

  let serve t flow ~handler =
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    let write s = Eio.Flow.copy_string s flow in
    let reply ?(close = false) body =
      write
        (sprintf
           "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n%s\r\n%s"
           (String.length body)
           (if close then "Connection: close\r\n" else "")
           body)
    in
    let dropped () =
      Eio.Flow.shutdown flow `All;
      t.dropped <- t.dropped + 1;
      Eio.Condition.broadcast t.drop
    in
    let rec loop n =
      match read_request reader with
      | None -> ()
      | Some line ->
        t.requests <- t.requests @ [ line ];
        (match (handler ~n line : Action.t) with
         | Text body ->
           reply body;
           loop (n + 1)
         | Text_then_close body -> reply ~close:true body
         | Text_then_drop body ->
           reply body;
           dropped ()
         | Drop -> dropped ()
         | Chunked (first, go, rest) ->
           let chunk s = sprintf "%x\r\n%s\r\n" (String.length s) s in
           write
             ("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
              ^ chunk first);
           Promise.await go;
           write (chunk rest ^ "0\r\n\r\n");
           loop (n + 1))
    in
    try loop 1 with
    | Eio.Io _ | End_of_file | Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _ -> ()
  ;;

  let start ?tls ~sw ~(env : Eio_unix.Stdenv.base) handler =
    let socket =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:8
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | `Unix _ -> assert false
    in
    let t =
      { port
      ; accepts = 0
      ; requests = []
      ; dropped = 0
      ; drop = Eio.Condition.create ()
      }
    in
    Fiber.fork_daemon ~sw (fun () ->
      Switch.run
      @@ fun sw ->
      while true do
        Eio.Net.accept_fork
          ~sw
          socket
          ~on_error:(fun exn -> eprintf "server: %s\n" (Exn.to_string exn))
          (fun flow _ ->
             t.accepts <- t.accepts + 1;
             match tls with
             | None -> serve t flow ~handler
             | Some config ->
               (match Tls_eio.server_of_flow config flow with
                | tls -> serve t tls ~handler
                | exception (Tls_eio.Tls_failure _ | End_of_file | Eio.Io _) ->
                  ()))
      done;
      `Stop_daemon);
    t
  ;;

  let url ?(scheme = "http") t path =
    sprintf "%s://127.0.0.1:%d%s" scheme t.port path
  ;;
end

(* A self-signed certificate for the TLS server, which the client takes on
   trust. *)
let tls_configs =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     let key = X509.Private_key.generate `P256 in
     let name =
       [ X509.Distinguished_name.Relative_distinguished_name.singleton
           (CN (X509.Distinguished_name.Common_name.v "localhost"))
       ]
     in
     let csr =
       Result.ok_or_failwith
         (Result.map_error
            (X509.Signing_request.create name key)
            ~f:(fun (`Msg m) -> m))
     in
     let cert =
       match
         X509.Signing_request.sign
           csr
           ~valid_from:Ptime.epoch
           ~valid_until:(Option.value_exn (Ptime.of_float_s 4e9))
           key
           name
       with
       | Ok cert -> cert
       | Error _ -> failwith "signing failed"
     in
     let server =
       Result.ok_or_failwith
         (Result.map_error
            (Tls.Config.server ~certificates:(`Single ([ cert ], key)) ())
            ~f:(fun (`Msg m) -> m))
     in
     let client =
       Result.ok_or_failwith
         (Result.map_error
            (Tls.Config.client
               ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
               ())
            ~f:(fun (`Msg m) -> m))
     in
     server, client)
;;

let run f = Eio_main.run @@ fun env -> Switch.run @@ fun sw -> f ~env ~sw

let show = function
  | Ok ((_ : Http.Response.t), body) -> print_endline ("ok: " ^ body)
  | Error e -> print_endline ("error: " ^ Http.Error.to_string e)
;;

let stats server pool =
  printf
    "accepted %d connection(s); %d idle in the pool\n"
    server.Server.accepts
    (Http_pool.idle pool)
;;

let%expect_test "requests to one server share a connection" =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env (fun ~n line -> Text (sprintf "%d %s" n line))
  in
  let pool = Http_pool.create () in
  show (Http.get ~pool ~env ~url:(Server.url server "/a") ~headers:[] ());
  show
    (Http.post
       ~pool
       ~env
       ~url:(Server.url server "/b")
       ~headers:[]
       ~body:"x"
       ());
  let chunks = ref [] in
  (match
     Http.post_stream
       ~pool
       ~env
       ~url:(Server.url server "/c")
       ~headers:[]
       ~body:"{}"
       ~on_chunk:(fun c -> chunks := c :: !chunks)
       ()
   with
   | Ok _ -> printf "streamed: %s\n" (String.concat (List.rev !chunks))
   | Error e -> print_endline (Http.Error.to_string e));
  stats server pool;
  [%expect
    {|
    ok: 1 GET /a HTTP/1.1
    ok: 2 POST /b HTTP/1.1
    streamed: 3 POST /c HTTP/1.1
    accepted 1 connection(s); 1 idle in the pool
    |}]
;;

let%expect_test "concurrent requests each get a connection, and both are kept" =
  run
  @@ fun ~env ~sw ->
  let go, release = Promise.create () in
  let arrived = ref 0 in
  let server =
    Server.start ~sw ~env (fun ~n:_ _ ->
      incr arrived;
      if !arrived = 2 then Promise.resolve release ();
      if !arrived <= 2 then Chunked ("part ", go, "end") else Text "third")
  in
  let pool = Http_pool.create () in
  let get () =
    Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ()
  in
  let a, b = Fiber.pair get get in
  show a;
  show b;
  stats server pool;
  show (get ());
  stats server pool;
  [%expect
    {|
    ok: part end
    ok: part end
    accepted 2 connection(s); 2 idle in the pool
    ok: third
    accepted 2 connection(s); 2 idle in the pool
    |}];
  (* At most [max_idle_per_host] are kept. *)
  let go, release = Promise.create () in
  let arrived = ref 0 in
  let server =
    Server.start ~sw ~env (fun ~n:_ _ ->
      incr arrived;
      if !arrived = 2 then Promise.resolve release ();
      Chunked ("part ", go, "end"))
  in
  let pool = Http_pool.create ~max_idle_per_host:1 () in
  let get () =
    Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ()
  in
  ignore (Fiber.pair get get : _ * _);
  stats server pool;
  [%expect {| accepted 2 connection(s); 1 idle in the pool |}]
;;

let%expect_test "a server's Connection: close is respected" =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env (fun ~n _ -> Text_then_close (Int.to_string n))
  in
  let pool = Http_pool.create () in
  let get () =
    Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ()
  in
  show (get ());
  stats server pool;
  show (get ());
  stats server pool;
  [%expect
    {|
    ok: 1
    accepted 1 connection(s); 0 idle in the pool
    ok: 1
    accepted 2 connection(s); 0 idle in the pool
    |}]
;;

let%expect_test "a connection the server closed while idle is not used" =
  run
  @@ fun ~env ~sw ->
  let server = Server.start ~sw ~env (fun ~n:_ _ -> Text_then_drop "hi") in
  let pool = Http_pool.create () in
  let get () =
    Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ()
  in
  show (get ());
  stats server pool;
  while server.dropped < 1 do
    Eio.Condition.await_no_mutex server.drop
  done;
  show (get ());
  stats server pool;
  print_s [%sexp (server.requests : string list)];
  [%expect
    {|
    ok: hi
    accepted 1 connection(s); 1 idle in the pool
    ok: hi
    accepted 2 connection(s); 1 idle in the pool
    ("GET / HTTP/1.1" "GET / HTTP/1.1")
    |}]
;;

let%expect_test
    "a reused connection that fails before any reply is retried once"
  =
  run
  @@ fun ~env ~sw ->
  (* The server closes a connection when its second request arrives, as when
     its idle timeout races the request. *)
  let server =
    Server.start ~sw ~env (fun ~n _ -> if n = 1 then Text "first" else Drop)
  in
  let pool = Http_pool.create () in
  let post () =
    Http.post ~pool ~env ~url:(Server.url server "/") ~headers:[] ~body:"{}" ()
  in
  show (post ());
  show (post ());
  stats server pool;
  print_s [%sexp (server.requests : string list)];
  [%expect
    {|
    ok: first
    ok: first
    accepted 2 connection(s); 1 idle in the pool
    ("POST / HTTP/1.1" "POST / HTTP/1.1" "POST / HTTP/1.1")
    |}];
  (* A new connection failing the same way is not retried. *)
  let server = Server.start ~sw ~env (fun ~n:_ _ -> Drop) in
  show
    (Http.post
       ~pool
       ~env
       ~url:(Server.url server "/")
       ~headers:[]
       ~body:"{}"
       ());
  stats server pool;
  [%expect
    {|
    error: connection failed: connection closed by peer
    accepted 1 connection(s); 1 idle in the pool
    |}]
;;

let%expect_test "a cancelled stream's connection is closed, not reused" =
  run
  @@ fun ~env ~sw ->
  let never, _ = Promise.create () in
  let server =
    Server.start ~sw ~env (fun ~n:_ line ->
      if String.is_substring line ~substring:"/stream"
      then Chunked ("first", never, "never")
      else Text "next")
  in
  let pool = Http_pool.create () in
  let cancel = Cancellation.create () in
  let result =
    Http.post_stream
      ~pool
      ~env
      ~cancel
      ~url:(Server.url server "/stream")
      ~headers:[]
      ~body:""
      ~on_chunk:(fun c ->
        printf "chunk: %s\n" c;
        Cancellation.cancel cancel)
      ()
  in
  print_s [%sexp (result : (Http.Response.t, Http.Error.t) Result.t)];
  stats server pool;
  show (Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ());
  stats server pool;
  [%expect
    {|
    chunk: first
    (Error Cancelled)
    accepted 1 connection(s); 0 idle in the pool
    ok: next
    accepted 2 connection(s); 1 idle in the pool
    |}]
;;

let%expect_test "idle connections expire" =
  run
  @@ fun ~env ~sw ->
  let server = Server.start ~sw ~env (fun ~n:_ _ -> Text "hi") in
  let pool = Http_pool.create ~idle_timeout:Time_ns.Span.zero () in
  let get () =
    Http.get ~pool ~env ~url:(Server.url server "/") ~headers:[] ()
  in
  show (get ());
  show (get ());
  stats server pool;
  [%expect
    {|
    ok: hi
    ok: hi
    accepted 2 connection(s); 1 idle in the pool
    |}]
;;

let%expect_test "TLS: one handshake for several requests" =
  run
  @@ fun ~env ~sw ->
  let server_tls, client_tls = force tls_configs in
  let server =
    Server.start ~tls:server_tls ~sw ~env (fun ~n _ -> Text (Int.to_string n))
  in
  let pool = Http_pool.create ~tls:(lazy client_tls) () in
  for _ = 1 to 3 do
    show
      (Http.post
         ~pool
         ~env
         ~url:(Server.url ~scheme:"https" server "/v1/messages")
         ~headers:[]
         ~body:"{}"
         ())
  done;
  stats server pool;
  [%expect
    {|
    ok: 1
    ok: 2
    ok: 3
    accepted 1 connection(s); 1 idle in the pool
    |}]
;;
