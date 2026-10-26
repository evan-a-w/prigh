(* The WebSocket transport against servers on localhost: plain and TLS (a
   throwaway CA trusted in place of the system's), proxies that answer with
   a sign-in page instead, and the keepalive. *)
open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
module Ws = Prigh_client_unix.Websocket_transport
module Server = Prigh_client_unix_testing.Test_server
module Cert = Prigh_client_unix_testing.Test_cert
module Transport = Prigh_client.Transport

let connect
      ?authenticator
      ?headers
      ?ping_after
      ?timeout
      ?(host = "localhost")
      ?(path = "/ws")
      ~tls
      server
  =
  let port = Server.port server in
  let url =
    sprintf "%s://%s:%d%s" (if tls then "wss" else "ws") host port path
  in
  Ws.connect
    ?authenticator
    ?headers
    ?ping_after
    ?timeout
    ~url
    ~tls
    ~host
    ~port
    ~authority:(sprintf "%s:%d" host port)
    ~path
    ()
;;

let print_error = function
  | Ok (_ : Transport.t) -> print_endline "connected"
  | Error e -> print_endline (Error.to_string_hum e)
;;

(* Accepts, sends [frames], then records what the client sends until it
   closes. *)
let echo_server ?tls frames =
  let got = Queue.create () in
  let finished = Ivar.create () in
  let%map server =
    Server.start ?tls (fun channel ->
      match%bind Server.Websocket.read_request channel with
      | None -> return ()
      | Some request ->
        Queue.enqueue got (Server.Websocket.Request.show request);
        Server.Websocket.accept channel request;
        List.iter frames ~f:(fun (fin, opcode, payload) ->
          Server.Websocket.send ~fin channel ~opcode payload);
        let rec loop () =
          match%bind Server.Websocket.read channel with
          | None -> return (Ivar.fill_if_empty finished ())
          | Some frame ->
            Queue.enqueue got [%sexp (frame : Server.Websocket.Frame.t)];
            if frame.opcode = 8
            then (
              channel.close ();
              return (Ivar.fill_if_empty finished ()))
            else loop ()
        in
        loop ())
  in
  server, got, Ivar.read finished
;;

let read_lines (t : Transport.t) n =
  Deferred.repeat_until_finished n (fun n ->
    if n = 0
    then return (`Finished ())
    else (
      match%map Pipe.read t.lines with
      | `Eof ->
        print_endline "(end)";
        `Finished ()
      | `Ok line ->
        print_endline ("line: " ^ line);
        `Repeat (n - 1)))
;;

let session ?tls ?authenticator ?headers ?host () =
  let%bind server, got, finished =
    echo_server
      ?tls
      [ true, 1, {|{"a":1}|} ^ "\n" ^ {|{"b":2}|}
      ; false, 1, {|{"frag|}
      ; true, 0, {|mented":3}|}
      ; true, 9, "are you there"
      ; true, 2, "binary is ignored"
      ; true, 1, {|{"c":4}|}
      ]
  in
  match%bind
    connect ?authenticator ?headers ?host ~tls:(Option.is_some tls) server
  with
  | Error e ->
    print_endline (Error.to_string_hum e);
    Server.close server
  | Ok t ->
    let%bind () = read_lines t 4 in
    t.send_line {|{"method":"ping"}|};
    t.close ();
    let%bind () = finished in
    let%bind () = t.closed in
    let port = Int.to_string (Server.port server) in
    Queue.iter got ~f:(fun sexp ->
      print_endline
        (String.substr_replace_all
           (Sexp.to_string_hum sexp)
           ~pattern:port
           ~with_:"PORT"));
    Server.close server
;;

let%expect_test "ws://: the upgrade, lines both ways, fragments and pings" =
  let%bind () = session ~headers:[ "X-Auth", "secret" ] () in
  [%expect
    {|
    line: {"a":1}
    line: {"b":2}
    line: {"fragmented":3}
    line: {"c":4}
    ((line "GET /ws HTTP/1.1")
     (headers
      ((Host localhost:PORT) (Upgrade websocket) (Connection Upgrade)
       (Sec-WebSocket-Version 13) (X-Auth secret))))
    ((fin true) (opcode 10) (masked true) (payload "are you there"))
    ((fin true) (opcode 1) (masked true) (payload "{\"method\":\"ping\"}"))
    ((fin true) (opcode 8) (masked true) (payload "\003\232"))
    |}];
  return ()
;;

let%expect_test "wss://: TLS checked against the CAs given" =
  let cert = Cert.create () in
  let%bind () = session ~tls:cert.server ~authenticator:cert.authenticator () in
  [%expect
    {|
    line: {"a":1}
    line: {"b":2}
    line: {"fragmented":3}
    line: {"c":4}
    ((line "GET /ws HTTP/1.1")
     (headers
      ((Host localhost:PORT) (Upgrade websocket) (Connection Upgrade)
       (Sec-WebSocket-Version 13))))
    ((fin true) (opcode 10) (masked true) (payload "are you there"))
    ((fin true) (opcode 1) (masked true) (payload "{\"method\":\"ping\"}"))
    ((fin true) (opcode 8) (masked true) (payload "\003\232"))
    |}];
  (* By IP address: the certificate names 127.0.0.1 too. *)
  let%bind () =
    session
      ~tls:cert.server
      ~authenticator:cert.authenticator
      ~host:"127.0.0.1"
      ()
  in
  [%expect
    {|
    line: {"a":1}
    line: {"b":2}
    line: {"fragmented":3}
    line: {"c":4}
    ((line "GET /ws HTTP/1.1")
     (headers
      ((Host 127.0.0.1:PORT) (Upgrade websocket) (Connection Upgrade)
       (Sec-WebSocket-Version 13))))
    ((fin true) (opcode 10) (masked true) (payload "are you there"))
    ((fin true) (opcode 1) (masked true) (payload "{\"method\":\"ping\"}"))
    ((fin true) (opcode 8) (masked true) (payload "\003\232"))
    |}];
  let%bind () = session ~tls:cert.server ~authenticator:(Cert.stranger ()) () in
  [%expect
    {| the TLS certificate of localhost is not trusted: invalid certificate chain. prigh checks it against the system's CAs ($SSL_CERT_FILE overrides them): use a certificate they trust (e.g. Let's Encrypt), or an SSH tunnel to plain HOST:PORT |}];
  return ()
;;

let%expect_test "wss:// to plain HTTP" =
  let cert = Cert.create () in
  let%bind plain =
    Server.start (fun channel ->
      Server.Websocket.refuse
        channel
        ~status:"400 Bad Request"
        ~headers:[ "Content-Type", "text/plain" ]
        ~body:"Client sent an HTTP request to an HTTPS server.";
      return ())
  in
  let%bind r = connect ~tls:true ~authenticator:cert.authenticator plain in
  print_error r;
  let%bind () = Server.close plain in
  [%expect
    {| localhost answered in plain HTTP, not TLS: connect with ws:// (or http://) instead |}];
  return ()
;;

let%expect_test "a proxy's answer instead of the WebSocket says what to do" =
  let answers =
    [ ( "302 Found"
      , [ ( "Location"
          , "https://login.example.com/outpost.goauthentik.io/start?rd=https://ai.example.com/ws"
          )
        ; "Content-Type", "text/html"
        ]
      , "" )
    ; ( "302 Found"
      , [ "Location", "https://sso.example.com/oauth2/sign_in" ]
      , "<html>login</html>" )
    ; ( "401 Unauthorized"
      , [ "WWW-Authenticate", {|Basic realm="authentik"|} ]
      , "" )
    ; ( "200 OK"
      , [ "Content-Type", "text/html; charset=utf-8" ]
      , "<!doctype html><title>Sign in</title>" )
    ; "301 Moved Permanently", [ "Location", "https://localhost/ws" ], ""
    ; "404 Not Found", [], ""
    ; "426 Upgrade Required", [], ""
    ; "502 Bad Gateway", [], ""
    ; "418 I'm a teapot", [], ""
    ]
  in
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      answers
      ~f:(fun (status, headers, body) ->
        let%bind server =
          Server.start (fun channel ->
            match%map Server.Websocket.read_request channel with
            | None -> ()
            | Some _ -> Server.Websocket.refuse channel ~status ~headers ~body)
        in
        let%bind r = connect ~tls:false server in
        print_endline
          (String.substr_replace_all
             (match r with
              | Ok _ -> "connected"
              | Error e -> Error.to_string_hum e)
             ~pattern:(Int.to_string (Server.port server))
             ~with_:"PORT");
        print_endline "";
        Server.close server)
  in
  [%expect
    {|
    localhost answered with a login page (HTTP 302 Found to https://login.example.com/outpost.goauthentik.io/start?rd=https://ai.example.com/ws): the proxy requires browser sign-in, which prigh-tui cannot do: exempt /ws and /terminal from it for prigh (prigh checks its own token; see DOCKER.md, "Behind a reverse proxy"), pass the proxy's credentials with -header, or use an SSH tunnel to the backend's HOST:PORT (authentik: add ^/(ws|terminal)(\?.*)?$ to the proxy provider's Unauthenticated Paths)

    localhost answered with a login page (HTTP 302 Found to https://sso.example.com/oauth2/sign_in): the proxy requires browser sign-in, which prigh-tui cannot do: exempt /ws and /terminal from it for prigh (prigh checks its own token; see DOCKER.md, "Behind a reverse proxy"), pass the proxy's credentials with -header, or use an SSH tunnel to the backend's HOST:PORT

    localhost refused the WebSocket (HTTP 401 Unauthorized, WWW-Authenticate: Basic realm="authentik"): the proxy requires browser sign-in, which prigh-tui cannot do: exempt /ws and /terminal from it for prigh (prigh checks its own token; see DOCKER.md, "Behind a reverse proxy"), pass the proxy's credentials with -header, or use an SSH tunnel to the backend's HOST:PORT

    localhost answered with a web page (HTTP 200 OK, text/html; charset=utf-8) instead of opening the WebSocket: if it is a login page, the proxy requires browser sign-in, which prigh-tui cannot do: exempt /ws and /terminal from it for prigh (prigh checks its own token; see DOCKER.md, "Behind a reverse proxy"), pass the proxy's credentials with -header, or use an SSH tunnel to the backend's HOST:PORT; otherwise check that ws://localhost:PORT/ws goes to prigh's -web port

    localhost redirects to HTTPS (HTTP 301 Moved Permanently to https://localhost/ws): connect with wss:// (or https://) instead of ws://localhost:PORT/ws

    localhost has no WebSocket at ws://localhost:PORT/ws (HTTP 404 Not Found): check the URL (with the path prigh-web is served under) and that the proxy forwards it to prigh's -web port

    localhost did not take the WebSocket upgrade (HTTP 426 Upgrade Required): the proxy must pass the Upgrade and Connection headers on to prigh's -web port (see DOCKER.md, "Behind a reverse proxy")

    localhost cannot reach prigh (HTTP 502 Bad Gateway): is prigh serve -web running, and the proxy pointing at its port?

    localhost did not open the WebSocket at ws://localhost:PORT/ws: it answered HTTP 418 I'm a teapot
    |}];
  return ()
;;

let%expect_test "keepalive: pings when quiet, closes when nothing answers" =
  let start ~answer =
    let pings = ref 0 in
    let%map server =
      Server.start (fun channel ->
        match%bind Server.Websocket.read_request channel with
        | None -> return ()
        | Some request ->
          Server.Websocket.accept channel request;
          let rec loop () =
            match%bind Server.Websocket.read channel with
            | None -> return ()
            | Some frame ->
              if frame.opcode = 9
              then (
                incr pings;
                if answer
                then Server.Websocket.send channel ~opcode:10 frame.payload);
              loop ()
          in
          loop ())
    in
    server, pings
  in
  let run ~answer =
    let%bind server, pings = start ~answer in
    let%bind t =
      connect
        ~tls:false
        ~ping_after:(Time_float.Span.of_ms 50.)
        ~timeout:(Time_float.Span.of_ms 300.)
        server
    in
    let t = Or_error.ok_exn t in
    let%bind result = Clock.with_timeout (Time_float.Span.of_sec 1.) t.closed in
    printf
      "answering pongs: %b; pinged: %b; %s\n"
      answer
      (!pings > 0)
      (match result with
       | `Result () -> "closed"
       | `Timeout -> "still open after 1s");
    t.close ();
    Server.close server
  in
  let%bind () = run ~answer:true in
  let%bind () = run ~answer:false in
  [%expect
    {|
    answering pongs: true; pinged: true; still open after 1s
    answering pongs: false; pinged: true; closed
    |}];
  return ()
;;

let%expect_test "-header values" =
  List.iter
    [ "X-Auth: secret"
    ; "Authorization:Basic  YTpi "
    ; "no colon"
    ; " : x"
    ; "A B: c"
    ]
    ~f:(fun h ->
      print_s [%sexp (Ws.parse_header h : (string * string, string) Result.t)]);
  [%expect
    {|
    (Ok (X-Auth secret))
    (Ok (Authorization "Basic  YTpi"))
    (Error
     "\"no colon\" is not a header: expected Name: value (e.g. 'X-Auth: ...')")
    (Error "\" : x\" is not a header: expected Name: value (e.g. 'X-Auth: ...')")
    (Error
     "\"A B: c\" is not a header: expected Name: value (e.g. 'X-Auth: ...')")
    |}];
  return ()
;;
