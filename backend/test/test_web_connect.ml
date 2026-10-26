open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* [prigh web]: prigh-web served here for a backend elsewhere. The backend
   runs in-process on a loopback port, as [prigh serve -web] would. *)

let%expect_test "backend addresses: HOST:PORT and prigh-web URLs" =
  List.iter
    [ "server:7790"
    ; ":7777"
    ; "[::1]:7790"
    ; "ws://server:7790/ws"
    ; "wss://example.com/ws"
    ; "https://example.com/prigh/"
    ; "wss://example.com:8443/prigh/ws"
    ; "http://server"
    ; "server"
    ; "server:0"
    ; "ftp://server/"
    ; "wss:///ws"
    ]
    ~f:(fun spec ->
      match Backend_address.parse spec with
      | Ok t ->
        print_s
          [%message
            spec
              (t : Backend_address.t)
              ~to_string:(Backend_address.to_string t : string)
              ~authority:(Backend_address.authority t : string)]
      | Error e -> print_s [%message spec (e : Error.t)]);
  [%expect
    {|
    (server:7790
     (t ((tls false) (host server) (port 7790) (prefix "") (lines true)))
     (to_string server:7790) (authority server:7790))
    (:7777
     (t ((tls false) (host 127.0.0.1) (port 7777) (prefix "") (lines true)))
     (to_string 127.0.0.1:7777) (authority 127.0.0.1:7777))
    ([::1]:7790 (t ((tls false) (host ::1) (port 7790) (prefix "") (lines true)))
     (to_string [::1]:7790) (authority [::1]:7790))
    (ws://server:7790/ws
     (t ((tls false) (host server) (port 7790) (prefix "") (lines false)))
     (to_string ws://server:7790/ws) (authority server:7790))
    (wss://example.com/ws
     (t ((tls true) (host example.com) (port 443) (prefix "") (lines false)))
     (to_string wss://example.com/ws) (authority example.com))
    (https://example.com/prigh/
     (t ((tls true) (host example.com) (port 443) (prefix /prigh) (lines false)))
     (to_string wss://example.com/prigh/ws) (authority example.com))
    (wss://example.com:8443/prigh/ws
     (t
      ((tls true) (host example.com) (port 8443) (prefix /prigh) (lines false)))
     (to_string wss://example.com:8443/prigh/ws) (authority example.com:8443))
    (http://server
     (t ((tls false) (host server) (port 80) (prefix "") (lines false)))
     (to_string ws://server/ws) (authority server))
    (server
     (e
      "\"server\" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"))
    (server:0
     (e
      "\"server:0\": bad port; expected HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"))
    (ftp://server/
     (e
      "\"ftp://server/\": the scheme must be ws, wss, http or https; expected HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"))
    (wss:///ws
     (e
      "\"wss:///ws\" has no host; expected HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"))
    |}]
;;

let%expect_test "backend addresses: saved normalised, as prigh-tui saves them" =
  List.iter
    [ "https://example.com/"
    ; " wss://example.com/ws "
    ; "http://example.com/prigh"
    ; "ws://[::1]:7790/ws"
    ; " server:7790 "
    ; "not an address"
    ]
    ~f:(fun spec -> printf "%S -> %S\n" spec (Backend_address.normalize spec));
  [%expect
    {|
    "https://example.com/" -> "wss://example.com/ws"
    " wss://example.com/ws " -> "wss://example.com/ws"
    "http://example.com/prigh" -> "ws://example.com/prigh/ws"
    "ws://[::1]:7790/ws" -> "ws://[::1]:7790/ws"
    " server:7790 " -> "server:7790"
    "not an address" -> "not an address"
    |}]
;;

let%expect_test "the saved connection: flags are saved, the environment is not" =
  let show
        ?saved
        ?(flags =
          Saved_connection.Settings.
            { connect = None; token = None; user = None })
        ?(env =
          Saved_connection.Settings.
            { connect = None; token = None; user = None })
        ()
    =
    print_s
      [%sexp
        (Saved_connection.resolve ~saved ~flags ~env ~local:false
         : Saved_connection.Resolved.t)]
  in
  let saved =
    { Saved_connection.connect = "server:7790"
    ; token = Some "sekrit"
    ; user = Some "me"
    }
  in
  show ~saved ();
  show
    ~saved
    ~flags:{ connect = Some "other:7790"; token = None; user = None }
    ();
  show ~saved ~flags:{ connect = None; token = Some "new"; user = None } ();
  show ~saved ~env:{ connect = None; token = Some "env"; user = None } ();
  [%expect
    {|
    ((settings ((connect (server:7790)) (token (sekrit)) (user (me)))) (save ()))
    ((settings ((connect (other:7790)) (token ()) (user ())))
     (save (((connect other:7790)))))
    ((settings ((connect (server:7790)) (token (new)) (user (me))))
     (save (((connect server:7790) (token new) (user me)))))
    ((settings ((connect (server:7790)) (token (env)) (user (me)))) (save ()))
    |}]
;;

(* A [prigh serve -web] backend with token [sekrit] (and file transfers);
   returns the server and its port. *)
let backend t ~sw =
  let _agent, (h : Test_rpc.H.t) =
    (* A tool host gone at the end is not waited for. *)
    Test_rpc.make_server
      ~token:"sekrit"
      ~host_grace:0.
      t
      ~sw
      ~provider:(Faux_provider.create [])
  in
  let router = Rpc_router.single h.server in
  let port =
    Web_server.listen
      ~env:t.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:None
      ~websockets:[ "/ws", Web_server.serve_rpc router ]
      ~on_lines:(Rpc_router.serve_lines router)
      ()
  in
  h, port
;;

let address spec = Or_error.ok_exn (Backend_address.parse spec)

let connect t ~sw ~port =
  Eio.Net.connect
    ~sw
    (Eio.Stdenv.net t.env)
    (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
;;

(* One HTTP request; prints the answer's status, headers (sorted) and body. *)
let http t ~sw ~port request =
  let flow = connect t ~sw ~port in
  Eio.Flow.copy_string request flow;
  let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  let status = Eio.Buf_read.line reader in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> headers (line :: acc)
  in
  let headers = headers [] in
  let body = Eio.Buf_read.take_all reader in
  printf "%s\n" status;
  List.iter (List.sort headers ~compare:String.compare) ~f:(fun h ->
    if
      not
        (String.is_prefix h ~prefix:"ETag"
         || String.is_prefix h ~prefix:"Last-Modified")
    then printf "  %s\n" h);
  if not (String.is_empty body) then printf "  body: %s\n" (mask t body)
;;

let post_code t ~sw ~port code =
  http
    t
    ~sw
    ~port
    (sprintf
       "POST %s HTTP/1.1\r\nHost: x\r\nContent-Length: %d\r\n\r\n%s"
       Signin_handoff.path
       (String.length code)
       code)
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

(* A page's RPC socket to the site on [port]: says [hello] with [params]
   and prints what of the answer shows where the session's tools run. *)
let page_hello t ~sw ~port params =
  let flow = connect t ~sw ~port in
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  let ws =
    Websocket.connect ~reader ~flow ~host:"127.0.0.1" ~target:"/ws"
    |> Result.ok_or_failwith
  in
  Websocket.send_text
    ws
    (sprintf {|{"id": "h", "method": "hello", "params": %s}|} params);
  let rec answer () =
    match Websocket.read_text ws with
    | None -> print_endline "(closed)"
    | Some line ->
      let json = Json.of_string line in
      (match field json "id" with
       | Some (`String "h") ->
         (match field json "error" with
          | Some (`String error) -> printf "hello: %s\n" error
          | _ ->
            let state =
              Option.bind (field json "result") ~f:(fun r -> field r "state")
              |> Option.value_exn
            in
            let hosts =
              match field state "hosts" with
              | Some (`Array hosts) ->
                List.filter_map hosts ~f:(fun h ->
                  match field h "id", field h "name", field h "cwd" with
                  | Some (`String id), Some (`String name), Some (`String cwd)
                    ->
                    let name =
                      if String.equal name (Core_unix.gethostname ())
                      then "<host>"
                      else name
                    in
                    Some (sprintf "%s (%s, %s)" id name (mask t cwd))
                  | _ -> None)
              | _ -> []
            in
            printf
              "hello: ok, active host %s; hosts: %s\n"
              (match field state "active_host" with
               | Some (`String h) -> h
               | _ -> "?")
              (String.concat ~sep:", " hosts))
       | _ -> answer ())
  in
  answer ();
  Websocket.close ws
;;

let site t =
  write t "site/index.html" "<html>prigh-web</html>";
  t.dir ^/ "site"
;;

let start
      ?(tools = Web_connect.Tools.Local)
      ?(token = Some "sekrit")
      t
      ~sw
      ~logs
      remote
  =
  let config : Web_connect.Config.t =
    { remote
    ; credentials = { token; user = None }
    ; tools
    ; name = "laptop"
    ; host_id = "host-laptop"
    ; cwd = t.dir ^/ "here"
    ; listen = At (Eio.Net.Ipaddr.V4.loopback, 0)
    ; root = site t
    }
  in
  Core_unix.mkdir_p config.cwd;
  let started =
    Web_connect.start ~env:t.env ~sw ~log:(Queue.enqueue logs) config
  in
  let url =
    String.substr_replace_all
      started.url
      ~pattern:(sprintf ":%d/" started.port)
      ~with_:":PORT/"
  in
  let code = String.lsplit2_exn started.url ~on:'=' |> snd in
  printf
    "url: %s\ntool host: %s\n"
    (String.substr_replace_all url ~pattern:code ~with_:"<code>")
    (match started.tool_host with
     | `Connected -> "connected"
     | `Connecting -> "connecting"
     | `Refused e -> "refused: " ^ e
     | `Off -> "off");
  started, code
;;

let%expect_test
    "prigh web -connect HOST:PORT: the site, the sign-in link, the backend \
     behind it and the tool host"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  (* Ends before the backend's switch, which waits for the tool host's
     connection to end. *)
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let started, code =
    start t ~sw ~logs (address (sprintf "127.0.0.1:%d" backend_port))
  in
  let port = started.port in
  [%expect
    {|
    url: http://127.0.0.1:PORT/#signin=<code>
    tool host: connected
    |}];
  (* The page, and the code traded for the credentials, once. *)
  http t ~sw ~port "GET / HTTP/1.1\r\nHost: x\r\n\r\n";
  printf "code: %d hex digits\n" (String.length code);
  post_code t ~sw ~port "wrong";
  post_code t ~sw ~port code;
  post_code t ~sw ~port code;
  http t ~sw ~port (sprintf "GET %s HTTP/1.1\r\n\r\n" Signin_handoff.path);
  [%expect
    {|
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 22
      Content-Type: text/html; charset=utf-8
      body: <html>prigh-web</html>
    code: 32 hex digits
    HTTP/1.1 403 Forbidden
      Cache-Control: no-cache
      Connection: close
      Content-Length: 95
      Content-Type: text/plain; charset=utf-8
      body: this sign-in link was used already: sign in on the page, or run prigh web again for a new link

    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 58
      Content-Type: application/json
      body: {"user":null,"token":"sekrit","prefer_host":"host-laptop"}
    HTTP/1.1 403 Forbidden
      Cache-Control: no-cache
      Connection: close
      Content-Length: 95
      Content-Type: text/plain; charset=utf-8
      body: this sign-in link was used already: sign in on the page, or run prigh web again for a new link

    HTTP/1.1 405 Method Not Allowed
      Cache-Control: no-cache
      Connection: close
      Content-Length: 19
      Content-Type: text/plain; charset=utf-8
      body: method not allowed
    |}];
  (* The RPC goes to the backend, which checks the page's token; with
     prefer_host the page's session runs on the tool host. *)
  page_hello t ~sw ~port {|{"name": "prigh-web", "token": "wrong"}|};
  page_hello t ~sw ~port {|{"name": "prigh-web", "token": "sekrit"}|};
  page_hello
    t
    ~sw
    ~port
    {|{"name": "prigh-web", "token": "sekrit", "prefer_host": "host-laptop"}|};
  [%expect
    {|
    hello: unauthorised: bad user name or password
    hello: ok, active host backend; hosts: backend (<host>, $DIR), host-laptop (laptop, $DIR/here)
    hello: ok, active host host-laptop; hosts: backend (<host>, $DIR), host-laptop (laptop, $DIR/here)
    |}];
  (* File transfers are the backend's. *)
  http
    t
    ~sw
    ~port
    "GET /files/download?ticket=nope HTTP/1.1\r\n\
     Host: x\r\n\
     Connection: keep-alive\r\n\
     \r\n";
  [%expect
    {|
    HTTP/1.1 410 Gone
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 90
      Content-Type: text/plain; charset=utf-8
      X-Content-Type-Options: nosniff
      body: this link has expired or was used already: start the transfer again from the file browser
    |}];
  List.iter (Queue.to_list logs) ~f:(fun line ->
    print_endline
      (String.substr_replace_all
         line
         ~pattern:(sprintf ":%d" backend_port)
         ~with_:":PORT"));
  [%expect {| tool host: connected to 127.0.0.1:PORT as client-2 |}]
;;

let%expect_test
    "prigh web -connect ws://...: everything over WebSockets; -tools remote"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  (* Ends before the backend's switch, which waits for the tool host's
     connection to end. *)
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let started, _ =
    start t ~sw ~logs (address (sprintf "ws://127.0.0.1:%d/ws" backend_port))
  in
  page_hello
    t
    ~sw
    ~port:started.port
    {|{"name": "prigh-web", "token": "sekrit", "prefer_host": "host-laptop"}|};
  [%expect
    {|
    url: http://127.0.0.1:PORT/#signin=<code>
    tool host: connected
    hello: ok, active host host-laptop; hosts: backend (<host>, $DIR), host-laptop (laptop, $DIR/here)
    |}];
  let started, code =
    start
      ~tools:Remote
      t
      ~sw
      ~logs
      (address (sprintf "127.0.0.1:%d" backend_port))
  in
  post_code t ~sw ~port:started.port code;
  [%expect
    {|
    url: http://127.0.0.1:PORT/#signin=<code>
    tool host: off
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 49
      Content-Type: application/json
      body: {"user":null,"token":"sekrit","prefer_host":null}
    |}]
;;

let%expect_test "prigh web: the first free port from the default" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  (* The backend has the first port. *)
  let config : Web_connect.Config.t =
    { remote = address (sprintf "127.0.0.1:%d" backend_port)
    ; credentials = { token = Some "sekrit"; user = None }
    ; tools = Remote
    ; name = "laptop"
    ; host_id = "host-laptop"
    ; cwd = t.dir
    ; listen = From (Eio.Net.Ipaddr.V4.loopback, backend_port)
    ; root = site t
    }
  in
  let started = Web_connect.start ~env:t.env ~sw ~log:ignore config in
  printf "port: first + %d\n" (started.port - backend_port);
  [%expect {| port: first + 1 |}]
;;

let%expect_test "prigh web: what is wrong with the backend, and what to do" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, port = backend t ~sw in
  let check ?(source = Web_connect.Check.Source.Flag) ?token ?user spec =
    let remote = address spec in
    let credentials = { Web_connect.Credentials.token; user } in
    match Web_connect.Check.run ~env:t.env remote credentials with
    | Ok () -> print_endline "ok"
    | Error error ->
      print_endline
        (String.substr_replace_all
           (Web_connect.Check.explain error remote ~source credentials)
           ~pattern:(sprintf ":%d" port)
           ~with_:":PORT")
  in
  let here = sprintf "127.0.0.1:%d" port in
  check ~token:"sekrit" here;
  check ~token:"sekrit" (sprintf "ws://%s/ws" here);
  check here;
  check ~token:"wrong" here;
  check ~token:"wrong" ~user:"me" here;
  check ~token:"sekrit" (sprintf "ws://%s/elsewhere/ws" here);
  check ~source:(Saved "~/.prigh/tui-connection.sexp") "127.0.0.1:1";
  check ~source:Env "127.0.0.1:1";
  [%expect
    {|
    ok
    ok
    127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    No token was given: pass the backend's with -token SECRET (or $PRIGH_TOKEN); it is saved for next time.
    127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -token (or $PRIGH_TOKEN) against the backend's; if it serves several users (-tokens), also give -user NAME (or $PRIGH_USER).
    127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the backend's -tokens NAME=TOKEN entries.
    the backend at ws://127.0.0.1:PORT/elsewhere/ws did not accept prigh web: 127.0.0.1:PORT did not open the WebSocket at /elsewhere/ws: it answered HTTP 404 Not Found
    Is the URL's path where its prigh-web is (the page's address, without index.html)?
    cannot reach the backend at 127.0.0.1:1: connection refused
    Is it running there (prigh serve -web HOST:PORT, or -listen)? The address is the saved connection (~/.prigh/tui-connection.sexp): pass -connect HOST:PORT to replace it.
    cannot reach the backend at 127.0.0.1:1: connection refused
    Is it running there (prigh serve -web HOST:PORT, or -listen)? Check $PRIGH_CONNECT, or pass -connect HOST:PORT.
    |}]
;;
