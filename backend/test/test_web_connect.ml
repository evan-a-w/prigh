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
              ~authority:(Backend_address.authority t : string)
              ~web_url:(Backend_address.web_url t : string)]
      | Error e -> print_s [%message spec (e : Error.t)]);
  [%expect
    {|
    (server:7790
     (t ((tls false) (host server) (port 7790) (prefix "") (lines true)))
     (to_string server:7790) (authority server:7790)
     (web_url http://server:7790/))
    (:7777
     (t ((tls false) (host 127.0.0.1) (port 7777) (prefix "") (lines true)))
     (to_string 127.0.0.1:7777) (authority 127.0.0.1:7777)
     (web_url http://127.0.0.1:7777/))
    ([::1]:7790 (t ((tls false) (host ::1) (port 7790) (prefix "") (lines true)))
     (to_string [::1]:7790) (authority [::1]:7790) (web_url http://[::1]:7790/))
    (ws://server:7790/ws
     (t ((tls false) (host server) (port 7790) (prefix "") (lines false)))
     (to_string ws://server:7790/ws) (authority server:7790)
     (web_url http://server:7790/))
    (wss://example.com/ws
     (t ((tls true) (host example.com) (port 443) (prefix "") (lines false)))
     (to_string wss://example.com/ws) (authority example.com)
     (web_url https://example.com/))
    (https://example.com/prigh/
     (t ((tls true) (host example.com) (port 443) (prefix /prigh) (lines false)))
     (to_string wss://example.com/prigh/ws) (authority example.com)
     (web_url https://example.com/prigh/))
    (wss://example.com:8443/prigh/ws
     (t
      ((tls true) (host example.com) (port 8443) (prefix /prigh) (lines false)))
     (to_string wss://example.com:8443/prigh/ws) (authority example.com:8443)
     (web_url https://example.com:8443/prigh/))
    (http://server
     (t ((tls false) (host server) (port 80) (prefix "") (lines false)))
     (to_string ws://server/ws) (authority server) (web_url http://server/))
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
    ; proxy_cookie = Some "proxy_session=abc"
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
     (save
      (((connect server:7790) (token new) (user me)
        (proxy_cookie proxy_session=abc)))))
    ((settings ((connect (server:7790)) (token (env)) (user (me)))) (save ()))
    |}]
;;

let%expect_test "choosing the backend: flags, environment, saved connection" =
  with_sandbox
  @@ fun t ->
  let none =
    { Saved_connection.Settings.connect = None; token = None; user = None }
  in
  let show ?(flags = none) ?(env = none) () =
    match Backend_choice.choose ~home:t.dir ~flags ~env with
    | Ok choice ->
      print_endline
        (mask_sexp
           t
           [%sexp
             { address = (Backend_address.to_string choice.remote : string)
             ; credentials = (choice.credentials : Web_connect.Credentials.t)
             ; source = (choice.source : Web_connect.Check.Source.t)
             ; token_source =
                 (choice.token_source : Web_connect.Check.Source.t option)
             ; save = (choice.save : Saved_connection.t option)
             }]);
      print_endline (mask t (Backend_choice.refused_hint choice))
    | Error e ->
      print_endline
        (mask t (Backend_choice.Error.to_string_hum e ~no_backend:"NO BACKEND"))
  in
  (* Nothing anywhere. *)
  show ();
  [%expect {| NO BACKEND |}];
  (* A flag alone (normalised as saved), and the environment's token. *)
  show
    ~flags:{ none with connect = Some "https://example.com/" }
    ~env:{ none with token = Some "env" }
    ();
  [%expect
    {|
    ((address wss://example.com/ws) (credentials ((token (env)) (user ())))
     (source Flag) (token_source (Env))
     (save (((connect wss://example.com/ws)))))
    Check -token (or $PRIGH_TOKEN) against the backend's; if it serves several users (-tokens), also give -user NAME (or $PRIGH_USER).
    |}];
  write
    t
    ".prigh/tui-connection.sexp"
    "((connect server:7790) (token sekrit) (user me))";
  (* -remote: the saved connection, its token and user. *)
  show ();
  [%expect
    {|
    ((address server:7790) (credentials ((token (sekrit)) (user (me))))
     (source (Saved $DIR/.prigh/tui-connection.sexp))
     (token_source ((Saved $DIR/.prigh/tui-connection.sexp))) (save ()))
    The credentials are the ones saved for server:7790 in $DIR/.prigh/tui-connection.sexp: sign in again with /signin in prigh-tui (or prigh-tui -connect server:7790 -token SECRET [-user NAME]), or pass -token SECRET (and -user NAME).
    |}];
  (* -connect to the saved address takes its token and user... *)
  show ~flags:{ none with connect = Some " server:7790 " } ();
  [%expect
    {|
    ((address server:7790) (credentials ((token (sekrit)) (user (me))))
     (source Flag) (token_source ((Saved $DIR/.prigh/tui-connection.sexp)))
     (save ()))
    The credentials are the ones saved for server:7790 in $DIR/.prigh/tui-connection.sexp: sign in again with /signin in prigh-tui (or prigh-tui -connect server:7790 -token SECRET [-user NAME]), or pass -token SECRET (and -user NAME).
    |}];
  (* ... another address does not. *)
  show ~flags:{ none with connect = Some "other:7790" } ();
  [%expect
    {|
    ((address other:7790) (credentials ((token ()) (user ()))) (source Flag)
     (token_source ()) (save (((connect other:7790)))))
    No token was given, and none is saved for other:7790: pass the backend's with -token SECRET (or $PRIGH_TOKEN), or sign in once with prigh-tui -connect other:7790 -token SECRET (or prigh web -connect other:7790 -token SECRET), which saves it.
    |}];
  (* $PRIGH_CONNECT wins over the saved address, so the saved token stays
     with it; the flag's token and user win over everything. *)
  show ~env:{ none with connect = Some "envhost:7790" } ();
  show
    ~flags:{ none with token = Some "flag"; user = Some "you" }
    ~env:{ connect = None; token = Some "env"; user = Some "them" }
    ();
  [%expect
    {|
    ((address envhost:7790) (credentials ((token ()) (user ()))) (source Env)
     (token_source ()) (save ()))
    No token was given, and none is saved for envhost:7790: pass the backend's with -token SECRET (or $PRIGH_TOKEN), or sign in once with prigh-tui -connect envhost:7790 -token SECRET (or prigh web -connect envhost:7790 -token SECRET), which saves it.
    ((address server:7790) (credentials ((token (flag)) (user (you))))
     (source (Saved $DIR/.prigh/tui-connection.sexp)) (token_source (Flag))
     (save (((connect server:7790) (token flag) (user you)))))
    Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the backend's -tokens NAME=TOKEN (or -host-tokens) entries.
    |}];
  (* Bad addresses say where they came from; a broken file how to fix it. *)
  show ~env:{ none with connect = Some "nohost" } ();
  write t ".prigh/tui-connection.sexp" "((connect nohost))";
  show ();
  write t ".prigh/tui-connection.sexp" "((connect";
  show ~flags:{ none with connect = Some "server:7790" } ();
  [%expect
    {|
    -connect: "nohost" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/) (from $PRIGH_CONNECT)
    -connect: "nohost" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/) (saved in $DIR/.prigh/tui-connection.sexp)
    cannot read the saved connection: ($DIR/.prigh/tui-connection.sexp
     (Failure
      "Sexplib.Sexp.gen_load_sexp: EOF in $DIR/.prigh/tui-connection.sexp while in state Parsing_list"))
    Fix or delete $DIR/.prigh/tui-connection.sexp, or pass -connect HOST:PORT.
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
       "POST %s HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: %d\r\n\r\n%s"
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
    Websocket.connect ~reader ~flow ~host:"127.0.0.1" ~target:"/ws" ()
    |> Result.map_error ~f:Websocket.Refusal.to_string
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
  http t ~sw ~port "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n";
  printf "code: %d hex digits\n" (String.length code);
  post_code t ~sw ~port "wrong";
  post_code t ~sw ~port code;
  post_code t ~sw ~port code;
  http
    t
    ~sw
    ~port
    (sprintf "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n" Signin_handoff.path);
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
     Host: 127.0.0.1\r\n\
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

(* [prigh desktop]: the same site, started with or without a working
   connection; the page drives it through [/desktop/] with the run's key. *)
module Desktop = struct
  type d =
    { url : string
    ; key : string
    ; port : int
    ; ended : Desktop_sidecar.Ended.t Eio.Promise.or_exn
    ; close_stdin : unit -> unit
    ; saved : Saved_connection.t Queue.t
    ; pages : Desktop_sidecar.Page.t Eio.Stream.t (** said after the first *)
    }

  (* A page line as the shell reads it, the page here's port and key
     masked. *)
  let page_line page =
    Re.replace
      (Re.compile (Re.Perl.re "127\\.0\\.0\\.1:[0-9]+/#desktop=[0-9a-f]+"))
      ~f:(fun _ -> "127.0.0.1:SITE/#desktop=<key>")
      (Desktop_sidecar.Page.to_json_line page)
  ;;

  let start
        ?(ui = Desktop_sidecar.Ui.Local)
        ?(tools = Web_connect.Tools.Local)
        ?save
        t
        ~sw
        ~logs
        initial
    =
    let saved = Queue.create () in
    let config : Desktop_sidecar.Config.t =
      { initial
      ; ui
      ; tools
      ; name = "laptop"
      ; host_id = "host-laptop"
      ; cwd = t.dir ^/ "here"
      ; listen = At (Eio.Net.Ipaddr.V4.loopback, 0)
      ; root = site t
      ; save =
          Option.value save ~default:(fun c ->
            Queue.enqueue saved c;
            Ok ())
      ; remember_signin =
          (fun remote grant ->
            printf
              "remember_signin: %s with %s (cookie %s)\n"
              (Backend_address.to_string remote)
              (Sexp.to_string_hum [%sexp (grant : Browser_signin.Grant.t)])
              (Option.value remote.proxy_cookie ~default:"none");
            Ok ())
      }
    in
    Core_unix.mkdir_p config.cwd;
    let stdin, stdin_writer = Eio_unix.pipe sw in
    let first, set_first = Eio.Promise.create () in
    let pages = Eio.Stream.create Int.max_value in
    let ended =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Desktop_sidecar.run
          ~env:t.env
          ~tool_host_wait:(Time_ns.Span.of_sec 10.)
          ~check_timeout:(Time_ns.Span.of_sec 5.)
          ~log:(Queue.enqueue logs)
          ~on_page:(fun page ->
            if not (Eio.Promise.try_resolve set_first page)
            then Eio.Stream.add pages page)
          ~stdin
          config)
    in
    let (first : Desktop_sidecar.Page.t) = Eio.Promise.await first in
    let url =
      if first.remote then Option.value_exn first.signin else first.url
    in
    if first.remote then printf "first page: %s\n" (page_line first);
    let port =
      String.lsplit2_exn url ~on:':'
      |> snd
      |> String.lsplit2_exn ~on:':'
      |> snd
      |> String.lsplit2_exn ~on:'/'
      |> fst
      |> Int.of_string
    in
    let key =
      String.lsplit2_exn url ~on:'='
      |> snd
      |> String.chop_suffix_if_exists ~suffix:"&signin=1"
    in
    if not first.remote
    then
      printf
        "url: %s\n"
        (url
         |> String.substr_replace_all
              ~pattern:(sprintf ":%d/" port)
              ~with_:":PORT/"
         |> String.substr_replace_all ~pattern:key ~with_:"<key>");
    { url
    ; key
    ; port
    ; ended
    ; close_stdin = (fun () -> Eio.Flow.close stdin_writer)
    ; saved
    ; pages
    }
  ;;

  (* The pages said since, as the shell reads them. *)
  let show_pages d =
    let rec loop () =
      match Eio.Stream.take_nonblocking d.pages with
      | Some page ->
        printf "page: %s\n" (page_line page);
        loop ()
      | None -> ()
    in
    loop ()
  ;;

  (* A [POST] of [fields] (and the key, unless [key] says otherwise) to
     [/desktop/what]; prints the answer with the codes masked. *)
  let post
        ?key
        ?(headers = "Host: 127.0.0.1\r\nContent-Type: application/json\r\n")
        t
        ~sw
        d
        what
        fields
    =
    let key = Option.value key ~default:d.key in
    let body = Jsonaf.to_string (`Object (("key", `String key) :: fields)) in
    let flow = connect t ~sw ~port:d.port in
    Eio.Flow.copy_string
      (sprintf
         "POST /desktop/%s HTTP/1.1\r\n%sContent-Length: %d\r\n\r\n%s"
         what
         headers
         (String.length body)
         body)
      flow;
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    let status = Eio.Buf_read.line reader in
    let rec skip () =
      match Eio.Buf_read.line reader with
      | "" -> ()
      | _ -> skip ()
    in
    skip ();
    let body = Eio.Buf_read.take_all reader in
    let json = Jsonaf.parse body |> Result.ok in
    let code =
      Option.bind json ~f:(fun json ->
        match field json "code" with
        | Some (`String code) -> Some code
        | _ -> None)
    in
    let shown =
      match code with
      | Some code ->
        String.substr_replace_all body ~pattern:code ~with_:"<code>"
      | None -> body
    in
    printf "%s %s: %s\n" what status (String.strip (mask t shown));
    code
  ;;
end

let backend_spec port = sprintf "127.0.0.1:%d" port

let unport port s =
  String.substr_replace_all s ~pattern:(sprintf ":%d" port) ~with_:":PORT"
;;

(* Which client the tool host is depends on the tests before. *)
let show_log t ~port line =
  let line =
    mask t (unport port line)
    |> Re.replace_string
         (Re.compile (Re.Perl.re "127\\.0\\.0\\.1:[0-9]+/$"))
         ~by:"127.0.0.1:SITE/"
  in
  match String.substr_index line ~pattern:" as client-" with
  | Some i -> print_endline (String.prefix line i ^ " as client-N")
  | None -> print_endline line
;;

let%expect_test
    "prigh desktop with no connection: the connect form, checked, saved, \
     signed in, then quit"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      t
      ~sw
      ~logs
      (Form { address = None; user = Some "me"; error = None })
  in
  let post ?key what fields =
    ignore (Desktop.post ?key t ~sw d what fields : string option)
  in
  [%expect {| url: http://127.0.0.1:PORT/#desktop=<key> |}];
  (* The page is served; the endpoints want the run's key. *)
  http t ~sw ~port:d.port "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n";
  post ~key:"wrong" "status" [];
  http
    t
    ~sw
    ~port:d.port
    "GET /desktop/status HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n";
  post "status" [];
  [%expect
    {|
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 22
      Content-Type: text/html; charset=utf-8
      body: <html>prigh-web</html>
    status HTTP/1.1 403 Forbidden: this is not the page prigh desktop opened: open prigh again
    HTTP/1.1 405 Method Not Allowed
      Cache-Control: no-cache
      Connection: close
      Content-Length: 19
      Content-Type: text/plain; charset=utf-8
      body: method not allowed

    status HTTP/1.1 200 OK: {"state":"connect","address":null,"user":"me","error":null}
    |}];
  (* Nothing to relay to yet. *)
  page_hello t ~sw ~port:d.port {|{"name": "prigh-web", "token": "sekrit"}|};
  [%expect {| (closed) |}];
  (* What is wrong, and what to do: the form stays, with what was typed. *)
  let here = backend_spec backend_port in
  let submit fields =
    ignore (Desktop.post t ~sw d "connect" fields : string option)
  in
  submit [];
  submit [ "address", `String "nowhere" ];
  submit [ "address", `String here ];
  submit
    [ "address", `String here; "token", `String "wrong"; "user", `String "me" ];
  post "status" [];
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    connect HTTP/1.1 200 OK: {"ok":false,"error":"Enter the backend's address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"\"nowhere\" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"127.0.0.1:PORT refused the login: unauthorised: bad user name or password\nNo password was given: enter the backend's token (its -token, or yours from its -tokens) as the password."}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"127.0.0.1:PORT refused the login: unauthorised: bad user name or password\nCheck the user name and password against the backend's -tokens NAME=TOKEN entries."}
    status HTTP/1.1 200 OK: {"state":"connect","address":"127.0.0.1:PORT","user":"me","error":"127.0.0.1:PORT refused the login: unauthorised: bad user name or password\nCheck the user name and password against the backend's -tokens NAME=TOKEN entries."}
    |}];
  (* Accepted: saved, the tool host connected, the page signed in by the
     code (once). *)
  let code =
    Desktop.post
      t
      ~sw
      d
      "connect"
      [ "address", `String here; "token", `String "sekrit" ]
  in
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  post "status" [];
  post_code t ~sw ~port:d.port (Option.value_exn code);
  post "status" [];
  submit [ "address", `String here; "token", `String "sekrit" ];
  page_hello
    t
    ~sw
    ~port:d.port
    {|{"name": "prigh-web", "token": "sekrit", "prefer_host": "host-laptop"}|};
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    connect HTTP/1.1 200 OK: {"ok":true,"code":"<code>"}
    (((connect 127.0.0.1:PORT) (token sekrit)))
    status HTTP/1.1 200 OK: {"state":"connected","address":"127.0.0.1:PORT","code":"<code>"}
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 58
      Content-Type: application/json
      body: {"user":null,"token":"sekrit","prefer_host":"host-laptop"}
    status HTTP/1.1 200 OK: {"state":"connected","address":"127.0.0.1:PORT","code":null}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"Already connected to 127.0.0.1:PORT: reload the page."}
    hello: ok, active host host-laptop; hosts: backend (<host>, $DIR), host-laptop (laptop, $DIR/here)
    |}];
  (* The page's Quit ends it, and with it the site. *)
  post ~key:"wrong" "quit" [];
  post "quit" [];
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  (match connect t ~sw ~port:d.port with
   | (_ : _ Eio.Net.stream_socket) -> print_endline "still listening"
   | exception exn -> print_endline (Net_error.to_string_hum exn));
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  [%expect
    {|
    quit HTTP/1.1 403 Forbidden: this is not the page prigh desktop opened: open prigh again
    quit HTTP/1.1 200 OK: {"ok":true}
    Quit
    connection refused
    prigh-web on http://127.0.0.1:SITE/
    127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    No password was given: enter the backend's token (its -token, or yours from its -tokens) as the password.
    127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check the user name and password against the backend's -tokens NAME=TOKEN entries.
    tool host: connected to 127.0.0.1:PORT as client-N
    tools run on this machine, in $DIR/here
    connected to 127.0.0.1:PORT
    quit from the page
    |}]
;;

let%expect_test
    "prigh desktop with the saved connection: signed in at once; stdin closing \
     ends it"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~tools:Remote
      t
      ~sw
      ~logs
      (Check
         { remote = address (backend_spec backend_port)
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Saved "~/.prigh/tui-connection.sexp"
         ; save = None
         })
  in
  let code = Desktop.post t ~sw d "status" [] in
  post_code t ~sw ~port:d.port (Option.value_exn code);
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"connected","address":"127.0.0.1:PORT","code":"<code>"}
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 49
      Content-Type: application/json
      body: {"user":null,"token":"sekrit","prefer_host":null}
    Stdin_closed
    prigh-web on http://127.0.0.1:SITE/
    tools run on the backend (-tools remote)
    connected to 127.0.0.1:PORT
    ()
    |}]
;;

let%expect_test
    "prigh desktop with a saved connection that fails: the form, saying so"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      t
      ~sw
      ~logs
      (Check
         { remote = address "127.0.0.1:1"
         ; credentials = { token = Some "sekrit"; user = Some "me" }
         ; source = Saved "~/.prigh/tui-connection.sexp"
         ; save = None
         })
  in
  ignore (Desktop.post t ~sw d "status" [] : string option);
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"connect","address":"127.0.0.1:1","user":"me","error":"The saved connection (~/.prigh/tui-connection.sexp) did not work: cannot reach the backend at 127.0.0.1:1: connection refused\nIs it running there (prigh serve -web HOST:PORT, or -listen)? Check the address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    Stdin_closed
    |}]
;;

let%expect_test
    "prigh desktop -ui remote: the connect form, then the backend's own \
     prigh-web; nothing relayed here"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      t
      ~sw
      ~logs
      (Form { address = None; user = None; error = None })
  in
  let here = backend_spec backend_port in
  ignore (Desktop.post t ~sw d "status" [] : string option);
  ignore
    (Desktop.post
       t
       ~sw
       d
       "connect"
       [ "address", `String (sprintf "ws://%s/ws" here)
       ; "token", `String "sekrit"
       ]
     : string option);
  Desktop.show_pages d;
  ignore (Desktop.post t ~sw d "status" [] : string option);
  (* The page here signs nothing in and relays nothing: the window is on
     the backend's site. *)
  post_code t ~sw ~port:d.port "anything";
  page_hello t ~sw ~port:d.port {|{"name": "prigh-web", "token": "sekrit"}|};
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"connect","address":null,"user":null,"error":null}
    connect HTTP/1.1 200 OK: {"ok":true,"remote":"http://127.0.0.1:PORT/#app=desktop&host=host-laptop"}
    page: {"url":"http://127.0.0.1:PORT/#app=desktop&host=host-laptop","remote":true,"signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    status HTTP/1.1 200 OK: {"state":"remote","url":"http://127.0.0.1:PORT/#app=desktop&host=host-laptop"}
    HTTP/1.1 405 Method Not Allowed
      Cache-Control: no-cache
      Connection: close
      Content-Length: 19
      body: method not allowed

    (closed)
    (((connect ws://127.0.0.1:PORT/ws) (token sekrit)))
    Stdin_closed
    prigh-web on http://127.0.0.1:SITE/
    tool host: connected to ws://127.0.0.1:PORT/ws as client-N
    tools run on this machine, in $DIR/here
    connected to ws://127.0.0.1:PORT/ws
    |}]
;;

let%expect_test
    "prigh desktop -ui remote with a working saved connection: straight to the \
     backend's site"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let start ~tools spec =
    let d =
      Desktop.start
        ~ui:Remote
        ~tools
        t
        ~sw
        ~logs
        (Check
           { remote = address spec
           ; credentials = { token = Some "sekrit"; user = None }
           ; source = Saved "~/.prigh/tui-connection.sexp"
           ; save = None
           })
    in
    Desktop.show_pages d;
    d.close_stdin ();
    print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)]
  in
  start ~tools:Local (backend_spec backend_port);
  start ~tools:Remote (sprintf "ws://127.0.0.1:%d/ws" backend_port);
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    first page: {"url":"http://127.0.0.1:PORT/#app=desktop&host=host-laptop","remote":true,"signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    Stdin_closed
    first page: {"url":"http://127.0.0.1:PORT/#app=desktop","remote":true,"signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    Stdin_closed
    tool host: connected to 127.0.0.1:PORT as client-N
    tools run on this machine, in $DIR/here
    connected to 127.0.0.1:PORT
    prigh-web on http://127.0.0.1:SITE/
    tools run on the backend (-tools remote)
    connected to ws://127.0.0.1:PORT/ws
    prigh-web on http://127.0.0.1:SITE/
    |}]
;;

let%expect_test
    "prigh desktop -ui remote with a saved connection that fails: the form here"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      t
      ~sw
      ~logs
      (Check
         { remote = address "127.0.0.1:1"
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Flag
         ; save = None
         })
  in
  ignore (Desktop.post t ~sw d "status" [] : string option);
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"connect","address":"127.0.0.1:1","user":null,"error":"-connect did not work: cannot reach the backend at 127.0.0.1:1: connection refused\nIs it running there (prigh serve -web HOST:PORT, or -listen)? Check the address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    Stdin_closed
    |}]
;;

let%expect_test "the remote site's URL, from each form of address" =
  List.iter
    [ "server:7790"
    ; "ws://server:7790/ws"
    ; "wss://ai.example.com/ws"
    ; "https://ai.example.com/"
    ; "https://example.com/prigh/"
    ; "wss://example.com:8443/prigh/ws"
    ; "[::1]:7790"
    ]
    ~f:(fun spec ->
      printf
        "%s -> %s\n"
        spec
        (Backend_address.web_url (Or_error.ok_exn (Backend_address.parse spec))));
  [%expect
    {|
    server:7790 -> http://server:7790/
    ws://server:7790/ws -> http://server:7790/
    wss://ai.example.com/ws -> https://ai.example.com/
    https://ai.example.com/ -> https://ai.example.com/
    https://example.com/prigh/ -> https://example.com/prigh/
    wss://example.com:8443/prigh/ws -> https://example.com:8443/prigh/
    [::1]:7790 -> http://[::1]:7790/
    |}]
;;

let%expect_test "the local sites: only their own pages, by their own names" =
  List.iter
    [ [ "Host", "127.0.0.1:7780" ]
    ; [ "Host", "localhost:7780" ]
    ; [ "Host", "[::1]:7780" ]
    ; [ "Host", "192.168.1.5:7780" ]
    ; [ "Host", "127.0.0.1" ]
    ; [ "Host", "127.0.0.1:7780"; "Origin", "http://127.0.0.1:7780" ]
    ; [ "host", "LOCALHOST:7780"; "origin", "http://localhost:7780" ]
    ; [ "Host", "127.0.0.1:7781" ]
    ; [ "Host", "evil.example:7780" ]
    ; [ "Host", "127.0.0.1.evil.example:7780" ]
    ; []
    ; [ "Host", "127.0.0.1:7780"; "Origin", "https://evil.example" ]
    ; [ "Host", "127.0.0.1:7780"; "Origin", "http://localhost:7780" ]
    ; [ "Host", "127.0.0.1:7780"; "Origin", "null" ]
    ]
    ~f:(fun headers ->
      let answer =
        match Local_site.check ~port:7780 ~headers with
        | Ok () -> "ok"
        | Error { status; body; _ } ->
          sprintf "%d %s" status (String.strip body)
      in
      print_s [%message answer (headers : (string * string) list)]);
  [%expect
    {|
    (ok (headers ((Host 127.0.0.1:7780))))
    (ok (headers ((Host localhost:7780))))
    (ok (headers ((Host [::1]:7780))))
    (ok (headers ((Host 192.168.1.5:7780))))
    (ok (headers ((Host 127.0.0.1))))
    (ok (headers ((Host 127.0.0.1:7780) (Origin http://127.0.0.1:7780))))
    (ok (headers ((host LOCALHOST:7780) (origin http://localhost:7780))))
    ("421 this site answers only at http://127.0.0.1:7780/ (or localhost, or this machine's IP address): open it there"
     (headers ((Host 127.0.0.1:7781))))
    ("421 this site answers only at http://127.0.0.1:7780/ (or localhost, or this machine's IP address): open it there"
     (headers ((Host evil.example:7780))))
    ("421 this site answers only at http://127.0.0.1:7780/ (or localhost, or this machine's IP address): open it there"
     (headers ((Host 127.0.0.1.evil.example:7780))))
    ("421 this site answers only at http://127.0.0.1:7780/ (or localhost, or this machine's IP address): open it there"
     (headers ()))
    ("403 only the page this site served may use it: open prigh's page itself"
     (headers ((Host 127.0.0.1:7780) (Origin https://evil.example))))
    ("403 only the page this site served may use it: open prigh's page itself"
     (headers ((Host 127.0.0.1:7780) (Origin http://localhost:7780))))
    ("403 only the page this site served may use it: open prigh's page itself"
     (headers ((Host 127.0.0.1:7780) (Origin null))))
    |}]
;;

(* Just the status line of [request]'s answer. *)
let status_line t ~sw ~port request =
  let flow = connect t ~sw ~port in
  Eio.Flow.copy_string request flow;
  let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  print_endline (Eio.Buf_read.line reader);
  Eio.Flow.close flow
;;

let upgrade ?(headers = "") target =
  sprintf
    "GET %s HTTP/1.1\r\n\
     Host: 127.0.0.1\r\n\
     %sUpgrade: websocket\r\n\
     Connection: Upgrade\r\n\
     Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
     Sec-WebSocket-Version: 13\r\n\
     \r\n"
    target
    headers
;;

let%expect_test
    "prigh desktop: other pages, other host names and simple requests are \
     refused"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      t
      ~sw
      ~logs
      (Check
         { remote = address (backend_spec backend_port)
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Flag
         ; save = None
         })
  in
  let post ?headers what =
    ignore (Desktop.post ?headers t ~sw d what [] : string option)
  in
  (* Once connected (the check is under way). *)
  post "status";
  (* Another page's WebSocket (no CORS for those), or under another name
     (DNS rebinding). *)
  let origin o = sprintf "Origin: %s\r\n" o in
  let page = sprintf "http://127.0.0.1:%d" d.port in
  status_line
    t
    ~sw
    ~port:d.port
    (upgrade ~headers:(origin "https://evil.example") "/ws");
  status_line
    t
    ~sw
    ~port:d.port
    (upgrade ~headers:(origin "https://evil.example") "/terminal?token=sekrit");
  status_line
    t
    ~sw
    ~port:d.port
    (String.substr_replace_first
       (upgrade "/ws")
       ~pattern:"Host: 127.0.0.1"
       ~with_:(sprintf "Host: evil.example:%d" d.port));
  status_line
    t
    ~sw
    ~port:d.port
    (String.substr_replace_first
       (upgrade ~headers:(origin page) "/ws")
       ~pattern:"Host: 127.0.0.1"
       ~with_:(sprintf "Host: 127.0.0.1:%d" d.port));
  page_hello t ~sw ~port:d.port {|{"name": "prigh-web", "token": "sekrit"}|};
  (* [/desktop/]: JSON only (so another page's needs a preflight), from the
     page itself. *)
  post ~headers:"Host: 127.0.0.1\r\n" "status";
  post ~headers:"Host: 127.0.0.1\r\nContent-Type: text/plain\r\n" "status";
  post
    ~headers:
      (sprintf
         "Host: 127.0.0.1:%d\r\nContent-Type: application/json\r\n%s"
         d.port
         (origin "https://evil.example"))
    "quit";
  post
    ~headers:
      (sprintf
         "Host: evil.example:%d\r\nContent-Type: application/json\r\n"
         d.port)
    "quit";
  post
    ~headers:
      (sprintf
         "Host: 127.0.0.1:%d\r\n\
          Content-Type: application/json; charset=utf-8\r\n\
          %s"
         d.port
         (origin page))
    "status";
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  (* The key goes to the shell only. *)
  List.iter (Queue.to_list logs) ~f:(fun line ->
    if String.is_substring line ~substring:"prigh-web on"
    then
      printf
        "logged: %s (key in it: %b)\n"
        (unport d.port line)
        (String.is_substring line ~substring:d.key));
  print_string (unport backend_port (unport d.port [%expect.output]));
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"connected","address":"127.0.0.1:PORT","code":"<code>"}
    HTTP/1.1 403 Forbidden
    HTTP/1.1 403 Forbidden
    HTTP/1.1 421 Misdirected Request
    HTTP/1.1 101 Switching Protocols
    hello: ok, active host backend; hosts: backend (<host>, $DIR), host-laptop (laptop, $DIR/here)
    status HTTP/1.1 415 Unsupported Media Type: a JSON body (Content-Type: application/json) expected
    status HTTP/1.1 415 Unsupported Media Type: a JSON body (Content-Type: application/json) expected
    quit HTTP/1.1 403 Forbidden: only the page this site served may use it: open prigh's page itself
    quit HTTP/1.1 421 Misdirected Request: this site answers only at http://127.0.0.1:PORT/ (or localhost, or this machine's IP address): open it there
    status HTTP/1.1 200 OK: {"state":"connected","address":"127.0.0.1:PORT","code":"<code>"}
    Stdin_closed
    logged: prigh-web on http://127.0.0.1:PORT/ (key in it: false)
    |}]
;;

let%expect_test
    "prigh desktop: an unencrypted backend elsewhere needs confirming"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      t
      ~sw
      ~logs
      (Form { address = None; user = None; error = None })
  in
  let submit fields =
    ignore (Desktop.post t ~sw d "connect" fields : string option)
  in
  (* 0.0.0.0 is not loopback to the check, but connecting to it reaches
     this machine, so the confirmed attempt fails at once. *)
  submit [ "address", `String "0.0.0.0:1"; "token", `String "sekrit" ];
  submit [ "address", `String "ws://0.0.0.0:1/ws"; "token", `String "sekrit" ];
  submit
    [ "address", `String "0.0.0.0:1"
    ; "token", `String "sekrit"
    ; "plaintext", `False
    ];
  submit
    [ "address", `String "0.0.0.0:1"
    ; "token", `String "sekrit"
    ; "plaintext", `True
    ];
  (* Loopback (an SSH tunnel) and TLS need no confirming. *)
  submit [ "address", `String "127.0.0.1:1"; "token", `String "sekrit" ];
  submit [ "address", `String "localhost:1"; "token", `String "sekrit" ];
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    connect HTTP/1.1 200 OK: {"ok":false,"error":"0.0.0.0:1 is not encrypted: the password and everything prigh sends would cross the network in the clear, and anyone on the way could run commands on this computer. Use the backend's HTTPS address (wss://host/ws, behind a TLS reverse proxy), or an SSH tunnel (ssh -L 7790:localhost:7790 host, then 127.0.0.1:7790). Connect anyway only on a network you trust.","plaintext":true}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"ws://0.0.0.0:1/ws is not encrypted: the password and everything prigh sends would cross the network in the clear, and anyone on the way could run commands on this computer. Use the backend's HTTPS address (wss://host/ws, behind a TLS reverse proxy), or an SSH tunnel (ssh -L 7790:localhost:7790 host, then 127.0.0.1:7790). Connect anyway only on a network you trust.","plaintext":true}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"0.0.0.0:1 is not encrypted: the password and everything prigh sends would cross the network in the clear, and anyone on the way could run commands on this computer. Use the backend's HTTPS address (wss://host/ws, behind a TLS reverse proxy), or an SSH tunnel (ssh -L 7790:localhost:7790 host, then 127.0.0.1:7790). Connect anyway only on a network you trust.","plaintext":true}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"cannot reach the backend at 0.0.0.0:1: connection refused\nIs it running there (prigh serve -web HOST:PORT, or -listen)? Check the address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"cannot reach the backend at 127.0.0.1:1: connection refused\nIs it running there (prigh serve -web HOST:PORT, or -listen)? Check the address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"cannot reach the backend at localhost:1: connection refused\nIs it running there (prigh serve -web HOST:PORT, or -listen)? Check the address: HOST:PORT (its -web or -listen port), or a prigh-web URL such as wss://host/ws."}
    Stdin_closed
    |}]
;;

let%expect_test
    "prigh desktop: a connection that fails while starting goes back to the \
     form, and the page waiting on status is answered"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~save:(fun (_ : Saved_connection.t) -> failwith "disk on fire")
      t
      ~sw
      ~logs
      (Form { address = None; user = None; error = None })
  in
  let here = backend_spec backend_port in
  ignore
    (Desktop.post
       t
       ~sw
       d
       "connect"
       [ "address", `String here; "token", `String "sekrit" ]
     : string option);
  ignore (Desktop.post t ~sw d "status" [] : string option);
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  print_string (unport backend_port [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    connect HTTP/1.1 200 OK: {"ok":false,"error":"connected to 127.0.0.1:PORT, but starting failed: (Failure \"disk on fire\")"}
    status HTTP/1.1 200 OK: {"state":"connect","address":"127.0.0.1:PORT","user":null,"error":"connected to 127.0.0.1:PORT, but starting failed: (Failure \"disk on fire\")"}
    Stdin_closed
    |}]
;;

(* [prigh desktop -ui remote] in front of a sign-in proxy (authentik's
   forward auth): the page here offers signing in through the default
   browser or in the window instead of the window looping through the
   login. *)
module Proxied = struct
  type p =
    { proxy : int
    ; remote : Backend_address.t
    }

  let start t ~sw ~backend_port ~ws_exempt =
    let proxy =
      Fake_signin_proxy.start
        ~env:t.env
        ~sw
        ~ws_exempt
        ~fresh_state:true
        ~upstream:backend_port
        ()
    in
    { proxy; remote = address (sprintf "ws://127.0.0.1:%d/ws" proxy) }
  ;;

  let cli_signin = Re.compile (Re.Perl.re "cli-signin=[0-9]+\\.[^&\"]+")

  (* Ports and the sign-in's one-time state masked. *)
  let clean t ~backend_port p s =
    Re.replace_string cli_signin ~by:"cli-signin=LISTENER.STATE" s
    |> mask t
    |> String.substr_replace_all
         ~pattern:(sprintf ":%d" p.proxy)
         ~with_:":PROXY"
    |> unport backend_port
  ;;

  (* The listener's port and state from the URL the shell is to open. *)
  let listener (page : Desktop_sidecar.Page.t) =
    let _, value = String.lsplit2_exn page.url ~on:'#' in
    let value = String.chop_prefix_exn value ~prefix:"cli-signin=" in
    let value, _ = String.lsplit2_exn value ~on:'&' in
    let port, state = String.lsplit2_exn value ~on:'.' in
    Int.of_string port, state
  ;;

  (* What the browser does once prigh-web's page there is allowed: posts
     the proxy's cookie with the state to the listener. *)
  let allow t ?(fields = []) page =
    let port, state = listener page in
    let response =
      Fake_signin_proxy.request
        ~env:t.env
        ~port
        ~meth:"POST"
        ~headers:[ "Content-Type", "application/x-www-form-urlencoded" ]
        ~body:
          (Uri.encoded_of_query
             (List.map
                ([ "state", state; "cookie", Fake_signin_proxy.cookie ] @ fields)
                ~f:(fun (k, v) -> k, [ v ])))
        "/callback"
    in
    List.hd_exn (String.split_lines response)
  ;;
end

let%expect_test "the site probe: a sign-in proxy's login, or not" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  let p = Proxied.start t ~sw ~backend_port ~ws_exempt:true in
  let probe remote =
    print_endline
      (Proxied.clean
         t
         ~backend_port
         p
         (Sexp.to_string_hum
            [%sexp (Site_probe.run ~env:t.env remote : Site_probe.Result.t)]))
  in
  probe p.remote;
  probe
    (Backend_address.with_proxy_cookie p.remote (Some Fake_signin_proxy.cookie));
  probe (address (sprintf "ws://127.0.0.1:%d/ws" backend_port));
  probe (address (sprintf "127.0.0.1:%d" p.proxy));
  probe (address "ws://127.0.0.1:1/ws");
  [%expect
    {|
    (Signin_required
     (why
      "127.0.0.1:PROXY answered with a login page (HTTP/1.1 302 Found to https://login.example.com/outpost.goauthentik.io/start)")
     (login (login.example.com)))
    Open
    Open
    Open
    (Unreachable "connection refused")
    |}]
;;

let%expect_test
    "prigh desktop -ui remote behind a sign-in proxy (/ws exempt): signing in \
     through the browser, then it expires"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  let p = Proxied.start t ~sw ~backend_port ~ws_exempt:true in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      t
      ~sw
      ~logs
      (Check
         { remote = p.remote
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Saved "~/.prigh/tui-connection.sexp"
         ; save = None
         })
  in
  let post ?(fields = []) what =
    ignore (Desktop.post t ~sw d what fields : string option)
  in
  let signin how = post "signin" ~fields:[ "how", `String how ] in
  post "status";
  (* Nothing is under way to open again or cancel. *)
  signin "again";
  signin "nonsense";
  post "connect" ~fields:[ "address", `String "127.0.0.1:1" ];
  (* Through the browser: the shell is told to open it there; the page
     shows it is waiting, and can open it again. *)
  let grant = ref "" in
  Eio.Fiber.both
    (fun () -> signin "browser")
    (fun () ->
       let page = Eio.Stream.take d.pages in
       printf "page: %s\n" (Desktop.page_line page);
       post "status";
       signin "again";
       Desktop.show_pages d;
       grant := Proxied.allow t page);
  print_endline !grant;
  Desktop.show_pages d;
  post "status";
  signin "browser";
  print_string (Proxied.clean t ~backend_port p [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":null,"error":null,"expired":false}
    signin HTTP/1.1 200 OK: {"ok":false,"error":"No sign-in through the browser is under way: start one."}
    signin HTTP/1.1 200 OK: {"ok":false,"error":"\"how\": \"browser\", \"again\", \"cancel\" or \"here\" expected"}
    connect HTTP/1.1 200 OK: {"ok":false,"error":"Signing in to 127.0.0.1:PROXY: reload the page."}
    page: {"url":"http://127.0.0.1:PROXY/#cli-signin=LISTENER.STATE&program=desktop","external":true}
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":"http://127.0.0.1:PROXY/#cli-signin=LISTENER.STATE&program=desktop","error":null,"expired":false}
    signin HTTP/1.1 200 OK: {"ok":true}
    page: {"url":"http://127.0.0.1:PROXY/#cli-signin=LISTENER.STATE&program=desktop","external":true}
    remember_signin: ws://127.0.0.1:PROXY/ws with ((proxy_cookie <opaque>) (token ()) (user ())) (cookie proxy_session=s3cret-session)
    signin HTTP/1.1 200 OK: {"ok":true,"remote":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    HTTP/1.1 200 OK
    page: {"url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop","remote":true,"cookie":"proxy_session=s3cret-session","signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    status HTTP/1.1 200 OK: {"state":"remote","url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    signin HTTP/1.1 200 OK: {"ok":false,"error":"Not signing in now: reload the page."}
    |}];
  (* The site sends the window to its login again: the shell brings it
     back here, and the page says so. Signing in in the window this time. *)
  post "status" ~fields:[ "signin", `True ];
  post "status";
  signin "here";
  Desktop.show_pages d;
  post "status";
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  print_string (Proxied.clean t ~backend_port p [%expect.output]);
  [%expect
    {|
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":null,"waiting":null,"error":null,"expired":true}
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":null,"waiting":null,"error":null,"expired":true}
    signin HTTP/1.1 200 OK: {"ok":true,"remote":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    page: {"url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop","remote":true,"signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1","here":true}
    status HTTP/1.1 200 OK: {"state":"remote","url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    Stdin_closed
    tool host: connected to ws://127.0.0.1:PROXY/ws as client-N
    tools run on this machine, in $DIR/here
    connected to ws://127.0.0.1:PROXY/ws
    127.0.0.1:PROXY is behind a sign-in (login.example.com)
    prigh-web on http://127.0.0.1:SITE/
    signing in through the browser
    signed in through the browser
    the sign-in to 127.0.0.1:PROXY expired: signing in again
    signing in in the window
    ()
    |}]
;;

let%expect_test
    "prigh desktop -ui remote behind a sign-in proxy: cancelling, a failed \
     sign-in, then in the window"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  let p = Proxied.start t ~sw ~backend_port ~ws_exempt:true in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      ~tools:Remote
      t
      ~sw
      ~logs
      (Check
         { remote = p.remote
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Flag
         ; save = None
         })
  in
  let post ?(fields = []) what =
    ignore (Desktop.post t ~sw d what fields : string option)
  in
  let signin how = post "signin" ~fields:[ "how", `String how ] in
  (* Cancelled here; a second click while one is under way joins it
     without opening the browser again. *)
  Eio.Fiber.all
    [ (fun () -> signin "browser")
    ; (fun () ->
        let (_ : Desktop_sidecar.Page.t) = Eio.Stream.take d.pages in
        print_endline "the browser is to open";
        Eio.Fiber.both
          (fun () -> signin "browser")
          (fun () ->
             post "status";
             signin "cancel"))
    ];
  Desktop.show_pages d;
  post "status";
  (* Cancelled in the browser. *)
  let grant = ref "" in
  Eio.Fiber.both
    (fun () -> signin "browser")
    (fun () ->
       grant
       := Proxied.allow t ~fields:[ "cancel", "1" ] (Eio.Stream.take d.pages));
  print_endline !grant;
  post "status";
  signin "here";
  Desktop.show_pages d;
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_string (Proxied.clean t ~backend_port p [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    the browser is to open
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":"http://127.0.0.1:PROXY/#cli-signin=LISTENER.STATE&program=desktop","error":null,"expired":false}
    signin HTTP/1.1 200 OK: {"ok":true}
    signin HTTP/1.1 200 OK: {"ok":false,"cancelled":true}
    signin HTTP/1.1 200 OK: {"ok":false,"cancelled":true}
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":null,"error":null,"expired":false}
    signin HTTP/1.1 200 OK: {"ok":false,"error":"Signing in through the browser did not work: the sign-in was cancelled in the browser. Try again, or sign in here."}
    HTTP/1.1 200 OK
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":null,"error":"Signing in through the browser did not work: the sign-in was cancelled in the browser. Try again, or sign in here.","expired":false}
    signin HTTP/1.1 200 OK: {"ok":true,"remote":"http://127.0.0.1:PROXY/#app=desktop"}
    page: {"url":"http://127.0.0.1:PROXY/#app=desktop","remote":true,"signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1","here":true}
    Stdin_closed
    tools run on the backend (-tools remote)
    connected to ws://127.0.0.1:PROXY/ws
    127.0.0.1:PROXY is behind a sign-in (login.example.com)
    prigh-web on http://127.0.0.1:SITE/
    signing in through the browser
    the sign-in through the browser was cancelled
    signing in through the browser
    Signing in through the browser did not work: the sign-in was cancelled in the browser. Try again, or sign in here.
    signing in in the window
    |}]
;;

let%expect_test
    "prigh desktop -ui remote with /ws behind the sign-in proxy too: the form, \
     signing in, then saved and the tool host started with the cookie"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  let p = Proxied.start t ~sw ~backend_port ~ws_exempt:false in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      t
      ~sw
      ~logs
      (Form { address = None; user = None; error = None })
  in
  let post ?(fields = []) what =
    ignore (Desktop.post t ~sw d what fields : string option)
  in
  post
    "connect"
    ~fields:
      [ "address", `String (Backend_address.to_string p.remote)
      ; "token", `String "sekrit"
      ];
  post "status";
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  let grant = ref "" in
  Eio.Fiber.both
    (fun () -> post "signin" ~fields:[ "how", `String "browser" ])
    (fun () -> grant := Proxied.allow t (Eio.Stream.take d.pages));
  print_endline !grant;
  print_s [%sexp (Queue.to_list d.saved : Saved_connection.t list)];
  Desktop.show_pages d;
  post "status";
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_string (Proxied.clean t ~backend_port p [%expect.output]);
  [%expect
    {|
    url: http://127.0.0.1:PORT/#desktop=<key>
    connect HTTP/1.1 200 OK: {"ok":true,"signin":true}
    status HTTP/1.1 200 OK: {"state":"signin","site":"127.0.0.1:PROXY","login":"login.example.com","waiting":null,"error":null,"expired":false}
    ()
    remember_signin: ws://127.0.0.1:PROXY/ws with ((proxy_cookie <opaque>) (token ()) (user ())) (cookie proxy_session=s3cret-session)
    signin HTTP/1.1 200 OK: {"ok":true,"remote":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    HTTP/1.1 200 OK
    (((connect ws://127.0.0.1:PROXY/ws) (token sekrit)))
    page: {"url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop","remote":true,"cookie":"proxy_session=s3cret-session","signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    status HTTP/1.1 200 OK: {"state":"remote","url":"http://127.0.0.1:PROXY/#app=desktop&host=host-laptop"}
    Stdin_closed
    prigh-web on http://127.0.0.1:SITE/
    127.0.0.1:PROXY is behind a sign-in (login.example.com)
    signing in through the browser
    signed in through the browser
    tool host: connected to ws://127.0.0.1:PROXY/ws as client-N
    tools run on this machine, in $DIR/here
    connected to ws://127.0.0.1:PROXY/ws
    |}]
;;

let%expect_test
    "prigh desktop -ui remote with a saved sign-in cookie: straight to the \
     site, with it"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _h, backend_port = backend t ~sw in
  let p = Proxied.start t ~sw ~backend_port ~ws_exempt:false in
  Eio.Switch.run
  @@ fun sw ->
  let logs = Queue.create () in
  let d =
    Desktop.start
      ~ui:Remote
      ~tools:Remote
      t
      ~sw
      ~logs
      (Check
         { remote =
             Backend_address.with_proxy_cookie
               p.remote
               (Some Fake_signin_proxy.cookie)
         ; credentials = { token = Some "sekrit"; user = None }
         ; source = Saved "~/.prigh/tui-connection.sexp"
         ; save = None
         })
  in
  d.close_stdin ();
  print_s [%sexp (Eio.Promise.await_exn d.ended : Desktop_sidecar.Ended.t)];
  List.iter (Queue.to_list logs) ~f:(show_log t ~port:backend_port);
  print_string (Proxied.clean t ~backend_port p [%expect.output]);
  [%expect
    {|
    first page: {"url":"http://127.0.0.1:PROXY/#app=desktop","remote":true,"cookie":"proxy_session=s3cret-session","signin":"http://127.0.0.1:SITE/#desktop=<key>&signin=1"}
    Stdin_closed
    tools run on the backend (-tools remote)
    connected to ws://127.0.0.1:PROXY/ws
    prigh-web on http://127.0.0.1:SITE/
    |}]
;;
