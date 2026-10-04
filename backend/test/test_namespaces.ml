open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let%expect_test "parsing -tokens specs" =
  List.iter
    [ "a=tok-a,b=tok-b"
    ; " a = tok-a , , default=tok-d ,"
    ; "a=x=y"
    ; "a=tok,a=other"
    ; "a=tok,b=tok"
    ; "a b=tok"
    ; "=tok"
    ; "a/../b=tok"
    ; "a="
    ; "a"
    ; " , "
    ]
    ~f:(fun spec ->
      print_s
        [%message
          spec ~_:(Namespace.parse_spec spec : Namespace.t list Or_error.t)]);
  [%expect
    {|
    (a=tok-a,b=tok-b (Ok (((name a) (token tok-a)) ((name b) (token tok-b)))))
    (" a = tok-a , , default=tok-d ,"
     (Ok (((name a) (token tok-a)) ((name default) (token tok-d)))))
    (a=x=y (Ok (((name a) (token x=y)))))
    (a=tok,a=other (Error "-tokens: duplicate namespace \"a\""))
    (a=tok,b=tok
     (Error "-tokens: namespace \"a\" reuses another namespace's token"))
    ("a b=tok"
     (Error "-tokens: bad namespace name \"a b\" (use letters, digits, _ and -)"))
    (=tok
     (Error "-tokens: bad namespace name \"\" (use letters, digits, _ and -)"))
    (a/../b=tok
     (Error
      "-tokens: bad namespace name \"a/../b\" (use letters, digits, _ and -)"))
    (a= (Error "-tokens: empty token for namespace \"a\""))
    (a (Error "-tokens entry \"a\" must be NAME=TOKEN"))
    (" , " (Error "-tokens: no NAME=TOKEN entries"))
    |}]
;;

let home t = Filename.concat t.dir "home"

(* A server per namespace, set up like [prigh serve -tokens]: each from its
   own world. *)
let make_router ?(backend_host = true) ?(replies = []) t ~sw spec =
  let worlds = ref [] in
  let router =
    Rpc_router.namespaced
      (Or_error.ok_exn (Namespace.parse_spec spec))
      ~home:(home t)
      ~legacy_auth_file:(Filename.concat t.dir "legacy-auth.json")
      ~create_server:(fun namespace world ->
        worlds := !worlds @ [ namespace.name, world ];
        let provider = Faux_provider.create replies in
        let new_agent ?session ~cwd () =
          Agent.create
            ~env:t.env
            ~sw
            ~provider
            ~tools:Tools.all
            ~sessions_dir:world.sessions_dir
            ~home:world.home
            ?session
            ~backend_host
            ~cwd
            ()
        in
        Rpc_server.create
          ~env:t.env
          ~sw
          ~token:namespace.token
          ~namespace:namespace.name
          ~backend_host
          ~login:
            (Login_manager.create
               ~env:t.env
               ~sw
               ~getenv:world.getenv
               ~store:world.store
               ())
          ~sessions_dir:world.sessions_dir
          ~cwd:t.dir
          ~new_agent
          ())
  in
  router, !worlds
;;

let server router name =
  List.Assoc.find_exn (Rpc_router.servers router) ~equal:String.equal name
;;

let request ?(id = "r") meth params =
  Json.of_string
    (sprintf {|{"id": "%s", "method": "%s", "params": %s}|} id meth params)
;;

let call server client ?(params = "{}") meth =
  Rpc_server.handle server client (request meth params)
;;

let result json = Option.value_exn (Json.member "result" json)

let connect server ~token ?(fields = "") () =
  let sent = Queue.create () in
  let client = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  let response =
    call
      server
      client
      ~params:(sprintf {|{"token": "%s"%s}|} token fields)
      "hello"
  in
  client, sent, response
;;

let show t json = print_endline (mask t (Json.to_string json))

let%expect_test "namespaces: worlds, sessions, logins, tool hosts, config" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router, worlds = make_router t ~sw "a=tok-a,b=tok-b,default=tok-d" in
  List.iter worlds ~f:(fun (name, (world : Namespace.World.t)) ->
    print_endline
      (mask
         t
         (sprintf
            "%s: home=%s sessions=%s auth=%s"
            name
            world.home
            world.sessions_dir
            (Auth_store.path world.store))));
  print_s
    [%sexp (Sys_unix.ls_dir (home t ^/ ".prigh/namespaces") : string list)];
  [%expect
    {|
    a: home=$DIR/home/.prigh/namespaces/a sessions=$DIR/home/.prigh/namespaces/a/.prigh/sessions auth=$DIR/home/.prigh/namespaces/a/.config/prigh/auth.json
    b: home=$DIR/home/.prigh/namespaces/b sessions=$DIR/home/.prigh/namespaces/b/.prigh/sessions auth=$DIR/home/.prigh/namespaces/b/.config/prigh/auth.json
    default: home=$DIR/home sessions=$DIR/home/.prigh/sessions auth=$DIR/legacy-auth.json
    (a b)
    |}];
  let a = server router "a" in
  let b = server router "b" in
  let ca, _, _ = connect a ~token:"tok-a" () in
  let cb, _, _ = connect b ~token:"tok-b" () in
  (* A namespace's token is not valid in another. *)
  let intruder = Rpc_server.connect b ~send:ignore in
  show t (call b intruder ~params:{|{"token": "tok-a"}|} "hello");
  show t (call b intruder ~params:{|{"user": "a", "token": "tok-a"}|} "hello");
  show t (call b intruder ~params:{|{"user": "a", "token": "tok-b"}|} "hello");
  show t (call b intruder "list_sessions");
  (* The user name is the namespace's, and the hello result says which. *)
  let namespace_of response =
    Option.bind (Json.member "result" response) ~f:(Json.member "namespace")
    |> Option.value_map ~default:"-" ~f:Json.to_string
  in
  print_endline
    (namespace_of
       (call b intruder ~params:{|{"user": "b", "token": "tok-b"}|} "hello"));
  print_endline
    (namespace_of
       (call
          b
          (Rpc_server.connect b ~send:ignore)
          ~params:{|{"token": "tok-b"}|}
          "hello"));
  [%expect
    {|
    {"type":"response","id":"r","ok":false,"error":"unauthorised: bad user name or password"}
    {"type":"response","id":"r","ok":false,"error":"unauthorised: bad user name or password"}
    {"type":"response","id":"r","ok":false,"error":"unauthorised: bad user name or password"}
    {"type":"response","id":"r","ok":false,"error":"unauthorised: send hello with the token first"}
    "b"
    "b"
    |}];
  (* Sessions. *)
  ignore (call a ca ~params:{|{"name": "in a"}|} "set_session_name" : Json.t);
  let sessions server client =
    match result (call server client "list_sessions") with
    | `Array items ->
      List.map items ~f:(fun s ->
        Json.member "name" s |> Option.value_map ~default:"" ~f:Json.to_string)
    | _ -> []
  in
  print_s [%message (sessions a ca : string list) (sessions b cb : string list)];
  [%expect {| (("sessions a ca" ("\"in a\"")) ("sessions b cb" ())) |}];
  (* Logins: a stored key in a's store only. *)
  let world name = List.Assoc.find_exn worlds ~equal:String.equal name in
  Or_error.ok_exn (Auth_store.set (world "a").store Deepseek (Api_key "sk-a"));
  let deepseek server client =
    match result (call server client "auth_status") with
    | `Array items ->
      List.find_map items ~f:(fun s ->
        match Json.member "provider" s with
        | Some (`String "deepseek") -> Json.member "configured" s
        | _ -> None)
      |> Option.value_map ~default:"?" ~f:Json.to_string
    | _ -> "?"
  in
  print_s [%message (deepseek a ca : string) (deepseek b cb : string)];
  [%expect
    {|
    (("deepseek a ca" "{\"method\":\"api_key\",\"source\":\"stored api key\"}")
     ("deepseek b cb" null))
    |}];
  (* Tool hosts: a's host is not visible in b. *)
  let _host, _, _ =
    connect a ~token:"tok-a" ~fields:{|, "name": "laptop", "tools": true|} ()
  in
  let hosts server client =
    (Agent.state (Rpc_server.agent_of_client server client)).hosts
    |> List.map ~f:(fun (h : Agent.Host.t) -> h.id)
  in
  print_s [%message (hosts a ca : string list) (hosts b cb : string list)];
  [%expect {| (("hosts a ca" (backend client-2)) ("hosts b cb" (backend))) |}];
  (* Config. *)
  ignore
    (call
       a
       ca
       ~params:{|{"config": {"confirm_tools": true, "scoped_models": []}}|}
       "set_config"
     : Json.t);
  let confirm server client =
    Json.member "confirm_tools" (result (call server client "get_config"))
    |> Option.value_map ~default:"?" ~f:Json.to_string
  in
  print_s [%message (confirm a ca : string) (confirm b cb : string)];
  print_s
    [%sexp
      (Sys_unix.file_exists_exn
         (home t ^/ ".prigh/namespaces/a/.prigh/config.json")
       : bool)];
  [%expect
    {|
    (("confirm a ca" true) ("confirm b cb" false))
    true
    |}];
  Rpc_router.shutdown router
;;

let%expect_test "routing connections by the hello token" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router, _ = make_router t ~sw "a=tok-a,b=tok-b" in
  let serve lines =
    let lines = ref lines in
    Rpc_router.serve_lines
      router
      ~read_line:(fun () ->
        match !lines with
        | [] -> None
        | line :: rest ->
          lines := rest;
          Some line)
      ~write_line:(fun line ->
        let json = Json.of_string line in
        match Json.member "type" json with
        | Some (`String "response") ->
          let session_path =
            Option.bind
              (Json.member "result" json)
              ~f:(Json.member "session_path")
          in
          print_endline
            (mask
               t
               (Json.to_string
                  (match session_path with
                   | Some path -> `Object [ "session_path", path ]
                   | None -> json)))
        | _ -> ());
    print_endline "--"
  in
  serve [ {|{"id": 1, "method": "ping"}|}; {|{"id": 2, "method": "ping"}|} ];
  serve [ "not json" ];
  serve [ {|{"id": 3, "method": "hello", "params": {}}|} ];
  serve [ {|{"id": 4, "method": "hello", "params": {"token": "nope"}}|} ];
  serve
    [ ""
    ; {|{"id": 5, "method": "hello", "params": {"token": "tok-b"}}|}
    ; {|{"id": 6, "method": "get_state"}|}
    ];
  serve
    [ {|{"id": 7, "method": "hello", "params": {"user": "a", "token": "tok-b"}}|}
    ; {|{"id": 8, "method": "get_state"}|}
    ];
  serve [ {|{"id": 9, "method": "hello", "params": {"user": "b"}}|} ];
  serve
    [ {|{"id": 10, "method": "hello", "params": {"user": "b", "token": "tok-b"}}|}
    ; {|{"id": 11, "method": "get_state"}|}
    ];
  [%expect
    {|
    {"type":"response","id":1,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":null,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":3,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":4,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":5,"ok":true,"result":{"client_id":"client-1","namespace":"b","state":{"session_id":"<id>","session_path":"$DIR/home/.prigh/namespaces/b/.prigh/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[]}}}
    {"session_path":"$DIR/home/.prigh/namespaces/b/.prigh/sessions/<stamp>_<id>.jsonl"}
    --
    {"type":"response","id":7,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":9,"ok":false,"error":"unauthorised: bad user name or password"}
    --
    {"type":"response","id":10,"ok":true,"result":{"client_id":"client-2","namespace":"b","state":{"session_id":"<id>","session_path":"$DIR/home/.prigh/namespaces/b/.prigh/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[]}}}
    {"session_path":"$DIR/home/.prigh/namespaces/b/.prigh/sessions/<stamp>_<id>.jsonl"}
    --
    |}];
  let which ?user token =
    match Rpc_router.lookup router ?user ~token () with
    | None -> "none"
    | Some s ->
      List.find_map_exn (Rpc_router.servers router) ~f:(fun (name, s') ->
        Option.some_if (phys_equal s s') name)
  in
  print_s
    [%message
      (which (Some "tok-a") : string)
        (which (Some "tok-b") : string)
        (which (Some "tok-c") : string)
        (which None : string)
        (which ~user:"a" (Some "tok-a") : string)
        (which ~user:"b" (Some "tok-a") : string)
        (which ~user:"a" None : string)
        (which ~user:"c" (Some "tok-c") : string)];
  [%expect
    {|
    (("which (Some \"tok-a\")" a) ("which (Some \"tok-b\")" b)
     ("which (Some \"tok-c\")" none) ("which None" none)
     ("which ~user:\"a\" (Some \"tok-a\")" a)
     ("which ~user:\"b\" (Some \"tok-a\")" none) ("which ~user:\"a\" None" none)
     ("which ~user:\"c\" (Some \"tok-c\")" none))
    |}];
  Rpc_router.shutdown router
;;

let%expect_test "single mode: lookup follows the server's token" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server ?token () =
    Rpc_router.single
      (Rpc_server.create
         ~env:t.env
         ~sw
         ?token
         ~login:
           (Login_manager.create
              ~env:t.env
              ~sw
              ~store:(Auth_store.create ~path:(t.dir ^/ "auth.json"))
              ())
         ~sessions_dir:(t.dir ^/ "sessions")
         ~cwd:t.dir
         ~new_agent:(fun ?session:_ ~cwd:_ () -> assert false)
         ())
  in
  let open_ = server () in
  let locked = server ~token:"s" () in
  let found ?user router token =
    Option.is_some (Rpc_router.lookup router ?user ~token ())
  in
  print_s
    [%message
      (found open_ None : bool)
        (found open_ (Some "x") : bool)
        (found locked None : bool)
        (found locked (Some "x") : bool)
        (found locked (Some "s") : bool)
        (found ~user:"anyone" open_ None : bool)
        (found ~user:"anyone" locked (Some "s") : bool)
        (found ~user:"anyone" locked (Some "x") : bool)];
  [%expect
    {|
    (("found open_ None" true) ("found open_ (Some \"x\")" true)
     ("found locked None" false) ("found locked (Some \"x\")" false)
     ("found locked (Some \"s\")" true)
     ("found ~user:\"anyone\" open_ None" true)
     ("found ~user:\"anyone\" locked (Some \"s\")" true)
     ("found ~user:\"anyone\" locked (Some \"x\")" false))
    |}]
;;

let%expect_test "provider keys from the environment: ignored in namespaces" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.putenv ~key:"DEEPSEEK_API_KEY" ~data:"sk-env";
  Exn.protect
    ~finally:(fun () -> Core_unix.unsetenv "DEEPSEEK_API_KEY")
    ~f:(fun () ->
      let router, worlds = make_router t ~sw "a=tok-a,default=tok-d" in
      let legacy =
        Namespace.World.legacy ~home:(home t) ~auth_file:(t.dir ^/ "auth.json")
      in
      let deepseek (world : Namespace.World.t) =
        Login_manager.status
          (Login_manager.create
             ~env:t.env
             ~sw
             ~getenv:world.getenv
             ~store:world.store
             ())
        |> Or_error.ok_exn
        |> List.find_map_exn ~f:(fun (s : Provider_auth.Status.t) ->
          Option.some_if (Provider_id.equal s.provider Deepseek) s.configured)
      in
      List.iter
        (worlds @ [ "single-token mode", legacy ])
        ~f:(fun (name, world) ->
          print_s
            [%message
              name
                ~_:(deepseek world : (Provider_auth.Method.t * string) option)]);
      let a = server router "a" in
      let client, _, _ = connect a ~token:"tok-a" () in
      let configured =
        match result (call a client "auth_status") with
        | `Array items ->
          List.find_map_exn items ~f:(fun s ->
            match Json.member "provider" s with
            | Some (`String "deepseek") -> Json.member "configured" s
            | _ -> None)
          |> Json.to_string
        | _ -> "?"
      in
      print_endline ("auth_status over rpc in a: " ^ configured);
      Rpc_router.shutdown router);
  [%expect
    {|
    (a ())
    (default ())
    ("single-token mode" ((Api_key DEEPSEEK_API_KEY)))
    auth_status over rpc in a: null
    |}]
;;

(* Over a real listener: the pi web frontend and the terminal pick their
   namespace by the [token] in the query. *)
let%expect_test "pi-web and /terminal look the namespace up by token" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router, _ = make_router t ~sw "a=tok-a,b=tok-b" in
  let b = server router "b" in
  let terminals = Terminals.create ~env:t.env ~sw () in
  let port =
    Web_server.listen
      ~env:t.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:None
      ~websockets:
        [ "/ws", Pi_rpc.serve_websocket router
        ; "/terminal", Web_server.serve_terminal router terminals
        ]
      ~on_lines:(Rpc_router.serve_lines router)
  in
  let browser target =
    let flow =
      Eio.Net.connect
        ~sw
        (Eio.Stdenv.net t.env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    Eio.Flow.copy_string
      (sprintf
         "GET %s HTTP/1.1\r\n\
          Host: x\r\n\
          Connection: Upgrade\r\n\
          Upgrade: websocket\r\n\
          Sec-WebSocket-Version: 13\r\n\
          Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
          \r\n"
         target)
      flow;
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    let rec skip_headers () =
      match Eio.Buf_read.line reader with
      | "" -> ()
      | _ -> skip_headers ()
    in
    skip_headers ();
    Websocket.create ~role:`Client ~reader ~flow ()
  in
  let first target =
    let ws = browser target in
    Option.iter (Websocket.read_text ws) ~f:(fun line ->
      print_endline (mask t line));
    ws
  in
  Websocket.close (first "/ws?token=nope");
  Websocket.close (first "/ws");
  Websocket.close (first "/ws?token=tok-b&user=a");
  Websocket.close (first "/ws?token=tok-b&user=b");
  let ws = first "/ws?token=tok-b" in
  Websocket.send_text ws {|{"id":"1","type":"get_state"}|};
  let rec until_response () =
    match Websocket.read_text ws with
    | None -> ()
    | Some line ->
      if String.is_substring line ~substring:{|"type":"response"|}
      then (
        let json = Json.of_string line in
        print_endline
          (mask
             t
             (Json.to_string
                (Option.value_exn
                   (Option.bind
                      (Json.member "data" json)
                      ~f:(Json.member "sessionFile"))))))
      else until_response ()
  in
  until_response ();
  Websocket.close ws;
  [%expect
    {|
    {"type":"prigh_hello_failed","error":"unauthorised: bad user name or password"}
    {"type":"prigh_hello_failed","error":"unauthorised: bad user name or password"}
    {"type":"prigh_hello_failed","error":"unauthorised: bad user name or password"}
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    "$DIR/home/.prigh/namespaces/b/.prigh/sessions/<stamp>_<id>.jsonl"
    |}];
  Websocket.close (first "/terminal?token=nope");
  Websocket.close (first "/terminal?token=tok-b&user=a");
  [%expect
    {|
    {"type":"error","message":"unauthorised: bad user name or password"}
    {"type":"error","message":"unauthorised: bad user name or password"}
    |}];
  (* A session whose tools run on a client host: relayed to that host, within
     the namespace. *)
  let client, _, _ = connect b ~token:"tok-b" () in
  let agent = Rpc_server.agent_of_client b client in
  let session_id = Session.id (Agent.session agent) in
  let laptop, laptop_sent, _ =
    connect
      b
      ~token:"tok-b"
      ~fields:
        (sprintf
           {|, "tools": true, "cwd": "/home/me", "session": "%s"|}
           session_id)
      ()
  in
  let ws =
    browser (sprintf "/terminal?user=b&token=tok-b&session=%s" session_id)
  in
  let is_open json =
    Option.equal
      Json.exactly_equal
      (Json.member "event" json)
      (Some (`String "terminal_open"))
  in
  let rec await_open () =
    match Queue.find laptop_sent ~f:is_open with
    | Some event -> event
    | None ->
      Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.01;
      await_open ()
  in
  let opened = await_open () in
  show t opened;
  let term_id =
    match Json.member "term_id" opened with
    | Some (`String id) -> id
    | _ -> assert false
  in
  show
    t
    (call
       b
       laptop
       ~params:
         (sprintf
            {|{"term_id": "%s", "kind": "text", "data": "{\"type\":\"exit\"}"}|}
            term_id)
       "terminal_frame");
  show
    t
    (call
       b
       laptop
       ~params:(sprintf {|{"term_id": "%s"}|} term_id)
       "terminal_closed");
  let rec drain () =
    match Websocket.read_text ws with
    | None -> print_endline "browser socket closed"
    | Some line ->
      print_endline line;
      drain ()
  in
  drain ();
  [%expect
    {|
    {"type":"event","event":"terminal_open","host":"client-4","term_id":"term-1","key":"b:<id>","cwd":"/home/me","cols":80,"rows":24}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"exit"}
    browser socket closed
    |}];
  Rpc_router.shutdown router
;;
