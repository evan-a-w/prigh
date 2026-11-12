open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let home t = Filename.concat t.dir "home"

(* A server per namespace, as [prigh serve -tokens] makes them. *)
let make_router t ~sw spec =
  let access =
    User_access.create
      (Or_error.ok_exn (Namespace.parse_spec spec))
      ~superusers:[]
      ~host_tokens:[]
    |> Or_error.ok_exn
  in
  Rpc_router.namespaced
    access
    ~home:(home t)
    ~legacy_auth_file:(Filename.concat t.dir "legacy-auth.json")
    ~create_server:(fun namespace world ->
      let provider = Faux_provider.create [] in
      let new_agent ?session ~cwd () =
        Agent.create
          ~env:t.env
          ~sw
          ~provider
          ~tools:Tools.all
          ~sessions_dir:world.sessions_dir
          ~home:world.home
          ?session
          ~backend_host:true
          ~cwd
          ()
      in
      Rpc_server.create
        ~env:t.env
        ~sw
        ~token:namespace.token
        ~namespace:namespace.name
        ~backend_host:true
        ~home:world.home
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
;;

let server router name =
  List.Assoc.find_exn (Rpc_router.servers router) ~equal:String.equal name
;;

let call server client ?(params = "{}") meth =
  Rpc_server.handle
    server
    client
    (Json.of_string
       (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
;;

let connect server ~token ?(fields = "") () =
  let sent = Queue.create () in
  let client = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  ignore
    (call
       server
       client
       ~params:(sprintf {|{"token": "%s"%s}|} token fields)
       "hello"
     : Json.t);
  client, sent
;;

let field json name =
  match Json.member name json with
  | Some (`String s) -> s
  | Some `Null | None -> "-"
  | Some other -> Json.to_string other
;;

(* A response, the config fields new sessions take, or the error. *)
let show_config t response =
  match Json.member "result" response, Json.member "error" response with
  | Some config, _ ->
    print_endline
      (mask
         t
         (sprintf
            "default_host=%s default_cwd=%s approval_mode=%s"
            (field config "default_host")
            (field config "default_cwd")
            (field config "approval_mode")))
  | None, Some (`String e) ->
    print_endline
      (mask
         t
         ("error: "
          ^ String.substr_replace_all
              e
              ~pattern:(Core_unix.gethostname ())
              ~with_:"<host>"))
  | None, _ -> print_endline (Json.to_string response)
;;

let show_session t server client =
  let s = Agent.state (Rpc_server.agent_of_client server client) in
  print_endline
    (mask
       t
       (sprintf
          "on %s in %s (mode %s)"
          s.active_host
          s.cwd
          (Approval_mode.to_string s.approval_mode)))
;;

(* What reached a client since last asked: the config events only. *)
let config_events sent =
  let events = Queue.to_list sent in
  Queue.clear sent;
  List.iter events ~f:(fun json ->
    if String.equal (field json "event") "config_changed"
    then (
      let config = Option.value_exn (Json.member "config" json) in
      printf
        "config_changed: default_host=%s fallback=%s\n"
        (field config "default_host")
        (field config "fallback_models")))
;;

let%expect_test
    "default_host and default_cwd: where prigh-web's new sessions start, per \
     namespace"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router = make_router t ~sw "a=tok-a,b=tok-b" in
  let a = server router "a" in
  let b = server router "b" in
  let page, _ = connect a ~token:"tok-a" () in
  let _laptop, _ =
    connect
      a
      ~token:"tok-a"
      ~fields:
        {|, "name": "laptop", "tools": true, "host_id": "host-laptop", "cwd": "/home/me/proj", "home": "/home/me"|}
      ()
  in
  let other_page, _ = connect b ~token:"tok-b" () in
  (* Without defaults, a page's new session starts where its session is. *)
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  [%expect {| on backend in $DIR (mode auto) |}];
  (* A host is named by its name or id and kept by id; an unknown one is
     refused with the connected ones. *)
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"default_host": "nowhere"}}|}
       "set_config");
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"default_host": "Laptop"}}|}
       "set_config");
  [%expect
    {|
    error: default_host: no connected tool host is called "nowhere"; connected: <host> (the backend), laptop
    default_host=host-laptop default_cwd=- approval_mode=auto
    |}];
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  [%expect {| on host-laptop in /home/me/proj (mode auto) |}];
  (* The directory is the default host's: [~] is that host's home, and the
     backend does not check a directory that is not its own. *)
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"default_cwd": "~/notes"}}|}
       "set_config");
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  [%expect
    {|
    default_host=host-laptop default_cwd=~/notes approval_mode=auto
    on host-laptop in /home/me/notes (mode auto)
    |}];
  (* Namespace b has its own file: its pages are unaffected. *)
  ignore (call b other_page "new_session" : Json.t);
  show_session t b other_page;
  [%expect {| on backend in $DIR (mode auto) |}];
  (* On the backend, the directory must be there; [off] the host, it is the
     backend's again. *)
  Core_unix.mkdir_p (t.dir ^/ "work");
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"default_host": null, "default_cwd": "/no/such"}}|}
       "set_config");
  show_config
    t
    (call
       a
       page
       ~params:
         (sprintf
            {|{"config": {"default_host": "backend", "default_cwd": "%s/work"}}|}
            t.dir)
       "set_config");
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  [%expect
    {|
    error: default_cwd: /no/such is not a directory on the backend (<host>)
    default_host=backend default_cwd=$DIR/work approval_mode=auto
    on backend in $DIR/work (mode auto)
    |}]
;;

let%expect_test "a default host that is away: the usual place, in its cwd" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router = make_router t ~sw "a=tok-a" in
  let a = server router "a" in
  let page, _ = connect a ~token:"tok-a" () in
  let laptop, _ =
    connect
      a
      ~token:"tok-a"
      ~fields:
        {|, "name": "laptop", "tools": true, "host_id": "host-laptop", "cwd": "/home/me/proj"|}
      ()
  in
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"default_host": "laptop", "default_cwd": "/srv"}}|}
       "set_config");
  Rpc_server.disconnect a laptop;
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  (* A terminal (a tool host) starts its sessions on itself, where it is. *)
  let tui, _ =
    connect
      a
      ~token:"tok-a"
      ~fields:
        {|, "name": "desk", "tools": true, "host_id": "host-desk", "cwd": "/home/me/desk"|}
      ()
  in
  ignore (call a tui "new_session" : Json.t);
  show_session t a tui;
  [%expect
    {|
    default_host=host-laptop default_cwd=/srv approval_mode=auto
    on backend in $DIR (mode auto)
    on host-desk in /home/me/desk (mode auto)
    |}]
;;

(* Settings change only the defaults; every session hears of a change and a
   later [set_config] from any of them keeps it. *)
let%expect_test "set_config: defaults_only, and every session follows" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let router = make_router t ~sw "a=tok-a" in
  let a = server router "a" in
  let page, _ = connect a ~token:"tok-a" () in
  let other, other_sent = connect a ~token:"tok-a" () in
  ignore (call a other "new_session" : Json.t);
  Queue.clear other_sent;
  show_config
    t
    (call
       a
       page
       ~params:
         {|{"config": {"approval_mode": "ask", "default_host": "backend"}, "defaults_only": true}|}
       "set_config");
  show_session t a page;
  config_events other_sent;
  [%expect
    {|
    default_host=backend default_cwd=- approval_mode=ask
    on backend in $DIR (mode auto)
    config_changed: default_host=backend fallback=[]
    |}];
  show_config
    t
    (call
       a
       other
       ~params:{|{"config": {"fallback_models": ["deepseek-flash"]}}|}
       "set_config");
  config_events other_sent;
  ignore (call a page "new_session" : Json.t);
  show_session t a page;
  (* Without [defaults_only] (/confirm), the session changes too. *)
  show_config
    t
    (call
       a
       page
       ~params:{|{"config": {"approval_mode": "auto-edit"}}|}
       "set_config");
  show_session t a page;
  [%expect
    {|
    default_host=backend default_cwd=- approval_mode=ask
    config_changed: default_host=backend fallback=["deepseek/deepseek-flash"]
    on backend in $DIR (mode ask)
    default_host=backend default_cwd=- approval_mode=auto-edit
    on backend in $DIR (mode auto-edit)
    |}]
;;
