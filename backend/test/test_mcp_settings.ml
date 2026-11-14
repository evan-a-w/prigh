open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let json text = Json.parse text |> Or_error.ok_exn

let print_entries ?(t : Tool_test_helpers.t option) result =
  match result with
  | Error e ->
    let text = Error.to_string_hum e in
    print_endline (Option.value_map t ~default:text ~f:(fun t -> mask t text))
  | Ok entries ->
    List.iter entries ~f:(fun entry ->
      print_s [%sexp (entry : Mcp_settings.Entry.t)])
;;

let show_file t =
  let path = Mcp_settings.path ~home:t.dir in
  match In_channel.read_all path with
  | exception Sys_error _ -> print_endline "file: none"
  | text ->
    printf "file (mode %o):\n%s" (Core_unix.stat path).st_perm (mask t text)
;;

let github =
  json
    {|{"type": "http", "url": "https://api.example.com/mcp",
       "headers": {"Authorization": "Bearer ${GITHUB_TOKEN}", "X-Key": "s3cret"}}|}
;;

let fs =
  json
    {|{"command": "npx", "args": ["-y", "fs-server", "/tmp"],
       "env": {"API_KEY": "hunter2"}}|}
;;

let%expect_test "add, list, edit, rename, disable, remove; the file" =
  with_sandbox
  @@ fun t ->
  let s = Mcp_settings.create ~home:t.dir in
  Mcp_settings.subscribe s ~f:(fun () -> print_endline "(changed)");
  print_entries (Mcp_settings.list s);
  show_file t;
  [%expect {| file: none |}];
  print_entries (Mcp_settings.add s [ "github", github; "fs", fs ]);
  (* Sexps hide the values of env and headers. *)
  [%expect
    {|
    (changed)
    ((name github) (enabled true)
     (config
      "{\"type\":\"http\",\"url\":\"https://api.example.com/mcp\",\"headers\":{\"Authorization\":\"<hidden>\",\"X-Key\":\"<hidden>\"}}"))
    ((name fs) (enabled true)
     (config
      "{\"command\":\"npx\",\"args\":[\"-y\",\"fs-server\",\"/tmp\"],\"env\":{\"API_KEY\":\"<hidden>\"}}"))
    |}];
  ignore (Mcp_settings.set_enabled s ~name:"fs" false : _ Or_error.t);
  (* Only its owner can read it: it holds secrets. *)
  show_file t;
  print_endline (Json.to_string (Mcp_settings.enabled s));
  [%expect
    {|
    (changed)
    file (mode 600):
    {
      "mcpServers": {
        "github": {
          "type": "http",
          "url": "https://api.example.com/mcp",
          "headers": {
            "Authorization": "Bearer ${GITHUB_TOKEN}",
            "X-Key": "s3cret"
          }
        },
        "fs": {
          "command": "npx",
          "args": [
            "-y",
            "fs-server",
            "/tmp"
          ],
          "env": {
            "API_KEY": "hunter2"
          }
        }
      },
      "disabled": [
        "fs"
      ]
    }
    {"github":{"type":"http","url":"https://api.example.com/mcp","headers":{"Authorization":"Bearer ${GITHUB_TOKEN}","X-Key":"s3cret"}}}
    |}];
  (* Renaming keeps it disabled. *)
  print_entries
    (Mcp_settings.update
       s
       ~name:"fs"
       ~rename:"files"
       (json {|{"command": "fs-mcp"}|}));
  [%expect
    {|
    (changed)
    ((name github) (enabled true)
     (config
      "{\"type\":\"http\",\"url\":\"https://api.example.com/mcp\",\"headers\":{\"Authorization\":\"<hidden>\",\"X-Key\":\"<hidden>\"}}"))
    ((name files) (enabled false) (config "{\"command\":\"fs-mcp\"}"))
    |}];
  print_entries (Mcp_settings.set_enabled s ~name:"files" true);
  print_entries (Mcp_settings.remove s ~name:"github");
  [%expect
    {|
    (changed)
    ((name github) (enabled true)
     (config
      "{\"type\":\"http\",\"url\":\"https://api.example.com/mcp\",\"headers\":{\"Authorization\":\"<hidden>\",\"X-Key\":\"<hidden>\"}}"))
    ((name files) (enabled true) (config "{\"command\":\"fs-mcp\"}"))
    (changed)
    ((name files) (enabled true) (config "{\"command\":\"fs-mcp\"}"))
    |}];
  (* An edit by hand counts. *)
  Out_channel.write_all
    (Mcp_settings.path ~home:t.dir)
    ~data:{|{"mcpServers": {"x": {"command": "x"}}, "disabled": ["x"]}|};
  print_entries (Mcp_settings.list s);
  print_endline (Json.to_string (Mcp_settings.enabled s));
  [%expect
    {|
    ((name x) (enabled false) (config "{\"command\":\"x\"}"))
    {}
    |}];
  Out_channel.write_all (Mcp_settings.path ~home:t.dir) ~data:"{";
  print_entries ~t (Mcp_settings.list s);
  print_entries ~t (Mcp_settings.add s [ "y", fs ]);
  print_endline (Json.to_string (Mcp_settings.enabled s));
  [%expect
    {|
    $DIR/.prigh/mcp-servers.json: invalid JSON (json > object: not enough input); fix or delete the file
    $DIR/.prigh/mcp-servers.json: invalid JSON (json > object: not enough input); fix or delete the file
    {}
    |}]
;;

let%expect_test "refused changes say what to fix and change nothing" =
  with_sandbox
  @@ fun t ->
  let s = Mcp_settings.create ~home:t.dir in
  ignore (Mcp_settings.add s [ "github", github; "fs", fs ] : _ Or_error.t);
  Mcp_settings.subscribe s ~f:(fun () -> print_endline "(changed)");
  let refused result =
    match result with
    | Ok _ -> print_endline "accepted"
    | Error e -> print_endline (Error.to_string_hum e)
  in
  refused (Mcp_settings.add s [ "github", fs ]);
  refused (Mcp_settings.add s [ "new", fs; "new", fs ]);
  refused (Mcp_settings.add s [ "bad name", fs ]);
  refused (Mcp_settings.add s [ "ok", fs; "empty", json {|{"command": ""}|} ]);
  refused (Mcp_settings.update s ~name:"nope" fs);
  refused (Mcp_settings.update s ~name:"fs" ~rename:"github" fs);
  refused (Mcp_settings.update s ~name:"fs" (json {|{"url": "ftp://x"}|}));
  refused (Mcp_settings.remove s ~name:"nope");
  refused (Mcp_settings.set_enabled s ~name:"nope" false);
  [%expect
    {|
    there is already an MCP server "github" in the settings: choose another name, or edit that one
    there is already an MCP server "new" in the settings: choose another name, or edit that one
    the name "bad name" may only have letters, digits, - and _ (its tools are named mcp__NAME__TOOL)
    server "empty": the "command" is empty: give the program to run
    there is no MCP server "nope" in the settings (they are: github, fs)
    there is already an MCP server "github" in the settings: choose another name, or edit that one
    server "fs": the url "ftp://x" must start with https:// (or http://)
    there is no MCP server "nope" in the settings (they are: github, fs)
    there is no MCP server "nope" in the settings (they are: github, fs)
    |}];
  print_s
    [%sexp
      (List.map (Or_error.ok_exn (Mcp_settings.list s)) ~f:(fun e -> e.name)
       : string list)];
  [%expect {| (github fs) |}]
;;

let%expect_test
    "pasted JSON: Claude Code's mcpServers, with or without the wrapper"
  =
  let parse text =
    match Mcp_settings.parse text with
    | Ok servers -> print_s [%sexp (List.map servers ~f:fst : string list)]
    | Error e -> print_endline (Error.to_string_hum e)
  in
  parse
    {|{"mcpServers": {"a": {"command": "npx", "args": ["a"]},
                      "b": {"type": "http", "url": "https://b.example/mcp"}}}|};
  parse {|{"c": {"command": "c"}}|};
  parse {| "d": {"command": "d"}, "e": {"command": "e"} |};
  parse {|{"command": "npx"}|};
  parse {|{"mcpServers": {}}|};
  parse {|{"f": {"command": "f"}, "g h": {"command": "g"}}|};
  parse {|{"f": {"type": "sse", "url": "https://x"}}|};
  parse {|not json|};
  parse {|["x"]|};
  [%expect
    {|
    (a b)
    (c)
    (d e)
    this is one server's definition without its name: write it as {"NAME": {...}}
    there are no servers in it
    the name "g h" may only have letters, digits, - and _ (its tools are named mcp__NAME__TOOL)
    server "f": the legacy SSE transport is not supported; use the server's streamable HTTP endpoint ("type": "http")
    this is not JSON (json: unexpected string: 'not'); paste an "mcpServers" object, e.g. {"mcpServers": {"NAME": {"command": "..."}}}
    paste an "mcpServers" object: {"NAME": {"command": ...}}
    |}]
;;

let%expect_test "each namespace has its own" =
  with_sandbox
  @@ fun t ->
  let world name =
    Namespace.world
      { Namespace.name; token = name ^ "-token" }
      ~home:t.dir
      ~legacy_auth_file:(t.dir ^/ ".config/prigh/auth.json")
  in
  let alice = world "alice"
  and bob = world "bob" in
  let of_world (w : Namespace.World.t) = Mcp_settings.create ~home:w.home in
  ignore (Mcp_settings.add (of_world alice) [ "fs", fs ] : _ Or_error.t);
  List.iter [ alice; bob ] ~f:(fun w ->
    printf
      "%s: %s\n"
      (mask t (Mcp_settings.path ~home:w.home))
      (Json.to_string (Mcp_settings.enabled (of_world w))));
  [%expect
    {|
    $DIR/.prigh/namespaces/alice/.prigh/mcp-servers.json: {"fs":{"command":"npx","args":["-y","fs-server","/tmp"],"env":{"API_KEY":"hunter2"}}}
    $DIR/.prigh/namespaces/bob/.prigh/mcp-servers.json: {}
    |}]
;;

let fake_server = Mcp_test_helpers.fake_server

let mask t s =
  mask
    t
    (String.substr_replace_all s ~pattern:fake_server ~with_:"$FAKE_SERVER")
;;

(* A backend (its own tool host, with an MCP hub) for the user whose home is
   [home]; clients print the [mcp_settings_changed] events they hear. *)
let backend t ~sw ~home =
  Core_unix.mkdir_p home;
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat home "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat home "sessions" in
  let mcp = Mcp_hub.create ~env:t.env ~sw () in
  let mcp_settings = Mcp_settings.create ~home in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir
      ~home
      ~mcp
      ~mcp_settings
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home
      ~login
      ~mcp_settings
      ~sessions_dir
      ~cwd:home
      ~new_agent
      ()
  in
  server, mcp
;;

let client t server name =
  Rpc_server.connect server ~send:(fun json ->
    match Json.member "event" json with
    | Some (`String "mcp_settings_changed") ->
      printf "%s hears %s\n" name (mask t (Json.to_string json))
    | _ -> ())
;;

let call t server client meth params =
  let response =
    Rpc_server.handle
      server
      client
      (`Object [ "id", `Number "1"; "method", `String meth; "params", params ])
  in
  match Json.member "ok" response, Json.member "result" response with
  | Some `True, Some result -> print_endline (mask t (Json.to_string result))
  | _ ->
    print_endline
      (mask
         t
         (Json.to_string (Option.value_exn (Json.member "error" response))))
;;

let log_lines path =
  match In_channel.read_lines path with
  | lines -> lines
  | exception Sys_error _ -> []
;;

let fake ~log =
  `Object
    [ "command", `String fake_server
    ; "env", `Object [ "FAKE_MCP_LOG", `String log; "TOKEN", `String "s3cret" ]
    ]
;;

let%expect_test
    "RPC: changes are broadcast, reach live sessions, and stay with their user"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let alice, alice_hub = backend t ~sw ~home:(Filename.concat t.dir "alice") in
  let bob, bob_hub = backend t ~sw ~home:(Filename.concat t.dir "bob") in
  let a = client t alice "alice" in
  let a2 = client t alice "alice's phone" in
  let b = client t bob "bob" in
  call t alice a "hello" (`Object []);
  call t alice a2 "hello" (`Object []);
  call t bob b "hello" (`Object []);
  ignore ([%expect.output] : string);
  let log = Filename.concat t.dir "fake.log" in
  let started () = List.count (log_lines log) ~f:(String.equal "start") in
  let stopped () =
    List.count (log_lines log) ~f:(String.equal "stdin closed")
  in
  call t alice a "get_mcp_settings" (`Object []);
  [%expect {| {"servers":[]} |}];
  call
    t
    alice
    a
    "add_mcp_servers"
    (`Object [ "servers", `Object [ "fake", fake ~log ] ]);
  [%expect
    {|
    alice hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    alice's phone hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    {"servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    |}];
  (* The session (live, so started at once) lists it: the user's own, no
     approval. Bob has none of it. *)
  Mcp_test_helpers.wait_until ~env:t.env (fun () -> started () = 1);
  call t alice a "list_mcp" (`Object []);
  call t bob b "list_mcp" (`Object []);
  call t bob b "get_mcp_settings" (`Object []);
  [%expect
    {|
    {"servers":[{"name":"fake","source":"settings","project":false,"status":"ready","tools":[{"name":"mcp__fake__echo","description":"Echoes its text"},{"name":"mcp__fake__image","description":"Returns an image"},{"name":"mcp__fake__error","description":"Fails"},{"name":"mcp__fake__slow","description":"Never finishes"},{"name":"mcp__fake__structured","description":"Returns structured content"},{"name":"mcp__fake__ping_client","description":"Pings the client"},{"name":"mcp__fake__change_tools","description":"Adds a tool"}]}],"problems":[]}
    {"servers":[],"problems":[]}
    {"servers":[]}
    |}];
  (* Disabling stops it on the host, without another request. *)
  call
    t
    alice
    a2
    "set_mcp_server_enabled"
    (`Object [ "name", `String "fake"; "enabled", `False ]);
  Mcp_test_helpers.wait_until ~env:t.env (fun () -> stopped () = 1);
  call t alice a "list_mcp" (`Object []);
  [%expect
    {|
    alice hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":false,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    alice's phone hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":false,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    {"servers":[{"name":"fake","enabled":false,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}}]}
    {"servers":[],"problems":[]}
    |}];
  call
    t
    alice
    a
    "set_mcp_server_enabled"
    (`Object [ "name", `String "fake"; "enabled", `True ]);
  Mcp_test_helpers.wait_until ~env:t.env (fun () -> started () = 2);
  ignore ([%expect.output] : string);
  (* Pasted text, an update and a removal; errors say what to fix. (The
     HTTP servers' address has nothing listening: they fail at once.) *)
  call
    t
    alice
    a
    "add_mcp_servers"
    (`Object
        [ ( "text"
          , `String
              {|"remote": {"type": "http", "url": "http://127.0.0.1:9/mcp"}|} )
        ]);
  call
    t
    alice
    a
    "update_mcp_server"
    (`Object
        [ "name", `String "remote"
        ; "rename", `String "web"
        ; ( "config"
          , `Object
              [ "type", `String "http"
              ; "url", `String "http://127.0.0.1:9/other"
              ] )
        ]);
  call t alice a "remove_mcp_server" (`Object [ "name", `String "fake" ]);
  Mcp_test_helpers.wait_until ~env:t.env (fun () -> stopped () = 2);
  call
    t
    alice
    a
    "add_mcp_servers"
    (`Object [ "text", `String "{\"web\": {\"command\": \"x\"}}" ]);
  call t alice a "add_mcp_servers" (`Object []);
  call t alice a "update_mcp_server" (`Object [ "name", `String "web" ]);
  call t alice a "remove_mcp_server" (`Object [ "name", `String "fake" ]);
  call t alice a "set_mcp_server_enabled" (`Object [ "name", `String "web" ]);
  [%expect
    {|
    alice hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"remote","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/mcp"}}]}
    alice's phone hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"remote","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/mcp"}}]}
    {"servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"remote","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/mcp"}}]}
    alice hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    alice's phone hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    {"servers":[{"name":"fake","enabled":true,"config":{"command":"$FAKE_SERVER","env":{"FAKE_MCP_LOG":"$DIR/fake.log","TOKEN":"s3cret"}}},{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    alice hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    alice's phone hears {"type":"event","event":"mcp_settings_changed","servers":[{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    {"servers":[{"name":"web","enabled":true,"config":{"type":"http","url":"http://127.0.0.1:9/other"}}]}
    "there is already an MCP server \"web\" in the settings: choose another name, or edit that one"
    "give \"servers\" (an mcpServers object) or \"text\" (pasted JSON)"
    "missing param \"config\""
    "there is no MCP server \"fake\" in the settings (they are: web)"
    "param \"enabled\" must be a boolean"
    |}];
  Mcp_hub.close alice_hub;
  Mcp_hub.close bob_hub;
  Rpc_server.shutdown alice;
  Rpc_server.shutdown bob
;;
