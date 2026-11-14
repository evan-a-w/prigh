open! Core
open! Prigh
open Mcp_test_helpers

let write path data =
  Core_unix.mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data
;;

let in_dir f =
  let dir =
    Filename_unix.realpath (Filename_unix.temp_dir "prigh-mcp-config" "")
  in
  Exn.protect
    ~f:(fun () -> f ~dir ~home:(Filename.concat dir "home"))
    ~finally:(fun () ->
      ignore (Sys_unix.command (sprintf "rm -rf %s" (Filename.quote dir)) : int))
;;

let getenv = function
  | "TOKEN" -> Some "s3cret"
  | "EMPTY" -> Some ""
  | _ -> None
;;

let print_discovered ~dir ({ servers; problems } : Mcp_config.Discovered.t) =
  List.iter servers ~f:(fun server ->
    print_s_masked
      ~dir
      [%sexp
        { name : string = server.name
        ; source : string = server.source
        ; project : bool = server.project
        ; dir : string = server.dir
        ; transport : Mcp_config.Transport.t = server.transport
        }]);
  List.iter problems ~f:(fun problem ->
    print_endline (mask ~dir ("problem: " ^ problem)))
;;

let discover ~dir ~home ~cwd =
  print_discovered ~dir (Mcp_config.discover ~getenv ~cwd ~home ())
;;

let%expect_test "the closest definition of a name wins" =
  in_dir
  @@ fun ~dir ~home ->
  let cwd = Filename.concat dir "a/b" in
  write
    (Filename.concat cwd ".mcp.json")
    {|{"mcpServers": {"x": {"command": "b-x"}, "only-b": {"command": "b"}}}|};
  write
    (Filename.concat dir "a/.mcp.json")
    {|{"mcpServers": {"x": {"command": "a-x"}, "y": {"command": "a-y"}}}|};
  write
    (Mcp_config.user_file ~home)
    {|{"mcpServers": {"x": {"command": "u-x"}, "y": {"command": "u-y"},
                      "z": {"type": "http", "url": "https://example.com/mcp"}}}|};
  discover ~dir ~home ~cwd;
  [%expect
    {|
    ((name x) (source $DIR/a/b/.mcp.json) (project true) (dir $DIR/a/b)
     (transport (Stdio (command b-x) (args ()) (env ()))))
    ((name only-b) (source $DIR/a/b/.mcp.json) (project true) (dir $DIR/a/b)
     (transport (Stdio (command b) (args ()) (env ()))))
    ((name y) (source $DIR/a/.mcp.json) (project true) (dir $DIR/a)
     (transport (Stdio (command a-y) (args ()) (env ()))))
    ((name z) (source $DIR/home/.prigh/mcp.json) (project false) (dir $DIR/home)
     (transport (Http (url https://example.com/mcp) (headers ()))))
    |}];
  (* From the parent only its own and the user's servers are seen. *)
  discover ~dir ~home ~cwd:(Filename.concat dir "a");
  [%expect
    {|
    ((name x) (source $DIR/a/.mcp.json) (project true) (dir $DIR/a)
     (transport (Stdio (command a-x) (args ()) (env ()))))
    ((name y) (source $DIR/a/.mcp.json) (project true) (dir $DIR/a)
     (transport (Stdio (command a-y) (args ()) (env ()))))
    ((name z) (source $DIR/home/.prigh/mcp.json) (project false) (dir $DIR/home)
     (transport (Http (url https://example.com/mcp) (headers ()))))
    |}]
;;

let%expect_test "variables are expanded; unset ones are problems" =
  in_dir
  @@ fun ~dir ~home ->
  write
    (Mcp_config.user_file ~home)
    {|{"mcpServers": {
        "remote": {"type": "http", "url": "https://${HOST:-example.com}/mcp",
                   "headers": {"Authorization": "Bearer ${TOKEN}"}},
        "local": {"command": "${BIN:-npx}", "args": ["-y", "${TOKEN}", "${EMPTY:-unused}", "$TOKEN"],
                  "env": {"KEY": "${TOKEN}-${TOKEN}"}},
        "missing": {"command": "x", "env": {"KEY": "${NOT_SET}"}}}}|};
  discover ~dir ~home ~cwd:dir;
  (* Sexps hide the values of env and headers: they hold secrets. *)
  [%expect
    {|
    ((name remote) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home)
     (transport
      (Http (url https://example.com/mcp) (headers ((Authorization <hidden>))))))
    ((name local) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home)
     (transport
      (Stdio (command npx) (args (-y s3cret "" $TOKEN)) (env ((KEY <hidden>))))))
    problem: $DIR/home/.prigh/mcp.json: server "missing": ${NOT_SET} is not set; set it or write ${NOT_SET:-default}
    |}];
  List.iter
    (Mcp_config.discover ~getenv ~cwd:dir ~home ()).servers
    ~f:(fun server ->
      match server.transport with
      | Http { headers; _ } ->
        print_s [%sexp (headers : (string * string) list)]
      | Stdio { env; _ } -> print_s [%sexp (env : (string * string) list)]);
  [%expect
    {|
    ((Authorization "Bearer s3cret"))
    ((KEY s3cret-s3cret))
    |}];
  print_s_masked
    ~dir
    [%sexp
      (Mcp_config.find
         ~getenv
         ~home
         ~source:(Mcp_config.user_file ~home)
         "missing"
       : Mcp_config.Server.t Or_error.t)];
  [%expect
    {|
    (Error
     "$DIR/home/.prigh/mcp.json: server \"missing\": ${NOT_SET} is not set; set it or write ${NOT_SET:-default}")
    |}]
;;

let%expect_test "problems say what to fix" =
  in_dir
  @@ fun ~dir ~home ->
  let cwd = Filename.concat dir "a/b" in
  write
    (Filename.concat cwd ".mcp.json")
    {|{"mcpServers": {"x": {"command": "x",}}|};
  write (Filename.concat dir "a/.mcp.json") {|{"mcpServers": ["x"]}|};
  write
    (Mcp_config.user_file ~home)
    {|{"mcpServers": {
        "ws": {"type": "websocket", "url": "wss://example.com"},
        "old": {"type": "sse", "url": "https://example.com/sse"},
        "no-command": {"type": "stdio", "args": ["x"]},
        "no-url": {"type": "http"},
        "nothing": {},
        "bad-args": {"command": "x", "args": "-y"},
        "bad-env": {"command": "x", "env": {"N": 1}},
        "bad-headers": {"url": "https://example.com", "headers": ["a"]},
        "fine": {"command": "x"}}}|};
  discover ~dir ~home ~cwd;
  [%expect
    {|
    ((name fine) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home) (transport (Stdio (command x) (args ()) (env ()))))
    problem: $DIR/a/b/.mcp.json: invalid JSON: json > object > object > object: char '}'
    problem: $DIR/a/.mcp.json: mcpServers must be an object
    problem: $DIR/home/.prigh/mcp.json: server "ws": unknown type "websocket"; use stdio or http
    problem: $DIR/home/.prigh/mcp.json: server "old": the legacy SSE transport is not supported; use the server's streamable HTTP endpoint ("type": "http")
    problem: $DIR/home/.prigh/mcp.json: server "no-command": give the "command" to run (a string)
    problem: $DIR/home/.prigh/mcp.json: server "no-url": give the server's "url" (a string)
    problem: $DIR/home/.prigh/mcp.json: server "nothing": give a "command" (stdio) or a "url" (http)
    problem: $DIR/home/.prigh/mcp.json: server "bad-args": args must be a list of strings
    problem: $DIR/home/.prigh/mcp.json: server "bad-env": env.N must be a string
    problem: $DIR/home/.prigh/mcp.json: server "bad-headers": headers must be an object of strings
    |}]
;;

let%expect_test
    "approvals: only project servers need them; they lapse when edited"
  =
  in_dir
  @@ fun ~dir ~home ->
  let project_file = Filename.concat dir ".mcp.json" in
  write
    project_file
    {|{"mcpServers": {"p": {"command": "run-me", "args": ["${TOKEN}"]}}}|};
  write
    (Mcp_config.user_file ~home)
    {|{"mcpServers": {"u": {"command": "mine"}}}|};
  let status () =
    let { Mcp_config.Discovered.servers; _ } =
      Mcp_config.discover ~getenv ~cwd:dir ~home ()
    in
    List.iter servers ~f:(fun server ->
      printf "%s: %b\n" server.name (Mcp_config.is_approved ~home server))
  in
  status ();
  [%expect
    {|
    p: false
    u: true
    |}];
  let p = ok_exn (Mcp_config.find ~getenv ~home ~source:project_file "p") in
  ok_exn (Mcp_config.approve ~home p);
  ok_exn (Mcp_config.approve ~home p);
  status ();
  [%expect
    {|
    p: true
    u: true
    |}];
  (* The approval is for the definition as written, not as expanded. *)
  let getenv = function
    | "TOKEN" -> Some "rotated"
    | _ -> None
  in
  let p' = ok_exn (Mcp_config.find ~getenv ~home ~source:project_file "p") in
  printf "%b\n" (Mcp_config.is_approved ~home p');
  [%expect {| true |}];
  write project_file {|{"mcpServers": {"p": {"command": "run-me-instead"}}}|};
  status ();
  [%expect
    {|
    p: false
    u: true
    |}];
  write
    project_file
    {|{"mcpServers": {"p": {"command": "run-me", "args": ["${TOKEN}"]}}}|};
  status ();
  [%expect
    {|
    p: true
    u: true
    |}];
  let approvals =
    Jsonaf.parse
      (In_channel.read_all (Filename.concat home ".prigh/mcp-approvals.json"))
  in
  print_s
    [%sexp
      (Or_error.map approvals ~f:(fun json ->
         List.length (Jsonaf.list_exn json))
       : int Or_error.t)];
  [%expect {| (Ok 1) |}]
;;

let settings json = Jsonaf.parse json |> ok_exn

let%expect_test "settings rank below the host's file and the project's" =
  in_dir
  @@ fun ~dir ~home ->
  let cwd = Filename.concat dir "p" in
  write
    (Filename.concat cwd ".mcp.json")
    {|{"mcpServers": {"both": {"command": "from-project"}}}|};
  write
    (Mcp_config.user_file ~home)
    {|{"mcpServers": {"host": {"command": "from-host-file"},
                      "both": {"command": "from-host-file"}}}|};
  let settings =
    settings
      {|{"host": {"command": "from-settings"},
         "both": {"command": "from-settings"},
         "mine": {"type": "http", "url": "https://${HOST:-example.com}/mcp",
                  "headers": {"Authorization": "Bearer ${TOKEN}"}},
         "unset": {"command": "x", "env": {"K": "${NOT_SET}"}}}|}
  in
  print_discovered ~dir (Mcp_config.discover ~getenv ~settings ~cwd ~home ());
  [%expect
    {|
    ((name both) (source $DIR/p/.mcp.json) (project true) (dir $DIR/p)
     (transport (Stdio (command from-project) (args ()) (env ()))))
    ((name host) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home)
     (transport (Stdio (command from-host-file) (args ()) (env ()))))
    ((name mine) (source settings) (project false) (dir $DIR/home)
     (transport
      (Http (url https://example.com/mcp) (headers ((Authorization <hidden>))))))
    problem: Settings → MCP servers: server "unset": ${NOT_SET} is not set; set it or write ${NOT_SET:-default}
    |}];
  (* The settings' own servers, approved as the user's. *)
  let find name =
    Mcp_config.find
      ~getenv
      ~settings
      ~home
      ~source:Mcp_config.settings_source
      name
  in
  (match find "mine" with
   | Ok server ->
     printf "approved: %b\n" (Mcp_config.is_approved ~home server);
     (match server.transport with
      | Http { headers; _ } ->
        print_s [%sexp (headers : (string * string) list)]
      | Stdio _ -> ())
   | Error e -> print_s [%sexp (e : Error.t)]);
  print_s [%sexp (Or_error.map (find "gone") ~f:ignore : unit Or_error.t)];
  [%expect
    {|
    approved: true
    ((Authorization "Bearer s3cret"))
    (Error
     "Settings \226\134\146 MCP servers no longer defines the MCP server \"gone\"")
    |}]
;;

let%expect_test "names and definitions are checked, saying what to fix" =
  List.iter
    [ "github"
    ; "my-server_2"
    ; ""
    ; "has space"
    ; "a.b"
    ; "two__parts"
    ; String.make 33 'x'
    ]
    ~f:(fun name ->
      print_s
        [%sexp (name : string), (Mcp_config.check_name name : unit Or_error.t)]);
  [%expect
    {|
    (github (Ok ()))
    (my-server_2 (Ok ()))
    ("" (Error "give the server a name"))
    ("has space"
     (Error
      "the name \"has space\" may only have letters, digits, - and _ (its tools are named mcp__NAME__TOOL)"))
    (a.b
     (Error
      "the name \"a.b\" may only have letters, digits, - and _ (its tools are named mcp__NAME__TOOL)"))
    (two__parts
     (Error
      "the name \"two__parts\" has \"__\", which separates the server from the tool in mcp__NAME__TOOL; use a single _"))
    (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     (Error
      "the name \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\" is longer than 32 characters; shorten it (its tools are named mcp__NAME__TOOL, at most 64 characters)"))
    |}];
  List.iter
    [ {|{"command": "npx", "args": ["-y", "${PKG}"], "env": {"T": "${TOKEN}"}}|}
    ; {|{"type": "http", "url": "${URL}"}|}
    ; {|{"command": "  "}|}
    ; {|{"type": "http", "url": ""}|}
    ; {|{"type": "http", "url": "example.com/mcp"}|}
    ; {|{"type": "sse", "url": "https://example.com/sse"}|}
    ; {|{"args": ["x"]}|}
    ; {|["npx"]|}
    ]
    ~f:(fun spec ->
      print_s [%sexp (Mcp_config.check (settings spec) : unit Or_error.t)]);
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Error "the \"command\" is empty: give the program to run")
    (Error "the \"url\" is empty: give the server's address")
    (Error "the url \"example.com/mcp\" must start with https:// (or http://)")
    (Error
     "the legacy SSE transport is not supported; use the server's streamable HTTP endpoint (\"type\": \"http\")")
    (Error "give a \"command\" (stdio) or a \"url\" (http)")
    (Error
     "a server is an object with a \"command\" (stdio) or a \"url\" (http)")
    |}]
;;
