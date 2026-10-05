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
  [%expect
    {|
    ((name remote) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home)
     (transport
      (Http (url https://example.com/mcp)
       (headers ((Authorization "Bearer s3cret"))))))
    ((name local) (source $DIR/home/.prigh/mcp.json) (project false)
     (dir $DIR/home)
     (transport
      (Stdio (command npx) (args (-y s3cret "" $TOKEN))
       (env ((KEY s3cret-s3cret))))))
    problem: $DIR/home/.prigh/mcp.json: server "missing": ${NOT_SET} is not set; set it or write ${NOT_SET:-default}
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
