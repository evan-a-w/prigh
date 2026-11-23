open! Core
open! Prigh
open Tool_test_helpers

let prigh_exe () = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"

let exec (t : Tool_test_helpers.t) : Host_stream.Exec.t =
  fun ~stdin ~cancel ~on_output ~name:_ arguments ->
  Host_process.execute
    ~env:t.env
    ~self:(prigh_exe ())
    ~stdin:(Some stdin)
    ~cancel
    ~on_output
    ~cwd:t.dir
    ~arguments
    ()
;;

let launch ?(mcp = false) argv : Adapter.Launch.t =
  { argv; env_set = [ "GREETING", "hello" ]; env_unset = []; cwd = "."; mcp }
;;

let%expect_test "a harness that is not installed on the host" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  (match
     Host_transport.spawn
       ~sw
       ~exec:(exec t)
       ~host:"laptop"
       (launch [ "codex-not-installed-anywhere"; "app-server" ])
   with
   | Ok _ -> print_endline "started?"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| codex-not-installed-anywhere is not installed on laptop, please install it |}]
;;

let%expect_test "lines both ways, the environment, and the exit" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let transport =
    Host_transport.spawn
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      (launch
         [ "sh"
         ; "-c"
         ; {|echo "$GREETING"; read line; echo "got $line"; echo oops >&2; exit 3|}
         ])
    |> Or_error.ok_exn
  in
  print_s [%sexp (Option.is_some (Line_transport.mcp transport) : bool)];
  print_s [%sexp (Line_transport.read_line transport : string option)];
  Line_transport.write_line transport "ping";
  print_s [%sexp (Line_transport.read_line transport : string option)];
  print_s [%sexp (Line_transport.read_line transport : string option)];
  print_s [%sexp (Line_transport.ended transport : Line_transport.Exit.t)];
  [%expect {|
    false
    (hello)
    ("got ping")
    ()
    ((code (3)) (stderr_tail oops))
    |}]
;;

let%expect_test "prigh's tools through the MCP side channel and prigh mcp-bridge" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let transport =
    Host_transport.spawn
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      (launch
         ~mcp:true
         [ "sh"
         ; "-c"
         ; {|read go; printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | "$1" mcp-bridge | head -c 120; echo; echo done|}
         ; "sh"
         ; Host_process.Placeholder.prigh
         ])
    |> Or_error.ok_exn
  in
  let mcp = Option.value_exn (Line_transport.mcp transport) in
  let name, config = Injected_tools.Bridge.codex_config mcp.bridge in
  print_s
    [%sexp
      (name : string)
    , (String.equal mcp.bridge.command (prigh_exe ()) : bool)
    , (mcp.bridge.args : string list)];
  ignore (config : Jsonaf.t);
  mcp.serve
    (Injected_tools.create
       ~tools:[ Tool_render_ui.tool ]
       ~execute:(fun _ _ -> Tool_result.ok "unused"));
  Line_transport.write_line transport "go";
  let rec lines () =
    match Line_transport.read_line transport with
    | Some line ->
      print_endline line;
      lines ()
    | None -> ()
  in
  lines ();
  print_s [%sexp (Line_transport.ended transport : Line_transport.Exit.t)];
  [%expect {|
    (mcp_servers.prigh true (mcp-bridge))
    {"jsonrpc":"2.0","id":1,"result":{"tools":[{"name":"render_ui","description":"Shows rich components inline in the user's
    done
    ((code (0)) (stderr_tail ""))
    |}]
;;
