open! Core
open! Prigh
open Mcp_test_helpers
module Json = Jsonaf

let write path data =
  Core_unix.mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data
;;

let server_log ~dir name = Filename.concat dir (name ^ ".log")

(* A fake server logging to [name.log] in [dir]. *)
let entry ?(args = []) ?(env = []) ~dir name =
  ( name
  , `Object
      [ "command", `String fake_server
      ; "args", `Array (List.map args ~f:(fun arg -> `String arg))
      ; ( "env"
        , `Object
            (("FAKE_MCP_LOG", `String (server_log ~dir name))
             :: List.map env ~f:(fun (k, v) -> k, `String v)) )
      ] )
;;

let write_config path entries =
  write path (Json.to_string (`Object [ "mcpServers", `Object entries ]))
;;

let starts ~dir name =
  match In_channel.read_lines (server_log ~dir name) with
  | lines -> List.count lines ~f:(String.equal "start")
  | exception _ -> 0
;;

let stops ~dir name =
  match In_channel.read_lines (server_log ~dir name) with
  | lines -> List.count lines ~f:(String.equal "stdin closed")
  | exception _ -> 0
;;

let print_counts ~dir names =
  List.iter names ~f:(fun name ->
    printf "%s: %d starts, %d stops\n" name (starts ~dir name) (stops ~dir name))
;;

module Setup = struct
  type t =
    { home : string
    ; cwd : string
    ; user_file : string
    ; project_file : string
    }

  let create ~dir =
    let home = Filename.concat dir "home" in
    let cwd = Filename.concat dir "project" in
    Core_unix.mkdir_p home;
    Core_unix.mkdir_p cwd;
    { home
    ; cwd
    ; user_file = Mcp_config.user_file ~home
    ; project_file = Filename.concat cwd ".mcp.json"
    }
  ;;
end

let print_servers ~dir (statuses, problems) =
  List.iter statuses ~f:(fun ({ server; status } : Mcp_hub.Server_status.t) ->
    let status =
      match status with
      | Ready tools -> sprintf "ready, %d tools" (List.length tools)
      | Failed e -> "failed: " ^ e
      | Needs_approval -> "needs approval"
    in
    print_endline
      (mask ~dir (sprintf "%s (%s): %s" server.name server.source status)));
  List.iter problems ~f:(fun problem ->
    print_endline (mask ~dir ("problem: " ^ problem)))
;;

let servers ?reconnect hub ~dir (setup : Setup.t) =
  print_servers
    ~dir
    (Mcp_hub.servers hub ?reconnect ~cwd:setup.cwd ~home:setup.home ())
;;

let call hub ~dir (setup : Setup.t) ~source ~server ~tool =
  Mcp_hub.call
    hub
    ~cancel:(Cancellation.create ())
    ~source
    ~server
    ~home:setup.home
    ~tool
    ~arguments:(`Object [ "text", `String "hi" ])
  |> print_result ~dir
;;

let%expect_test
    "project servers need approval; calls are routed by source and name"
  =
  run
  @@ fun ~env ~sw ~dir ->
  let setup = Setup.create ~dir in
  write_config setup.user_file [ entry ~dir "mine" ];
  write_config setup.project_file [ entry ~dir "theirs" ];
  let hub = Mcp_hub.create ~env ~sw () in
  servers hub ~dir setup;
  [%expect
    {|
    theirs ($DIR/project/.mcp.json): needs approval
    mine ($DIR/home/.prigh/mcp.json): ready, 7 tools
    |}];
  call hub ~dir setup ~source:setup.project_file ~server:"theirs" ~tool:"echo";
  print_counts ~dir [ "mine"; "theirs" ];
  [%expect
    {|
    ERROR: the MCP server "theirs" (from $DIR/project/.mcp.json) is not approved; approve it with /mcp
    mine: 1 starts, 0 stops
    theirs: 0 starts, 0 stops
    |}];
  let theirs =
    ok_exn
      (Mcp_config.find ~home:setup.home ~source:setup.project_file "theirs")
  in
  ok_exn (Mcp_config.approve ~home:setup.home theirs);
  servers hub ~dir setup;
  [%expect
    {|
    theirs ($DIR/project/.mcp.json): ready, 7 tools
    mine ($DIR/home/.prigh/mcp.json): ready, 7 tools
    |}];
  call hub ~dir setup ~source:setup.project_file ~server:"theirs" ~tool:"echo";
  call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"echo";
  call hub ~dir setup ~source:setup.user_file ~server:"theirs" ~tool:"echo";
  call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"nope";
  [%expect
    {|
    hi
    (echoed)
    hi
    (echoed)
    ERROR: $DIR/home/.prigh/mcp.json no longer defines the MCP server "theirs"
    ERROR: Unknown tool: nope (MCP error -32602)
    |}];
  Mcp_hub.close hub;
  print_counts ~dir [ "mine"; "theirs" ];
  [%expect
    {|
    mine: 1 starts, 1 stops
    theirs: 1 starts, 1 stops
    |}]
;;

let%expect_test "concurrent callers share one start" =
  run
  @@ fun ~env ~sw ~dir ->
  let setup = Setup.create ~dir in
  write_config setup.user_file [ entry ~dir "mine"; entry ~dir "other" ];
  let hub = Mcp_hub.create ~env ~sw () in
  Eio.Fiber.all
    (List.init 3 ~f:(fun _ () ->
       ignore (Mcp_hub.servers hub ~cwd:setup.cwd ~home:setup.home () : _ * _)));
  Eio.Fiber.both
    (fun () ->
       call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"echo")
    (fun () -> servers hub ~dir setup);
  [%expect
    {|
    mine ($DIR/home/.prigh/mcp.json): ready, 7 tools
    other ($DIR/home/.prigh/mcp.json): ready, 7 tools
    hi
    (echoed)
    |}];
  Mcp_hub.close hub;
  print_counts ~dir [ "mine"; "other" ];
  [%expect
    {|
    mine: 1 starts, 1 stops
    other: 1 starts, 1 stops
    |}]
;;

let%expect_test "failed starts are remembered until reconnect; edits restart" =
  run
  @@ fun ~env ~sw ~dir ->
  let setup = Setup.create ~dir in
  write_config
    setup.user_file
    [ entry ~dir "broken" ~args:[ "--exit-with-stderr" ] ];
  let hub = Mcp_hub.create ~env ~sw () in
  servers hub ~dir setup;
  servers hub ~dir setup;
  call hub ~dir setup ~source:setup.user_file ~server:"broken" ~tool:"echo";
  print_counts ~dir [ "broken" ];
  [%expect
    {|
    broken ($DIR/home/.prigh/mcp.json): failed: the server exited (code 1): fake: could not read the config
    fake: FAKE_TOKEN is not set
    broken ($DIR/home/.prigh/mcp.json): failed: the server exited (code 1): fake: could not read the config
    fake: FAKE_TOKEN is not set
    ERROR: the MCP server "broken" is not running: the server exited (code 1): fake: could not read the config
    fake: FAKE_TOKEN is not set; once that is fixed, reconnect it with /mcp
    broken: 1 starts, 0 stops
    |}];
  servers hub ~dir setup ~reconnect:true;
  print_counts ~dir [ "broken" ];
  [%expect
    {|
    broken ($DIR/home/.prigh/mcp.json): failed: the server exited (code 1): fake: could not read the config
    fake: FAKE_TOKEN is not set
    broken: 2 starts, 0 stops
    |}];
  (* Fixing the definition starts it again without a reconnect. *)
  write_config setup.user_file [ entry ~dir "broken" ];
  servers hub ~dir setup;
  print_counts ~dir [ "broken" ];
  [%expect
    {|
    broken ($DIR/home/.prigh/mcp.json): ready, 7 tools
    broken: 3 starts, 0 stops
    |}];
  (* Another edit replaces the running server. *)
  write_config setup.user_file [ entry ~dir "broken" ~env:[ "FOO", "bar" ] ];
  servers hub ~dir setup;
  print_counts ~dir [ "broken" ];
  [%expect
    {|
    broken ($DIR/home/.prigh/mcp.json): ready, 7 tools
    broken: 4 starts, 1 stops
    |}];
  Mcp_hub.close hub;
  print_counts ~dir [ "broken" ];
  [%expect {| broken: 4 starts, 2 stops |}]
;;

let%expect_test "a server that dies is failed until reconnected" =
  run
  @@ fun ~env ~sw ~dir ->
  let setup = Setup.create ~dir in
  write_config setup.user_file [ entry ~dir "mine" ];
  let hub = Mcp_hub.create ~env ~sw () in
  call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"crash";
  servers hub ~dir setup;
  call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"echo";
  [%expect
    {|
    ERROR: the server exited (code 3): fake: crashing on purpose
    mine ($DIR/home/.prigh/mcp.json): failed: the server exited (code 3): fake: crashing on purpose
    ERROR: the MCP server "mine" is not running: the server exited (code 3): fake: crashing on purpose; once that is fixed, reconnect it with /mcp
    |}];
  servers hub ~dir setup ~reconnect:true;
  call hub ~dir setup ~source:setup.user_file ~server:"mine" ~tool:"echo";
  [%expect
    {|
    mine ($DIR/home/.prigh/mcp.json): ready, 7 tools
    hi
    (echoed)
    |}];
  Mcp_hub.close hub;
  print_counts ~dir [ "mine" ];
  [%expect {| mine: 2 starts, 1 stops |}]
;;
