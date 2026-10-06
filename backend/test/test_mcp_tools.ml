open! Core
open! Prigh
module Json = Jsonaf

let tool ?(read_only = false) name =
  { Mcp_client.Tool.name
  ; description = "does " ^ name
  ; input_schema =
      `Object
        [ "type", `String "object"
        ; "properties", `Object [ "path", `Object [ "type", `String "string" ] ]
        ]
  ; read_only
  }
;;

let listing =
  { Mcp_tools.Listing.servers =
      [ { name = "fs"
        ; source = "/p/.mcp.json"
        ; project = true
        ; status = Ready
        ; tools = [ tool ~read_only:true "read_file"; tool "write file" ]
        }
      ; { name = "gh"
        ; source = "/h/.prigh/mcp.json"
        ; project = false
        ; status = Failed "server exited (code 1): no token"
        ; tools = []
        }
      ; { name = "db"
        ; source = "/p/.mcp.json"
        ; project = true
        ; status = Needs_approval
        ; tools = []
        }
      ]
  ; problems =
      [ "/p/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" \
         (http)"
      ]
  }
;;

let%expect_test "tool names" =
  List.iter
    [ "fs", "read_file"; "my server", "do.it/now"; "s", String.make 80 'x' ]
    ~f:(fun (server, tool) -> print_endline (Mcp_tools.tool_name ~server ~tool));
  [%expect
    {|
    mcp__fs__read_file
    mcp__my_server__do_it_now
    mcp__s__xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    |}]
;;

let%expect_test "listing: host wire format round trip, RPC shape, notices" =
  let json = Mcp_tools.Listing.to_json listing in
  print_endline (Json.to_string json);
  [%expect
    {| {"servers":[{"name":"fs","source":"/p/.mcp.json","project":true,"status":"ready","tools":[{"name":"read_file","description":"does read_file","input_schema":{"type":"object","properties":{"path":{"type":"string"}}},"read_only":true},{"name":"write file","description":"does write file","input_schema":{"type":"object","properties":{"path":{"type":"string"}}},"read_only":false}]},{"name":"gh","source":"/h/.prigh/mcp.json","project":false,"status":"failed","error":"server exited (code 1): no token","tools":[]},{"name":"db","source":"/p/.mcp.json","project":true,"status":"needs_approval","tools":[]}],"problems":["/p/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]} |}];
  let back = Mcp_tools.Listing.of_json json |> Or_error.ok_exn in
  print_s
    [%sexp
      (Sexp.equal
         [%sexp (back : Mcp_tools.Listing.t)]
         [%sexp (listing : Mcp_tools.Listing.t)]
       : bool)];
  [%expect {| true |}];
  print_endline (Json.to_string (Mcp_tools.Listing.to_rpc_json listing));
  [%expect
    {| {"servers":[{"name":"fs","source":"/p/.mcp.json","project":true,"status":"ready","tools":[{"name":"mcp__fs__read_file","description":"does read_file"},{"name":"mcp__fs__write_file","description":"does write file"}]},{"name":"gh","source":"/h/.prigh/mcp.json","project":false,"status":"failed","error":"server exited (code 1): no token","tools":[]},{"name":"db","source":"/p/.mcp.json","project":true,"status":"needs_approval","tools":[]}],"problems":["/p/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]} |}];
  List.iter (Mcp_tools.Listing.notices listing) ~f:print_endline;
  [%expect
    {|
    /p/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    MCP server gh (/h/.prigh/mcp.json) failed: server exited (code 1): no token; once that is fixed, /mcp reconnect starts it
    MCP server db from /p/.mcp.json is not started until you approve it: /mcp
    |}];
  print_s
    [%sexp
      (Mcp_tools.Listing.of_json (`Object []) : Mcp_tools.Listing.t Or_error.t)];
  [%expect {| (Error "an MCP listing needs \"servers\"") |}]
;;

let%expect_test
    "tools: only ready servers', flags from readOnlyHint, calls routed"
  =
  let tools =
    Mcp_tools.tools listing ~call:(fun _context ~source ~server ~tool args ->
      Tool_result.ok
        (sprintf "%s %s %s %s" source server tool (Json.to_string args)))
  in
  List.iter tools ~f:(fun (t : Tool.t) ->
    print_s [%sexp ({ t.spec with parameters = `Null } : Tool_spec.t)]);
  [%expect
    {|
    ((name mcp__fs__read_file) (description "does read_file") (parameters Null)
     (parallel_safe true) (destructive false) (on_host false))
    ((name mcp__fs__write_file) (description "does write file") (parameters Null)
     (parallel_safe false) (destructive true) (on_host false))
    |}];
  Eio_main.run (fun env ->
    let context = Tool.Context.create ~env ~cwd:"/" () in
    let result =
      Tool.execute (List.hd_exn tools) context (`Object [ "path", `String "a" ])
    in
    print_s [%sexp (result : Tool_result.t)]);
  [%expect
    {| ((text "/p/.mcp.json fs read_file {\"path\":\"a\"}") (is_error false)) |}]
;;
