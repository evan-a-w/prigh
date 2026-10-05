open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let fake_server = Mcp_test_helpers.fake_server

let mcp_json servers =
  Json.to_string
    (`Object
        [ ( "mcpServers"
          , `Object
              (List.map servers ~f:(fun name ->
                 name, `Object [ "command", `String fake_server ])) )
        ])
;;

let%expect_test "agent: MCP tools offered, called, approved and reported" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let home = Filename.concat t.dir "home" in
  write t "home/.prigh/mcp.json" (mcp_json [ "user" ]);
  write t "proj/.mcp.json" (mcp_json [ "proj" ]);
  let requests = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(Queue.enqueue requests)
      [ Reply.tool_call
          ~id:"c1"
          ~name:"mcp__user__echo"
          ~arguments:{|{"text":"hi"}|}
          ()
      ; Reply.text "done"
      ; Reply.text "again"
      ]
  in
  let mcp = Mcp_hub.create ~env:t.env ~sw () in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home
      ~mcp
      ~cwd:(Filename.concat t.dir "proj")
      ()
  in
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline (mask t ("notice: " ^ n))
    | Loop (Tool_end { result; _ }) ->
      print_endline
        (sprintf "tool_end %s: %s" result.tool_name (String.strip result.text))
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "use the echo tool");
  Agent.wait_idle agent;
  [%expect
    {|
    notice: MCP server proj from $DIR/proj/.mcp.json is not started until you approve it: /mcp
    tool_end mcp__user__echo: hi
    (echoed)
    |}];
  let names (r : Provider.Request.t) =
    List.filter_map r.tools ~f:(fun s ->
      Option.some_if (String.is_prefix s.name ~prefix:"mcp__") s.name)
  in
  let first = Queue.dequeue_exn requests in
  print_s [%sexp (names first : string list)];
  print_s
    [%sexp
      (String.is_substring
         (Option.value_exn first.system)
         ~substring:"- mcp__user__echo: Echoes its text"
       : bool)];
  [%expect
    {|
    (mcp__user__echo mcp__user__image mcp__user__error mcp__user__slow
     mcp__user__structured mcp__user__ping_client mcp__user__change_tools)
    true
    |}];
  let listing = Or_error.ok_exn (Agent.mcp_servers agent) in
  print_endline
    (mask
       t
       (Json.to_string
          (Mcp_tools.Listing.to_rpc_json
             { listing with
               servers =
                 List.map listing.servers ~f:(fun s ->
                   { s with tools = List.take s.tools 1 })
             })));
  [%expect
    {| {"servers":[{"name":"proj","source":"$DIR/proj/.mcp.json","project":true,"status":"needs_approval","tools":[]},{"name":"user","source":"$DIR/home/.prigh/mcp.json","project":false,"status":"ready","tools":[{"name":"mcp__user__echo","description":"Echoes its text"}]}],"problems":[]} |}];
  let listing =
    Or_error.ok_exn
      (Agent.approve_mcp
         agent
         ~source:(Filename.concat t.dir "proj/.mcp.json")
         ~server:"proj")
  in
  print_s
    [%sexp
      (List.map listing.servers ~f:(fun s ->
         s.name, s.status, List.length s.tools)
       : (string * Mcp_tools.Status.t * int) list)];
  [%expect {| ((proj Ready 7) (user Ready 7)) |}];
  (* The next run has the approved server's tools; the notice is not
     repeated. *)
  Or_error.ok_exn (Agent.prompt agent "again");
  Agent.wait_idle agent;
  Queue.clear requests;
  Or_error.ok_exn (Agent.prompt agent "and again");
  Agent.wait_idle agent;
  [%expect {| |}];
  print_s
    [%sexp
      (List.count
         (names (Queue.dequeue_exn requests))
         ~f:(String.is_prefix ~prefix:"mcp__proj__")
       : int)];
  [%expect {| 7 |}];
  Mcp_hub.close mcp
;;
