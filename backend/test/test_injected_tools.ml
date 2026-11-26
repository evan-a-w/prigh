open! Core
open! Prigh
module Json = Jsonaf

let fake name =
  { Tool.spec =
      { name
      ; description = sprintf "the %s tool" name
      ; parameters =
          `Object
            [ "type", `String "object"
            ; ( "properties"
              , `Object [ "action", `Object [ "type", `String "string" ] ] )
            ]
      ; parallel_safe = false
      ; destructive = Never
      ; on_host = false
      ; freeform = None
      ; native = None
      }
  ; run =
      (fun _ _ -> Tool_result.error "the fake runs only through the executor")
  }
;;

(* What a session offers: the file and process tools, the interaction
   tools and the backend's own, in no particular order. *)
let session_tools =
  Tools.all
  @ Tools.interaction
  @ List.map
      ~f:fake
      [ "worktree"
      ; "memory"
      ; "pull_request"
      ; "sessions"
      ; "issue"
      ; "repo"
      ; "subagent"
      ; "ask_session"
      ; "answer_session"
      ; "custom_instructions"
      ; "reply_to_comment"
      ; "goal_complete"
      ]
;;

let%expect_test "default set" =
  let names native_questions =
    Injected_tools.select ~native_questions session_tools
    |> List.map ~f:Tool.name
  in
  print_s [%sexp (names true : string list)];
  print_s [%sexp (names false : string list)];
  [%expect
    {|
    (render_ui memory custom_instructions sessions ask_session answer_session
     pull_request issue repo worktree reply_to_comment goal_complete subagent)
    (render_ui memory custom_instructions sessions ask_session answer_session
     pull_request issue repo worktree reply_to_comment goal_complete subagent
     ask_user)
    |}];
  let canonical server tool =
    print_s
      [%sexp
        (server : string)
      , (tool : string)
      , (Injected_tools.canonical_of_mcp ~server ~tool : string option)]
  in
  canonical "prigh" "render_ui";
  canonical "prigh" "ask_user";
  canonical "prigh" "bash";
  canonical "github" "render_ui";
  [%expect
    {|
    (prigh render_ui (render_ui))
    (prigh ask_user (ask_user))
    (prigh bash ())
    (github render_ui ())
    |}]
;;

let server () =
  let execute tool args =
    print_s
      [%sexp
        "executed", (Tool.name tool : string), (Json.to_string args : string)];
    match Tool.name tool with
    | "memory" -> Tool_result.ok "remembered"
    | "render_ui" ->
      Tool_result.ok
        ~images:[ { Image.mime_type = "image/png"; data = "iVBORw0KGgo=" } ]
        "Shown: chart \"Build times\"."
    | "repo" ->
      Tool_result.error "not signed in to GitHub: Settings → Git hosting"
    | "issue" -> failwith "boom"
    | _ -> Tool_result.ok ""
  in
  Injected_tools.create
    ~tools:(Injected_tools.select ~native_questions:true session_tools)
    ~execute
;;

let exchange t line =
  print_endline ("> " ^ line);
  match Injected_tools.handle_line t line with
  | None -> print_endline "< (nothing)"
  | Some reply ->
    let version = sprintf {|"version":%S|} Version.to_string in
    print_endline
      ("< "
       ^ String.substr_replace_all
           reply
           ~pattern:version
           ~with_:{|"version":"VERSION"|})
;;

let%expect_test "initialize and list" =
  let t = server () in
  exchange
    t
    {|{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"codex","version":"0.166.0"}}}|};
  exchange t {|{"jsonrpc":"2.0","method":"notifications/initialized"}|};
  exchange t {|{"jsonrpc":"2.0","id":"p","method":"ping"}|};
  [%expect
    {|
    > {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"codex","version":"0.166.0"}}}
    < {"jsonrpc":"2.0","id":0,"result":{"protocolVersion":"2025-03-26","capabilities":{"tools":{}},"serverInfo":{"name":"prigh","version":"VERSION"},"instructions":"prigh's tools for the user's session: components shown in the chat, the user's memory, other sessions, pull requests, issues and CI through the user's sign-in, worktrees, and prigh subagents (for delegating work to another host or engine when the user explicitly asks; defaults to this session's engine and host)."}}
    > {"jsonrpc":"2.0","method":"notifications/initialized"}
    < (nothing)
    > {"jsonrpc":"2.0","id":"p","method":"ping"}
    < {"jsonrpc":"2.0","id":"p","result":{}}
    |}];
  (* An unknown revision gets ours. *)
  exchange
    t
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"1999-01-01"}}|};
  [%expect
    {|
    > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"1999-01-01"}}
    < {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18","capabilities":{"tools":{}},"serverInfo":{"name":"prigh","version":"VERSION"},"instructions":"prigh's tools for the user's session: components shown in the chat, the user's memory, other sessions, pull requests, issues and CI through the user's sign-in, worktrees, and prigh subagents (for delegating work to another host or engine when the user explicitly asks; defaults to this session's engine and host)."}}
    |}];
  (match
     Injected_tools.handle
       t
       (Json.parse {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|}
        |> Or_error.ok_exn)
   with
   | Some (`Object fields) ->
     (match List.Assoc.find fields "result" ~equal:String.equal with
      | Some result ->
        (match Json.member "tools" result with
         | Some (`Array tools) ->
           List.iter tools ~f:(fun tool ->
             let str name =
               match Json.member name tool with
               | Some (`String s) -> s
               | _ -> "?"
             in
             let description = str "description" in
             printf
               "%s: %s%s\n"
               (str "name")
               (String.prefix description 50)
               (match Json.member "inputSchema" tool with
                | Some (`Object _) -> ""
                | _ -> " (no schema!)"))
         | _ -> print_endline "no tools")
      | None -> print_endline "no result")
   | _ -> print_endline "no response");
  [%expect
    {|
    render_ui: Shows rich components inline in the user's chat in
    memory: the memory tool
    custom_instructions: the custom_instructions tool
    sessions: the sessions tool
    ask_session: the ask_session tool
    answer_session: the answer_session tool
    pull_request: the pull_request tool
    issue: the issue tool
    repo: the repo tool
    worktree: the worktree tool
    reply_to_comment: the reply_to_comment tool
    goal_complete: the goal_complete tool
    subagent: the subagent tool
    |}]
;;

let%expect_test "tools/call" =
  let t = server () in
  exchange
    t
    {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"memory","arguments":{"action":"recall"}}}|};
  exchange
    t
    {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"render_ui","arguments":{"components":[]}}}|};
  exchange
    t
    {|{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"repo"}}|};
  exchange
    t
    {|{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"issue","arguments":{}}}|};
  [%expect
    {|
    > {"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"memory","arguments":{"action":"recall"}}}
    (executed memory "{\"action\":\"recall\"}")
    < {"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"remembered"}],"isError":false}}
    > {"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"render_ui","arguments":{"components":[]}}}
    (executed render_ui "{\"components\":[]}")
    < {"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"Shown: chart \"Build times\"."},{"type":"image","data":"iVBORw0KGgo=","mimeType":"image/png"}],"isError":false}}
    > {"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"repo"}}
    (executed repo {})
    < {"jsonrpc":"2.0","id":5,"result":{"content":[{"type":"text","text":"not signed in to GitHub: Settings → Git hosting"}],"isError":true}}
    > {"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"issue","arguments":{}}}
    (executed issue {})
    < {"jsonrpc":"2.0","id":6,"result":{"content":[{"type":"text","text":"(Failure boom)"}],"isError":true}}
    |}]
;;

let%expect_test "errors" =
  let t = server () in
  exchange
    t
    {|{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"bash","arguments":{"command":"ls"}}}|};
  exchange
    t
    {|{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"memory","arguments":"recall"}}|};
  exchange t {|{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{}}|};
  exchange t {|{"jsonrpc":"2.0","id":10,"method":"resources/list"}|};
  exchange t {|{"jsonrpc":"2.0","id":11}|};
  exchange t {|{"jsonrpc":"2.0","id":12,"result":{}}|};
  exchange t {|not json|};
  [%expect
    {|
    > {"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"bash","arguments":{"command":"ls"}}}
    < {"jsonrpc":"2.0","id":7,"error":{"code":-32602,"message":"unknown tool bash: prigh offers render_ui, memory, custom_instructions, sessions, ask_session, answer_session, pull_request, issue, repo, worktree, reply_to_comment, goal_complete, subagent"}}
    > {"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"memory","arguments":"recall"}}
    < {"jsonrpc":"2.0","id":8,"error":{"code":-32602,"message":"arguments must be an object"}}
    > {"jsonrpc":"2.0","id":9,"method":"tools/call","params":{}}
    < {"jsonrpc":"2.0","id":9,"error":{"code":-32602,"message":"tools/call needs a tool name"}}
    > {"jsonrpc":"2.0","id":10,"method":"resources/list"}
    < {"jsonrpc":"2.0","id":10,"error":{"code":-32601,"message":"method not found: resources/list"}}
    > {"jsonrpc":"2.0","id":11}
    < {"jsonrpc":"2.0","id":11,"error":{"code":-32600,"message":"no method"}}
    > {"jsonrpc":"2.0","id":12,"result":{}}
    < (nothing)
    > not json
    < {"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"parse error: json: unexpected string: 'not'"}}
    |}]
;;

let%expect_test "Claude Code in-band" =
  let t = server () in
  let codec = Claude_codec.create () in
  let feed line =
    List.iter (Claude_codec.handle_line codec line) ~f:(function
      | Mcp request -> print_endline (Injected_tools.claude_reply t request)
      | other -> print_s [%sexp (other : Claude_codec.Output.t)])
  in
  feed
    {|{"type":"control_request","request_id":"r-1","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":0,"method":"ping"}}}|};
  [%expect
    {| {"type":"control_response","response":{"subtype":"success","request_id":"r-1","response":{"mcp_response":{"jsonrpc":"2.0","id":0,"result":{}}}}} |}];
  feed
    {|{"type":"control_request","request_id":"r-2","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","method":"notifications/initialized"}}}|};
  feed
    {|{"type":"control_request","request_id":"r-3","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"memory","arguments":{"action":"recall"}}}}}|};
  feed
    {|{"type":"control_request","request_id":"r-4","request":{"subtype":"mcp_message","server_name":"linear","message":{"jsonrpc":"2.0","id":8,"method":"tools/list"}}}|};
  [%expect
    {|
    {"type":"control_response","response":{"subtype":"success","request_id":"r-2","response":{"mcp_response":{"jsonrpc":"2.0","result":{}}}}}
    (executed memory "{\"action\":\"recall\"}")
    {"type":"control_response","response":{"subtype":"success","request_id":"r-3","response":{"mcp_response":{"jsonrpc":"2.0","id":7,"result":{"content":[{"type":"text","text":"remembered"}],"isError":false}}}}}
    {"type":"control_response","response":{"subtype":"success","request_id":"r-4","response":{"mcp_response":{"jsonrpc":"2.0","id":8,"error":{"code":-32601,"message":"no in-band MCP server \"linear\" here: the only one is \"prigh\""}}}}}
    |}]
;;

let%expect_test "bridge config" =
  let bridge =
    Injected_tools.Bridge.create
      ~prigh:"/nix/store/xxx-prigh/bin/prigh"
      ~socket:"/run/user/1000/prigh/mcp-3.sock"
      ~token:"t0k3n"
  in
  print_s [%sexp (bridge : Injected_tools.Bridge.t)];
  let key, value = Injected_tools.Bridge.codex_config bridge in
  print_endline (key ^ " = " ^ Json.to_string value);
  print_endline (Json.to_string (Injected_tools.Bridge.acp_server bridge));
  [%expect
    {|
    ((command /nix/store/xxx-prigh/bin/prigh) (args (mcp-bridge))
     (env
      ((PRIGH_MCP_SOCKET /run/user/1000/prigh/mcp-3.sock)
       (PRIGH_MCP_TOKEN t0k3n))))
    mcp_servers.prigh = {"command":"/nix/store/xxx-prigh/bin/prigh","args":["mcp-bridge"],"env":{"PRIGH_MCP_SOCKET":"/run/user/1000/prigh/mcp-3.sock","PRIGH_MCP_TOKEN":"t0k3n"}}
    {"name":"prigh","command":"/nix/store/xxx-prigh/bin/prigh","args":["mcp-bridge"],"env":[{"name":"PRIGH_MCP_SOCKET","value":"/run/user/1000/prigh/mcp-3.sock"},{"name":"PRIGH_MCP_TOKEN","value":"t0k3n"}]}
    |}]
;;

let%expect_test "mcp-bridge relay" =
  Eio_main.run (fun env ->
    let dir = Filename_unix.temp_dir "prigh-bridge" "" in
    let path = dir ^/ "mcp.sock" in
    Eio.Switch.run (fun sw ->
      let listening =
        Eio.Net.listen ~sw ~backlog:1 (Eio.Stdenv.net env) (`Unix path)
      in
      (* The backend's end: answers each line until the bridge's input ends,
         then says goodbye and closes. *)
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Net.accept_fork ~sw listening ~on_error:raise (fun flow _ ->
          let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
          Stdlib.Seq.iter
            (fun line ->
               Eio.Flow.copy_string (sprintf "reply to %s\n" line) flow)
            (Eio.Buf_read.lines reader);
          Eio.Flow.copy_string "bye\n" flow));
      let socket = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix path) in
      let out = Buffer.create 64 in
      Mcp_bridge.relay
        ~stdin:(Eio.Flow.string_source "one\ntwo\n")
        ~stdout:(Eio.Flow.buffer_sink out)
        socket;
      print_string (Buffer.contents out));
    let show = function
      | Ok () -> print_endline "ok"
      | Error e ->
        print_endline
          (String.substr_replace_all
             (Error.to_string_hum e)
             ~pattern:dir
             ~with_:"$DIR")
    in
    show (Mcp_bridge.run env ~socket:None ~token:None);
    show
      (Mcp_bridge.run
         env
         ~socket:(Some (dir ^/ "gone.sock"))
         ~token:(Some "t0k3n")));
  [%expect
    {|
    reply to one
    reply to two
    bye
    $PRIGH_MCP_SOCKET or $PRIGH_MCP_TOKEN is not set: prigh mcp-bridge is started by a harness prigh runs (Codex, an ACP agent), not by hand
    cannot reach prigh's tools at $DIR/gone.sock: the session that started this harness is gone
    |}]
;;
