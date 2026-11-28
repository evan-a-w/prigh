open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The real [prigh acp] over pipes, as an editor runs it: its own backend
   in the process, a scripted model, the editor answering fs requests.
   Every line on stdout is protocol JSON. *)

let exe = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"
let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let%expect_test "prigh acp -faux-script over stdio" =
  with_sandbox
  @@ fun t ->
  write t "proj/a.txt" "on disk\n";
  write
    t
    "script.json"
    {|[{"thinking": "Look first.", "text": "Reading.", "tool_calls": [{"id": "c1", "name": "read", "arguments": {"path": "a.txt"}}]},
       {"tool_calls": [{"id": "c2", "name": "bash", "arguments": {"command": "echo from bash"}}]},
       {"text": "Done."}]|};
  let proj = Filename.concat t.dir "proj" in
  let p =
    Core_unix.create_process_env
      ~prog:exe
      ~args:
        [ "acp"
        ; "-faux-script"
        ; Filename.concat t.dir "script.json"
        ; "-name"
        ; "testhost"
        ]
      ~env:
        (`Replace
            [ "HOME", t.dir
            ; "PATH", Option.value (Sys.getenv "PATH") ~default:"/usr/bin:/bin"
            ])
      ()
  in
  let to_agent = Core_unix.out_channel_of_descr p.stdin in
  let from_agent = Core_unix.in_channel_of_descr p.stdout in
  let send line =
    Out_channel.output_string to_agent (line ^ "\n");
    Out_channel.flush to_agent
  in
  let show line =
    let shown =
      match Json.parse line with
      | Error _ -> "NOT JSON: " ^ line
      | Ok json ->
        let jsonrpc = string_field "jsonrpc" json in
        let what =
          match field "method" json, field "params" json with
          | `String "session/update", params ->
            let update = field "update" params in
            let kind = string_field "sessionUpdate" update in
            (match kind, string_field "status" update with
             | "tool_call_update", "" -> "" (* output so far: timing *)
             | "tool_call_update", status ->
               let text =
                 match field "content" update with
                 | `Array (block :: _) ->
                   string_field "text" (field "content" block)
                 | _ -> ""
               in
               sprintf "update %s %s %S" kind status text
             | _ -> "update " ^ kind)
          | `String method_, _ -> method_
          | _ ->
            (match Json.member "error" json with
             | Some error -> "error " ^ Json.to_string error
             | None -> "result " ^ Json.to_string (field "id" json))
        in
        if String.equal jsonrpc "2.0" then what else "NOT JSON-RPC: " ^ line
    in
    if not (String.is_empty shown) then print_endline (mask t shown)
  in
  (* Reads and shows lines (answering the agent's requests) until the
     response to [id]; returns it. *)
  let rec until_response id =
    match In_channel.input_line from_agent with
    | None ->
      print_endline "(stdout closed)";
      `Null
    | Some line ->
      show line;
      let json = Json.of_string line in
      (match Json.member "id" json, Json.member "method" json with
       | Some request_id, Some (`String "fs/read_text_file") ->
         send
           (Json.to_string
              (`Object
                  [ "jsonrpc", `String "2.0"
                  ; "id", request_id
                  ; "result", `Object [ "content", `String "from the editor\n" ]
                  ]));
         until_response id
       | Some (`Number n), None when Int.of_string n = id -> json
       | _ -> until_response id)
  in
  send
    {|{"jsonrpc": "2.0", "id": 0, "method": "initialize", "params": {"protocolVersion": 1, "clientCapabilities": {"fs": {"readTextFile": true}}}}|};
  ignore (until_response 0 : Json.t);
  send
    (sprintf
       {|{"jsonrpc": "2.0", "id": 1, "method": "session/new", "params": {"cwd": %S, "mcpServers": []}}|}
       proj);
  let session = string_field "sessionId" (field "result" (until_response 1)) in
  send
    (sprintf
       {|{"jsonrpc": "2.0", "id": 2, "method": "session/prompt", "params": {"sessionId": %S, "prompt": [{"type": "text", "text": "go"}]}}|}
       session);
  print_endline (Json.to_string (field "result" (until_response 2)));
  Out_channel.close to_agent;
  let rest = In_channel.input_all from_agent in
  let err = In_channel.input_all (Core_unix.in_channel_of_descr p.stderr) in
  print_endline (mask t (sprintf "after stdin closed: %S, stderr: %S" rest err));
  print_s [%sexp (Core_unix.waitpid p.pid : Core_unix.Exit_or_signal.t)];
  [%expect
    {|
    result 0
    result 1
    update available_commands_update
    update agent_thought_chunk
    update agent_message_chunk
    update tool_call
    fs/read_text_file
    update tool_call_update in_progress ""
    update tool_call_update completed "from the editor\n"
    update tool_call
    update tool_call_update in_progress ""
    update tool_call_update completed "```\nfrom bash\n```"
    update agent_message_chunk
    result 2
    {"stopReason":"end_turn"}
    after stdin closed: "", stderr: ""
    (Ok ())
    |}]
;;

let%expect_test
    "prigh serve backend:acp:antigravity end-to-end: wrapper, mcp-bridge, \
     hashed MCP tool output, multi-turn"
  =
  with_sandbox
  @@ fun t ->
  write t "proj/hello.txt" "hello from disk\n";
  let proj = Filename.concat t.dir "proj" in
  let fake_agy = Filename.concat t.dir "fake-agy.sh" in
  let log_file = Filename.concat t.dir "agy.log" in
  (* A self-contained fake [agy] script exercising the real backend_wrapper,
     --gemini-dir / prigh-backend.md, Harness_pool, Host_transport MCP Unix
     socket, and the real [prigh mcp-bridge] executable. *)
  write
    t
    "fake-agy.sh"
    (sprintf
       {|#!/bin/sh
set -u
LOG=%s
printf 'args=%%s\n' "$*" >> "$LOG"
printf 'cwd=%%s\n' "$(pwd)" >> "$LOG"
gemini_dir=""
prev=""
for a in "$@"; do
  if [ "$prev" = "--gemini-dir" ]; then gemini_dir="$a"; fi
  prev="$a"
done
if [ -f "$gemini_dir/config/agents/prigh-backend.md" ]; then
  printf 'agent_md=present\n' >> "$LOG"
else
  printf 'agent_md=missing\n' >> "$LOG"
fi
mcp_cmd=""
mcp_sock=""
mcp_tok=""
turn=0
while IFS= read -r line; do
  id=$(printf '%%s' "$line" | sed -n 's/.*"id":\([0-9]*\).*/\1/p')
  case "$line" in
    *'"method":"initialize"'*)
      printf '{"jsonrpc":"2.0","id":%%s,"result":{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"sessionCapabilities":{"close":{}},"_meta":{"antigravity":{"systemPrompt":{"append":true}}}},"authMethods":[]}}\n' "$id"
      ;;
    *'"method":"session/new"'*)
      case "$line" in
        *'"systemPrompt":{"append":'*) printf 'sys_prompt_append=yes\n' >> "$LOG" ;;
        *) printf 'sys_prompt_append=no\n' >> "$LOG" ;;
      esac
      mcp_cmd=$(printf '%%s' "$line" | sed -n 's/.*"mcpServers":\[{"name":"prigh","command":"\([^"]*\)".*/\1/p')
      mcp_sock=$(printf '%%s' "$line" | sed -n 's/.*"name":"PRIGH_MCP_SOCKET","value":"\([^"]*\)".*/\1/p')
      mcp_tok=$(printf '%%s' "$line" | sed -n 's/.*"name":"PRIGH_MCP_TOKEN","value":"\([^"]*\)".*/\1/p')
      printf '{"jsonrpc":"2.0","id":%%s,"result":{"sessionId":"ses_real","models":{"currentModelId":"gemini-2.5-pro","availableModels":[{"modelId":"gemini-2.5-pro","name":"Gemini 2.5 Pro"}]}}}\n' "$id"
      ;;
    *'"method":"session/prompt"'*)
      turn=$((turn + 1))
      case "$line" in
        *'Context handoff'*) printf 'turn%%d_handoff=yes\n' "$turn" >> "$LOG" ;;
        *) printf 'turn%%d_handoff=no\n' "$turn" >> "$LOG" ;;
      esac
      if [ "$turn" = 1 ]; then
        printf '{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_real","update":{"sessionUpdate":"tool_call","toolCallId":"c1","kind":"other","title":"mcp_prigh_79ce654a_ls","status":"pending","rawInput":{}}}}\n'
        # Keep mcp-bridge stdin open until its tools/call reply arrives on stdout.
        mcp_in="$gemini_dir/mcp.in"
        mcp_out_f="$gemini_dir/mcp.out"
        mkfifo "$mcp_in" "$mcp_out_f"
        PRIGH_MCP_SOCKET="$mcp_sock" PRIGH_MCP_TOKEN="$mcp_tok" "$mcp_cmd" mcp-bridge <"$mcp_in" >"$mcp_out_f" &
        mcp_pid=$!
        exec 3>"$mcp_in"
        exec 4<"$mcp_out_f"
        printf '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"fake-agy","version":"1"}}}\n' >&3
        IFS= read -r _init_resp <&4
        printf '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"ls","arguments":{}}}\n' >&3
        IFS= read -r mcp_out <&4
        exec 3>&-
        exec 4<&-
        wait "$mcp_pid" 2>/dev/null || true
        case "$mcp_out" in
          *'hello.txt'*) printf 'mcp_bridge_ls=ok\n' >> "$LOG" ;;
          *) printf 'mcp_bridge_ls=fail:%%s\n' "$mcp_out" >> "$LOG" ;;
        esac
        # Omit output on tool_call_update, matching real agy 1.5.13.
        printf '{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_real","update":{"sessionUpdate":"tool_call_update","toolCallId":"c1","status":"completed"}}}\n'
      fi
      printf '{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_real","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"reply %%d"}}}}\n' "$turn"
      printf '{"jsonrpc":"2.0","id":%%s,"result":{"stopReason":"end_turn"}}\n' "$id"
      ;;
    *'"method":"session/close"'*)
      printf '{"jsonrpc":"2.0","id":%%s,"result":{}}\n' "$id"
      ;;
    *)
      if [ -n "$id" ]; then
        printf '{"jsonrpc":"2.0","id":%%s,"result":{}}\n' "$id"
      fi
      ;;
  esac
done
|}
       (Filename.quote log_file));
  Core_unix.chmod fake_agy ~perm:0o755;
  let p =
    Core_unix.create_process_env
      ~prog:exe
      ~args:
        [ "serve"
        ; "-faux"
        ; "-cwd"
        ; proj
        ; "-engines"
        ; "harness,composer"
        ; "-adapters"
        ; sprintf "backend:acp:antigravity=%s acp" fake_agy
        ]
      ~env:
        (`Replace
            [ "HOME", t.dir
            ; "PATH", Option.value (Sys.getenv "PATH") ~default:"/usr/bin:/bin"
            ; "PRIGH_HOST_NAME", "<host>"
            ])
      ()
  in
  let to_srv = Core_unix.out_channel_of_descr p.stdin in
  let from_srv = Core_unix.in_channel_of_descr p.stdout in
  let send line =
    Out_channel.output_string to_srv (line ^ "\n");
    Out_channel.flush to_srv
  in
  let rec read_until ~pred =
    match In_channel.input_line from_srv with
    | None -> failwith "prigh serve closed stdout early"
    | Some line ->
      let json = Json.of_string line in
      if pred json then json else read_until ~pred
  in
  let call id meth params =
    send (sprintf {|{"id":%S,"method":%S,"params":%s}|} id meth params);
    let resp =
      read_until ~pred:(fun j ->
        String.equal (string_field "type" j) "response"
        && String.equal (string_field "id" j) id)
    in
    match Json.member "ok" resp with
    | Some `True -> ()
    | _ -> failwith ("RPC error: " ^ Json.to_string resp)
  in
  let wait_agent_end () =
    let tool_ends = ref [] in
    let reply = ref "" in
    ignore
      (read_until ~pred:(fun j ->
         match string_field "event" j with
         | "tool_end" ->
           let r = field "result" j in
           tool_ends
           := !tool_ends
              @ [ ( string_field "tool_name" r
                  , String.strip (string_field "text" r) )
                ];
           false
         | "message_end" ->
           let m = field "message" j in
           if String.equal (string_field "role" m) "assistant"
           then (
             match field "content" m with
             | `Array blocks ->
               List.iter blocks ~f:(fun b ->
                 if String.equal (string_field "type" b) "text"
                 then reply := string_field "text" b)
             | _ -> ());
           false
         | "agent_end" -> true
         | _ -> false)
       : Json.t);
    !tool_ends, !reply
  in
  call "1" "hello" {|{}|};
  call "2" "new_session" {|{"adapter":"backend:acp:antigravity"}|};
  call "3" "set_mode" {|{"approval":"auto"}|};
  call "4" "prompt" {|{"text":"list files"}|};
  let t1_tools, t1_reply = wait_agent_end () in
  call "5" "prompt" {|{"text":"second turn"}|};
  let t2_tools, t2_reply = wait_agent_end () in
  Out_channel.close to_srv;
  ignore (In_channel.input_all from_srv : string);
  ignore (Core_unix.waitpid p.pid : Core_unix.Exit_or_signal.t);
  List.iter t1_tools ~f:(fun (name, text) ->
    printf "turn1 tool_end: name=%s text=%S\n" name text);
  printf "turn1 reply: %S\n" t1_reply;
  List.iter t2_tools ~f:(fun (name, text) ->
    printf "turn2 tool_end: name=%s text=%S\n" name text);
  printf "turn2 reply: %S\n" t2_reply;
  let log =
    In_channel.read_all log_file
    |> String.split_lines
    |> List.map ~f:(fun line ->
      Re.replace_string
        (Re.compile (Re.Pcre.re "/prigh-agy\\.[0-9]+\\.[A-Za-z0-9]+"))
        ~by:"/prigh-agy.<pid>.<tmp>"
        line
      |> Re.replace_string
           (Re.compile (Re.Pcre.re "/tmp|/run/user/[0-9]+"))
           ~by:"<TMP>"
      |> mask t)
  in
  List.iter log ~f:print_endline;
  [%expect
    {|
    turn1 tool_end: name=ls text="hello.txt"
    turn1 reply: "reply 1"
    turn2 reply: "reply 2"
    args=acp --process-per-session --agent prigh-backend --include-default-customizations=false --disable-telemetry --dangerously-skip-permissions --gemini-dir <TMP>/prigh-agy.<pid>.<tmp>
    cwd=<TMP>/prigh-agy.<pid>.<tmp>/cwd
    agent_md=present
    sys_prompt_append=yes
    turn1_handoff=no
    mcp_bridge_ls=ok
    turn2_handoff=no
    |}]
;;
