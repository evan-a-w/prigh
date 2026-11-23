open! Core
open! Prigh
module C = Claude_codec
module Json = Jsonaf

(* Long strings (tool outputs, diffs, inputs) cut so the expectations stay
   readable. *)
let rec trim (sexp : Sexp.t) : Sexp.t =
  match sexp with
  | Atom s when String.length s > 70 -> Atom (String.prefix s 67 ^ "...")
  | Atom _ -> sexp
  | List l -> List (List.map l ~f:trim)
;;

let print_sexp sexp = print_endline (Sexp.to_string_hum (trim sexp))

module Printed = struct
  type t =
    | Out of C.Output.t
    | Wrote of string (** a line the test sent back *)
end

(* Runs of deltas on one item print as one line. *)
let print_all (printed : Printed.t list) =
  let flush = function
    | None -> ()
    | Some (kind, item, n, text) ->
      print_sexp
        [%sexp
          (kind : string)
        , (item : string)
        , (sprintf "%d chunks" n : string)
        , (text : string)]
  in
  let pending =
    List.fold printed ~init:None ~f:(fun pending printed ->
      let delta =
        match printed with
        | Out (Event (Text_delta { item; text })) ->
          Some ("Text_delta", item, text)
        | Out (Event (Thinking_delta { item; text })) ->
          Some ("Thinking_delta", item, text)
        | Out _ | Wrote _ -> None
      in
      match pending, delta with
      | Some (k, i, n, acc), Some (k', i', text)
        when String.equal k k' && String.equal i i' ->
        Some (k, i, n + 1, acc ^ text)
      | _, Some (k, i, text) ->
        flush pending;
        Some (k, i, 1, text)
      | _, None ->
        flush pending;
        (match printed with
         | Out output -> print_sexp [%sexp (output : C.Output.t)]
         | Wrote line -> print_endline ("<- " ^ line));
        None)
  in
  flush pending
;;

let print_outputs outputs =
  print_all (List.map outputs ~f:(fun o -> Printed.Out o))
;;

let fixture name = In_channel.read_lines ("fixtures/claude/" ^ name ^ ".jsonl")

let feed ?(on_output = fun _ _ -> []) t lines =
  List.concat_map lines ~f:(fun line ->
    List.concat_map (C.handle_line t line) ~f:(fun o ->
      Printed.Out o :: List.map (on_output t o) ~f:(fun l -> Printed.Wrote l)))
  |> print_all
;;

let replay ?on_output ?(t = C.create ()) name =
  feed ?on_output t (fixture name);
  t
;;

let%expect_test "launch" =
  let argv launch = print_s [%sexp (Claude_launch.argv launch : string list)] in
  argv Claude_launch.default;
  [%expect
    {|
    (claude --output-format stream-json --verbose --permission-prompt-tool stdio
     --strict-mcp-config --setting-sources=user --include-partial-messages
     --input-format stream-json)
    |}];
  argv
    { Claude_launch.default with
      model = Some "claude-sonnet-4-6"
    ; permission_mode = Some Bypass_permissions
    ; resume = Resume { session_id = "abc"; fork = true; at = Some "-uuid" }
    ; mcp_config = Some (Claude_launch.sdk_mcp_config [ "prigh" ])
    ; max_thinking_tokens = Some 8000
    ; effort = Some "high"
    };
  [%expect
    {|
    (claude --output-format stream-json --verbose --model claude-sonnet-4-6
     --permission-prompt-tool stdio --permission-mode bypassPermissions
     --allow-dangerously-skip-permissions --resume=abc --fork-session
     --resume-session-at=-uuid --mcp-config
     "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
     --strict-mcp-config --setting-sources=user --include-partial-messages
     --max-thinking-tokens 8000 --effort=high --input-format stream-json)
    |}];
  argv
    { Claude_launch.default with resume = New { session_id = Some "pinned" } };
  [%expect
    {|
    (claude --output-format stream-json --verbose --permission-prompt-tool stdio
     --session-id=pinned --strict-mcp-config --setting-sources=user
     --include-partial-messages --input-format stream-json)
    |}];
  (* A value that could be read as a flag goes after an [=]. *)
  argv
    { Claude_launch.default with model = Some "--dangerously-skip-permissions" };
  [%expect
    {|
    (claude --output-format stream-json --verbose
     --model=--dangerously-skip-permissions --permission-prompt-tool stdio
     --strict-mcp-config --setting-sources=user --include-partial-messages
     --input-format stream-json)
    |}];
  (* Every prigh mode launches Claude Code asking ([default]): prigh's
     approval, with its rules, answers. The user's instructions are not on
     the command line. *)
  List.iter Approval_mode.all ~f:(fun approval ->
    let launch =
      Claude_adapter.launch
        { (Adapter.Start.default ~cwd:"/w") with
          approval
        ; developer_instructions = Some "secret about the user"
        }
    in
    print_s [%sexp (approval : Approval_mode.t), (launch.argv : string list)]);
  [%expect
    {|
    (Ask
     (claude --output-format stream-json --verbose --permission-prompt-tool stdio
      --permission-mode default --mcp-config
      "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
      --strict-mcp-config --setting-sources=user --include-partial-messages
      --input-format stream-json))
    (Auto_edit
     (claude --output-format stream-json --verbose --permission-prompt-tool stdio
      --permission-mode default --mcp-config
      "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
      --strict-mcp-config --setting-sources=user --include-partial-messages
      --input-format stream-json))
    (Auto
     (claude --output-format stream-json --verbose --permission-prompt-tool stdio
      --permission-mode default --mcp-config
      "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
      --strict-mcp-config --setting-sources=user --include-partial-messages
      --input-format stream-json))
    (Smart
     (claude --output-format stream-json --verbose --permission-prompt-tool stdio
      --permission-mode default --mcp-config
      "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
      --strict-mcp-config --setting-sources=user --include-partial-messages
      --input-format stream-json))
    |}];
  print_s
    [%sexp
      ((Claude_adapter.launch
          { (Adapter.Start.default ~cwd:"/w") with plan = true })
         .argv
       : string list)];
  [%expect
    {|
    (claude --output-format stream-json --verbose --permission-prompt-tool stdio
     --permission-mode plan --mcp-config
     "{\"mcpServers\":{\"prigh\":{\"type\":\"sdk\",\"name\":\"prigh\"}}}"
     --strict-mcp-config --setting-sources=user --include-partial-messages
     --input-format stream-json)
    |}]
;;

let%expect_test "the user's instructions go in initialize, not on argv" =
  let t = C.create () in
  print_endline
    (C.initialize
       t
       ~append_system_prompt:"About the user"
       ~sdk_mcp_servers:[]
       ());
  [%expect
    {| {"type":"control_request","request_id":"req_1_prigh","request":{"subtype":"initialize","hooks":null,"sdkMcpServers":[],"appendSystemPrompt":"About the user"}} |}]
;;

let%expect_test "a resumed session's running cost total counts from its last" =
  let result total =
    sprintf
      {|{"type":"result","subtype":"success","is_error":false,"result":"ok","session_id":"s-1","total_cost_usd":%s,"usage":{"input_tokens":10,"output_tokens":3}}|}
      total
  in
  (* Resumed with the total it reported last: only the new part is spend. *)
  let t = C.create ~cost_total:0.5 () in
  feed t [ result "0.75" ];
  print_s [%sexp (C.cost_total t : float option)];
  (* A new session counts from nothing. *)
  let t = C.create () in
  print_s [%sexp (C.cost_total t : float option)];
  feed t [ result "0.25" ];
  [%expect
    {|
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Usage
      ((usage ((input 10) (output 3) (cache_read 0))) (cost_usd (0.25))
       (context_tokens ()) (context_window ()))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    (0.75)
    ()
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Usage
      ((usage ((input 10) (output 3) (cache_read 0))) (cost_usd (0.25))
       (context_tokens ()) (context_window ()))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}]
;;

let%expect_test "error results: errors first, then result, then the HTTP status"
  =
  let t = C.create () in
  feed
    t
    [ {|{"type":"result","subtype":"error_during_execution","is_error":true,"result":"ignored","errors":["first",{"code":2}],"session_id":"s-1","usage":{"input_tokens":1,"output_tokens":1}}|}
    ; {|{"type":"result","subtype":"error_during_execution","is_error":true,"session_id":"s-1","api_error_status":500,"usage":{"input_tokens":1,"output_tokens":1}}|}
    ];
  [%expect
    {|
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Usage
      ((usage ((input 1) (output 1) (cache_read 0))) (cost_usd ())
       (context_tokens ()) (context_window ()))))
    (Event
     (Turn_end (turn_id (turn-1))
      (stop (Failed (kind Other) (message "first; {\"code\":2}")))))
    (Event (Turn_started (turn_id turn-2)))
    (Event
     (Usage
      ((usage ((input 1) (output 1) (cache_read 0))) (cost_usd ())
       (context_tokens ()) (context_window ()))))
    (Event
     (Turn_end (turn_id (turn-2))
      (stop (Failed (kind Other) (message "API error (HTTP 500)")))))
    |}]
;;

let%expect_test "our requests and the answers to them" =
  let t = C.create () in
  print_endline (C.initialize t ~sdk_mcp_servers:[ "prigh" ] ());
  print_endline
    (C.user_message
       t
       ~text:"look"
       ~images:[ { Image.mime_type = "image/png"; data = "iVBOR" } ]
       ());
  print_endline (C.compact t ~instructions:(Some " keep the plan "));
  print_endline (C.set_model t (Some "claude-opus-4-1"));
  print_endline (C.set_max_thinking_tokens t None);
  print_endline (C.set_permission_mode t Plan);
  print_endline (C.interrupt t);
  [%expect
    {|
    {"type":"control_request","request_id":"req_1_prigh","request":{"subtype":"initialize","hooks":null,"sdkMcpServers":["prigh"]}}
    {"type":"user","session_id":"","parent_tool_use_id":null,"message":{"role":"user","content":[{"type":"text","text":"look"},{"type":"image","source":{"type":"base64","media_type":"image/png","data":"iVBOR"}}]}}
    {"type":"user","session_id":"","parent_tool_use_id":null,"message":{"role":"user","content":[{"type":"text","text":"/compact keep the plan"}]}}
    {"type":"control_request","request_id":"req_2_prigh","request":{"subtype":"set_model","model":"claude-opus-4-1"}}
    {"type":"control_request","request_id":"req_3_prigh","request":{"subtype":"set_max_thinking_tokens","max_thinking_tokens":null}}
    {"type":"control_request","request_id":"req_4_prigh","request":{"subtype":"set_permission_mode","mode":"plan"}}
    {"type":"control_request","request_id":"req_5_prigh","request":{"subtype":"interrupt"}}
    |}];
  let reply line = print_outputs (C.handle_line t line) in
  reply
    {|{"type":"control_response","response":{"subtype":"success","request_id":"req_1_prigh","response":{"claude_code_version":"2.1.330","account":{"email":"l@example.com","subscriptionType":"max","tokenSource":"claude.ai"},"models":[{"value":"default","displayName":"Default (recommended)","description":"Opus 4.7"},{"value":"haiku","displayName":"Haiku"}],"commands":[{"name":"compact","description":"Clear history but keep a summary","argumentHint":""}]}}}|};
  reply
    {|{"type":"control_response","response":{"subtype":"success","request_id":"req_2_prigh","response":{}}}|};
  reply
    {|{"type":"control_response","response":{"subtype":"error","request_id":"req_4_prigh","error":"Cannot set permission mode"}}|};
  (* Not ours, or already answered: ignored. *)
  reply
    {|{"type":"control_response","response":{"subtype":"success","request_id":"req_2_prigh","response":{}}}|};
  [%expect
    {|
    (Response
     (Initialized
      ((version (2.1.330))
       (account
        (((email (l@example.com)) (organization ()) (subscription_type (max))
          (token_source (claude.ai)) (api_key_source ()) (api_provider ()))))
       (models
        (((value default) (display_name "Default (recommended)")
          (description ("Opus 4.7")))
         ((value haiku) (display_name Haiku) (description ()))))
       (commands
        (((name compact) (description ("Clear history but keep a summary"))))))))
    (Response (Done Set_model))
    (Response
     (Failed (command Set_permission_mode)
      (message "Cannot set permission mode")))
    |}];
  (* The process exits with requests unanswered. *)
  print_outputs (C.process_exited t ~code:(Some 1) ~stderr_tail:"");
  [%expect
    {|
    (Response
     (Failed (command Set_max_thinking_tokens) (message "Claude Code exited")))
    (Response (Failed (command Interrupt) (message "Claude Code exited")))
    |}]
;;

let%expect_test "logged in?" =
  let account
        ?token_source
        ?api_key_source
        ?email
        ?subscription_type
        ?api_provider
        ()
    =
    C.Account.logged_in
      { email
      ; organization = None
      ; subscription_type
      ; token_source
      ; api_key_source
      ; api_provider
      }
  in
  print_s
    [%sexp
      ([ account ~token_source:"none" ~api_provider:"firstParty" ()
       ; account ~token_source:"claude.ai" ()
       ; account ~token_source:"none" ~api_key_source:"ANTHROPIC_API_KEY" ()
       ; account ~api_provider:"bedrock" ()
       ]
       : bool list)];
  [%expect {| (false true true true) |}]
;;

let%expect_test "a simple turn" =
  let t = replay "simple" in
  [%expect
    {|
    (Event
     (Thread_started (thread_id 77171d01-fb4f-4dff-a961-d9dd334be93d)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Text_done (item msg_01CM449j71BXKjsRZZpW2z64:0) (text "fixture simple ok")))
    (Event
     (Usage
      ((usage ((input 11796) (output 6) (cache_read 9455))) (cost_usd (0.011703))
       (context_tokens (11797)) (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}];
  print_s [%sexp (C.session_id t : string option), (C.in_turn t : bool)];
  [%expect {| ((77171d01-fb4f-4dff-a961-d9dd334be93d) false) |}]
;;

let%expect_test "streamed text and a command with its output" =
  ignore (replay "streaming_bash" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id 95b68ca2-f0b8-4902-839b-3586d4ba3f3d)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Thinking_delta msg_011CfefxsSL5BaoUrPAkdYT6:0 "8 chunks"
     "The user wants me to run a specific shell command in the foreground...")
    (Event
     (Thinking_done (item msg_011CfefxsSL5BaoUrPAkdYT6:0)
      (text
       "The user wants me to run a specific shell command in the foreground...")))
    (Text_delta msg_011CfefxsSL5BaoUrPAkdYT6:1 "3 chunks"
     "Running the specified command now.")
    (Event
     (Text_done (item msg_011CfefxsSL5BaoUrPAkdYT6:1)
      (text "Running the specified command now.")))
    (Event
     (Tool_start (id toolu_01CJnQSvsvh3iZtmvSWQVVvg) (kind Command)
      (native_name Bash)
      (title
       "Bash: for i in 1 2 3 4; do printf '\\033[32mstream line %s\\033[0m\\n'...")
      (input
       (Object
        ((command
          (String
           "for i in 1 2 3 4; do printf '\\033[32mstream line %s\\033[0m\\n' $i; s...")))))
      (parent ())))
    (Event
     (Tool_end (id toolu_01CJnQSvsvh3iZtmvSWQVVvg) (ok true)
      (output
        "\027[32mstream line 1\027[0m\
       \n\027[32mstream line 2\027[0m\
       \n\027[32mstream line 3\027[0...")
      (changes ()) (images ())))
    (Text_delta msg_011CfefyTbebJFTYTaoceKGP:0 "2 chunks"
     "command output fixture complete")
    (Event
     (Text_done (item msg_011CfefyTbebJFTYTaoceKGP:0)
      (text "command output fixture complete")))
    (Event
     (Usage
      ((usage ((input 269947) (output 175) (cache_read 256120)))
       (cost_usd (0.16666124999999998)) (context_tokens (135090))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}]
;;

let%expect_test "two turns: cost per turn, not the process's total" =
  ignore (replay "multi_turn" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id 01899f29-45f7-43c3-b963-0f9181ed013f)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Text_done (item msg_018QDgm81Jgo5XonK3Bm8dEc:0)
      (text "first fixture turn complete")))
    (Event
     (Usage
      ((usage ((input 11795) (output 7) (cache_read 9455)))
       (cost_usd (0.01171425)) (context_tokens (11796))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    (Event (Turn_started (turn_id turn-2)))
    (Event
     (Text_done (item msg_01PifXzuLoccLsnMjRC9hXJ8:0)
      (text "second fixture turn complete")))
    (Event
     (Usage
      ((usage ((input 11814) (output 7) (cache_read 11792)))
       (cost_usd (0.0037228499999999998)) (context_tokens (11821))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-2)) (stop Done)))
    |}]
;;

let%expect_test "subagents: their calls name the Agent call as parent" =
  ignore (replay "subagent" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id edf38df1-8d2f-4781-b9e3-f9e72ba7b76d)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Thinking_done (item msg_014tG9SY8VfiqS7bCEAJ1oKt:0)
      (text
       "The user wants me to spawn 2 subagents in parallel - one to read pa...")))
    (Event
     (Text_done (item msg_014tG9SY8VfiqS7bCEAJ1oKt:1)
      (text "I'll spawn both subagents simultaneously right now!")))
    (Event
     (Tool_start (id toolu_01FfHTrUeymLi4LfqJVmTZjz) (kind Subagent)
      (native_name Agent) (title "Agent Read package.json")
      (input
       (Object
        ((description (String "Read package.json"))
         (prompt
          (String
           "Read the file `package.json` in the current working directory and r...")))))
      (parent ())))
    (Event
     (Tool_start (id toolu_0113VSWTCamvp1adXDZrK5NG) (kind Subagent)
      (native_name Agent) (title "Agent Read tsconfig.json")
      (input
       (Object
        ((description (String "Read tsconfig.json"))
         (prompt
          (String
           "Read the file `tsconfig.json` in the current working directory and ...")))))
      (parent ())))
    (Event
     (Tool_start (id toolu_01EV8j88CPEmhoWF3iuvaFSC) (kind Read)
      (native_name Read)
      (title
       "Read /private/var/folders/b3/b51_pdxj7dl0t981zpqkxxhr0000gn/T/t3-or...")
      (input
       (Object
        ((file_path
          (String
           /private/var/folders/b3/b51_pdxj7dl0t981zpqkxxhr0000gn/T/t3-orchest...)))))
      (parent (toolu_01FfHTrUeymLi4LfqJVmTZjz))))
    (Event
     (Tool_start (id toolu_01P6KQcUPy8t2bCbFjv8K4Jz) (kind Read)
      (native_name Read)
      (title
       "Read /private/var/folders/b3/b51_pdxj7dl0t981zpqkxxhr0000gn/T/t3-or...")
      (input
       (Object
        ((file_path
          (String
           /private/var/folders/b3/b51_pdxj7dl0t981zpqkxxhr0000gn/T/t3-orchest...)))))
      (parent (toolu_0113VSWTCamvp1adXDZrK5NG))))
    (Event
     (Tool_end (id toolu_01P6KQcUPy8t2bCbFjv8K4Jz) (ok true)
      (output
       "1\t{\"compilerOptions\":{\"module\":\"ESNext\",\"strict\":true,\"target\":\"ES2...")
      (changes ()) (images ())))
    (Event
     (Tool_end (id toolu_01EV8j88CPEmhoWF3iuvaFSC) (ok true)
      (output
       "1\t{\"name\":\"claude-read-only-fixture\",\"private\":true,\"scripts\":{\"typ...")
      (changes ()) (images ())))
    (Event
     (Tool_end (id toolu_0113VSWTCamvp1adXDZrK5NG) (ok true)
      (output
        "Here are the full contents of `tsconfig.json`:\
       \n\
       \n```json\
       \n{\"compilerO...")
      (changes ()) (images ())))
    (Event
     (Tool_end (id toolu_01FfHTrUeymLi4LfqJVmTZjz) (ok true)
      (output
        "Here are the full contents of `package.json`:\
       \n\
       \n```json\
       \n{\"name\":\"cla...")
      (changes ()) (images ())))
    (Event
     (Text_done (item msg_0149tB2WYRkkPLcXTkYZknLb:0)
      (text
       "Both subagents completed in parallel! Here's a summary of what they...")))
    (Event
     (Usage
      ((usage ((input 24305) (output 419) (cache_read 21261)))
       (cost_usd (0.064978650000000013)) (context_tokens (12498))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}]
;;

let%expect_test "an error result" =
  ignore (replay "result_is_error" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id aa401aa4-e597-4e40-8e40-1e401e401e40)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Turn_end (turn_id (turn-1))
      (stop
       (Failed
        (kind (Auth_required (hint "run `claude auth login` on the tool host")))
        (message
         "Failed to authenticate. API Error: 401 Invalid authentication crede...")))))
    (Event (Turn_started (turn_id turn-2)))
    (Event
     (Text_done (item msg_01iserrorfixture1:0)
      (text "claude result is_error fixture recovered")))
    (Event
     (Usage
      ((usage ((input 2) (output 2) (cache_read 0))) (cost_usd (0.001))
       (context_tokens (2)) (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-2)) (stop Done)))
    |}]
;;

let%expect_test "usage limit" =
  ignore (replay "usage_limit" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id b8eed9d2-ed85-48fb-a4f2-316f6a0b1117)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event (Retrying (attempt (1)) (message "rate_limit (429)")))
    (Event (Retrying (attempt (2)) (message "rate_limit (429)")))
    (Event
     (Turn_end (turn_id (turn-1))
      (stop
       (Failed (kind (Usage_limit (resets_at ())))
        (message
         "API Error: Request rejected (429) \194\183 All credentials for model clau...")))))
    |}]
;;

let%expect_test "compaction" =
  ignore (replay "compact" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id c0de0c0d-5e55-4c0a-9a7c-0000000000c1)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Text_delta msg_011CfasheXv56NiEcGmwdisp:0 "2 chunks"
     "compact probe first turn")
    (Event
     (Text_done (item msg_011CfasheXv56NiEcGmwdisp:0)
      (text "compact probe first turn")))
    (Event
     (Usage
      ((usage ((input 27438) (output 7) (cache_read 0)))
       (cost_usd (0.16794900000000001)) (context_tokens (27439))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    (Event (Turn_started (turn_id turn-2)))
    (Event (Text_done (item msg_011CfZFfrqqHPHu8XPypPQCh:0) (text PEER_ACK)))
    (Event
     (Usage
      ((usage ((input 3) (output 4) (cache_read 0))) (cost_usd (0.012))
       (context_tokens (7)) (context_window ()))))
    (Event (Turn_end (turn_id (turn-2)) (stop Done)))
    (Event (Turn_started (turn_id turn-3)))
    (Event (Compacted (tokens_before (27445)) (tokens_after (1192))))
    (Event
     (Usage
      ((usage ((input 0) (output 0) (cache_read 0))) (cost_usd (0.1852365))
       (context_tokens (7)) (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-3)) (stop Done)))
    |}]
;;

let%expect_test "web search" =
  ignore (replay "web_search" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id f5ff7f51-4729-4331-93bd-416f3fdf2d19)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Thinking_done (item msg_013ghDLNooL9JjT7LM8dfvKv:0)
      (text
       "The user wants me to search the web for FIFA World Cup ticket prici...")))
    (Event
     (Tool_start (id toolu_01KGz7z4A7CtpkSnPyMCS7EP) (kind (Other ToolSearch))
      (native_name ToolSearch) (title ToolSearch)
      (input
       (Object ((query (String select:WebSearch)) (max_results (Number 1)))))
      (parent ())))
    (Event
     (Tool_end (id toolu_01KGz7z4A7CtpkSnPyMCS7EP) (ok true) (output "")
      (changes ()) (images ())))
    (Event
     (Thinking_done (item msg_01YTXwWhu5v3exh54x4Y9mgo:0)
      (text "Now let me search the web for FIFA World Cup ticket pricing.")))
    (Event
     (Tool_start (id toolu_011fKbh9ER69cDiCCpp93oRG) (kind Web_search)
      (native_name WebSearch)
      (title "WebSearch FIFA World Cup 2026 ticket pricing")
      (input (Object ((query (String "FIFA World Cup 2026 ticket pricing")))))
      (parent ())))
    (Event
     (Tool_end (id toolu_011fKbh9ER69cDiCCpp93oRG) (ok true)
      (output
        "Web search results for query: \"FIFA World Cup 2026 ticket pricing\"\
       \n...")
      (changes ()) (images ())))
    (Event
     (Text_done (item msg_01Cu2HWhZV3Mp5ukA4H97kP7:0)
      (text
        "web search fixture complete\
       \n\
       \nSources:\
       \n- [Tickets - FIFA World Cup 2...")))
    (Event
     (Usage
      ((usage ((input 37763) (output 471) (cache_read 24246)))
       (cost_usd (0.093582800000000008)) (context_tokens (13512))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}]
;;

let%expect_test "an approval the user denies" =
  let on_output t (output : C.Output.t) =
    match output with
    | Event (Approval_request { request_id; _ }) ->
      [ ok_exn (C.respond_approval t ~request_id (Deny (Some "not that file")))
      ]
    | _ -> []
  in
  ignore (replay ~on_output "denied_write" : C.t);
  [%expect
    {|
    (Event
     (Thread_started (thread_id d745e7a5-b8b2-4bc8-aa7e-5072f50380d6)
      (model (claude-sonnet-4-6))))
    (Event (Turn_started (turn_id turn-1)))
    (Thinking_delta msg_011Cf3qXskWdy2bUko985KjZ:0 "2 chunks" "")
    (Event (Thinking_done (item msg_011Cf3qXskWdy2bUko985KjZ:0) (text "")))
    (Event
     (Tool_start (id toolu_016StH6rgvnkDxU2MWt46gYE) (kind File_change)
      (native_name Write)
      (title
       "Write /tmp/claude-replay-tool_call_denied_write/.claude-denied-writ...")
      (input
       (Object
        ((file_path
          (String
           /tmp/claude-replay-tool_call_denied_write/.claude-denied-write.txt))
         (content (String "approval fixture")))))
      (parent ())))
    (Event
     (Approval_request (request_id cli-req-1)
      (tool_id (toolu_016StH6rgvnkDxU2MWt46gYE)) (kind File_change)
      (summary
       "Write /tmp/claude-replay-tool_call_denied_write/.claude-denied-writ...")
      (input
       (Object
        ((file_path
          (String
           /tmp/claude-replay-tool_call_denied_write/.claude-denied-write.txt))
         (content (String "approval fixture")))))
      (offers_always true)))
    <- {"type":"control_response","response":{"subtype":"success","request_id":"cli-req-1","response":{"behavior":"deny","message":"not that file"}}}
    (Event
     (Tool_end (id toolu_016StH6rgvnkDxU2MWt46gYE) (ok false)
      (output "User declined tool execution.") (changes ()) (images ())))
    (Text_delta msg_011Cf3qYNcr3jSaN9m685qT7:0 "2 chunks"
     "write permission denied")
    (Event
     (Text_done (item msg_011Cf3qYNcr3jSaN9m685qT7:0)
      (text "write permission denied")))
    (Event
     (Usage
      ((usage ((input 43461) (output 224) (cache_read 35908)))
       (cost_usd (0.060467400000000004)) (context_tokens (21867))
       (context_window (200000)))))
    (Event (Turn_end (turn_id (turn-1)) (stop Done)))
    |}]
;;

let init_line =
  {|{"type":"system","subtype":"init","session_id":"s-1","model":"claude-opus-4-7","tools":[],"mcp_servers":[],"permissionMode":"default","uuid":"u0"}|}
;;

let%expect_test "questions, plans, todos, always-allow and in-band MCP" =
  let t = C.create () in
  let on_output t (output : C.Output.t) =
    match output with
    | Event (User_question { request_id; _ }) ->
      [ ok_exn
          (C.answer t ~request_id (Answered [ [ "OCaml"; "Rust" ]; [ "Yes" ] ]))
      ]
    | Event (Approval_request { request_id; offers_always = true; _ }) ->
      [ ok_exn (C.respond_approval t ~request_id Allow_always) ]
    | Event (Approval_request { request_id; kind = Other "ExitPlanMode"; _ }) ->
      [ ok_exn (C.respond_approval t ~request_id Allow) ]
    | Mcp { request_id; message; _ } ->
      let id = Option.value_exn (Json.member "id" message) in
      [ C.mcp_reply
          ~request_id
          (Some
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "id", id
                 ; "result", `Object [ "content", `Array [] ]
                 ]))
      ]
    | _ -> []
  in
  feed
    ~on_output
    t
    [ init_line
    ; {|{"type":"assistant","parent_tool_use_id":null,"session_id":"s-1","message":{"id":"m1","role":"assistant","model":"claude-opus-4-7","content":[{"type":"tool_use","id":"tu-q","name":"AskUserQuestion","input":{"questions":[{"question":"Which languages?","header":"Langs","multiSelect":true,"options":[{"label":"OCaml","description":"the best"},{"label":"Rust"}]},{"question":"Tests too?","header":"Tests","multiSelect":false,"options":[{"label":"Yes"},{"label":"No"}]}]}}],"usage":{"input_tokens":10,"output_tokens":5}}}|}
    ; {|{"type":"control_request","request_id":"r-q","request":{"subtype":"can_use_tool","tool_name":"AskUserQuestion","tool_use_id":"tu-q","input":{"questions":[{"question":"Which languages?","header":"Langs","multiSelect":true,"options":[{"label":"OCaml","description":"the best"},{"label":"Rust"}]},{"question":"Tests too?","header":"Tests","multiSelect":false,"options":[{"label":"Yes"},{"label":"No"}]}]}}}|}
    ; {|{"type":"control_request","request_id":"r-p","request":{"subtype":"can_use_tool","tool_name":"ExitPlanMode","tool_use_id":"tu-p","input":{"plan":"1. Write the codec\n2. Test it"}}}|}
    ; {|{"type":"control_request","request_id":"r-b","request":{"subtype":"can_use_tool","tool_name":"Bash","tool_use_id":"tu-b","input":{"command":"dune build"},"permission_suggestions":[{"type":"addRules","rules":[{"toolName":"Bash","ruleContent":"dune build:*"}],"behavior":"allow","destination":"localSettings"}]}}|}
    ; {|{"type":"assistant","parent_tool_use_id":null,"session_id":"s-1","message":{"id":"m2","role":"assistant","model":"claude-opus-4-7","content":[{"type":"tool_use","id":"tu-t","name":"TodoWrite","input":{"todos":[{"content":"Write the codec","status":"completed","activeForm":"Writing"},{"content":"Test it","status":"in_progress","activeForm":"Testing"}]}},{"type":"tool_use","id":"tu-m","name":"mcp__prigh__render_ui","input":{"components":[]}}]}}|}
    ; {|{"type":"control_request","request_id":"r-m","request":{"subtype":"mcp_message","server_name":"prigh","message":{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"render_ui","arguments":{}}}}}|}
    ; {|{"type":"control_request","request_id":"r-h","request":{"subtype":"hook_callback","callback_id":"hook_0","input":{}}}|}
    ; {|{"type":"control_request","request_id":"r-x","request":{"subtype":"elicitation"}}|}
    ; {|not json|}
    ; {|{"type":"control_request","request_id":"r-c","request":{"subtype":"can_use_tool","tool_name":"Read","tool_use_id":"tu-r","input":{"file_path":"/etc/hosts"}}}|}
    ; {|{"type":"control_cancel_request","request_id":"r-c"}|}
    ; {|{"type":"some_future_message","x":1}|}
    ];
  [%expect
    {|
    (Event (Thread_started (thread_id s-1) (model (claude-opus-4-7))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Tool_start (id tu-q) (kind (Other AskUserQuestion))
      (native_name AskUserQuestion) (title AskUserQuestion)
      (input
       (Object
        ((questions
          (Array
           ((Object
             ((question (String "Which languages?")) (header (String Langs))
              (multiSelect True)
              (options
               (Array
                ((Object
                  ((label (String OCaml)) (description (String "the best"))))
                 (Object ((label (String Rust)))))))))
            (Object
             ((question (String "Tests too?")) (header (String Tests))
              (multiSelect False)
              (options
               (Array
                ((Object ((label (String Yes)))) (Object ((label (String No)))))))))))))))
      (parent ())))
    (Event
     (User_question (request_id r-q)
      (questions
       (((question "Which languages?") (header (Langs))
         (choices
          (((label OCaml) (description ("the best")))
           ((label Rust) (description ()))))
         (multiple true))
        ((question "Tests too?") (header (Tests))
         (choices (((label Yes) (description ())) ((label No) (description ()))))
         (multiple false))))))
    <- {"type":"control_response","response":{"subtype":"success","request_id":"r-q","response":{"behavior":"allow","updatedInput":{"questions":[{"question":"Which languages?","header":"Langs","multiSelect":true,"options":[{"label":"OCaml","description":"the best"},{"label":"Rust"}]},{"question":"Tests too?","header":"Tests","multiSelect":false,"options":[{"label":"Yes"},{"label":"No"}]}],"answers":{"Which languages?":"OCaml, Rust","Tests too?":"Yes"}}}}}
    (Event
     (Approval_request (request_id r-p) (tool_id (tu-p))
      (kind (Other ExitPlanMode)) (summary  "1. Write the codec\
                                           \n2. Test it")
      (input (Object ((plan (String  "1. Write the codec\
                                    \n2. Test it")))))
      (offers_always false)))
    <- {"type":"control_response","response":{"subtype":"success","request_id":"r-p","response":{"behavior":"allow","updatedInput":{"plan":"1. Write the codec\n2. Test it"}}}}
    (Event
     (Approval_request (request_id r-b) (tool_id (tu-b)) (kind Command)
      (summary "Bash: dune build")
      (input (Object ((command (String "dune build"))))) (offers_always true)))
    <- {"type":"control_response","response":{"subtype":"success","request_id":"r-b","response":{"behavior":"allow","updatedInput":{"command":"dune build"},"updatedPermissions":[{"type":"addRules","rules":[{"toolName":"Bash","ruleContent":"dune build:*"}],"behavior":"allow","destination":"session"}]}}}
    (Event
     (Tool_start (id tu-t) (kind Todo) (native_name TodoWrite)
      (title "Update todos")
      (input
       (Object
        ((todos
          (Array
           ((Object
             ((content (String "Write the codec")) (status (String completed))
              (activeForm (String Writing))))
            (Object
             ((content (String "Test it")) (status (String in_progress))
              (activeForm (String Testing))))))))))
      (parent ())))
    (Event
     (Plan
      ((explanation ())
       (steps
        (((step "Write the codec") (status Completed))
         ((step "Test it") (status In_progress)))))))
    (Event
     (Tool_start (id tu-m) (kind (Mcp (server prigh) (tool render_ui)))
      (native_name mcp__prigh__render_ui) (title "prigh: render_ui")
      (input (Object ((components (Array ()))))) (parent ())))
    (Mcp
     ((request_id r-m) (server prigh)
      (message
       (Object
        ((jsonrpc (String 2.0)) (id (Number 7)) (method (String tools/call))
         (params (Object ((name (String render_ui)) (arguments (Object ()))))))))))
    <- {"type":"control_response","response":{"subtype":"success","request_id":"r-m","response":{"mcp_response":{"jsonrpc":"2.0","id":7,"result":{"content":[]}}}}}
    (Send
     "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request...")
    (Send
     "{\"type\":\"control_response\",\"response\":{\"subtype\":\"error\",\"request_i...")
    (Event (Notice "Claude Code wrote a line prigh could not read: not json"))
    (Event
     (Approval_request (request_id r-c) (tool_id (tu-r)) (kind Read)
      (summary "Read /etc/hosts")
      (input (Object ((file_path (String /etc/hosts))))) (offers_always false)))
    (Event (Request_withdrawn (request_id r-c)))
    |}];
  (* Answering what was already answered or withdrawn says so. *)
  print_s
    [%sexp (C.respond_approval t ~request_id:"r-c" Allow : string Or_error.t)];
  [%expect {| (Error "Claude Code is not waiting for an answer to r-c") |}]
;;

let%expect_test
    "interrupt: the result after it is Interrupted, open calls settle"
  =
  let t = C.create () in
  feed
    t
    [ init_line
    ; {|{"type":"assistant","parent_tool_use_id":null,"session_id":"s-1","message":{"id":"m1","role":"assistant","model":"claude-opus-4-7","content":[{"type":"tool_use","id":"tu-1","name":"Bash","input":{"command":"sleep 30"}}]}}|}
    ];
  print_endline (C.interrupt t);
  feed
    t
    [ {|{"type":"control_response","response":{"subtype":"success","request_id":"req_1_prigh","response":{}}}|}
    ; {|{"type":"result","subtype":"error_during_execution","is_error":true,"terminal_reason":"aborted_tools","session_id":"s-1","total_cost_usd":0.02,"usage":{"input_tokens":100,"output_tokens":3},"modelUsage":{"claude-opus-4-7":{"contextWindow":1000000}}}|}
    ];
  [%expect
    {|
    (Event (Thread_started (thread_id s-1) (model (claude-opus-4-7))))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Tool_start (id tu-1) (kind Command) (native_name Bash)
      (title "Bash: sleep 30") (input (Object ((command (String "sleep 30")))))
      (parent ())))
    {"type":"control_request","request_id":"req_1_prigh","request":{"subtype":"interrupt"}}
    (Response (Done Interrupt))
    (Event
     (Usage
      ((usage ((input 100) (output 3) (cache_read 0))) (cost_usd (0.02))
       (context_tokens ()) (context_window (1000000)))))
    (Event
     (Tool_end (id tu-1) (ok false)
      (output "The turn ended before this call finished.") (changes ())
      (images ())))
    (Event (Turn_end (turn_id (turn-1)) (stop Interrupted)))
    |}];
  (* A crash mid-turn. *)
  feed
    t
    [ {|{"type":"assistant","parent_tool_use_id":null,"session_id":"s-1","message":{"id":"m2","role":"assistant","model":"claude-opus-4-7","content":[{"type":"tool_use","id":"tu-2","name":"Edit","input":{"file_path":"a.ml","old_string":"x","new_string":"y"}}]}}|}
    ; {|{"type":"control_request","request_id":"r-e","request":{"subtype":"can_use_tool","tool_name":"Edit","tool_use_id":"tu-2","input":{"file_path":"a.ml","old_string":"x","new_string":"y"}}}|}
    ];
  print_outputs (C.process_exited t ~code:(Some 137) ~stderr_tail:"Killed");
  print_s [%sexp (C.in_turn t : bool)];
  [%expect
    {|
    (Event (Turn_started (turn_id turn-2)))
    (Event
     (Tool_start (id tu-2) (kind File_change) (native_name Edit)
      (title "Edit a.ml")
      (input
       (Object
        ((file_path (String a.ml)) (old_string (String x))
         (new_string (String y)))))
      (parent ())))
    (Event
     (Approval_request (request_id r-e) (tool_id (tu-2)) (kind File_change)
      (summary "Edit a.ml")
      (input
       (Object
        ((file_path (String a.ml)) (old_string (String x))
         (new_string (String y)))))
      (offers_always false)))
    (Event
     (Tool_end (id tu-2) (ok false)
      (output "The turn ended before this call finished.") (changes ())
      (images ())))
    (Event (Request_withdrawn (request_id r-e)))
    (Event
     (Turn_end (turn_id (turn-2))
      (stop
       (Failed (kind (Crashed (stderr_tail Killed)))
        (message "Claude Code exited (code 137)")))))
    false
    |}]
;;

let%expect_test "usage limit from rate_limit_event, with its reset time" =
  let t = C.create () in
  feed
    t
    [ init_line
    ; {|{"type":"rate_limit_event","rate_limit_info":{"status":"allowed_warning","rateLimitType":"five_hour","utilization":0.9,"resetsAt":1764000000},"session_id":"s-1"}|}
    ; {|{"type":"rate_limit_event","rate_limit_info":{"status":"allowed_warning","rateLimitType":"five_hour","utilization":0.95,"resetsAt":1764000000},"session_id":"s-1"}|}
    ; {|{"type":"rate_limit_event","rate_limit_info":{"status":"rejected","rateLimitType":"five_hour","resetsAt":1764000000},"session_id":"s-1"}|}
    ; {|{"type":"assistant","parent_tool_use_id":null,"error":"rate_limit","session_id":"s-1","message":{"id":"m1","role":"assistant","model":"<synthetic>","content":[{"type":"text","text":"You've hit your limit"}]}}|}
    ; {|{"type":"result","subtype":"success","is_error":true,"result":"You've hit your limit · resets 3am","session_id":"s-1","total_cost_usd":0,"usage":{"input_tokens":0,"output_tokens":0}}|}
      (* The legacy form carries its reset time in the text. *)
    ; {|{"type":"result","subtype":"success","is_error":true,"result":"Claude AI usage limit reached|1764003600","session_id":"s-1","usage":{"input_tokens":0,"output_tokens":0}}|}
    ];
  [%expect
    {|
    (Event (Thread_started (thread_id s-1) (model (claude-opus-4-7))))
    (Event (Notice "Claude Code has used 90% of its five_hour limit."))
    (Event (Turn_started (turn_id turn-1)))
    (Event
     (Turn_end (turn_id (turn-1))
      (stop
       (Failed (kind (Usage_limit (resets_at (1764000000))))
        (message "You've hit your limit \194\183 resets 3am")))))
    (Event (Turn_started (turn_id turn-2)))
    (Event
     (Turn_end (turn_id (turn-2))
      (stop
       (Failed (kind (Usage_limit (resets_at (1764003600))))
        (message "Claude AI usage limit reached|1764003600")))))
    |}]
;;

let%expect_test "edits: diffs from Claude Code's patch, else from the input" =
  let changes name input result =
    print_s
      [%sexp
        (Claude_tool.changes
           ~name
           ~input:(ok_exn (Json.parse input))
           ~result:(Option.map result ~f:(fun r -> ok_exn (Json.parse r)))
         : Adapter_event.Change.t list)]
  in
  changes
    "Edit"
    {|{"file_path":"src/a.ml","old_string":"let x = 1","new_string":"let x = 2"}|}
    (Some
       {|{"filePath":"src/a.ml","structuredPatch":[{"oldStart":3,"oldLines":3,"newStart":3,"newLines":3,"lines":[" a","-let x = 1","+let x = 2"," b"]}]}|});
  changes
    "Edit"
    {|{"file_path":"src/a.ml","old_string":"let x = 1\n","new_string":"let x = 2\n"}|}
    None;
  changes
    "Write"
    {|{"file_path":"new.txt","content":"hello\n"}|}
    (Some
       {|{"type":"create","filePath":"new.txt","content":"hello\n","structuredPatch":[]}|});
  changes
    "MultiEdit"
    {|{"file_path":"b.ml","edits":[{"old_string":"a\n","new_string":"b\n"},{"old_string":"c\n","new_string":"d\n"}]}|}
    None;
  changes "Bash" {|{"command":"ls"}|} None;
  [%expect
    {|
    (((path src/a.ml) (kind (Update (move_to ())))
      (diff
        "--- a/src/a.ml\
       \n+++ b/src/a.ml\
       \n@@ -3,3 +3,3 @@\
       \n a\
       \n-let x = 1\
       \n+let x = 2\
       \n b\
       \n")))
    (((path src/a.ml) (kind (Update (move_to ())))
      (diff
        "diff --git a/src/a.ml b/src/a.ml\
       \n--- a/src/a.ml\
       \n+++ b/src/a.ml\
       \n@@ -1,1 +1,1 @@\
       \n-let x = 1\
       \n+let x = 2\
       \n")))
    (((path new.txt) (kind Add)
      (diff
        "diff --git a/new.txt b/new.txt\
       \n--- a/new.txt\
       \n+++ b/new.txt\
       \n@@ -0,0 +1,1 @@\
       \n+hello\
       \n")))
    (((path b.ml) (kind (Update (move_to ())))
      (diff
        "diff --git a/b.ml b/b.ml\
       \n--- a/b.ml\
       \n+++ b/b.ml\
       \n@@ -1,1 +1,1 @@\
       \n-a\
       \n+b\
       \ndiff --git a/b.ml b/b.ml\
       \n--- a/b.ml\
       \n+++ b/b.ml\
       \n@@ -1,1 +1,1 @@\
       \n-c\
       \n+d\
       \n")))
    ()
    |}];
  List.iter
    [ "Bash"
    ; "Grep"
    ; "Glob"
    ; "Task"
    ; "Agent"
    ; "mcp__github__create_pr"
    ; "mcp__odd"
    ; "Skill"
    ]
    ~f:(fun name ->
      print_s
        [%sexp
          (name : string), (Claude_tool.kind name : Adapter_event.Tool_kind.t)]);
  [%expect
    {|
    (Bash Command)
    (Grep Search)
    (Glob Search)
    (Task Subagent)
    (Agent Subagent)
    (mcp__github__create_pr (Mcp (server github) (tool create_pr)))
    (mcp__odd (Other mcp__odd))
    (Skill (Other Skill))
    |}]
;;

let%expect_test "health" =
  let version s =
    print_s [%sexp (Claude_health.parse_version s : string option)]
  in
  version "2.1.330 (Claude Code)\n";
  version "claude: command not found";
  [%expect
    {|
    (2.1.330)
    ()
    |}];
  let auth stdout exit_code =
    print_s
      [%sexp
        (Claude_health.parse_auth_status ~stdout ~exit_code
         : Claude_health.Auth.t)]
  in
  auth {|{"loggedIn":true,"authMethod":"claude.ai","email":"l@example.com"}|} 0;
  auth {|{"loggedIn":false,"authMethod":"none"}|} 1;
  auth {|{"authMethod":"api_key"}|} 0;
  auth "Not logged in" 1;
  auth "Segmentation fault" 139;
  [%expect
    {|
    (Logged_in (method_ (claude.ai)) (email (l@example.com)))
    Logged_out
    (Logged_in (method_ (api_key)) (email ()))
    Logged_out
    (Unknown "Segmentation fault")
    |}];
  List.iter
    [ None, Claude_health.Auth.Logged_out
    ; Some "2.1.330", Logged_out
    ; Some "2.1.330", Logged_in { method_ = None; email = None }
    ]
    ~f:(fun (version, auth) ->
      print_s
        [%sexp
          (Claude_health.problem ~host:"lloyd-desktop" ~version ~auth
           : string option)]);
  [%expect
    {|
    ("Claude Code is not installed on lloyd-desktop: install it there (e.g. `npm install -g @anthropic-ai/claude-code`), then try again.")
    ("Claude Code is not logged in on lloyd-desktop: run `claude auth login` there, then try again.")
    ()
    |}]
;;
