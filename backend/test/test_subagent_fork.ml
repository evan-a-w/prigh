(* [subagent]'s [fork]: the subagent starts from its caller's conversation,
   with a result for every pending call and its framed task, and (with its
   caller's model and all its tools) the caller's system prompt and tool
   specs, so the provider's prompt cache covers the shared prefix. *)

open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let one_line s =
  String.substr_replace_all (String.strip s) ~pattern:"\n" ~with_:" | "
;;

let message_line (m : Message.t) =
  match m with
  | User { text; _ } -> "user: " ^ one_line text
  | Assistant a ->
    let calls =
      List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
        sprintf " %s %s%s" c.id c.name c.arguments)
    in
    "assistant: " ^ one_line (Message.Assistant.text a) ^ String.concat calls
  | Tool_result r ->
    sprintf "tool_result %s %s: %s" r.tool_call_id r.tool_name (one_line r.text)
;;

let tool_names (r : Provider.Request.t) = List.map r.tools ~f:(fun s -> s.name)

(* A request compared with its caller's last one: what a prompt cache
   needs to be the same. *)
let print_request ~(caller : Provider.Request.t) (r : Provider.Request.t) =
  let same = function
    | true -> "the caller's"
    | false -> "its own"
  in
  printf
    "system: %s%s\n"
    (same ([%equal: string option] r.system caller.system))
    (match r.system with
     | Some s when String.is_prefix s ~prefix:"You are a subagent" ->
       " (You are a subagent...)"
     | Some _ | None -> "");
  printf
    "tools: %s%s\n"
    (same (List.equal (fun a b -> Poly.equal a b) r.tools caller.tools))
    (sprintf " (%s)" (String.concat ~sep:" " (tool_names r)));
  printf
    "cache_key: %s\n"
    (same ([%equal: string option] r.cache_key caller.cache_key));
  printf "model: %s, thinking: %s\n" r.model.id (Thinking.to_string r.thinking);
  (* The shared prefix: the caller's last request's messages. *)
  let shared =
    List.is_prefix r.messages ~prefix:caller.messages ~equal:Message.equal
  in
  printf "starts with the caller's last request's messages: %b\n" shared;
  List.iter r.messages ~f:(fun m -> print_endline (message_line m))
;;

let subagent_tools ~provider =
  Tool_subagent.create
    ~provider
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
    ()
  :: Tool_subagent.control_tools
;;

let with_agent ?(extra_tools = []) ~main children f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key r -> Queue.enqueue requests (key, r))
      ~main
      children
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ subagent_tools ~provider @ extra_tools)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(fun (event : Agent.Event.t) ->
    match event with
    | Loop (Subagent_start { agent_id; task; forked; tools; _ }) ->
      printf
        "subagent_start %s%s: %s (%d tools)\n"
        agent_id
        (if forked then " (forked)" else "")
        task
        (List.length tools)
    | Loop (Subagent { event = Tool_end { call; result }; agent_id; _ }) ->
      printf
        "  %s: %s -> %s%s\n"
        agent_id
        call.name
        (if result.is_error then "error: " else "")
        (one_line result.text)
    | Loop (Subagent_end { agent_id; result; turns; _ }) ->
      printf
        "subagent_end %s (%d turns): %s\n"
        agent_id
        turns
        (one_line result.text)
    | _ -> ());
  f t agent requests
;;

(* The requests for [key], and the main agent's last before the first of
   them. *)
let requests_of requests key =
  let all = Queue.to_list requests in
  let caller =
    List.take_while all ~f:(fun (k, _) -> not (String.equal k key))
    |> List.filter ~f:(fun (k, _) -> String.equal k "main")
    |> List.last_exn
    |> snd
  in
  ( caller
  , List.filter_map all ~f:(fun (k, r) -> Option.some_if (String.equal k key) r)
  )
;;

let run agent =
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Agent.wait_idle agent;
    Eio.Fiber.yield ()
  done
;;

let%expect_test
    "a fork starts from its caller's conversation, with a result for every \
     pending call"
  =
  with_agent
    ~main:
      [ Reply.tool_call
          ~id:"r1"
          ~name:"read"
          ~arguments:{|{"path":"notes.txt"}|}
          ()
      ; Reply.tool_calls
          ~text:"I know enough: forking."
          [ "r2", "read", {|{"path":"other.txt"}|}
          ; "c1", "subagent", {|{"task":"implement it","fork":true}|}
          ]
      ; Reply.text "started the fork"
      ; Reply.text "thanks"
      ]
    [ "implement it", [ Reply.text "implemented" ] ]
  @@ fun t agent requests ->
  Out_channel.write_all (Filename.concat t.dir "notes.txt") ~data:"the plan";
  Out_channel.write_all (Filename.concat t.dir "other.txt") ~data:"other";
  run agent;
  [%expect
    {|
    subagent_start a1 (forked): implement it (13 tools)
    subagent_end a1 (1 turns): implemented | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    |}];
  let caller, fork = requests_of requests "implement it" in
  List.iter fork ~f:(print_request ~caller);
  [%expect
    {|
    system: the caller's
    tools: the caller's (bash read write edit ls grep find web_fetch subagent subagent_status subagent_cancel subagent_message subagent_continue)
    cache_key: the caller's
    model: deepseek-flash, thinking: on
    starts with the caller's last request's messages: true
    user: go
    assistant:  r1 read{"path":"notes.txt"}
    tool_result r1 read: the plan
    assistant: I know enough: forking. r2 read{"path":"other.txt"} c1 subagent{"task":"implement it","fork":true}
    tool_result r2 read: [not run here: your caller runs this call and gets its result; you, its fork, do not see it]
    tool_result c1 subagent: This call started you: you are the subagent it forked from this conversation. Your task follows.
    user: [You are now a subagent forked from the conversation above. Your caller carries on separately: you do not see what it does next, and it sees only your final report. Do only the task below (not the rest of the conversation's work), then end with a concise report of what you found or changed and anything your caller must know.] |  | You have your caller's tools, but not all of them are yours to use: subagent_status, subagent_cancel, subagent_message refuse. |  | Your own subagents run synchronously: subagent blocks until its subagent finishes and returns its report, and they cannot delegate further. |  | Your task: | implement it
    |}];
  (* The caller's own context is unchanged: its calls got their real
     results. *)
  List.iter (Agent.messages agent) ~f:(fun m -> print_endline (message_line m));
  [%expect
    {|
    user: go
    assistant:  r1 read{"path":"notes.txt"}
    tool_result r1 read: the plan
    assistant: I know enough: forking. r2 read{"path":"other.txt"} c1 subagent{"task":"implement it","fork":true}
    tool_result r2 read: other
    tool_result c1 subagent: started agent a1 (forked: implement it); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: started the fork
    user: [subagent a1 finished] implement it | implemented | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    assistant: thanks
    |}]
;;

let%expect_test
    "a fork sees its caller's tools; those it may not use refuse, saying why"
  =
  with_agent
    ~extra_tools:Tools.interaction
    ~main:
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"try everything","fork":true}|}
          ()
      ; Reply.text "started"
      ; Reply.text "thanks"
      ]
    [ ( "try everything"
      , [ Reply.tool_calls
            [ "s1", "subagent_status", "{}"
            ; "s2", "ask_user", {|{"question":"ok?"}|}
            ; "s3", "subagent", {|{"task":"inner"}|}
            ]
        ; Reply.text "tried"
        ] )
    ; "inner", [ Reply.text "inner done" ]
    ]
  @@ fun _t agent requests ->
  run agent;
  [%expect
    {|
    subagent_start a1 (forked): try everything (15 tools)
      a1: subagent_status -> error: subagent_status is not available to you: you are a subagent, and background subagents are the main agent's (yours run synchronously: subagent returns their report). You see it because you were forked from your caller's conversation, with its tools.
      a1: ask_user -> error: ask_user is not available to you: it is the main agent's (the user's plan, questions and goal), and you are a subagent: put what you need from the user in your report. You see it because you were forked from your caller's conversation, with its tools.
      a1: subagent -> inner done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    subagent_end a1 (2 turns): tried | [subagent: 2 turns, 40 in / 18 out tokens, $0.0000]
    |}];
  (* The synchronous grandchild is not a fork: its own prompt and the tools
     of depth 2. *)
  let caller, inner = requests_of requests "inner" in
  List.iter inner ~f:(print_request ~caller);
  [%expect
    {|
    system: its own (You are a subagent...)
    tools: its own (bash read write edit ls grep find web_fetch)
    cache_key: its own
    model: deepseek-flash, thinking: off
    starts with the caller's last request's messages: false
    user: inner
    |}]
;;

let%expect_test
    "with another model or a tools subset, a fork has its own system prompt \
     and tools"
  =
  with_agent
    ~main:
      [ Reply.tool_calls
          [ ( "c1"
            , "subagent"
            , {|{"task":"other model","fork":true,"model":"openai/gpt-5","thinking":"high"}|}
            )
          ; ( "c2"
            , "subagent"
            , {|{"task":"subset","fork":true,"tools":["read","subagent"],"context":"the plan is X"}|}
            )
          ]
      ; Reply.text "started"
      ; Reply.text "one"
      ; Reply.text "two"
      ]
    [ "other model", [ Reply.text "done" ]; "subset", [ Reply.text "done" ] ]
  @@ fun _t agent requests ->
  run agent;
  let caller, other = requests_of requests "other model" in
  List.iter other ~f:(print_request ~caller);
  let caller, subset = requests_of requests "subset" in
  List.iter subset ~f:(print_request ~caller);
  [%expect
    {|
    subagent_start a1 (forked): other model (10 tools)
    subagent_start a2 (forked): subset (2 tools)
    subagent_end a1 (1 turns): done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0001]
    subagent_end a2 (1 turns): done | [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    system: its own (You are a subagent...)
    tools: its own (bash read write apply_patch ls grep find web_fetch subagent subagent_continue)
    cache_key: its own
    model: gpt-5, thinking: high
    starts with the caller's last request's messages: true
    user: go
    assistant:  c1 subagent{"task":"other model","fork":true,"model":"openai/gpt-5","thinking":"high"} c2 subagent{"task":"subset","fork":true,"tools":["read","subagent"],"context":"the plan is X"}
    tool_result c1 subagent: This call started you: you are the subagent it forked from this conversation. Your task follows.
    tool_result c2 subagent: [not run here: your caller runs this call and gets its result; you, its fork, do not see it]
    user: [You are now a subagent forked from the conversation above. Your caller carries on separately: you do not see what it does next, and it sees only your final report. Do only the task below (not the rest of the conversation's work), then end with a concise report of what you found or changed and anything your caller must know.] |  | Your task: | other model
    system: its own (You are a subagent...)
    tools: its own (read subagent)
    cache_key: its own
    model: deepseek-flash, thinking: on
    starts with the caller's last request's messages: true
    user: go
    assistant:  c1 subagent{"task":"other model","fork":true,"model":"openai/gpt-5","thinking":"high"} c2 subagent{"task":"subset","fork":true,"tools":["read","subagent"],"context":"the plan is X"}
    tool_result c1 subagent: [not run here: your caller runs this call and gets its result; you, its fork, do not see it]
    tool_result c2 subagent: This call started you: you are the subagent it forked from this conversation. Your task follows.
    user: [You are now a subagent forked from the conversation above. Your caller carries on separately: you do not see what it does next, and it sees only your final report. Do only the task below (not the rest of the conversation's work), then end with a concise report of what you found or changed and anything your caller must know.] |  | Context: | the plan is X |  | Your task: | subset
    |}]
;;

let%expect_test "fork needs a conversation to fork" =
  with_sandbox
  @@ fun t ->
  let provider = Faux_provider.create [] in
  let subagent = List.hd_exn (subagent_tools ~provider) in
  let context =
    Tool.Context.create ~env:t.env ~cwd:t.dir ~tools:[ subagent ] ()
  in
  let result =
    Tool.execute
      subagent
      context
      (Jsonaf.of_string {|{"task":"x","fork":true}|})
  in
  print_endline result.text;
  [%expect
    {| invalid arguments: fork is not available here (there is no conversation to fork): call subagent without it, with a self-contained task |}]
;;

let%expect_test
    "a subagent's fork: depth 2 refuses subagent; the sync caller waits for it"
  =
  with_agent
    ~main:
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"outer"}|}
          ()
      ; Reply.text "started"
      ; Reply.text "thanks"
      ]
    [ ( "outer"
      , [ Reply.tool_call
            ~id:"c2"
            ~name:"subagent"
            ~arguments:{|{"task":"forked inner","fork":true}|}
            ()
        ; Reply.text "outer done"
        ] )
    ; ( "forked inner"
      , [ Reply.tool_call
            ~id:"c3"
            ~name:"subagent"
            ~arguments:{|{"task":"deeper"}|}
            ()
        ; Reply.text "inner done"
        ] )
    ]
  @@ fun _t agent requests ->
  run agent;
  [%expect
    {|
    subagent_start a1: outer (10 tools)
      a1: subagent -> inner done | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    subagent_end a1 (2 turns): outer done | [subagent: 2 turns, 60 in / 26 out tokens, $0.0000]
    |}];
  let caller, inner = requests_of requests "forked inner" in
  ignore caller;
  (* Its caller is the subagent [a1], whose requests are keyed "outer". *)
  let _, outer = requests_of requests "outer" in
  let caller = List.hd_exn outer in
  print_request ~caller (List.hd_exn inner);
  print_endline (message_line (List.last_exn (List.last_exn inner).messages));
  [%expect
    {|
    system: the caller's (You are a subagent...)
    tools: the caller's (bash read write edit ls grep find web_fetch subagent subagent_continue)
    cache_key: the caller's
    model: deepseek-flash, thinking: off
    starts with the caller's last request's messages: true
    user: outer
    assistant:  c2 subagent{"task":"forked inner","fork":true}
    tool_result c2 subagent: This call started you: you are the subagent it forked from this conversation. Your task follows.
    user: [You are now a subagent forked from the conversation above. Your caller carries on separately: you do not see what it does next, and it sees only your final report. Do only the task below (not the rest of the conversation's work), then end with a concise report of what you found or changed and anything your caller must know.] |  | You have your caller's tools, but not all of them are yours to use: subagent, subagent_continue refuse. |  | Your task: | forked inner
    tool_result c3 subagent: subagent is not available to you: you are a subagent's subagent, which cannot delegate further: do the work yourself. You see it because you were forked from your caller's conversation, with its tools.
    |}]
;;
