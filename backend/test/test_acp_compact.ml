open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Compacting an ACP agent's session: through its [/compact] when it lists
   one, else by prigh's summary handed to a new thread; on request and when
   its context passes the threshold. And how full its context is shown. *)

module P = Test_acp_adapter.P

(* A scripted agent: each [session/new] is a new thread ([s1], [s2]); a
   turn's first reply is long (so that there is something to compact) and
   each turn reports its context as [used turn] of 10k tokens. *)
let agent ~commands ~used ~threads ~turns (p : Line_transport.Peer.t) =
  let rec loop () =
    match P.read p with
    | None -> ()
    | Some j ->
      let params = Option.value (Json.member "params" j) ~default:`Null in
      (match P.str "method" j with
       | Some "initialize" ->
         P.result
           p
           j
           {|{"protocolVersion":1,"agentCapabilities":{"loadSession":false}}|}
       | Some "session/new" ->
         incr threads;
         let session = sprintf "s%d" !threads in
         printf "agent: new thread %s\n" session;
         P.result p j (sprintf {|{"sessionId":%S}|} session);
         if commands
         then
           P.update
             p
             session
             {|{"sessionUpdate":"available_commands_update","availableCommands":[{"name":"compact","description":"Compact the conversation now"}]}|}
       | Some "session/prompt" ->
         let session = Option.value (P.str "sessionId" params) ~default:"" in
         let text =
           match Json.member "prompt" params with
           | Some (`Array (first :: _)) ->
             Option.value (P.str "text" first) ~default:""
           | _ -> ""
         in
         printf
           "agent: prompt on %s: %S%s\n"
           session
           (List.last_exn (String.split_lines text))
           (if String.is_substring text ~substring:"SUMMARY-OF-EARLIER"
            then " (handed the summary)"
            else "");
         if String.equal text "/compact"
         then P.text p session "Compacted."
         else (
           incr turns;
           P.text
             p
             session
             (if !turns = 1 then String.make 40_000 'x' else "Done.");
           if used !turns > 0
           then
             P.update
               p
               session
               (sprintf
                  {|{"sessionUpdate":"usage_update","used":%d,"size":10000}|}
                  (used !turns)));
         P.result p j {|{"stopReason":"end_turn"}|}
       | Some _ when Option.is_some (Json.member "id" j) -> P.result p j "{}"
       | Some _ | None -> ());
      loop ()
  in
  loop ()
;;

let print_event (e : Agent.Event.t) =
  match e with
  | Loop (Message_end (Assistant a)) ->
    let text = Message.Assistant.text a in
    printf
      "assistant: %S\n"
      (if String.length text > 20 then String.prefix text 5 ^ "..." else text)
  | Compacted { summary } -> printf "compacted: %S\n" summary
  | Notice n -> printf "notice: %s\n" n
  | _ -> ()
;;

let with_agent ~commands ~used f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let acp = Option.value_exn (Acp_agent.find_known "opencode") in
  let threads = ref 0 in
  let turns = ref 0 in
  let composer =
    Composer_runner.create
      ~sw
      ~adapter:(Acp_adapter.create ~load_quiet:(Time_float.Span.of_sec 0.2) acp)
      ~spawn:(fun (_ : Adapter.Launch.t) ->
        Ok
          (Line_transport.Peer.spawn
             ~sw
             (agent ~commands ~used ~threads ~turns)))
      ~interrupt_grace:(Time_float.Span.of_sec 0.5)
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create [ Faux_provider.Reply.text "SUMMARY-OF-EARLIER" ])
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~runner:(Composer_runner.runner composer)
      ~cwd:t.dir
      ()
  in
  Agent.set_engine agent { engine = Composer; adapter = Some (Acp acp.name) };
  Agent.set_model agent (External_model.pending ~adapter:"acp:opencode");
  Agent.subscribe agent ~f:print_event;
  f agent;
  Composer_runner.close composer
;;

let prompt agent text =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let show agent =
  let session = Agent.session agent in
  let state = Agent.state agent in
  printf
    "context %d of %d; compactions %d\n"
    state.context_tokens
    state.model.context_window
    (List.count (Session.active_path session) ~f:(fun e ->
       match e.payload with
       | Compaction _ -> true
       | _ -> false))
;;

let%expect_test
    "without a compact command: /compact summarises into a new thread"
  =
  with_agent ~commands:false ~used:(fun _ -> 1000)
  @@ fun agent ->
  prompt agent "first";
  prompt agent "second";
  show agent;
  [%expect
    {|
    agent: new thread s1
    agent: prompt on s1: "first"
    assistant: "xxxxx..."
    agent: prompt on s1: "second"
    assistant: "Done."
    context 1000 of 10000; compactions 0
    |}];
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  prompt agent "third";
  show agent;
  [%expect
    {|
    compacted: "SUMMARY-OF-EARLIER"
    (Ok SUMMARY-OF-EARLIER)
    agent: new thread s2
    agent: prompt on s2: "third" (handed the summary)
    assistant: "Done."
    context 1000 of 10000; compactions 1
    |}]
;;

let%expect_test
    "without a compact command: a full context is summarised between turns"
  =
  with_agent ~commands:false ~used:(fun turn -> if turn = 2 then 9000 else 1000)
  @@ fun agent ->
  prompt agent "first";
  prompt agent "second";
  prompt agent "third";
  show agent;
  [%expect
    {|
    agent: new thread s1
    agent: prompt on s1: "first"
    assistant: "xxxxx..."
    agent: prompt on s1: "second"
    assistant: "Done."
    compacted: "SUMMARY-OF-EARLIER"
    agent: new thread s2
    agent: prompt on s2: "third" (handed the summary)
    assistant: "Done."
    context 1000 of 10000; compactions 1
    |}]
;;

let%expect_test "with a compact command: /compact and a full context send it" =
  with_agent ~commands:true ~used:(fun turn -> if turn = 2 then 9000 else 1000)
  @@ fun agent ->
  prompt agent "first";
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  prompt agent "second";
  prompt agent "third";
  show agent;
  [%expect
    {|
    agent: new thread s1
    agent: prompt on s1: "first"
    assistant: "xxxxx..."
    agent: prompt on s1: "/compact"
    notice: OpenCode compacted its context
    assistant: "Compacted."
    (Ok "")
    agent: prompt on s1: "second"
    assistant: "Done."
    agent: prompt on s1: "/compact"
    notice: OpenCode compacted its context
    assistant: "Compacted."
    agent: prompt on s1: "third"
    assistant: "Done."
    context 1000 of 10000; compactions 0
    |}]
;;

let%expect_test "an agent that does not say how full its context is" =
  with_agent ~commands:false ~used:(fun _ -> 0)
  @@ fun agent ->
  prompt agent "first";
  prompt agent "second";
  (* The session's estimate, of no known window: not compacted. *)
  show agent;
  [%expect
    {|
    agent: new thread s1
    agent: prompt on s1: "first"
    assistant: "xxxxx..."
    agent: prompt on s1: "second"
    assistant: "Done."
    context 10037 of 0; compactions 0
    |}]
;;

let%expect_test "context windows of external models" =
  List.iter
    [ "acp:antigravity", "gemini-3.1-pro", Some "Gemini 3.1 Pro (High)"
    ; ( "acp:antigravity"
      , "claude-sonnet-4-6"
      , Some "Claude Sonnet 4.6 (Thinking)" )
    ; "acp:antigravity", "gpt-oss-120b", Some "GPT-OSS 120B (Medium)"
    ; "acp:antigravity", "default", None
    ; "claude", "opus[1m]", Some "Opus (1M context)"
    ; "codex", "gpt-5", None
    ]
    ~f:(fun (adapter, id, name) ->
      printf
        "%s/%s: %d\n"
        adapter
        id
        (External_model.model ~adapter ?name id).context_window);
  [%expect
    {|
    acp:antigravity/gemini-3.1-pro: 1048576
    acp:antigravity/claude-sonnet-4-6: 200000
    acp:antigravity/gpt-oss-120b: 131072
    acp:antigravity/default: 0
    claude/opus[1m]: 1000000
    codex/gpt-5: 200000
    |}]
;;
