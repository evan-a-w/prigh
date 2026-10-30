open! Core
open! Expect_test_helpers_core
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let start ~continued =
  P.Event.Subagent_start
    { call_id = "c1"
    ; agent_id = "a1"
    ; task = "fix the parser"
    ; name = None
    ; model = "claude-haiku"
    ; tools = [ "bash" ]
    ; worktree = None
    ; forked = true
    ; continued
    }
;;

let end_ ~turns ~cost_usd ~is_error text =
  P.Event.Subagent_end
    { call_id = "c1"
    ; agent_id = "a1"
    ; usage = { input = 10 * turns; output = 5 * turns; cache_read = 0 }
    ; turns
    ; cost_usd
    ; result = { text; is_error }
    }
;;

let in_subagent event =
  P.Event.Subagent { call_id = "c1"; agent_id = "a1"; event }
;;

let bash h id command =
  let call = tool_call ~arguments:(sprintf {|{"command":%S}|} command) id in
  H.event h (in_subagent (Tool_start call));
  H.event h (in_subagent (Tool_end { call; result = tool_result ~id "ok" }))
;;

(* A forked subagent's card says so; one that ran out of turns and is
   continued runs again in the same card and agent view, keeping what it
   showed, and ends with the totals over both runs. *)
let%expect_test "forked subagent: out of turns, continued, done" =
  let h = Test_app.connected ~width:90 ~height:14 () in
  H.event h (State (state ~running:true ()));
  let call =
    tool_call
      ~name:"subagent"
      ~arguments:{|{"task":"fix the parser","fork":true}|}
      "c1"
  in
  H.event h (Tool_start call);
  H.event h (start ~continued:false);
  H.event h (in_subagent Turn_start);
  bash h "t1" "make";
  H.event
    h
    (Tool_end
       { call
       ; result =
           tool_result
             ~name:"subagent"
             ~id:"c1"
             "started agent a1 (fix the parser)"
       });
  H.event h (State (state ~subagents:[ "a1", "fix the parser", true ] ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    … subagent fix the parser  forked 1 turn
        ↳ bash $ make
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋ bg
    |}];
  let out_of_turns =
    "subagent hit the 1-turn limit without finishing\n\
     [out of turns: call subagent_continue with id a1 (optionally max_turns, \
     default 1, and a message) to give it more turns: it carries on from where \
     it stopped; or subagent_cancel it, or leave it]"
  in
  H.event h (end_ ~turns:1 ~cost_usd:0.01 ~is_error:true out_of_turns);
  (* The backend hands the report to the idle main agent. *)
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Message_start
       (delivery ("[subagent a1 out of turns] fix the parser\n" ^ out_of_turns)));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
      earlier answer
    ✕ subagent fix the parser  forked failed
        subagent hit the 1-turn limit without finishing
        [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and
        a message) to give it more turns: it carries on from where it stopped; or
        subagent_cancel it, or leave it]
    ↩ subagent a1 "fix the parser"  out of turns
        subagent hit the 1-turn limit without finishing
        [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and
        a message) to give it more turns: it carries on from where it stopped; or
        subagent_cancel it, or leave it]
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:[main] 1✗
    |}];
  (* [subagent_continue a1] *)
  H.event h (start ~continued:true);
  H.event h (State (state ~subagents:[ "a1", "fix the parser", true ] ()));
  H.event h (in_subagent Turn_start);
  bash h "t2" "make test";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    … subagent fix the parser  forked 2 turns
        ↳ bash $ make test
    ↩ subagent a1 "fix the parser"  out of turns
        subagent hit the 1-turn limit without finishing
        [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and
        a message) to give it more turns: it carries on from where it stopped; or
        subagent_cancel it, or leave it]
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋ bg
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 2 turns  "fix the parser"






    ✓ bash $ make
        ok
    ✓ bash $ make test
        ok
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏Steer a1 · fix the parser (Enter: steer, Alt+Enter: follow-up)
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:main [1⠋] bg  → a1 · Esc: main agent
    |}];
  H.event h (end_ ~turns:2 ~cost_usd:0.02 ~is_error:false "parser fixed");
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ✓ done 2 turns $0.02  "fix the parser"






    ✓ bash $ make
        ok
    ✓ bash $ make test
        ok
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏a1 has finished · Esc goes back to the main agent
    deepseek-flash  ctx:0.1%/1.0M  $0.01  agents:main [1✓]  → a1 (finished) · Esc: main agent
    |}]
;;
