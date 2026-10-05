open! Core
open Prigh_web
module H = Harness

(* The agents panel: subagents (live from their events and [list_subagents],
   nested ones under their parent) and background jobs ([list_jobs]). *)

let call ~id ~name args =
  sprintf
    {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"%s","name":"%s","arguments":%s}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}}|}
    id
    name
    (Jsonaf.to_string (`String args))
;;

let text_reply text =
  sprintf
    {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":%s}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}}|}
    (Jsonaf.to_string (`String text))
;;

let tool_start ~id ~name args =
  sprintf
    {|{"event":"tool_start","call":{"id":"%s","name":"%s","arguments":%s}}|}
    id
    name
    (Jsonaf.to_string (`String args))
;;

let tool_end ~id ~name text =
  sprintf
    {|{"event":"tool_end","call":{"id":"%s","name":"%s","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"%s","tool_name":"%s","text":%s,"is_error":false}}|}
    id
    name
    id
    name
    (Jsonaf.to_string (`String text))
;;

let inner ~call ~agent json =
  sprintf
    {|{"event":"subagent","call_id":"%s","agent_id":"%s","inner":%s}|}
    call
    agent
    json
;;

let start ~call ~agent task =
  sprintf
    {|{"event":"subagent_start","call_id":"%s","agent_id":"%s","task":"%s","model":"deepseek-flash","tools":["bash"]}|}
    call
    agent
    task
;;

let end_ ?(error = false) ~call ~agent ~turns text =
  sprintf
    {|{"event":"subagent_end","call_id":"%s","agent_id":"%s","usage":{"input":0,"output":0,"cache_read":0},"turns":%d,"cost_usd":0.012,"result":{"text":%s,"is_error":%b}}|}
    call
    agent
    turns
    (Jsonaf.to_string (`String text))
    error
;;

let in_a1 = inner ~call:"s1" ~agent:"a1"
let in_n1 json = in_a1 (inner ~call:"n1" ~agent:"a1/n1" json)
let events h = List.iter ~f:(H.event h)
let at seconds = Time_ns.add H.now (Time_ns.Span.of_sec seconds)

let job
      ?(running = true)
      ?exit
      ?(delivered = false)
      ?(elapsed = 1.)
      ?last_line
      id
      command
  =
  let opt = Option.value_map ~default:"null" ~f:(sprintf "%S") in
  sprintf
    {|{"id":"%s","command":"%s","running":%b,"exit":%s,"delivered":%b,"elapsed":%g,"bytes":%d,"last_line":%s}|}
    id
    command
    running
    (opt exit)
    delivered
    elapsed
    (Option.value_map last_line ~default:0 ~f:String.length)
    (opt last_line)
;;

(* A background subagent a1 that started a synchronous one, a1/n1, which is
   running bash. *)
let survey h =
  events
    h
    [ {|{"event":"agent_start"}|}
    ; call ~id:"s1" ~name:"subagent" {|{"task":"Survey the repository"}|}
    ; tool_start ~id:"s1" ~name:"subagent" {|{"task":"Survey the repository"}|}
    ; start ~call:"s1" ~agent:"a1" "Survey the repository"
    ; in_a1 {|{"event":"agent_start"}|}
    ; in_a1 {|{"event":"turn_start"}|}
    ; in_a1
        {|{"event":"message_start","message":{"role":"user","text":"Survey the repository"}}|}
    ; in_a1 (call ~id:"n1" ~name:"subagent" {|{"task":"Count the modules"}|})
    ; in_a1
        (tool_start ~id:"n1" ~name:"subagent" {|{"task":"Count the modules"}|})
    ; in_a1 (start ~call:"n1" ~agent:"a1/n1" "Count the modules")
    ; in_n1 {|{"event":"agent_start"}|}
    ; in_n1 {|{"event":"turn_start"}|}
    ; in_n1
        {|{"event":"message_start","message":{"role":"user","text":"Count the modules"}}|}
    ; in_n1 (call ~id:"b1" ~name:"bash" {|{"command":"ls lib | wc -l"}|})
    ; in_n1 (tool_start ~id:"b1" ~name:"bash" {|{"command":"ls lib | wc -l"}|})
    ; tool_end
        ~id:"s1"
        ~name:"subagent"
        "started agent a1 (Survey the repository); its result will be \
         delivered to you when it finishes"
    ]
;;

let%expect_test "subagents start, nest and show in the panel, live" =
  let h = H.create () in
  survey h;
  (* Each start asks for the backend's times; the events already tell. *)
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    |}];
  H.text h ~selector:".status .background";
  H.show h ~selector:".agents-toggle";
  [%expect
    {|
    (2 agents running)
    <button type="button"
            title="Subagents and jobs (/agents)"
            aria-label="Subagents and jobs (/agents)"
            class="agents-toggle btn ghost icon"
            @on_click>
      <icon class="bot"> </icon>
      <span class="badge"> 2 </span>
    </button>
    |}];
  H.act h (Open_subagents None);
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents 2 agents running (Close (Alt+0))
    Subagents 2 running
    (1 Survey the repository 0s a1 · deepseek-flash · 1 turn · 1 tool call subagent Count the modules · 0s) (2 Count the modules 0s a1/n1 · deepseek-flash · 1 turn · 1 tool call bash ls lib | wc -l · 0s)
    |}];
  (* The clock ticks while something runs. *)
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  H.act h (Clock (at 75.));
  H.text h ~selector:".agents-row.nested";
  [%expect
    {|
    true
    (2 Count the modules 1m 15s a1/n1 · deepseek-flash · 1 turn · 1 tool call bash ls lib | wc -l · 1m 15s)
    |}];
  (* The backend's record has the real start times. *)
  H.reply
    h
    "list_subagents"
    (sprintf
       {|[{"id":"a1","call_id":"s1","parent":null,"task":"Survey the repository","model":"deepseek-flash","state":"running","started_at_ms":%.0f,"updated_at_ms":0,"ended_at_ms":null,"turns":1,"tool_calls":1,"current_tool":"subagent","current_tool_started_at_ms":%.0f,"message_count":1,"stale":false,"result":null},
          {"id":"a1/n1","call_id":"n1","parent":"a1","task":"Count the modules","model":"deepseek-flash","state":"running","started_at_ms":%.0f,"updated_at_ms":0,"ended_at_ms":null,"turns":1,"tool_calls":1,"current_tool":"bash","current_tool_started_at_ms":%.0f,"message_count":1,"stale":false,"result":null}]|}
       (Time_ns.to_span_since_epoch (at (-30.)) |> Time_ns.Span.to_ms)
       (Time_ns.to_span_since_epoch (at 0.) |> Time_ns.Span.to_ms)
       (Time_ns.to_span_since_epoch (at 1.) |> Time_ns.Span.to_ms)
       (Time_ns.to_span_since_epoch (at 2.) |> Time_ns.Span.to_ms));
  H.text h ~selector:".agents-section";
  [%expect
    {|
    Subagents 2 running
    (1 Survey the repository 1m 45s a1 · deepseek-flash · 1 turn · 1 tool call subagent Count the modules · 1m 15s) (2 Count the modules 1m 14s a1/n1 · deepseek-flash · 1 turn · 1 tool call bash ls lib | wc -l · 1m 13s)
    |}];
  (* The nested one finishes, then a1 (whose report is delivered), and a
     second one fails. *)
  events
    h
    [ in_n1 (tool_end ~id:"b1" ~name:"bash" "42")
    ; in_n1 {|{"event":"turn_start"}|}
    ; in_n1 (text_reply "There are **42** modules.")
    ; in_a1
        (end_
           ~call:"n1"
           ~agent:"a1/n1"
           ~turns:2
           "There are **42** modules.\n\
            [subagent: 2 turns, 0 in / 0 out tokens, $0.0120]")
    ; in_a1 (tool_end ~id:"n1" ~name:"subagent" "There are 42 modules.")
    ; in_a1 {|{"event":"turn_start"}|}
    ; in_a1 (text_reply "## Survey\n\n42 modules.")
    ; end_
        ~call:"s1"
        ~agent:"a1"
        ~turns:2
        "## Survey\n\n\
         42 modules.\n\
         [subagent: 2 turns, 0 in / 0 out tokens, $0.0240]"
    ; call ~id:"s2" ~name:"subagent" {|{"task":"Draft the docs"}|}
    ; tool_start ~id:"s2" ~name:"subagent" {|{"task":"Draft the docs"}|}
    ; start ~call:"s2" ~agent:"a2" "Draft the docs"
    ; end_
        ~error:true
        ~call:"s2"
        ~agent:"a2"
        ~turns:1
        "subagent failed: rate limited (429)\n\
         [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]"
    ];
  H.act h (Clock (at 80.));
  H.text h ~selector:".agents-panel";
  H.text h ~selector:".status .background";
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    Agents (Close (Alt+0))
    Subagents 3
    (1 ✓ Survey the repository 1m 45s a1 · deepseek-flash · 2 turns · 1 tool call · $0.01 Survey) (2 ✓ Count the modules 1m 14s a1/n1 · deepseek-flash · 2 turns · 1 tool call · $0.01 There are 42 modules.) (3 ✕ Draft the docs 0s a2 · deepseek-flash · 1 turn · $0.01 subagent failed: rate limited (429))
    (3 agents)
    false
    |}]
;;

let%expect_test "a subagent in full: its transcript, cancelling it, its card" =
  let h = H.create () in
  survey h;
  (* Alt+N follows the Nth listed (the panel numbers them). *)
  H.key h ~alt:true "2";
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Focus_agent 2)
    (All agents (Esc)) Subagent a1/n1 2 agents running (Close (Alt+0))
    (Survey the repository) › Count the modules
    Count the modules
    running 0s a1/n1 deepseek-flash 1 turn 1 tool call
    (Cancel) (Show in chat)
    Count the modules
    bash ls lib | wc -l
    |}];
  H.key h ~alt:true "3";
  H.key h ~alt:true ~ctrl:true "1";
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  H.act h (Cancel_subagent "a1/n1");
  H.text h ~selector:".detail-actions";
  [%expect
    {|
    (Rpc (method_ cancel_subagent) (params ((agent_id a1/n1))) (tag Show_error))
    (Stopping…) (Show in chat)
    |}];
  H.act h (Show_in_chat "a1/n1");
  [%expect {| (Reveal (s1 n1)) |}];
  (* Its parent from the crumbs, then back to the list; Esc from the page
     goes back, from the editor it is the editor's. *)
  H.act h (Select_item (Agent "a1"));
  H.text h ~selector:".agents-detail > .agents-section";
  H.key h ~target:Page "Escape";
  print_s [%sexp ((H.model h).agents.selected : Agents.Item.t option)];
  H.key h ~alt:true "]";
  H.key h ~alt:true "]";
  H.key h ~alt:true "]";
  H.key h ~alt:true "[";
  print_s [%sexp ((H.model h).agents.selected : Agents.Item.t option)];
  H.key h "Escape";
  H.key h ~target:Page "Escape";
  H.key h ~target:Page "Escape";
  print_s [%sexp ((H.model h).agents.open_ : bool)];
  H.key h ~alt:true "0";
  [%expect
    {|
    Its subagents 1
    (Count the modules 0s a1/n1 · deepseek-flash · 1 turn · 1 tool call bash ls lib | wc -l · 0s)
    Agents_back
    ()
    (Cycle_agent 1)
    (Cycle_agent 1)
    (Cycle_agent 1)
    (Cycle_agent -1)
    ((Agent a1/n1))
    (browser default)
    Agents_back
    Agents_back
    (Focus editor)
    false
    (browser default)
    |}];
  (* The nested one ends: its report is the transcript's end; it is no
     longer stopping. *)
  H.act h (Select_item (Agent "a1/n1"));
  events
    h
    [ in_n1 (tool_end ~id:"b1" ~name:"bash" "[cancelled]")
    ; in_a1
        (end_
           ~error:true
           ~call:"n1"
           ~agent:"a1/n1"
           ~turns:1
           "[cancelled]\n[subagent: 1 turns, 0 in / 0 out tokens, $0.0000]")
    ];
  H.text h ~selector:".agents-detail";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Survey the repository) › Count the modules
    ✕ Count the modules
    failed 0s a1/n1 deepseek-flash 1 turn 1 tool call $0.01
    (Show in chat)
    Count the modules
    ✓ bash ls lib | wc -l cancelled
    Cancelled
    |}]
;;

let%expect_test "/agents and its argument" =
  let h = H.create () in
  H.type_ h "/agents 9";
  H.act h Send;
  H.text h ~selector:".toast";
  survey h;
  H.type_ h "/agents 2";
  H.act h Send;
  H.type_ h "/agents a1";
  H.act h Send;
  H.type_ h "/agents n1";
  H.act h Send;
  print_s [%sexp ((H.model h).agents.selected : Agents.Item.t option)];
  H.type_ h "/agents nope";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Save_history ("/agents 9"))
    No subagent or job 9: none has run in this session.
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Save_history ("/agents 2" "/agents 9"))
    (Save_history ("/agents a1" "/agents 2" "/agents 9"))
    (Save_history ("/agents n1" "/agents a1" "/agents 2" "/agents 9"))
    ((Agent a1/n1))
    (Save_history
     ("/agents nope" "/agents n1" "/agents a1" "/agents 2" "/agents 9"))
    No subagent or job 9: none has run in this session.
    No subagent or job nope: give its number (1-2) or id; /agents lists them.
    |}]
;;

let%expect_test "jobs: listed, their output polled, killed" =
  let h = H.create () in
  let state jobs =
    sprintf
      {|{"event":"state","state":%s}|}
      (H.state_json ~fields:[ "jobs", Jsonaf.of_string jobs ] ())
  in
  H.event
    h
    (state {|[{"id":"j1","command":"make test","running":true,"exit":null}]|});
  H.reply
    h
    "list_jobs"
    (sprintf "[%s]" (job "j1" "make test" ~elapsed:3. ~last_line:"ok 12 tests"));
  H.text h ~selector:".status .background";
  H.act h (Focus_agent 1);
  H.act h (Focus_agent 1);
  [%expect
    {|
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (1 job running)
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    |}];
  H.reply
    h
    "job_output"
    {|{"text":"[job j1 running after 3s, 40 bytes; lines 1-2 of 2] make test\nok 11 tests\nok 12 tests"}|};
  H.text h ~selector:".agents-panel";
  [%expect
    {|
    (All agents (Esc)) Job j1 1 job running (Close (Alt+0))
    make test
    running 3s j1 11 B
    (Kill)
    Output, lines 1-2 of 2
    ok 11 tests
    ok 12 tests
    Its output refreshes while it runs.
    |}];
  (* Polled every 2 seconds while the panel is open and it runs. *)
  H.act h (Clock (at 1.));
  H.act h (Clock (at 2.5));
  H.act h (Clock (at 3.));
  H.act h (Clock (at 5.));
  H.text h ~selector:".detail-meta";
  [%expect
    {|
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    running 8s j1 11 B
    |}];
  H.act h (Kill_job "j1");
  H.text h ~selector:".detail-actions";
  H.event
    h
    (state
       {|[{"id":"j1","command":"make test","running":false,"exit":"killed"}]|});
  H.reply
    h
    "list_jobs"
    (sprintf
       "[%s]"
       (job "j1" "make test" ~running:false ~exit:"killed" ~elapsed:6.));
  H.text h ~selector:".detail-head";
  H.act h (Clock (at 9.));
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  [%expect
    {|
    (Rpc (method_ kill_job) (params ((job_id j1))) (tag Show_error))
    (Stopping…)
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    ✕ make test
    killed 6s j1
    false
    |}]
;;

let%expect_test
    "finished work folds away at the next prompt; a session switch resets the \
     panel"
  =
  let h = H.create () in
  survey h;
  events
    h
    [ in_a1 (end_ ~call:"n1" ~agent:"a1/n1" ~turns:1 "42")
    ; end_ ~call:"s1" ~agent:"a1" ~turns:2 "done"
    ; call ~id:"s2" ~name:"subagent" {|{"task":"Draft the docs"}|}
    ; tool_start ~id:"s2" ~name:"subagent" {|{"task":"Draft the docs"}|}
    ; start ~call:"s2" ~agent:"a2" "Draft the docs"
    ];
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ ( "jobs"
              , Jsonaf.of_string
                  {|[{"id":"j1","command":"sleep 1","running":false,"exit":"exited 0"}]|}
              )
            ]
          ()));
  H.reply
    h
    "list_jobs"
    (sprintf
       "[%s,%s]"
       (job "j1" "sleep 1" ~running:false ~exit:"exited 0")
       (job "j0" "old" ~running:false ~exit:"exited 0" ~delivered:true));
  H.act h (Open_subagents None);
  H.text h ~selector:".agents-body";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Subagents 1 running
    (1 Draft the docs 0s a2 · deepseek-flash starting…) (2 ✓ Survey the repository 0s a1 · deepseek-flash · 2 turns · 1 tool call · $0.01 done) (3 ✓ Count the modules 0s a1/n1 · deepseek-flash · 1 turn · 1 tool call · $0.01 42)
    Jobs 1
    (4 ✓ sleep 1 1s j1 · exited 0)
    Earlier (1)
    (✓ old 1s j0 · exited 0)
    |}];
  (* a2 still runs: it stays. *)
  H.type_ h "next";
  H.act h Send;
  H.text h ~selector:".agents-body";
  [%expect
    {|
    (Save_history (next))
    (Rpc (method_ prompt) (params ((text next))) (tag Show_error))
    Subagents 1 running
    (1 Draft the docs 0s a2 · deepseek-flash starting…)
    Earlier (4)
    (✓ Survey the repository 0s a1 · deepseek-flash · 2 turns · 1 tool call · $0.01 done) (✓ Count the modules 0s a1/n1 · deepseek-flash · 1 turn · 1 tool call · $0.01 42)
    (✓ sleep 1 1s j1 · exited 0)
    (✓ old 1s j0 · exited 0)
    |}];
  H.act h (Select_item (Agent "a2"));
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  print_s
    [%sexp
      { open_ = ((H.model h).agents.open_ : bool)
      ; selected = ((H.model h).agents.selected : Agents.Item.t option)
      ; agents = (List.length (H.model h).agents.agents : int)
      }];
  [%expect
    {|
    (Set_url_session s2)
    Scroll_to_bottom
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    ((open_ true) (selected ()) (agents 0))
    |}]
;;

let%expect_test "after a reload, a nested subagent's transcript is fetched" =
  let h = H.create () in
  H.act
    h
    (Reply
       ( Messages
       , Ok
           (Jsonaf.of_string
              {|[{"role":"user","text":"survey"},
                 {"role":"assistant","content":[{"type":"tool_call","id":"s1","name":"subagent","arguments":"{\"task\":\"Survey the repository\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},
                 {"role":"tool_result","tool_call_id":"s1","tool_name":"subagent","text":"started agent a1 (Survey the repository); ...","is_error":false}]|})
       ));
  H.reply
    h
    "get_subagent"
    {|{"subagent":{"id":"a1","call_id":"s1","parent":null,"task":"Survey the repository","model":"deepseek-flash","state":"running","turns":1,"result":null},
       "messages":[{"role":"user","text":"Survey the repository"},
                   {"role":"assistant","content":[{"type":"tool_call","id":"n1","name":"subagent","arguments":"{\"task\":\"Count the modules\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|};
  H.reply
    h
    "list_subagents"
    {|[{"id":"a1","call_id":"s1","parent":null,"task":"Survey the repository","model":"deepseek-flash","state":"running","started_at_ms":1791187200000,"updated_at_ms":0,"ended_at_ms":null,"turns":1,"tool_calls":1,"current_tool":"subagent","current_tool_started_at_ms":1791187200000,"message_count":2,"stale":false,"result":null},
       {"id":"a1/n1","call_id":"n1","parent":"a1","task":"Count the modules","model":"deepseek-flash","state":"complete","started_at_ms":1791187201000,"updated_at_ms":0,"ended_at_ms":1791187231000,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":3,"stale":false,"result":{"text":"42 modules","is_error":false}}]|};
  H.act h (Select_item (Agent "a1/n1"));
  H.text h ~selector:".agents-transcript";
  H.reply
    h
    "get_subagent"
    {|{"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"Count the modules","model":"deepseek-flash","state":"complete","turns":2,"result":{"text":"42 modules","is_error":false}},
       "messages":[{"role":"user","text":"Count the modules"},
                   {"role":"assistant","content":[{"type":"text","text":"42 modules"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|};
  H.text h ~selector:".agents-detail";
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id s1))) (tag (Subagent s1)))
    (Rpc (method_ get_subagent) (params ((id a1/n1))) (tag (Subagent a1/n1)))
    Loading the transcript…
    (Survey the repository) › Count the modules
    ✓ Count the modules
    complete 30s a1/n1 deepseek-flash 2 turns 1 tool call
    (Show in chat)
    Count the modules
    42 modules
    |}]
;;
