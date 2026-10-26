open! Core
open Prigh_web
module H = Harness

(* The agents panel: subagents (live from their events and [list_subagents],
   nested ones under their parent) and background jobs ([list_jobs]). *)

open Agent_events

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
  H.text h ~selector:".status-extras .background";
  H.show h ~selector:".agents-toggle";
  [%expect
    {|
    (2 agents running)
    <button type="button"
            title="Subagents and jobs (/agents)"
            aria-label="Subagents and jobs: 2 running"
            aria-expanded="false"
            class="agents-toggle btn ghost icon"
            @on_click>
      <icon class="bot"> </icon>
      <span class="badge"> 2 </span>
    </button>
    |}];
  H.act h (Open_subagents None);
  H.text h ~selector:".agents-panel";
  (* A tree of buttons, the nested one in its parent's item; Alt+N opens
     the Nth. *)
  H.show h ~selector:".agents-section > .agents-tree";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Agents
    2 agents running (Close (Alt+0))
    Subagents 2 running
    (Running Survey the repository 0s subagent Count the modules a1  deepseek-flash  1 turn  1 tool call)
    (Running Count the modules 0s bash ls lib | wc -l a1/n1  deepseek-flash  1 turn  1 tool call)
    <ul class="agents-tree">
      <li>
        <button type="button"
                aria-keyshortcuts="Alt+1"
                title="Alt+1"
                class="agents-row running"
                @on_click>
          <span class="row-mark">
            <span title="Running" aria-hidden="true" class="lamp"> </span>
          </span>
          <span class="sr-only"> Running </span>
          <span class="row-title"> Survey the repository </span>
          <span class="row-time"> 0s </span>
          <span class="row-line"> subagent Count the modules </span>
          <span class="row-meta"> a1  deepseek-flash  1 turn  1 tool call </span>
        </button>
        <ul class="agents-tree nested">
          <li>
            <button type="button"
                    aria-keyshortcuts="Alt+2"
                    title="Alt+2"
                    class="agents-row running"
                    @on_click>
              <span class="row-mark">
                <span title="Running" aria-hidden="true" class="lamp"> </span>
              </span>
              <span class="sr-only"> Running </span>
              <span class="row-title"> Count the modules </span>
              <span class="row-time"> 0s </span>
              <span class="row-line"> bash ls lib | wc -l </span>
              <span class="row-meta"> a1/n1  deepseek-flash  1 turn  1 tool call </span>
            </button>
          </li>
        </ul>
      </li>
    </ul>
    |}];
  (* The clock ticks while something runs. *)
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  H.act h (Clock (at 75.));
  H.text h ~selector:".agents-row.nested";
  [%expect {| true |}];
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
    (Running Survey the repository 1m 45s subagent Count the modules a1  deepseek-flash  1 turn  1 tool call)
    (Running Count the modules 1m 14s bash ls lib | wc -l a1/n1  deepseek-flash  1 turn  1 tool call)
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
  H.text h ~selector:".status-extras .background";
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    Agents
    (Close (Alt+0))
    Subagents 3
    (✓ Done Survey the repository 1m 45s Survey)
    (✓ Done Count the modules 1m 14s There are 42 modules.)
    (✕ Failed Draft the docs rate limited (429))
    (3 agents)
    false
    |}]
;;

let%expect_test "what is shown in full stays pinned under what scrolls" =
  let h = H.create () in
  H.quiet h (fun () ->
    survey h;
    H.key h ~alt:true "2");
  (* The panel's own head (back, close), the transcript that scrolls, then
     the footer with the task, how it stands and the actions. *)
  H.elements h ~selector:".agents-panel > *";
  H.show h ~selector:".agents-foot";
  [%expect
    {|
    (Focus_agent 2)
    <div class="resizer resizer-agents">
    <div class="agents-head">
    <div class="agents-body">
    <footer class="agents-foot">
    <footer aria-label="This subagent" class="agents-foot">
      <nav aria-label="Started by" class="agents-crumbs">
        <button type="button" title="a1" class="crumb" @on_click> Survey the repository </button>
        <span aria-hidden="true" class="crumb-sep"> › </span>
      </nav>
      <div class="detail-head running">
        <h2 class="detail-title">
          <span class="row-mark">
            <span title="Running" aria-hidden="true" class="lamp"> </span>
          </span>
          <span title="Count the modules" class="detail-task"> Count the modules </span>
        </h2>
        <div class="detail-meta">
          <button type="button"
                  title="a1/n1 runs inside a1 (a subagent's own subagent) and takes no messages: steer a1 instead."
                  disabled=""
                  class="agents-steer btn ghost small"
                  @on_click>
            <icon class="target"> </icon>
            Steer
          </button>
          <button type="button"
                  title="Cancel this subagent (/agents lists the others)"
                  class="btn small stop"
                  @on_click>
            <icon class="stop"> </icon>
            Cancel
          </button>
          <button type="button" class="agents-locate btn ghost small" @on_click>
            <icon class="locate"> </icon>
            Show in chat
          </button>
          <span class="detail-state"> Running </span>

          <span class="detail-elapsed"> 0s </span>

          <span class="detail-more"> deepseek-flash  1 turn  1 tool call </span>
        </div>
      </div>
    </footer>
    |}];
  (* The list has none. *)
  H.quiet h (fun () -> H.act h Agents_back);
  H.elements h ~selector:".agents-foot";
  [%expect {| |}]
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
    (All agents (Esc))
    Subagent a1/n1
    (Close (Alt+0))
    Count the modules
    bash ls lib | wc -l
    (Survey the repository) ›
    Count the modules
    (Steer) (Cancel) (Show in chat) Running 0s deepseek-flash  1 turn  1 tool call
    |}];
  H.key h ~alt:true "3";
  H.key h ~alt:true ~ctrl:true "1";
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  H.act h (Cancel_subagent "a1/n1");
  H.text h ~selector:".detail-meta";
  [%expect
    {|
    (Rpc (method_ cancel_subagent) (params ((agent_id a1/n1))) (tag Show_error))
    (Steer) (Stopping…) (Show in chat) Running 0s deepseek-flash  1 turn  1 tool call
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
    (Running Count the modules 0s bash ls lib | wc -l a1/n1  deepseek-flash  1 turn  1 tool call)
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
  H.text h ~selector:".agents-foot";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    Count the modules
    ✓ bash ls lib | wc -l cancelled
    – Cancelled
    (Survey the repository) ›
    – Count the modules
    (Show in chat) Cancelled deepseek-flash  1 turn  1 tool call  $0.01
    |}]
;;

let%expect_test
    "how agents and jobs ended: failed, cancelled, interrupted, killed"
  =
  let h = H.create () in
  let agent ?(parent = "null") ~id ~started ~ended result =
    sprintf
      {|{"id":"%s","call_id":"c-%s","parent":%s,"task":"Task %s","model":"m","state":"x","started_at_ms":%.0f,"updated_at_ms":0,"ended_at_ms":%.0f,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":false,"result":%s}|}
      id
      id
      parent
      id
      (Time_ns.to_span_since_epoch (at started) |> Time_ns.Span.to_ms)
      (Time_ns.to_span_since_epoch (at ended) |> Time_ns.Span.to_ms)
      result
  in
  H.act h (Open_subagents None);
  H.reply
    h
    "list_subagents"
    (sprintf
       "[%s]"
       (String.concat
          ~sep:","
          [ agent
              ~id:"a1"
              ~started:0.
              ~ended:0.4
              {|{"text":"quick","is_error":false}|}
          ; agent
              ~id:"a2"
              ~started:0.
              ~ended:65.
              {|{"text":"subagent failed: HTTP 529: overloaded","is_error":true}|}
          ; agent
              ~id:"a3"
              ~started:0.
              ~ended:3.
              {|{"text":"[cancelled]\nhalf a thought","is_error":true}|}
          ; agent
              ~id:"a4"
              ~started:0.
              ~ended:3.
              {|{"text":"[interrupted] the backend stopped before it finished","is_error":true}|}
          ]));
  H.reply
    h
    "list_jobs"
    (sprintf
       "[%s,%s]"
       (job
          "j1"
          "make"
          ~running:false
          ~exit:"exited 2"
          ~elapsed:4.
          ~last_line:"error: no rule")
       (job "j2" "sleep 99" ~running:false ~exit:"killed" ~elapsed:0.2));
  (* Under a second is not worth a time; stopped work is not a failure. *)
  H.text h ~selector:".agents-body";
  H.elements h ~selector:".agents-row";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Subagents 4
    (✓ Done Task a1 quick)
    (✕ Failed Task a2 1m 05s HTTP 529: overloaded)
    (– Task a3 3s Cancelled)
    (– Task a4 3s Interrupted: the backend stopped before it finished)
    Jobs 2
    (✕ Failed make 4s exited 2: error: no rule)
    (– sleep 99 Killed)
    <button class="agents-row complete">
    <button class="agents-row failed">
    <button class="agents-row stopped">
    <button class="agents-row stopped">
    <button class="agents-row failed">
    <button class="agents-row stopped">
    |}];
  (* The marks are icons (not glyphs a font may lack), coloured by their
     class and hidden from screen readers: the row says it in words. *)
  H.show h ~selector:".agents-row .row-mark";
  [%expect
    {|
    <span class="row-mark">
      <span aria-hidden="true" class="done mark">
        <icon class="check"> </icon>
      </span>
    </span>
    <span class="row-mark">
      <span aria-hidden="true" class="failed mark">
        <icon class="cross"> </icon>
      </span>
    </span>
    <span class="row-mark">
      <span aria-hidden="true" class="mark stopped">
        <icon class="dash"> </icon>
      </span>
    </span>
    <span class="row-mark">
      <span aria-hidden="true" class="mark stopped">
        <icon class="dash"> </icon>
      </span>
    </span>
    <span class="row-mark">
      <span aria-hidden="true" class="failed mark">
        <icon class="cross"> </icon>
      </span>
    </span>
    <span class="row-mark">
      <span aria-hidden="true" class="mark stopped">
        <icon class="dash"> </icon>
      </span>
    </span>
    |}];
  H.act h (Select_item (Agent "a3"));
  H.text h ~selector:".agents-detail";
  H.text h ~selector:".agents-foot";
  H.act h (Select_item (Agent "a4"));
  H.text h ~selector:".agents-detail";
  H.text h ~selector:".agents-foot";
  H.act h (Select_item (Agent "a2"));
  H.text h ~selector:".agents-detail";
  H.text h ~selector:".agents-foot";
  H.act h (Select_item (Job "j1"));
  H.text h ~selector:".detail-head";
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a3) (elide true)))
     (tag (Subagent a3)))
    Loading the transcript…
    – Cancelled
    half a thought
    – Task a3
    (Show in chat) Cancelled 3s m  1 turn
    (Rpc (method_ get_subagent) (params ((id a4) (elide true)))
     (tag (Subagent a4)))
    Loading the transcript…
    – Interrupted
    the backend stopped before it finished
    – Task a4
    (Show in chat) Interrupted 3s m  1 turn
    (Rpc (method_ get_subagent) (params ((id a2) (elide true)))
     (tag (Subagent a2)))
    Loading the transcript…
    ✕ Failed
    HTTP 529: overloaded
    ✕ Task a2
    (Show in chat) Failed 1m 05s m  1 turn
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    ✕ make
    Failed 4s exited 2  14 B
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
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents 9") (max 500))) (tag Ignore))
    No subagent or job 9: none has run in this session. (Dismiss)
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents 2") (max 500))) (tag Ignore))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents a1") (max 500))) (tag Ignore))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents n1") (max 500))) (tag Ignore))
    ((Agent a1/n1))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents nope") (max 500))) (tag Ignore))
    No subagent or job nope: give its number (1-2) or id; /agents lists them. (Dismiss)
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
  H.text h ~selector:".status-extras .background";
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
    (All agents (Esc))
    Job j1
    (Close (Alt+0))
    ok 11 tests
    ok 12 tests
    Output, lines 1-2 of 2  refreshes while it runs
    make test
    (Kill) Running 3s 11 B
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
    (Kill) Running 8s 11 B
    |}];
  H.act h (Kill_job "j1");
  H.text h ~selector:".detail-meta";
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
  (* Its last lines are fetched once more now that it has exited. *)
  H.text h ~selector:".detail-head";
  H.act h (Clock (at 9.));
  print_s [%sexp (App.Model.ticking (H.model h) : bool)];
  [%expect
    {|
    (Rpc (method_ kill_job) (params ((job_id j1))) (tag Show_error))
    (Stopping…) Running 8s 11 B
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ job_output) (params ((job_id j1))) (tag (Job_output j1)))
    – make test
    Killed 6s
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
    (Running Draft the docs 0s Starting… a2  deepseek-flash)
    (✓ Done Survey the repository done)
    (✓ Done Count the modules 42)
    Jobs 1
    (✓ Done sleep 1 1s)
    Earlier 1
    (✓ Done old 1s)
    |}];
  (* a2 still runs: it stays. *)
  H.type_ h "next";
  H.act h Send;
  H.text h ~selector:".agents-body";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value next) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text next)))
     (tag (Sent (text next) (images ()))))
    Subagents 1 running
    (Running Draft the docs 0s Starting… a2  deepseek-flash)
    Earlier 4
    (✓ Done Survey the repository done)
    (✓ Done Count the modules 42)
    (✓ Done sleep 1 1s)
    (✓ Done old 1s)
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
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    ((open_ true) (selected ()) (agents 0))
    |}]
;;

let%expect_test "after a reload, a nested subagent's transcript is fetched" =
  let h = H.create () in
  H.act
    h
    (Reply
       ( H.newest "s1"
       , Ok
           (Jsonaf.of_string
            @@ H.page
                 {|[{"role":"user","text":"survey"},
                 {"role":"assistant","content":[{"type":"tool_call","id":"s1","name":"subagent","arguments":"{\"task\":\"Survey the repository\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},
                 {"role":"tool_result","tool_call_id":"s1","tool_name":"subagent","text":"started agent a1 (Survey the repository); ...","is_error":false}]|}
           ) ));
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
  H.text h ~selector:".agents-note";
  H.reply
    h
    "get_subagent"
    {|{"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"Count the modules","model":"deepseek-flash","state":"complete","turns":2,"result":{"text":"42 modules","is_error":false}},
       "messages":[{"role":"user","text":"Count the modules"},
                   {"role":"assistant","content":[{"type":"text","text":"42 modules"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|};
  H.text h ~selector:".agents-detail";
  H.text h ~selector:".agents-foot";
  [%expect
    {|
    Scroll_to_bottom
    (Rpc (method_ get_subagent) (params ((id s1) (elide true)))
     (tag (Subagent s1)))
    (Rpc (method_ get_subagent) (params ((id a1/n1) (elide true)))
     (tag (Subagent a1/n1)))
    Loading the transcript…
    Count the modules
    42 modules
    (Survey the repository) ›
    ✓ Count the modules
    (Show in chat) Done 30s deepseek-flash  2 turns  1 tool call
    |}]
;;
