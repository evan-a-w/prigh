open! Core
module H = Harness
open Agent_events

(* The session card in the chat's top-right corner: the session's subagents
   (each opens its transcript in the chat's place) and its repository's
   worktrees; on a phone a sheet from the bottom. *)

let events h = List.iter ~f:(H.event h)

let worktrees =
  {|{"root":"/work","project_dir":"/home/ada/.prigh/worktrees/work","worktrees":[
     {"path":"/work","branch":"main","head":"abc1234","main":true,"current":true,"changes":0},
     {"path":"/home/ada/.prigh/worktrees/work/feat-ui","branch":"feat/ui","head":"def5678","managed":true,"changes":3},
     {"path":"/home/ada/.prigh/worktrees/work/fix-chips","branch":"fix/chips","head":"9876fed","managed":true}]}|}
;;

(* Two subagents: a1 in a tool, a2 between them; then a1 finishes. *)
let two_agents h =
  H.quiet h (fun () ->
    events
      h
      [ {|{"event":"agent_start"}|}
      ; call ~id:"s1" ~name:"subagent" {|{"task":"Fix the touch targets"}|}
      ; tool_start
          ~id:"s1"
          ~name:"subagent"
          {|{"task":"Fix the touch targets"}|}
      ; start ~call:"s1" ~agent:"a1" "Fix the touch targets"
      ; inner ~call:"s1" ~agent:"a1" {|{"event":"agent_start"}|}
      ; inner
          ~call:"s1"
          ~agent:"a1"
          (call ~id:"b1" ~name:"bash" {|{"command":"dune build"}|})
      ; inner
          ~call:"s1"
          ~agent:"a1"
          (tool_start ~id:"b1" ~name:"bash" {|{"command":"dune build"}|})
      ; call ~id:"s2" ~name:"subagent" {|{"task":"Review the phone layout"}|}
      ; tool_start
          ~id:"s2"
          ~name:"subagent"
          {|{"task":"Review the phone layout"}|}
      ; start ~call:"s2" ~agent:"a2" "Review the phone layout"
      ; inner ~call:"s2" ~agent:"a2" {|{"event":"agent_start"}|}
      ])
;;

let%expect_test "nothing to show, no card and no push" =
  let h = H.create () in
  H.elements h ~selector:".session-card";
  H.elements h ~selector:".session-card-toggle";
  H.elements h ~selector:".main";
  [%expect {| <main class="fresh main"> |}]
;;

let%expect_test
    "the card: subagents unfolded, worktrees folded; the chat keeps its margin"
  =
  let h = H.create () in
  two_agents h;
  H.reply h "list_worktrees" worktrees;
  H.text h ~selector:".session-card";
  [%expect
    {|
    (Subagents 2)
    (Fix the touch targets working…) (Review the phone layout thinking…)
    (Worktrees 3)
    |}];
  (* Unfolding the worktrees: each opens in this session, but the current
     one. *)
  H.click h ~selector:"[aria-controls=session-card-worktrees-content]";
  H.text h ~selector:".session-card";
  H.names h ~selector:".session-card-worktree";
  [%expect
    {|
    (Session_card (Fold Worktrees))
    (Subagents 2)
    (Fix the touch targets working…) (Review the phone layout thinking…)
    (Worktrees 3)
    (main) (feat/ui 3 changed) (fix/chips)
    button "main" -- /work
    button "feat/ui 3 changed" -- /home/ada/.prigh/worktrees/work/feat-ui
    button "fix/chips" -- /home/ada/.prigh/worktrees/work/fix-chips
    |}];
  H.click h ~nth:1 ~selector:".session-card-worktree";
  [%expect
    {|
    (Open_worktree (path /home/ada/.prigh/worktrees/work/feat-ui)
     (new_session false))
    (Focus editor)
    (Rpc (method_ set_cwd)
     (params ((path /home/ada/.prigh/worktrees/work/feat-ui) (worktree true)))
     (tag
      (Worktree_entered
       "Working in the worktree feat/ui (/home/ada/.prigh/worktrees/work/feat-ui)")))
    |}];
  (* Folding the subagents away. *)
  H.click h ~selector:"[aria-controls=session-card-subagents-content]";
  H.text h ~selector:".session-card";
  [%expect
    {|
    (Session_card (Fold Subagents))
    (Subagents 2)
    (Worktrees 3)
    (main) (feat/ui 3 changed) (fix/chips)
    |}];
  (* The main pane is a size container that pads the chat clear of the
     card by what its margin lacks. *)
  H.elements h ~selector:".main";
  [%expect
    {| <main class="@container @max-[820px]:*:[--push:0px] [&>.chat-frame]:pr-[var(--push)] [&>.composer]:max-w-[calc(var(--column)+2*var(--edge)+var(--push))] [&>.composer]:pr-[calc(var(--edge)+var(--push))] [--push:clamp(0px,calc(464px+var(--column)-100cqw),calc(232px-var(--edge)))] main"> |}];
  (* The chrome's button hides it, and the padding goes with it. *)
  H.click h ~selector:".session-card-toggle";
  H.elements h ~selector:".session-card";
  H.elements h ~selector:".main";
  [%expect
    {|
    Toggle_session_card
    <main class="main">
    |}];
  H.show h ~selector:".session-card-toggle";
  [%expect
    {|
    <button type="button"
            title="Subagents and worktrees"
            aria-label="Subagents and worktrees"
            aria-pressed="false"
            class="btn ghost icon session-card-toggle"
            @on_click>
      <svg xmlns="http://www.w3.org/2000/svg"
           width="24"
           height="24"
           viewBox="0 0 24 24"
           fill="none"
           stroke="currentColor"
           stroke-width="2"
           stroke-linecap="round"
           stroke-linejoin="round"
           class="-scale-x-100 lucide lucide-panel-left size-4">
        <rect width="18" height="18" x="3" y="3" rx="2"> </rect>
        <path d="M9 3v18"> </path>
      </svg>
    </button>
    |}]
;;

let%expect_test "a subagent finishes: how long ago, then failed" =
  let h = H.create () in
  two_agents h;
  H.quiet h (fun () ->
    events
      h
      [ end_ ~call:"s1" ~agent:"a1" ~turns:2 "Done: 44px everywhere."
      ; end_ ~error:true ~call:"s2" ~agent:"a2" ~turns:1 "The phone died."
      ]);
  H.texts h ~selector:".session-card-agent" |> List.iter ~f:print_endline;
  H.quiet h (fun () ->
    H.act h (Tick (Time_ns.add H.now (Time_ns.Span.of_min 12.))));
  H.texts h ~selector:".session-card-agent" |> List.iter ~f:print_endline;
  [%expect
    {|
    Fix the touch targets now
    Review the phone layout failed now
    Fix the touch targets 12m
    Review the phone layout failed 12m
    |}]
;;

let%expect_test
    "a subagent's transcript in the chat's place, read-only; Esc goes back"
  =
  let h = H.create () in
  two_agents h;
  H.click h ~selector:".session-card-agent";
  [%expect
    {|
    (Open_subagent_chat a1)
    (Focus chat)
    |}];
  H.text h ~selector:"#chat";
  H.elements h ~selector:".main > *";
  [%expect
    {|
    (New session) /
    Fix the touch targets
    working…
    bash dune build
    <div class="chrome-anchor">
    <div class="h-0 loon relative session-card-anchor z-[5]">
    <Vdom.Node.none-widget class="">
    <div class="chat-frame">
    <div class="toast-anchor">
    <div class="composer flex gap-2 items-center justify-between loon subagent-chat-footer text-muted-foreground text-xs">
    |}];
  H.text h ~selector:".subagent-chat-footer";
  [%expect {| A subagent's transcript: read-only. (Back (Esc)) |}];
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".subagent-chat";
  H.elements h ~selector:".main > footer";
  [%expect
    {|
    Close_subagent_chat
    (Focus editor)
    <footer class="composer">
    |}]
;;

let%expect_test "a transcript not loaded is fetched, and can be asked for again"
  =
  let h = H.create () in
  H.reply
    h
    "list_subagents"
    {|[{"id":"a1","call_id":"s1","parent":null,"task":"Count the modules","model":"deepseek-flash","state":"complete","started_at_ms":1791187201000,"updated_at_ms":0,"ended_at_ms":1791187231000,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":3,"stale":false,"result":{"text":"42 modules","is_error":false}}]|};
  H.act h (Open_subagent_chat "a1");
  H.text h ~selector:"#chat";
  H.fail h "get_subagent" "gone";
  H.text h ~selector:".subagent-chat-note";
  H.click h ~selector:".subagent-chat-note button";
  H.reply
    h
    "get_subagent"
    {|{"subagent":{"id":"a1","call_id":"s1","parent":null,"task":"Count the modules","model":"deepseek-flash","state":"complete","turns":2,"result":{"text":"42 modules","is_error":false}},
       "messages":[{"role":"user","text":"Count the modules"},
                   {"role":"assistant","content":[{"type":"text","text":"There are 42 modules."}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|};
  H.text h ~selector:"#chat";
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    (Focus chat)
    (New session) /
    Count the modules
    1h
    Loading the transcript…
    Report
    42 modules
    Couldn't load the transcript: gone. (Retry)
    (Open_subagent_chat a1)
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    (Focus chat)
    (New session) /
    Count the modules
    1h
    Count the modules
    There are 42 modules.
    |}];
  H.click h ~selector:".subagent-chat-back";
  print_s [%sexp ((H.model h).subagent_chat : string option)];
  [%expect
    {|
    Close_subagent_chat
    (Focus editor)
    ()
    |}]
;;

let%expect_test "switching sessions leaves the subagent's transcript" =
  let h = H.create () in
  two_agents h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a2"));
  print_s [%sexp ((H.model h).subagent_chat : string option)];
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json ~fields:[ "session_id", `String "s2" ] ())));
  print_s [%sexp ((H.model h).subagent_chat : string option)];
  [%expect
    {|
    (a2)
    ()
    |}]
;;

let%expect_test
    "on a phone: no card beside the chat; the ⋯ sheet opens it as a sheet from \
     the bottom, Esc closes it"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  two_agents h;
  H.elements h ~selector:".session-card-anchor";
  H.elements h ~selector:".main";
  H.quiet h (fun () -> H.act h Toggle_status_tray);
  H.click h ~selector:".session-card-row";
  H.text h ~selector:".session-card";
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".session-card";
  [%expect
    {|
    <main class="main">
    Toggle_session_card
    This session
    (Subagents 2)
    (Fix the touch targets working…) (Review the phone layout thinking…)
    (Session_card (Sheet false))
    |}]
;;
