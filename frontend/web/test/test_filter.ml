open! Core
open Prigh_web
module H = Harness

(* The transcript filter (/filter): its button left of the files', its menu,
   the line over the transcript saying what is hidden, and what it hides. *)

let user text = sprintf {|{"role":"user","text":"%s"}|} text

let assistant ?(stop = {|{"type":"end_turn"}|}) content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":%s,"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    (String.concat ~sep:"," content)
    stop
;;

let text s = sprintf {|{"type":"text","text":"%s"}|} s
let thinking s = sprintf {|{"type":"thinking","text":"%s"}|} s

let call ~id ~name args =
  sprintf
    {|{"type":"tool_call","id":"%s","name":"%s","arguments":%s}|}
    id
    name
    (Jsonaf.to_string (`String args))
;;

let result ~id ~name text =
  sprintf
    {|{"role":"tool_result","tool_call_id":"%s","tool_name":"%s","text":"%s","is_error":false}|}
    id
    name
    text
;;

(* A session with one of each kind: the user's message, thinking, a tool
   call, a subagent, a reply, and an interrupted reply (a notice). *)
let session () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ());
    H.reply
      h
      "get_messages_page"
      (H.page
         ("["
          ^ String.concat
              ~sep:","
              [ user "Survey the repository"
              ; assistant
                  ~stop:{|{"type":"tool_use"}|}
                  [ thinking "Let me look first."
                  ; call ~id:"c1" ~name:"bash" {|{"command":"ls"}|}
                  ; call
                      ~id:"s1"
                      ~name:"subagent"
                      {|{"task":"Count the modules"}|}
                  ]
              ; result ~id:"c1" ~name:"bash" "README.md"
              ; result ~id:"s1" ~name:"subagent" "12 modules"
              ; assistant [ text "Twelve modules and a README." ]
              ; user "And the tests?"
              ; assistant ~stop:{|{"type":"aborted"}|} [ text "Looking" ]
              ]
          ^ "]")));
  h
;;

let transcript h = H.text h ~selector:".chat"

let%expect_test "everything shows at first; the button is left of the files'" =
  let h = session () in
  H.names h ~selector:".chat-tools .panel-toggles > button";
  transcript h;
  [%expect
    {|
    button "Filter the transcript" -- Filter the transcript (/filter)
    button "Files" -- Files (Ctrl+Shift+F)
    button "Terminal" -- Terminal (Ctrl+`)
    Survey the repository
    Thought Let me look first.
    Let me look first.
    ✓ bash ls
    README.md
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    Twelve modules and a README.
    And the tests?
    (✂ Trim everything before this)
    Looking
    ■ Interrupted
    |}]
;;

let%expect_test "the menu: a row per kind, checked when shown, then Show all" =
  let h = session () in
  H.click h ~selector:"#filter-toggle";
  [%expect
    {|
    Open_filter
    (Focus dialog)
    (Place_menu (Element filter-toggle))
    |}];
  H.text h ~selector:".filter-menu";
  H.names h ~selector:".filter-menu [role^=menuitem]";
  [%expect
    {|
    Show in the transcript
    ☑
    Your messages what you sent, and the skills you ran
    ☑
    Replies the model's answers
    ☑
    Thinking the model's reasoning
    ☑
    Tool calls commands, edits, searches and their output
    ☑
    Subagents the agents it started, with their work
    ☑
    Notices errors, compactions, background reports
    Show all everything the transcript has
    Space shows or hides · Enter keeps · Esc undoes
    [menuitemcheckbox] div "Your messages what you sent, and the skills you ran"
    [menuitemcheckbox] div "Replies the model's answers"
    [menuitemcheckbox] div "Thinking the model's reasoning"
    [menuitemcheckbox] div "Tool calls commands, edits, searches and their output"
    [menuitemcheckbox] div "Subagents the agents it started, with their work"
    [menuitemcheckbox] div "Notices errors, compactions, background reports"
    [menuitem] div "Show all everything the transcript has"
    |}]
;;

let%expect_test "each kind toggled from the menu, alone" =
  List.iter Prigh_ui.Transcript_filter.Kind.all ~f:(fun kind ->
    let h = session () in
    H.quiet h (fun () ->
      H.act h Open_filter;
      H.act h (Filter_press (Some (Kind kind))));
    printf "== without %s\n" (Prigh_ui.Transcript_filter.Kind.name kind);
    H.names h ~selector:"#filter-toggle";
    transcript h);
  [%expect
    {|
    == without user
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding your messages (/filter)
    Hiding your messages · 2 items hidden (Show all)
    Thought Let me look first.
    Let me look first.
    ✓ bash ls
    README.md
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    Twelve modules and a README.
    Looking
    ■ Interrupted
    == without assistant
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding replies (/filter)
    Hiding replies · 2 items hidden (Show all)
    Survey the repository
    Thought Let me look first.
    Let me look first.
    ✓ bash ls
    README.md
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    And the tests?
    (✂ Trim everything before this)
    ■ Interrupted
    == without thinking
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding thinking (/filter)
    Hiding thinking · 1 item hidden (Show all)
    Survey the repository
    ✓ bash ls
    README.md
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    Twelve modules and a README.
    And the tests?
    (✂ Trim everything before this)
    Looking
    ■ Interrupted
    == without tools
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding tool calls (/filter)
    Hiding tool calls · 1 item hidden (Show all)
    Survey the repository
    Thought Let me look first.
    Let me look first.
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    Twelve modules and a README.
    And the tests?
    (✂ Trim everything before this)
    Looking
    ■ Interrupted
    == without subagents
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding subagents (/filter)
    Hiding subagents · 1 item hidden (Show all)
    Survey the repository
    Thought Let me look first.
    Let me look first.
    ✓ bash ls
    README.md
    Twelve modules and a README.
    And the tests?
    (✂ Trim everything before this)
    Looking
    ■ Interrupted
    == without notes
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding notices (/filter)
    Hiding notices · 1 item hidden (Show all)
    Survey the repository
    Thought Let me look first.
    Let me look first.
    ✓ bash ls
    README.md
    ✓ subagent Count the modules
    Report 12 modules
    12 modules
    Twelve modules and a README.
    And the tests?
    (✂ Trim everything before this)
    Looking
    |}]
;;

let%expect_test
    "the keyboard: arrows choose, Space toggles, Esc undoes, Enter keeps"
  =
  let h = session () in
  H.quiet h (fun () -> H.act h Open_filter);
  let target = Keys.Target.Page in
  H.key h ~target "ArrowDown";
  H.key h ~target "ArrowDown";
  H.key h ~target " ";
  H.text h ~selector:".filter-menu .selected";
  H.text h ~selector:".filter-note";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    (Filter_press ())
    ☐
    Thinking the model's reasoning
    Hiding thinking · 1 item hidden (Show all)
    |}];
  (* Esc puts back the filter it opened with. *)
  H.key h ~target "Escape";
  H.text h ~selector:".filter-note";
  H.names h ~selector:"#filter-toggle";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    button "Filter the transcript" -- Filter the transcript (/filter)
    |}];
  H.quiet h (fun () -> H.act h Open_filter);
  H.key h ~target "ArrowDown";
  H.key h ~target "ArrowDown";
  H.key h ~target " ";
  H.key h ~target "Enter";
  H.text h ~selector:".filter-note";
  H.names h ~selector:"#filter-toggle";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    (Filter_press ())
    Dialog_accept
    (Cache_prefs (((key web.transcript-filter) (value (thinking)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.transcript-filter thinking))))) (tag Show_error))
    (Focus editor)
    Hiding thinking · 1 item hidden (Show all)
    button "Filter the transcript: 1 kind hidden" -- Filter: hiding thinking (/filter)
    |}];
  (* Show all, from the menu's last row. *)
  H.quiet h (fun () -> H.act h Open_filter);
  List.iter (List.init 6 ~f:Fn.id) ~f:(fun _ ->
    H.quiet h (fun () -> H.key h ~target "ArrowDown"));
  H.key h ~target " ";
  H.text h ~selector:".filter-menu .selected";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    (Dialog_move 1)
    (Dialog_move 1)
    (Dialog_move 1)
    (Dialog_move 1)
    (Filter_press ())
    Show all everything the transcript has
    |}];
  H.key h ~target "Enter";
  H.text h ~selector:".filter-note";
  H.names h ~selector:"#filter-toggle";
  [%expect
    {|
    Dialog_accept
    (Cache_prefs (((key web.transcript-filter) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.transcript-filter null)))))
     (tag Show_error))
    (Focus editor)
    button "Filter the transcript" -- Filter the transcript (/filter)
    |}]
;;

let%expect_test "/filter: arguments, an error that says what to type, Show all" =
  let h = session () in
  H.type_ h "/filter -thinking -tools";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/filter -thinking -tools") (max 500)))
     (tag Ignore))
    (Cache_prefs (((key web.transcript-filter) (value (thinking,tools)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.transcript-filter thinking,tools)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 4300))
    |}];
  H.text h ~selector:".toasts";
  H.text h ~selector:".filter-note";
  [%expect
    {|
    Transcript filter: −thinking −tools (/filter all shows everything)
    Hiding thinking, tool calls · 2 items hidden (Show all)
    |}];
  H.type_ h "/filter -thinkng";
  H.key h "Enter";
  H.text h ~selector:".toasts";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/filter -thinkng") (max 500))) (tag Ignore))
    Transcript filter: −thinking −tools (/filter all shows everything)
    Unknown kind “thinkng”. Did you mean thinking? Use all, or kinds (user, assistant, thinking, tools, subagents, notes) to show only those, -KIND to hide one, +KIND to show it. (Dismiss)
    |}];
  H.click h ~selector:".filter-show-all";
  H.text h ~selector:".filter-note";
  H.names h ~selector:"#filter-toggle";
  [%expect
    {|
    (Run "/filter all")
    (Cache_prefs (((key web.transcript-filter) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.transcript-filter null)))))
     (tag Show_error))
    (Expire_toast (id 2) (after_ms 2980))
    button "Filter the transcript" -- Filter the transcript (/filter)
    |}]
;;

let%expect_test
    "hiding your messages keeps the rail's marks and their places to jump to"
  =
  let h = session () in
  H.quiet h (fun () -> H.act h (Run "/filter -user"));
  H.elements h ~selector:".entries > [data-entry]";
  H.names h ~selector:".jump-nav button";
  [%expect
    {|
    <div class="filtered">
    <div class="filtered">
    button "Jump to a message" -- Jump to a message (Ctrl+J)
    button "Your message 1, Survey the repository"
    button "Your message 2, And the tests?"
    |}]
;;

let%expect_test
    "the filter is a preference: kept, restored and kept across sessions"
  =
  let h = session () in
  H.act h (Run "/filter -thinking");
  [%expect
    {|
    (Cache_prefs (((key web.transcript-filter) (value (thinking)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.transcript-filter thinking))))) (tag Show_error))
    (Expire_toast (id 0) (after_ms 3940))
    |}];
  (* Changed in another browser. *)
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.transcript-filter":"tools,notes"}}|};
  H.names h ~selector:"#filter-toggle";
  [%expect
    {|
    (Cache_prefs (((key web.transcript-filter) (value (tools,notes)))))
    button "Filter the transcript: 2 kinds hidden" -- Filter: hiding tool calls, notices (/filter)
    |}];
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s3.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s3" ] ());
    H.reply h "get_messages_page" (H.page ("[" ^ user "hello" ^ "]")));
  H.names h ~selector:"#filter-toggle";
  H.text h ~selector:".filter-note";
  [%expect
    {|
    button "Filter the transcript: 2 kinds hidden" -- Filter: hiding tool calls, notices (/filter)
    Hiding tool calls, notices: nothing loaded is hidden (Show all)
    |}]
;;
