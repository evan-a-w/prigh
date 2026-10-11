open! Core
open! Prigh_web
module H = Harness

(* The harness's additions to the conversation (environment notes, attached
   files, hand-overs, goals, /review, ...) as margin notes rather than the
   user's words, and the commands that look at the working tree: /diff,
   /review and /init. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let host_note =
  "[Environment: the tool host is now laptop, so tools run there and its \
   filesystem may differ from the one described above.]"
;;

let noted =
  sprintf
    {|{"type":"event","event":"message_start","message":{"role":"user","text":%s,"origin":{"body":"fix the build","notes":[{"kind":"host","title":"Tools now run on laptop","text":%s}],"attachments":[{"path":"Makefile"}]},"at":1791194280000}}|}
    (Jsonaf.to_string
       (`String
           (host_note
            ^ "\n\nfix the build\n\n<file path=\"Makefile\">\nall:\n</file>")))
    (Jsonaf.to_string (`String host_note))
;;

let source ~kind ~title text =
  sprintf
    {|{"type":"event","event":"message_start","message":{"role":"user","text":%s,"origin":{"source":{"kind":"%s","title":"%s"}}}}|}
    (Jsonaf.to_string (`String text))
    kind
    title
;;

let%expect_test
    "notes stand in the margin above the prompt, which shows only what the \
     user wrote"
  =
  let chat = Chat_harness.chat [ noted ] in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <div class="msg noted">
        <details data-kind="host" class="note">
          <summary>
            <span aria-hidden="true" class="glyph"> ⇄ </span>
            <span class="title"> Tools now run on laptop </span>
          </summary>
          <pre class="note-text">
            [Environment: the tool host is now laptop, so tools run there and its filesystem may differ from the one described above.]
          </pre>
        </details>
        <div class="msg user">
          <div class="bubble"> fix the build </div>
          <div class="attachments">
            <span title="Attached file" class="attachment"> @Makefile </span>
          </div>
          <time title="Monday 5 October 2026, 11:58:00" datetime="2026-10-05T09:58:00Z" class="time"> 11:58 </time>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "tools a model change added or removed, also an older backend's"
  =
  let notes kind title text =
    sprintf
      {|{"type":"event","event":"message_start","message":{"role":"user","text":%s,"origin":{"body":"go on","notes":[{"kind":"%s","title":"%s","text":%s}]}}}|}
      (Jsonaf.to_string (`String (text ^ "\n\ngo on")))
      kind
      title
      (Jsonaf.to_string (`String text))
  in
  let chat =
    Chat_harness.chat
      [ notes
          "tools"
          "Tools changed: web_search removed"
          "[Tools: the tools changed with the model. No longer available: \
           web_search.]"
      ; notes
          "editor"
          "File edits now use edit instead of apply_patch"
          "[Tools: edit replaces apply_patch for editing files; apply_patch is \
           no longer available.]"
      ]
  in
  Chat_harness.show ~selector:"details.note summary" chat;
  [%expect {|
    <summary>
      <span aria-hidden="true" class="glyph"> ± </span>
      <span class="title"> Tools changed: web_search removed </span>
    </summary>
    <summary>
      <span aria-hidden="true" class="glyph"> ± </span>
      <span class="title"> File edits now use edit instead of apply_patch </span>
    </summary>
    |}]
;;

let%expect_test
    "the harness's own messages are notes, the summary is the compaction fold"
  =
  let chat =
    Chat_harness.chat
      [ source
          ~kind:"goal_continuation"
          ~title:"Goal: continuing (3/20)"
          "[prigh goal: continuation 3 of 20] Continue working toward the goal"
      ; source
          ~kind:"resume"
          ~title:"Usage limit reset: carrying on"
          "[prigh: the usage limit has reset.]"
      ; source
          ~kind:"review"
          ~title:"Review of changes against main"
          "[prigh review] Review the changes"
      ; source
          ~kind:"init"
          ~title:"Write or improve AGENTS.md"
          "[prigh init] Write an AGENTS.md"
      ; source
          ~kind:"shell"
          ~title:"Output of $ ls added to the context"
          "$ ls\na.ml"
      ; source
          ~kind:"summary"
          ~title:"Summary of the earlier conversation"
          "Summary of the conversation so far:\nWe fixed **the parser**."
      ]
  in
  Chat_harness.text chat;
  [%expect
    {|
    ◷ Goal: continuing (3/20) [prigh goal: continuation 3 of 20] Continue working toward the goal ↻ Usage limit reset: carrying on [prigh: the usage limit has reset.] ⚑ Review of changes against main [prigh review] Review the changes ▤ Write or improve AGENTS.md [prigh init] Write an AGENTS.md $ Output of $ ls added to the context $ ls
    a.ml Context compacted We fixed the parser. We fixed the parser .
    |}]
;;

let%expect_test "/fork and /tree label the harness's messages by their notes" =
  let h = H.create () in
  let message json =
    (* The message of an event, as an entry carries it. *)
    match Jsonaf.of_string json with
    | `Object fields ->
      Jsonaf.to_string
        (List.Assoc.find_exn fields "message" ~equal:String.equal)
    | _ -> assert false
  in
  run h "/fork";
  H.reply
    h
    "get_entries"
    (sprintf
       {|{"head":"e2","entries":[{"id":"e1","parent":null,"kind":"message","message":%s},{"id":"e2","parent":"e1","kind":"message","message":%s}]}|}
       (message noted)
       (message
          (source
             ~kind:"review"
             ~title:"Review of uncommitted changes"
             "[prigh review] …")));
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/fork))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    fix the build #1
    ⚑ Review of uncommitted changes #2 ✓
    |}]
;;

let diff_json =
  {|{"root":"/work","against":"HEAD","truncated":false,"diff":"diff --git a/src/a.ml b/src/a.ml\nindex 1..2 100644\n--- a/src/a.ml\n+++ b/src/a.ml\n@@ -1,2 +1,2 @@\n let x = 1\n-let y = 2\n+let y = 3\ndiff --git a/notes.txt b/notes.txt\nnew file mode 100644\nindex 0..1\n--- /dev/null\n+++ b/notes.txt\n@@ -0,0 +1 @@\n+hello\n"}|}
;;

let%expect_test
    "/diff opens the changes file by file; none, or not a repository, is a \
     toast"
  =
  let h = H.create () in
  run h "/diff";
  H.reply h "git_diff" diff_json;
  H.show h ~selector:".diff-dialog";
  [%expect
    {|
    (Save_history (/diff))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ git_diff) (params ()) (tag Git_diff))
    (Focus dialog)
    <div id="dialog"
         role="dialog"
         aria-modal="true"
         aria-label="Changes"
         tabindex="-1"
         class="diff-dialog modal">
      <header class="modal-head">
        <h2> Changes </h2>
        <button type="button"
                title="Close (Esc)"
                aria-label="Close (Esc)"
                class="btn ghost icon"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </header>
      <div class="modal-body">
        <p class="dialog-note">
          <span class="root"> /work </span>
           against HEAD
          <span class="summary"> 2 files changed, +2 −1 </span>
        </p>
        <details class="diff-file" #open="true">
          <summary>
            <span class="path"> src/a.ml </span>
            <span class="add count"> +1 </span>
            <span class="count del"> −1 </span>
          </summary>
          <div class="diff-view">
            <table class="diff">
              <tbody>
                <tr class="hunk">
                  <td colspan="4"> @@ -1,2 +1,2 @@ </td>
                </tr>
                <tr class="same">
                  <td class="ln"> 1 </td>
                  <td class="ln"> 1 </td>
                  <td class="sign">   </td>
                  <td class="text">
                    <span class="hl-kw"> let </span>
                     x =
                    <span class="hl-num"> 1 </span>
                  </td>
                </tr>
                <tr class="del">
                  <td class="ln"> 2 </td>
                  <td class="ln">  </td>
                  <td class="sign"> - </td>
                  <td class="text">
                    <span class="hl-kw"> let </span>
                     y =
                    <span class="hl-num"> 2 </span>
                  </td>
                </tr>
                <tr class="add">
                  <td class="ln">  </td>
                  <td class="ln"> 2 </td>
                  <td class="sign"> + </td>
                  <td class="text">
                    <span class="hl-kw"> let </span>
                     y =
                    <span class="hl-num"> 3 </span>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </details>
        <details class="diff-file" #open="true">
          <summary>
            <span class="path"> notes.txt </span>
            <span class="chip"> new </span>
            <span class="add count"> +1 </span>
            <span class="count del"> −0 </span>
          </summary>
          <div class="diff-view">
            <table class="diff">
              <tbody>
                <tr class="hunk">
                  <td colspan="4"> @@ -0,0 +1 @@ </td>
                </tr>
                <tr class="add">
                  <td class="ln">  </td>
                  <td class="ln"> 1 </td>
                  <td class="sign"> + </td>
                  <td class="text"> hello </td>
                </tr>
              </tbody>
            </table>
          </div>
        </details>
      </div>
      <footer class="modal-buttons">
        <span class="keys-hint"> /diff PATH shows one file or directory </span>
        <button type="button" class="btn primary" @on_click> Done </button>
      </footer>
    </div>
    |}];
  H.act h Close_dialog;
  run h "/diff src";
  H.reply
    h
    "git_diff"
    {|{"root":"/work","against":"HEAD","truncated":false,"diff":""}|};
  run h "/diff";
  H.fail
    h
    "git_diff"
    "not a git repository: /work (git init starts one, or /cd into one)";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ list_paths) (params ((prefix src))) (tag (Paths src)))
    (Save_history ("/diff src" /diff))
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ git_diff) (params ((path src))) (tag Git_diff))
    (Expire_toast (id 2) (after_ms 2780))
    (Save_history (/diff "/diff src" /diff))
    (Expire_toast (id 3) (after_ms 2500))
    (Rpc (method_ git_diff) (params ()) (tag Git_diff))
    No changes in /work against HEAD
    Not a git repository: /work (git init starts one, or /cd into one)
    |}]
;;

let%expect_test
    "/review and /init: refused ones go back to the editor; a busy agent \
     queues them"
  =
  let h = H.create () in
  run h "/review main look at errors";
  H.fail h "review" "nothing to review: no changes against main in /work";
  H.text h ~selector:".toast";
  H.text h ~selector:".composer";
  [%expect
    {|
    (Save_history ("/review main look at errors"))
    (Expire_toast (id 0) (after_ms 2780))
    (Rpc (method_ review) (params ((args "main look at errors")))
     (tag (Command_prompt "/review main look at errors")))
    Nothing to review: no changes against main in /work. /review main look at errors is back in the editor.
    [/review main look at errors]
    (Attach files (or drop them here)) (Send (Enter))
    Ready 0% ↑0 ↓0 $0.0000
    (Notifications not allowed yet: click to ask the browser)
    |}];
  H.quiet h (fun () -> H.act h (Set_draft ""));
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "running", `True ] ()));
  run h "/init";
  H.reply h "init" "{}";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (/init "/review main look at errors"))
    (Rpc (method_ init) (params ()) (tag (Command_prompt /init)))
    (Expire_toast (id 2) (after_ms 3220))
    Queued: it starts when the current run ends
    |}]
;;

let%expect_test "/diff, /review and /init are in the completions and /help" =
  let h = H.create () in
  H.type_ h "/di";
  H.text h ~selector:".popup";
  H.type_ h "/i";
  H.text h ~selector:".popup";
  [%expect
    {|
    Commands ↑↓ Tab Enter Esc
    /diff [path] show the uncommitted changes where the tools run, untracked files too
    /default-dir [path|off] show or set the directory new sessions start in
    /delete-archived delete every archived session, after a confirmation
    /retry-backend-connection reconnect to the backend now
    Commands ↑↓ Tab Enter Esc
    /init write or improve the project's AGENTS.md
    /import [path] import a session from a JSONL file on the backend
    /diff [path] show the uncommitted changes where the tools run, untracked files too
    /quit how to leave (close the tab; /signout signs out)
    /files [path] browse, preview, upload and download files where the tools run (Ctrl+Shift+F)
    /login [provider] log in to a model provider (or /login custom)
    /notify [on|off] tell you when a turn ends or something waits for you while the tab is hidden
    /review [base] [instructions] review the uncommitted changes, or those against a base branch, for prioritised findings
    /switch [path] switch to a saved session
    /rewind go back to an earlier message in this session
    /skills pick a skill to invoke (/skill:name)
    /skill:<name> [args] send a skill's instructions to the agent, with your arguments
    /confirm [on|off] ask before bash, job_input, edits and MCP tools run (mode ask or auto, also for new sessions)
    /session show the session's details and statistics
    /archive [session] archive a session (this one without an argument): it moves under Archived in the sidebar
    /signout sign out of this account
    /thinking [off|low|on|high|max] pick or set the thinking level
    /sessions search the saved sessions
    /terminal open a shell where the tools run, in the session's directory (Ctrl+`)
    /verbosity [quiet|normal|verbose] how much of tool calls and thinking the transcript shows
    /unarchive [session] unarchive a session (this one without an argument)
    /permissions show the mode and the allow/ask/deny rules (config and this session)
    /default-dir [path|off] show or set the directory new sessions start in
    /delete-archived delete every archived session, after a confirmation
    /retry-backend-connection reconnect to the backend now
    |}]
;;

let%expect_test
    "notifications: asked for from /notify on, raised only while the tab is \
     hidden, counted in the title until it is seen"
  =
  let h = H.create () in
  H.act h (Set_notifications { enabled = true; permission = "default" });
  run h "/notify";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (/notify))
    (Expire_toast (id 0) (after_ms 4700))
    Notifications are on, but the browser has not been asked yet: /notify on asks it
    |}];
  run h "/notify on";
  H.act h (Notification_permission "granted");
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history ("/notify on" /notify))
    (Save_notifications true)
    Request_notification_permission
    (Expire_toast (id 1) (after_ms 6000))
    Notifications are on, but the browser has not been asked yet: /notify on asks it
    Notifications are on: the browser tells you when a turn ends, a tool waits for you or background work finishes while this tab is hidden
    |}];
  let finish () =
    H.event
      h
      {|{"event":"agent_end","messages":[{"role":"assistant","content":[{"type":"text","text":"All tests pass."}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":1,"cache_read":0},"model":"m"}]}|}
  in
  (* Seen: nothing. *)
  finish ();
  [%expect {| |}];
  H.act h (Page_hidden true);
  finish ();
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c1","name":"bash","summary":"rm -rf build"}|};
  H.event
    h
    {|{"event":"ask_user","call_id":"q1","questions":[{"question":"Which database?","options":[{"label":"SQLite"}]}]}|};
  [%expect
    {|
    (Set_title "(1) prigh")
    (Notify ((title "prigh: done in work") (body "All tests pass.")))
    (Focus confirm)
    (Set_title "(2) prigh")
    (Notify
     ((title "prigh: waiting for you in work")
      (body "Allow bash: rm -rf build?")))
    (Set_title "(3) prigh")
    (Notify
     ((title "prigh: a question for you in work") (body "Which database?")))
    |}];
  H.act h (Page_hidden false);
  [%expect {|
    (Set_title prigh)
    Probe_connection
    |}];
  (* Off: the title still counts, no notification. *)
  run h "/notify off";
  H.act h (Page_hidden true);
  finish ();
  H.act h (Page_hidden false);
  [%expect
    {|
    (Save_history ("/notify off" "/notify on" /notify))
    (Save_notifications false)
    (Expire_toast (id 2) (after_ms 3220))
    (Set_title "(1) prigh")
    (Set_title prigh)
    Probe_connection
    |}]
;;

let%expect_test
    "notifications the browser refuses say how to allow them; the account menu \
     turns them on and off"
  =
  let h = H.create () in
  H.act h (Set_notifications { enabled = false; permission = "denied" });
  H.act h Open_accounts;
  H.act h (Picker_choose "notify on");
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus dialog)
    (Focus editor)
    (Focus editor)
    (Save_notifications true)
    The browser blocks notifications for this page: allow them in its site settings (the icon left of the address), then /notify on
    |}];
  H.act h (Set_notifications { enabled = true; permission = "unsupported" });
  run h "/notify";
  run h "/notify loud";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (/notify))
    (Expire_toast (id 1) (after_ms 6000))
    (Save_history ("/notify loud" /notify))
    This browser cannot show notifications here (they need https or localhost); the tab's title still counts what happened
    Unknown /notify argument "loud": /notify on or /notify off (/notify alone says what is on)
    |}]
;;

let%expect_test
    "the status line shows whether notifications are on; its bell turns them \
     on and off"
  =
  let h = H.create () in
  let bell () =
    H.elements h ~selector:".status .notify";
    H.text h ~selector:".status .notify"
  in
  H.act h (Set_notifications { enabled = true; permission = "granted" });
  bell ();
  [%expect
    {|
    <button class="link notify on status-item">
    (Notifications on (/notify off))
    |}];
  H.quiet h (fun () -> H.click h ~selector:".status .notify");
  bell ();
  [%expect
    {|
    (Run "/notify off")
    <button class="link notify off status-item">
    (Notifications off (/notify on))
    |}];
  H.click h ~selector:".status .notify";
  bell ();
  [%expect
    {|
    (Run "/notify on")
    (Save_notifications true)
    (Expire_toast (id 1) (after_ms 6000))
    <button class="link notify on status-item">
    (Notifications on (/notify off))
    |}];
  H.act h (Set_notifications { enabled = true; permission = "denied" });
  bell ();
  [%expect
    {|
    <button class="link notify status-item warm">
    (Notifications blocked by the browser: click for how to allow them)
    |}];
  H.act h (Set_notifications { enabled = true; permission = "unsupported" });
  bell ();
  [%expect
    {|
    <button class="link notify off status-item">
    (Notifications need https or localhost)
    |}]
;;
