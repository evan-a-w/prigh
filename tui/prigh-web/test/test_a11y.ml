open! Core
open! Prigh_web
module H = Harness

let names ?(within = "") h =
  List.iter [ "button"; "a"; "input"; "textarea"; "select"; "[role]" ] ~f:(fun selector ->
    H.names ~selector:(within ^ selector) h)
;;

let sessions =
  sprintf
    "[%s,%s]"
    (H.session_json ~name:"Fix the build" "s1")
    (H.session_json ~name:"Old spike" ~archived:true "s2")
;;

(* Every control is named by what it does (its text, else its action), the
   tooltip giving the hint (its key, the command). *)
let%expect_test "the names of the controls" =
  let h = H.create ~sessions () in
  names h;
  [%expect {|
    button "Hide sidebar" -- Hide sidebar (Ctrl+B)
    button "New session" -- Start a new session (/new)
    button "Select" -- Select sessions to delete (/delete)
    button "Fix the build 2m ago /work" -- /sessions/s1.jsonl
    button "Archive Fix the build" -- Archive (A)
    button "Delete Fix the build" -- Delete session (Delete)
    button "Archived 1" -- Show the archived sessions
    button "Commands & keys" -- Commands and keys (/help)
    button "Sessions" -- Sessions (Ctrl+B)
    button "New session" -- Rename (/name)
    button "Model, thinking and approvals" -- Claude Opus 5.5, thinking on, auto: never asks
    button "Claude Opus 5.5" -- Claude Opus 5.5: switch model (Ctrl+L)
    button "Thinking on" -- Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)
    button "Auto: never asks" -- Tools run without asking (Alt+M cycles, /mode picks)
    button "Files" -- Files (Ctrl+Shift+F)
    button "Terminal" -- Terminal (Ctrl+`)
    button "Attach files" -- Attach files (or drop them here)
    button "Send" -- Send (Enter)
    button "Notifications not allowed yet" -- Notifications not allowed yet: click to ask the browser
    input "Search sessions"
    textarea "Message"
    [separator] div "Resize the sessions sidebar" -- Drag to resize, double-click for the default size
    [region] div "Transcript"
    [status] span "Ready"
    |}];
  H.quiet h (fun () -> H.act h (Select_sessions true));
  names ~within:".sidebar " h;
  [%expect {|
    button "Hide sidebar" -- Hide sidebar (Ctrl+B)
    button "New session" -- Start a new session (/new)
    [checkbox] button "Fix the build 2m ago /work 0 B" -- /sessions/s1.jsonl
    button "Archived 1" -- Show the archived sessions
    button "Cancel" -- Stop selecting (Esc)
    button "Delete sessions" -- Delete the selected sessions (Delete)
    button "Commands & keys" -- Commands and keys (/help)
    input "Search sessions"
    [checkbox] button "Fix the build 2m ago /work 0 B" -- /sessions/s1.jsonl
    |}]
;;

let%expect_test "the names of the panels' controls" =
  let h = H.create ~sessions () in
  H.quiet h (fun () ->
    H.act h (Files (Set_base "http://box:7790"));
    H.act h (Files Toggle);
    H.reply
      h
      "fs_list"
      {|{"path":"/work","entries":[{"name":"src","kind":"dir","mtime":0},{"name":"README.md","kind":"file","size":2048,"mtime":0}],"parent":"/","home":"/work","total":2,"hidden":0}|};
    H.act h Toggle_subagents;
    H.act h Toggle_terminal);
  names ~within:".files-panel " h;
  names ~within:".agents-panel " h;
  names ~within:".terminal-panel " h;
  [%expect {|
    button "Up" -- Up (Backspace)
    button "Refresh"
    button "Show dot files"
    button "New folder"
    button "Upload files here"
    button "Close files" -- Close (Ctrl+Shift+F)
    button "(title) Type a path" -- Type a path
    button "Name \226\134\145" -- Sort by name
    button "Size" -- Sort by size
    button "Modified" -- Sort by modified
    [separator] div "Resize the file browser" -- Drag to resize, double-click for the default size
    [row] div "Name \226\134\145 Size Modified"
    [listbox] div "Files"
    [option] div "src/ 1 Jan 1970 00:00"
    [option] div "README.md 2.0 KB 1 Jan 1970 00:00"
    button "(title) Close (Alt+0)" -- Close (Alt+0)
    [separator] div "Resize the agents panel" -- Drag to resize, double-click for the default size
    button "Close terminal" -- Close (Ctrl+`): the shell keeps running for 10 minutes
    [separator] div "Resize the terminal" -- Drag to resize, double-click for the default size
    |}]
;;

let%expect_test "the names in dialogs, pickers and the transcript" =
  let h = H.create ~sessions () in
  H.quiet h (fun () ->
    H.event
      h
      {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Counting."},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"seq 30\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}}|};
    H.event
      h
      {|{"event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"seq 30\"}"}}|};
    H.event
      h
      (sprintf
         {|{"event":"tool_output","call_id":"c1","chunk":%S}|}
         (String.concat (List.init 30 ~f:(sprintf "%d\n"))));
    H.event
      h
      (sprintf
         {|{"event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":%S,"is_error":false}}|}
         (String.concat (List.init 30 ~f:(sprintf "%d\n"))));
    H.act
      h
      (Add_image { Prigh_protocol.Image.mime_type = "image/png"; data = "iVBORw0KGgo="; bytes = 8 });
    H.act h (Show_toast { text = "Couldn't do that"; error = true }));
  names ~within:"#chat " h;
  names ~within:".composer " h;
  names ~within:".toasts " h;
  [%expect {|
    button "Show all 30 lines"
    button "Remove image"
    button "Attach files" -- Attach files (or drop them here)
    button "Send" -- Send (Enter)
    button "Notifications not allowed yet" -- Notifications not allowed yet: click to ask the browser
    textarea "Message"
    [status] span "Ready"
    button "Dismiss"
    [alert] div "Couldn't do that" -- Dismiss
    |}];
  H.quiet h (fun () ->
    H.event
      h
      {|{"event":"tool_confirm","call_id":"c2","agent_id":null,"name":"bash","summary":"rm -rf build","rules":[]}|});
  names ~within:".modal " h;
  [%expect {|
    button "Close" -- Close (Esc)
    button "Allow once" -- y or Enter
    button "Deny" -- n or Esc
    button "Deny, saying why" -- r
    |}];
  H.quiet h (fun () -> H.act h Open_model_picker);
  names ~within:".modal " h;
  [%expect {|
    button "Close" -- Close (Esc)
    button "Close" -- Close (Esc)
    button "Allow once" -- y or Enter
    button "Deny" -- n or Esc
    button "Deny, saying why" -- r
    [combobox] input "Filter models"
    [combobox] input "Filter models"
    [listbox] div "Switch model"
    [option] div "Claude Opus 5.5 anthropic, 200k ctx \226\156\147"
    [option] div "Claude Sonnet 5 anthropic, 200k ctx"
    [option] div "GPT-6 openai, 200k ctx, not logged in"
    [option] div "DeepSeek Chat deepseek, 200k ctx, not logged in"
    |}]
;;

(* A phone's closed drawer is moved out of sight: Tab must not reach it
   (nor a screen reader). *)
let%expect_test "the closed drawer on a phone is inert" =
  let h = H.create ~sessions () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".sidebar[inert]";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.elements h ~selector:".sidebar[inert]";
  H.elements h ~selector:".main > [inert]";
  [%expect {|
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat verbosity-normal">
    <footer class="composer">
    |}]
;;
