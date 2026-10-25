open! Core
open Prigh_web
module H = Harness
module Handle = Resize.Handle

(* Every panel resizes from a handle on its inner edge: by dragging (the page
   commits [Set] on release), with the arrow keys on the focused handle, or
   back to its default with a double click or Enter. Sizes are kept in the
   browser and fit the window. *)

(* The page's sizes ([.app]'s CSS variables) and the handles shown, with
   their aria values. *)
let sizes h =
  let m = H.model h in
  let shown = App.Model.shown m in
  print_endline
    (String.concat
       ~sep:" "
       (List.map (Resize.vars m.layout shown) ~f:(fun (var, value) ->
          sprintf "%s=%s" (String.chop_prefix_exn var ~prefix:"--") value)));
  H.elements h ~selector:".resizer";
  List.iter Handle.all ~f:(fun handle ->
    let b = Resize.bounds m.layout shown handle in
    printf "%s %d [%d, %d]\n" (Handle.name handle) b.value b.min b.max)
;;

let hosts =
  {|[{"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null}]|}
;;

let state =
  H.state_json
    ~fields:
      [ "active_host", `String "c7"
      ; "cwd", `String "/home/me/proj"
      ; "hosts", Jsonaf.of_string hosts
      ]
    ()
;;

let viewport h width height = H.act h (Resize (Viewport { width; height }))

let%expect_test "a handle: a focusable separator with its size and bounds" =
  let h = H.create () in
  viewport h 1440 900;
  H.show h ~selector:".resizer-sidebar";
  [%expect
    {|
    <div role="separator"
         tabindex="0"
         aria-label="Resize the sessions sidebar"
         aria-orientation="vertical"
         aria-valuenow="280"
         aria-valuemin="200"
         aria-valuemax="520"
         title="Drag to resize, double-click for the default size"
         data-resize="sidebar"
         data-resize-var="--sidebar-width"
         data-resize-unit="px"
         data-resize-grows="1"
         data-resize-target=".sidebar"
         class="resizer resizer-sidebar"
         @on_dblclick> </div>
    |}];
  sizes h;
  [%expect
    {|
    sidebar-width=280px files-width=400px files-split=45% agents-width=420px terminal-height=342px
    <div class="resizer resizer-sidebar">
    sidebar 280 [200, 520]
    files 400 [280, 1200]
    files-split 45 [20, 80]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    |}]
;;

let%expect_test "the arrow keys on a handle resize its panel and keep the size" =
  let h = H.create () in
  viewport h 1440 900;
  let key ?shift k = H.key h ?shift ~target:(Resize Sidebar) k in
  key "ArrowRight";
  key "ArrowRight" ~shift:true;
  key "ArrowLeft";
  (* Up and down are not along a vertical edge. *)
  key "ArrowUp";
  key "End";
  key "Home";
  key "Home";
  key "Enter";
  key "Enter";
  [%expect
    {|
    (Resize (Nudge Sidebar 1))
    (Cache_prefs (((key web.sidebar-width) (value (296)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 296)))))
     (tag Show_error))
    (Resize (Nudge Sidebar 4))
    (Cache_prefs (((key web.sidebar-width) (value (360)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 360)))))
     (tag Show_error))
    (Resize (Nudge Sidebar -1))
    (Cache_prefs (((key web.sidebar-width) (value (344)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 344)))))
     (tag Show_error))
    (browser default)
    (Resize (Largest Sidebar))
    (Cache_prefs (((key web.sidebar-width) (value (520)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 520)))))
     (tag Show_error))
    (Resize (Smallest Sidebar))
    (Cache_prefs (((key web.sidebar-width) (value (200)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 200)))))
     (tag Show_error))
    (Resize (Smallest Sidebar))
    (Resize (Reset Sidebar))
    (Cache_prefs (((key web.sidebar-width) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width null)))))
     (tag Show_error))
    (Resize (Reset Sidebar))
    |}];
  (* The file browser's edge is on its left: left makes it wider. *)
  H.quiet h (fun () -> H.act h (Files Toggle));
  H.key h ~target:(Resize Files) "ArrowLeft";
  H.key h ~target:(Resize Files) "ArrowRight" ~shift:true;
  H.key h ~target:(Resize Files) "ArrowDown";
  H.key h ~target:(Resize Files) "a";
  [%expect
    {|
    (Resize (Nudge Files 1))
    (Cache_prefs (((key web.files-width) (value (416)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-width 416)))))
     (tag Show_error))
    (Resize (Nudge Files -4))
    (Cache_prefs (((key web.files-width) (value (352)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-width 352)))))
     (tag Show_error))
    (browser default)
    (browser default)
    |}];
  sizes h;
  [%expect
    {|
    sidebar-width=280px files-width=352px files-split=45% agents-width=420px terminal-height=342px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    sidebar 280 [200, 520]
    files 352 [280, 760]
    files-split 45 [20, 80]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    |}]
;;

let%expect_test "the kept sizes come back; dragging commits once, clamped" =
  let h = H.create () in
  viewport h 1440 900;
  H.act
    h
    (Resize
       (Restore
          [ Sidebar, Some 333
          ; Files, Some 5000
          ; Agents, Some 10
          ; Terminal, Some 2000
          ; Files_split, Some 60
          ]));
  [%expect {| |}];
  sizes h;
  [%expect
    {|
    sidebar-width=333px files-width=1200px files-split=60% agents-width=300px terminal-height=640px
    <div class="resizer resizer-sidebar">
    sidebar 333 [200, 520]
    files 1200 [280, 1200]
    files-split 60 [20, 80]
    agents 300 [300, 1200]
    terminal 640 [120, 640]
    |}];
  H.act h (Resize (Set (Sidebar, 900)));
  H.act h (Resize (Set (Sidebar, 520)));
  H.act h (Resize (Set (Terminal, 50)));
  [%expect
    {|
    (Cache_prefs (((key web.sidebar-width) (value (520)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.sidebar-width 520)))))
     (tag Show_error))
    (Cache_prefs (((key web.terminal-height) (value (120)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height 120)))))
     (tag Show_error))
    |}];
  H.act h (Resize (Reset Terminal));
  H.act h (Resize (Reset Terminal));
  [%expect
    {|
    (Cache_prefs (((key web.terminal-height) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height null)))))
     (tag Show_error))
    |}]
;;

let%expect_test "docked panels shrink with the window, keeping the chat usable" =
  let h = H.create ~state () in
  viewport h 1600 900;
  H.quiet h (fun () ->
    H.act h (Files Toggle);
    H.act h Toggle_subagents);
  H.act h (Resize (Restore [ Files, Some 600; Agents, Some 500 ]));
  sizes h;
  [%expect
    {|
    sidebar-width=256px files-width=504px files-split=45% agents-width=440px terminal-height=342px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 256 [200, 256]
    files 504 [280, 504]
    files-split 45 [20, 80]
    agents 440 [300, 440]
    terminal 342 [120, 640]
    |}];
  (* Narrower: the side panels give up width in proportion, down to their
     minimums, so that the chat keeps 400px. *)
  viewport h 1300 900;
  sizes h;
  [%expect
    {|
    sidebar-width=216px files-width=344px files-split=45% agents-width=340px terminal-height=342px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 216 [200, 216]
    files 344 [280, 344]
    files-split 45 [20, 80]
    agents 340 [300, 340]
    terminal 342 [120, 640]
    |}];
  (* The agents panel floats over the chat below 1180px, the file browser
     below 1100px: at most 86% of the window. *)
  viewport h 1150 700;
  sizes h;
  [%expect
    {|
    sidebar-width=254px files-width=496px files-split=45% agents-width=500px terminal-height=266px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 254 [200, 254]
    files 496 [280, 496]
    files-split 45 [20, 80]
    agents 500 [300, 989]
    terminal 266 [120, 440]
    |}];
  (* Wide again: back to what was chosen. *)
  viewport h 1600 900;
  sizes h;
  [%expect
    {|
    sidebar-width=256px files-width=504px files-split=45% agents-width=440px terminal-height=342px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 256 [200, 256]
    files 504 [280, 504]
    files-split 45 [20, 80]
    agents 440 [300, 440]
    terminal 342 [120, 640]
    |}]
;;

let%expect_test "the file list and the preview split; the terminal's top edge" =
  let h = H.create ~state () in
  viewport h 1440 900;
  H.quiet h (fun () ->
    H.act h (Files Toggle);
    H.reply
      h
      "fs_list"
      {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
          {"name":"main.ml","kind":"file","size":20,"mtime":1791100000000}],
        "total":1,"hidden":0}|});
  H.elements h ~selector:".resizer";
  [%expect
    {|
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    |}];
  H.quiet h (fun () ->
    H.click h ~selector:".files-row[data-name='main.ml']";
    H.reply
      h
      "fs_preview"
      {|{"path":"/home/me/proj/main.ml","name":"main.ml","size":20,"mtime":null,"kind":"text","text":"let x = 1\n","truncated":false}|});
  H.elements h ~selector:".files-body > *";
  H.key h ~target:(Resize Files_split) "ArrowDown";
  H.key h ~target:(Resize Files_split) "ArrowUp" ~shift:true;
  H.key h ~target:(Resize Files_split) "ArrowLeft";
  [%expect
    {|
    (Files (Select main.ml))
    <div class="files-browse">
    <div class="resizer resizer-files-split">
    <section class="files-preview">
    (Resize (Nudge Files_split 1))
    (Cache_prefs (((key web.files-split) (value (50)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-split 50)))))
     (tag Show_error))
    (Resize (Nudge Files_split -4))
    (Cache_prefs (((key web.files-split) (value (30)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-split 30)))))
     (tag Show_error))
    (browser default)
    |}];
  H.quiet h (fun () -> H.act h Toggle_terminal);
  H.key h ~target:(Resize Terminal) "ArrowUp";
  H.key h ~target:(Resize Terminal) "End";
  [%expect
    {|
    (Resize (Nudge Terminal 1))
    (Cache_prefs (((key web.terminal-height) (value (358)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height 358)))))
     (tag Show_error))
    (Resize (Largest Terminal))
    (Cache_prefs (((key web.terminal-height) (value (640)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.terminal-height 640)))))
     (tag Show_error))
    |}];
  sizes h;
  [%expect
    {|
    sidebar-width=280px files-width=400px files-split=30% agents-width=420px terminal-height=640px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-terminal">
    <div class="resizer resizer-files">
    <div class="resizer resizer-files-split">
    sidebar 280 [200, 520]
    files 400 [280, 760]
    files-split 30 [20, 80]
    agents 420 [300, 1200]
    terminal 640 [120, 640]
    |}]
;;

let%expect_test "phones keep sheets: no handles" =
  let h = H.create ~state () in
  H.quiet h (fun () ->
    H.act h (Set_narrow true);
    H.act h (Files Toggle));
  H.elements h ~selector:".resizer";
  [%expect {| |}]
;;

let%expect_test "keys and /help" =
  let h = H.create () in
  H.key h ~target:(Resize Agents) "ArrowLeft" ~ctrl:true;
  H.key h ~target:(Resize Agents) "Escape";
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  List.iter Keys.help ~f:(fun (key, text) ->
    if String.is_substring key ~substring:"panel's edge"
    then printf "%s: %s\n" key text);
  [%expect
    {| ← → / ↑ ↓ (on a panel's edge): resize the panel (Shift: faster; Home / End: smallest / largest; Enter: the default size); or drag the edge, double-click to reset |}]
;;
