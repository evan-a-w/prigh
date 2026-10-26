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
         data-resize-grows="1"
         data-resize-target=".sidebar"
         class="resizer resizer-sidebar"
         @on_dblclick> </div>
    |}];
  sizes h;
  [%expect
    {|
    sidebar-width=280px files-width=400px files-preview-width=480px agents-width=420px terminal-height=342px review-chat-width=400px review-files-width=260px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    sidebar 280 [200, 520]
    files 400 [280, 1200]
    files-preview 480 [320, 1238]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 260 [160, 440]
    review-margin 240 [240, 240]
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
    sidebar-width=280px files-width=352px files-preview-width=480px agents-width=420px terminal-height=342px review-chat-width=400px review-files-width=260px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    sidebar 280 [200, 520]
    files 352 [280, 760]
    files-preview 480 [320, 1238]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 260 [160, 440]
    review-margin 240 [240, 240]
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
          ; Files_preview, Some 5000
          ; Review_chat, Some 100
          ; Review_files, Some 400
          ; Review_margin, None
          ]));
  [%expect {| |}];
  sizes h;
  [%expect
    {|
    sidebar-width=333px files-width=1200px files-preview-width=1238px agents-width=300px terminal-height=640px review-chat-width=300px review-files-width=400px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    sidebar 333 [200, 520]
    files 1200 [280, 1200]
    files-preview 1238 [320, 1238]
    agents 300 [300, 1200]
    terminal 640 [120, 640]
    review-chat 300 [300, 1200]
    review-files 400 [160, 487]
    review-margin 240 [240, 240]
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
    sidebar-width=256px files-width=504px files-preview-width=480px agents-width=440px terminal-height=342px review-chat-width=400px review-files-width=161px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 256 [200, 256]
    files 504 [280, 504]
    files-preview 480 [320, 1376]
    agents 440 [300, 440]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 161 [160, 161]
    review-margin 240 [240, 240]
    |}];
  (* Narrower: the side panels give up width in proportion, down to their
     minimums, so that the chat keeps 400px. *)
  viewport h 1300 900;
  sizes h;
  [%expect
    {|
    sidebar-width=216px files-width=344px files-preview-width=480px agents-width=340px terminal-height=342px review-chat-width=400px review-files-width=161px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 216 [200, 216]
    files 344 [280, 344]
    files-preview 480 [320, 1118]
    agents 340 [300, 340]
    terminal 342 [120, 640]
    review-chat 400 [300, 1118]
    review-files 161 [160, 161]
    review-margin 240 [240, 240]
    |}];
  (* The agents panel floats over the chat below 1180px, the file browser
     below 1100px: at most 86% of the window. *)
  viewport h 1150 700;
  sizes h;
  [%expect
    {|
    sidebar-width=254px files-width=496px files-preview-width=480px agents-width=500px terminal-height=266px review-chat-width=400px review-files-width=161px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 254 [200, 254]
    files 496 [280, 496]
    files-preview 480 [320, 989]
    agents 500 [300, 989]
    terminal 266 [120, 440]
    review-chat 400 [300, 989]
    review-files 161 [160, 161]
    review-margin 240 [240, 240]
    |}];
  (* Wide again: back to what was chosen. *)
  viewport h 1600 900;
  sizes h;
  [%expect
    {|
    sidebar-width=256px files-width=504px files-preview-width=480px agents-width=440px terminal-height=342px review-chat-width=400px review-files-width=161px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 256 [200, 256]
    files 504 [280, 504]
    files-preview 480 [320, 1376]
    agents 440 [300, 440]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 161 [160, 161]
    review-margin 240 [240, 240]
    |}]
;;

let files_listed ?(viewport = 1440, 900) () =
  let h = H.create ~state () in
  let width, height = viewport in
  H.act h (Resize (Viewport { width; height }));
  H.quiet h (fun () ->
    H.act h (Files Toggle);
    H.reply
      h
      "fs_list"
      {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
          {"name":"main.ml","kind":"file","size":20,"mtime":1791100000000}],
        "total":1,"hidden":0}|});
  h
;;

let open_main h =
  H.quiet h (fun () ->
    H.click h ~selector:".files-row[data-name='main.ml']";
    H.reply
      h
      "fs_preview"
      {|{"path":"/home/me/proj/main.ml","name":"main.ml","size":20,"mtime":null,"kind":"text","text":"let x = 1\n","truncated":false}|})
;;

(* The panel, its columns left to right, and their handles. *)
let columns h =
  H.elements h ~selector:".files-panel";
  H.elements h ~selector:".files-panel > *";
  H.elements h ~selector:".files-panel .resizer"
;;

let%expect_test "an open file's preview sits left of the list, each with its edge" =
  let h = files_listed () in
  columns h;
  [%expect
    {|
    <aside class="files-panel">
    <Vdom.Node.none-widget class="">
    <div class="files-side">
    <div class="resizer resizer-files">
    |}];
  open_main h;
  columns h;
  H.text h ~selector:".files-preview-head";
  [%expect
    {|
    (Files (Select main.ml))
    <aside class="files-panel previewing">
    <section class="files-preview">
    <div class="files-side">
    <div class="resizer resizer-files-preview">
    <div class="resizer resizer-files">
    main.ml 20 B (Close the preview (Esc))
    |}];
  (* Its edge is on its left: left makes it wider. *)
  H.key h ~target:(Resize Files_preview) "ArrowLeft";
  H.key h ~target:(Resize Files_preview) "ArrowRight" ~shift:true;
  H.key h ~target:(Resize Files_preview) "End";
  [%expect
    {|
    (Resize (Nudge Files_preview 1))
    (Cache_prefs (((key web.files-preview-width) (value (427)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-preview-width 427)))))
     (tag Show_error))
    (Resize (Nudge Files_preview -4))
    (Cache_prefs (((key web.files-preview-width) (value (339)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-preview-width 339)))))
     (tag Show_error))
    (Resize (Largest Files_preview))
    (Cache_prefs (((key web.files-preview-width) (value (360)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-preview-width 360)))))
     (tag Show_error))
    |}];
  (* The list and the preview share what the chat leaves: 1440 - 400 for the
     chat - 280 for the sidebar. *)
  sizes h;
  [%expect {|
    sidebar-width=280px files-width=400px files-preview-width=360px agents-width=420px terminal-height=342px review-chat-width=400px review-files-width=260px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files-preview">
    <div class="resizer resizer-files">
    sidebar 280 [200, 280]
    files 400 [280, 400]
    files-preview 360 [320, 360]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 260 [160, 440]
    review-margin 240 [240, 240]
    |}];
  (* Closed, the list is alone again; open again, the preview has its
     width. *)
  H.key h "Escape" ~target:Files;
  columns h;
  [%expect {|
    (Files Close_preview)
    (Files (Focus files-list))
    <aside class="files-panel">
    <Vdom.Node.none-widget class="">
    <div class="files-side">
    <div class="resizer resizer-files">
    |}];
  open_main h;
  H.act h (Resize (Set (Files, 500)));
  sizes h;
  [%expect {|
    (Files (Select main.ml))
    (Cache_prefs (((key web.files-width) (value (400)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-width 400)))))
     (tag Show_error))
    sidebar-width=280px files-width=400px files-preview-width=360px agents-width=420px terminal-height=342px review-chat-width=400px review-files-width=260px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files-preview">
    <div class="resizer resizer-files">
    sidebar 280 [200, 280]
    files 400 [280, 400]
    files-preview 360 [320, 360]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 260 [160, 440]
    review-margin 240 [240, 240]
    |}]
;;

let%expect_test "without room for both, the preview takes the list's place" =
  let h = files_listed () in
  H.quiet h (fun () -> H.act h Toggle_subagents);
  open_main h;
  (* The sidebar, the agents panel, the list and the preview at their least
     leave the chat less than 400px. *)
  columns h;
  H.text h ~selector:".files-preview-head";
  [%expect {|
    (Files (Select main.ml))
    <aside class="alone files-panel previewing">
    <section class="files-preview">
    <div class="files-side">
    <div class="resizer resizer-files">
    (Back to the list (Esc)) main.ml 20 B
    |}];
  H.key h ~target:(Resize Files) "ArrowLeft";
  sizes h;
  [%expect {|
    (Resize (Nudge Files 1))
    (Cache_prefs (((key web.files-width) (value (378)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.files-width 378)))))
     (tag Show_error))
    sidebar-width=269px files-width=365px files-preview-width=480px agents-width=404px terminal-height=342px review-chat-width=400px review-files-width=161px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-files">
    <div class="resizer resizer-agents">
    sidebar 269 [200, 271]
    files 365 [280, 367]
    files-preview 480 [320, 1238]
    agents 404 [300, 406]
    terminal 342 [120, 640]
    review-chat 400 [300, 1200]
    review-files 161 [160, 161]
    review-margin 240 [240, 240]
    |}];
  H.click h ~selector:".files-back";
  columns h;
  [%expect {|
    (Files Close_preview)
    (Files (Focus files-list))
    <aside class="files-panel">
    <Vdom.Node.none-widget class="">
    <div class="files-side">
    <div class="resizer resizer-files">
    |}];
  (* Floating over the chat, likewise. *)
  H.quiet h (fun () -> H.act h Toggle_subagents);
  viewport h 1050 800;
  open_main h;
  columns h;
  [%expect {|
    (Files (Select main.ml))
    <aside class="alone files-panel previewing">
    <section class="files-preview">
    <div class="files-side">
    <div class="resizer resizer-files">
    |}];
  viewport h 1440 900;
  columns h;
  [%expect {|
    <aside class="files-panel previewing">
    <section class="files-preview">
    <div class="files-side">
    <div class="resizer resizer-files-preview">
    <div class="resizer resizer-files">
    |}]
;;

let%expect_test "the terminal's top edge" =
  let h = H.create ~state () in
  viewport h 1440 900;
  H.quiet h (fun () -> H.act h Toggle_terminal);
  H.key h ~target:(Resize Terminal) "ArrowUp";
  H.key h ~target:(Resize Terminal) "End";
  [%expect {|
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
  [%expect {|
    sidebar-width=280px files-width=400px files-preview-width=480px agents-width=420px terminal-height=640px review-chat-width=400px review-files-width=260px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-terminal">
    sidebar 280 [200, 520]
    files 400 [280, 1200]
    files-preview 480 [320, 1238]
    agents 420 [300, 1200]
    terminal 640 [120, 640]
    review-chat 400 [300, 1200]
    review-files 260 [160, 440]
    review-margin 240 [240, 240]
    |}]
;;

let main_ml_diff =
  "diff --git a/src/main.ml b/src/main.ml\n\
   index 1111111..2222222 100644\n\
   --- a/src/main.ml\n\
   +++ b/src/main.ml\n\
   @@ -1,2 +1,2 @@\n\
   -let a = 1\n\
   +let a = 2\n\
  \ let b = 2\n"
;;

let reviewing ?(width = 1440) () =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "session_id", `String "s1"
             ; "session_path", `String "/sessions/s1.jsonl"
             ; "cwd", `String "/home/me/proj"
             ]
           ())
      ()
  in
  viewport h width 900;
  H.quiet h (fun () ->
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    H.type_ h "/review";
    H.key h "Enter";
    H.reply
      h
      "git_diff"
      (Jsonaf.to_string
         (`Object
             [ "root", `String "/home/me/proj"
             ; "against", `String "HEAD"
             ; "diff", `String main_ml_diff
             ; "truncated", `False
             ]));
    H.reply
      h
      "fs_preview"
      {|{"path":"/home/me/proj/src/main.ml","name":"main.ml","size":20,"kind":"text","text":"let a = 2\nlet b = 2\n","truncated":false,"md5":"m1"}|});
  h
;;

let%expect_test "the review: the chat's edge beside it, its file list's edge" =
  let h = reviewing () in
  H.elements h ~selector:".resizer";
  H.show h ~selector:".resizer-review-chat";
  [%expect {|
    (Complete_accept (run true))
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-review-files">
    <div class="resizer resizer-review-chat">
    <div role="separator"
         tabindex="0"
         aria-label="Resize the chat beside the review"
         aria-orientation="vertical"
         aria-valuenow="400"
         aria-valuemin="300"
         aria-valuemax="680"
         title="Drag to resize, double-click for the default size"
         data-resize="review-chat"
         data-resize-var="--review-chat-width"
         data-resize-grows="-1"
         data-resize-target="main.main"
         class="resizer resizer-review-chat"
         @on_dblclick> </div>
    |}];
  (* The chat takes at most what leaves the review page 480px (1440 - 280
     for the sidebar - 480), and the file list what leaves the code 320. *)
  H.key h ~target:(Resize Review_chat) "ArrowLeft";
  H.key h ~target:(Resize Review_chat) "End";
  H.key h ~target:(Resize Review_files) "ArrowRight" ~shift:true;
  [%expect {|
    (Resize (Nudge Review_chat 1))
    (Cache_prefs (((key web.review-chat-width) (value (416)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-chat-width 416)))))
     (tag Show_error))
    (Resize (Largest Review_chat))
    (Cache_prefs (((key web.review-chat-width) (value (680)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-chat-width 680)))))
     (tag Show_error))
    (Resize (Nudge Review_files 4))
    (Cache_prefs (((key web.review-files-width) (value (160)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-files-width 160)))))
     (tag Show_error))
    |}];
  sizes h;
  [%expect {|
    sidebar-width=280px files-width=400px files-preview-width=480px agents-width=420px terminal-height=342px review-chat-width=680px review-files-width=160px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-review-files">
    <div class="resizer resizer-review-chat">
    sidebar 280 [200, 280]
    files 400 [280, 1200]
    files-preview 480 [320, 1238]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 680 [300, 680]
    review-files 160 [160, 160]
    review-margin 240 [240, 240]
    |}];
  (* Narrower: the chat gives way to the review page, and the file list to
     the code. *)
  viewport h 1100 900;
  sizes h;
  [%expect {|
    sidebar-width=220px files-width=400px files-preview-width=480px agents-width=420px terminal-height=342px review-chat-width=399px review-files-width=160px review-margin-width=240px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-review-files">
    <div class="resizer resizer-review-chat">
    sidebar 220 [200, 221]
    files 400 [280, 946]
    files-preview 480 [320, 946]
    agents 420 [300, 946]
    terminal 342 [120, 640]
    review-chat 399 [300, 400]
    review-files 160 [160, 161]
    review-margin 240 [240, 240]
    |}];
  viewport h 1440 900;
  H.key h ~target:(Resize Review_chat) "Enter";
  H.key h ~target:(Resize Review_files) "Home";
  [%expect {|
    (Resize (Reset Review_chat))
    (Cache_prefs (((key web.review-chat-width) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-chat-width null)))))
     (tag Show_error))
    (Resize (Smallest Review_files))
    |}];
  (* The comments' column right of the code fits what the file list and
     the code leave it. *)
  H.act h (Resize (Restore [ Review_chat, Some 340; Review_files, Some 200 ]));
  H.act h (Resize (Nudge (Review_margin, -2)));
  H.act h (Resize (Set (Review_margin, 5000)));
  [%expect {|
    (Cache_prefs (((key web.review-margin-width) (value (268)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-margin-width 268)))))
     (tag Show_error))
    (Cache_prefs (((key web.review-margin-width) (value (300)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.review-margin-width 300)))))
     (tag Show_error))
    |}];
  sizes h;
  [%expect {|
    sidebar-width=280px files-width=400px files-preview-width=480px agents-width=420px terminal-height=342px review-chat-width=340px review-files-width=200px review-margin-width=300px
    <div class="resizer resizer-sidebar">
    <div class="resizer resizer-review-files">
    <div class="resizer resizer-review-chat">
    sidebar 280 [200, 520]
    files 400 [280, 1200]
    files-preview 480 [320, 1238]
    agents 420 [300, 1200]
    terminal 342 [120, 640]
    review-chat 340 [300, 680]
    review-files 200 [160, 500]
    review-margin 300 [240, 300]
    |}];
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".resizer";
  [%expect {| |}]
;;

let%expect_test "phones keep sheets: no handles; the preview in the list's place" =
  let h = files_listed () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  open_main h;
  H.elements h ~selector:".resizer";
  [%expect {| (Files (Select main.ml)) |}];
  columns h;
  [%expect {|
    <aside class="alone files-panel previewing">
    <section class="files-preview">
    <div class="files-side">
    |}]
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
