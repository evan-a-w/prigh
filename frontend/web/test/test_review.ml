open! Core
module H = Harness

(* The review: the page in the chat's place, the changed files' approvals,
   comments on lines and their threads, the agent's proposals, edit mode,
   and the file browser marking what changed. *)

let state () =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String "/home/me/proj"
      ]
    ()
;;

let main_ml_diff =
  "diff --git a/src/main.ml b/src/main.ml\n\
   index 1111111..2222222 100644\n\
   --- a/src/main.ml\n\
   +++ b/src/main.ml\n\
   @@ -2,3 +2,4 @@\n\
  \ let b = 2\n\
   -let c = 3\n\
   +let c = 3 * 2\n\
   +let d = 4\n\
  \ let e = 5\n"
;;

let util_diff =
  "diff --git a/src/util.ml b/src/util.ml\n\
   new file mode 100644\n\
   index 0000000..3333333\n\
   --- /dev/null\n\
   +++ b/src/util.ml\n\
   @@ -0,0 +1,2 @@\n\
   +let id x = x\n\
   +let twice f x = f (f x)\n"
;;

let diff_json diff =
  Jsonaf.to_string
    (`Object
        [ "root", `String "/home/me/proj"
        ; "against", `String "HEAD"
        ; "diff", `String diff
        ; "truncated", `False
        ])
;;

let main_ml =
  "let a = 1\nlet b = 2\nlet c = 3 * 2\nlet d = 4\nlet e = 5\nlet f = 6\n"
;;

let preview_json ?(md5 = "m1") path text =
  Jsonaf.to_string
    (`Object
        [ "path", `String path
        ; "name", `String (Filename.basename path)
        ; "size", `Number (Int.to_string (String.length text))
        ; "kind", `String "text"
        ; "text", `String text
        ; "truncated", `False
        ; "md5", `String md5
        ])
;;

let comment_json
      ?(status = "open")
      ?(thread = [])
      ?(anchor = "let c = 3 * 2\nlet d = 4")
      ?side
      ?columns
      id
      ~first
      ~last
      text
  =
  let number n = `Number (Int.to_string n) in
  Jsonaf.to_string
    (`Object
        ([ "id", `String id
         ; "root", `String "/home/me/proj"
         ; "path", `String "src/main.ml"
         ; "first", number first
         ; "last", number last
         ]
         @ Option.value_map side ~default:[] ~f:(fun side ->
           [ "side", `String side ])
         @ Option.value_map columns ~default:[] ~f:(fun (a, b) ->
           [ "start_col", number a; "end_col", number b ])
         @ [ "anchor", `String anchor
           ; "status", `String status
           ; ( "thread"
             , `Array
                 (Jsonaf.of_string
                    (sprintf
                       {|{"author":"user","text":%S,"at_ms":1791194400000,"delivered":false}|}
                       text)
                  :: List.map thread ~f:Jsonaf.of_string) )
           ]))
;;

let review_event ?(approvals = "[]") comments =
  sprintf
    {|{"type":"event","event":"review","review":{"comments":[%s],"approvals":%s}}|}
    (String.concat ~sep:"," comments)
    approvals
;;

(* The page open on the two changed files, src/main.ml's text read. *)
let opened () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    H.type_ h "/review";
    H.key h "Enter";
    H.reply h "git_diff" (diff_json (main_ml_diff ^ util_diff));
    H.reply h "fs_preview" (preview_json "/home/me/proj/src/main.ml" main_ml));
  h
;;

let%expect_test "/review opens the page in the chat's place, at the first file" =
  let h = H.create ~state:(state ()) () in
  H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
  H.type_ h "/review";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /review) (max 500)))
     (tag Ignore))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    (Focus review-page)
    |}];
  H.text h ~selector:".review-page";
  [%expect
    {|
    Review
    0/0 approved
    (Comment) (Edit)
    (Changes) (Whole file)
    (Send comments) (Close the review (Esc, Alt+R))
    Reading the changes…
    |}];
  H.reply h "git_diff" (diff_json (main_ml_diff ^ util_diff));
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/main.ml)))
     (tag (Review (Content src/main.ml))))
    |}];
  H.text h ~selector:".review-page";
  [%expect
    {|
    Review
    0/2 approved
    (Read the changes again (they also update as prigh edits))
    (Comment) (Edit)
    (Changes) (Whole file)
    (Send comments) (Close the review (Esc, Alt+R))
    Reviewing
    (proj /home/me/proj)
    against
    (HEAD the uncommitted changes)
    (○ src/ main.ml +2 −1)
    (○ src/ util.ml +2 −0)
    2 files changed, +4 −1
    (Previous file ([, p, k)) (Next file (], n, j))
    src/main.ml
    (Approve)
    @@ -2,3 +2,4 @@
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    |}];
  H.elements h ~selector:".app";
  [%expect {| <div class="app reviewing sidebar-open"> |}];
  (* The chat is still there, beside it. *)
  H.elements h ~selector:".main .composer";
  [%expect {| <footer class="composer"> |}]
;;

let%expect_test
    "keys: next and previous file, approving, the layout; Esc closes"
  =
  let h = opened () in
  H.key h "]" ~target:Review;
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Step 1))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/util.ml)))
     (tag (Review (Content src/util.ml))))
    |}];
  H.text h ~selector:".review-file.current";
  H.key h "[" ~target:Review;
  H.text h ~selector:".review-file.current";
  [%expect
    {|
    (○ src/ util.ml +2 −0)
    (Review (Step -1))
    (○ src/ main.ml +2 −1)
    |}];
  (* j and k too; past the last it wraps round. *)
  H.key h "j" ~target:Review;
  H.key h "j" ~target:Review;
  H.text h ~selector:".review-file.current";
  [%expect
    {|
    (Review (Step 1))
    (Review (Step 1))
    (○ src/ main.ml +2 −1)
    |}];
  H.key h "a" ~target:Review;
  [%expect
    {|
    (Review (Toggle_approval src/main.ml))
    (Rpc (method_ review_approve)
     (params
      ((root /home/me/proj) (path src/main.ml)
       (hash 7f57d42d80a860a5b495b9ed7f7e860e)))
     (tag (Review (Changed ()))))
    |}];
  H.reply
    h
    "review_approve"
    {|{"comments":[],"approvals":[{"path":"src/main.ml","hash":"7f57d42d80a860a5b495b9ed7f7e860e"}]}|};
  H.text h ~selector:".review-progress";
  H.text h ~selector:".review-file-head";
  [%expect
    {|
    1/2 approved
    (Previous file ([, p, k)) (Next file (], n, j))
    src/main.ml
    (Approved)
    |}];
  (* The file changes (an agent's edit): the approval no longer counts. *)
  H.event
    h
    {|{"type":"event","event":"tool_end","call":{"id":"e1","name":"edit","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"e1","tool_name":"edit","text":"ok","is_error":false}}|};
  [%expect {|
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    |}];
  H.reply h "git_diff" (diff_json (main_ml_diff ^ "+let g = 7\n" ^ util_diff));
  H.text h ~selector:".review-file-head";
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/main.ml)))
     (tag (Review (Content src/main.ml))))
    (Previous file ([, p, k)) (Next file (], n, j))
    src/main.ml
    Changed since you approved it (Approve again)
    |}];
  H.key h "v" ~target:Review;
  H.key h "Escape" ~target:Review;
  [%expect
    {|
    (Review (Set_layout Full))
    (Review Close)
    (Focus editor)
    |}];
  H.elements h ~selector:".review-page";
  [%expect {| |}];
  H.key h "r" ~alt:true ~code:"KeyR";
  [%expect
    {|
    (Review Toggle)
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    (Focus review-page)
    |}]
;;

let point side line offset =
  { Prigh_web.Code_selection.Point.side; line; offset }
;;

let select h ?pop start end_ =
  H.act h (Review (Select_text { path = "src/main.ml"; start; end_; pop }))
;;

let%expect_test
    "commenting on selected code, sending, the agent's reply and its proposal"
  =
  let h = opened () in
  (* The page reports text selected from line 3 into line 4: the Comment
     button floats by its end. *)
  select
    h
    (point New 3 8)
    (point New 4 5)
    ~pop:{ Prigh_web.Code_review.Pop.left = 140; top = 96 };
  H.show h ~selector:".review-pop";
  [%expect
    {|
    (Complete_accept (run true))
    <div class="review-pop" style={ left: 140px; top: 96px; }>
      <button type="button"
              title="Comment on the selected code (c)"
              class="btn primary small"
              @on_click
              @on_mousedown>
        <icon class="pencil"> </icon>
        Comment
        <kbd class="review-pop-key"> c </kbd>
      </button>
    </div>
    |}];
  (* Rows are not drawn again for it: the page's own highlight shows it. *)
  H.elements h ~selector:".review-diff tr.selected";
  H.elements h ~selector:".review-diff mark";
  [%expect {| |}];
  (* c (wherever the focus is, but in a field) opens the comment box, the
     selected text quoted. *)
  H.key h "c" ~target:Page;
  H.text h ~selector:".review-thread.draft";
  [%expect
    {|
    (Review Start_comment)
    (Focus review-draft)
    Comment on lines 3–4
    3 * 2
    let d
    []
    Enter adds it · Shift+Enter a new line · Esc cancels (Cancel) (Add comment)
    |}];
  (* While it is written, its code stays marked. *)
  H.elements h ~selector:".review-diff mark";
  H.elements h ~selector:".review-pop";
  [%expect
    {|
    <mark class="draft review-mark">
    <mark class="draft review-mark">
    |}];
  H.input h ~selector:"#review-draft" "Why double it?";
  H.click h ~selector:".review-draft-actions .btn.primary";
  [%expect
    {|
    (Review (Draft_input "Why double it?"))
    (Review Draft_send)
    (Rpc (method_ review_comment)
     (params
      ((root /home/me/proj) (path src/main.ml) (first 3) (last 4) (side new)
       (start_col 8) (end_col 5) (anchor  "let c = 3 * 2\
                                         \nlet d = 4")
       (text "Why double it?")))
     (tag (Review Comment_added)))
    |}];
  H.reply
    h
    "review_comment"
    (comment_json "c1" ~first:3 ~last:4 ~columns:(8, 5) "Why double it?");
  H.text h ~selector:".review-thread";
  [%expect
    {|
    c1 lines 3–4
    (Resolve) (Delete the comment and its thread)
    3 * 2
    let d
    You 10:00 not sent yet
    Why double it?
    []
    |}];
  (* Its code stays marked; pointing at the thread lights it, and the other
     way round. *)
  H.elements h ~selector:".review-diff mark";
  H.act h (Review (Hover (Some "c1")));
  H.elements h ~selector:".review-diff mark.hot";
  H.elements h ~selector:".review-thread.hot";
  H.act h (Review (Hover None));
  H.elements h ~selector:".hot";
  [%expect
    {|
    <mark class="review-mark">
    <mark class="review-mark">
    <mark class="hot review-mark">
    <mark class="hot review-mark">
    <article class="hot review-thread">
    |}];
  H.text h ~selector:".review-controls .btn.primary";
  H.key h "s" ~target:Review;
  [%expect
    {|
    (Send 1 comment)
    (Review Submit)
    (Rpc (method_ review_submit) (params ((root /home/me/proj)))
     (tag (Review Submitted)))
    |}];
  H.reply h "review_submit" {|{"count":1}|};
  [%expect {| (Expire_toast (id 0) (after_ms 2500)) |}];
  (* The agent's reply, proposing a change, arrives with the review. *)
  H.event
    h
    (review_event
       [ comment_json
           "c1"
           ~first:3
           ~last:4
           ~columns:(8, 5)
           "Why double it?"
           ~thread:
             [ {|{"author":"agent","text":"It **scales**. Or:","at_ms":1791194460000,"delivered":true,
                  "proposal":{"edits":[{"old_text":"3 * 2","new_text":"3"}],"state":"pending"}}|}
             ]
       ]);
  H.text h ~selector:".review-thread";
  [%expect
    {|
    c1 lines 3–4
    (Resolve) (Delete the comment and its thread)
    3 * 2
    let d
    You 10:00 not sent yet
    Why double it?
    prigh 10:01
    It scales . Or:
    - 3 * 2
    + 3
    (Accept) (Reject)
    []
    |}];
  H.click h ~selector:".review-proposal-actions .btn.primary";
  [%expect
    {|
    (Review (Decide (id c1) (post 1) (accept true)))
    (Rpc (method_ review_decide) (params ((id c1) (post 1) (accept true)))
     (tag (Review (Decided c1))))
    |}];
  H.reply
    h
    "review_decide"
    (comment_json
       "c1"
       ~first:3
       ~last:4
       ~columns:(8, 5)
       "Why double it?"
       ~thread:
         [ {|{"author":"agent","text":"Or:","at_ms":1791194460000,"delivered":true,
              "proposal":{"edits":[{"old_text":"3 * 2","new_text":"3"}],"state":"accepted"}}|}
         ]);
  [%expect {|
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    |}];
  H.text h ~selector:".review-proposal-state";
  [%expect {| Accepted: applied to the file |}];
  (* A reply, then resolving. *)
  H.input h ~selector:"#review-reply-c1" "Thanks";
  H.click h ~selector:".review-reply .btn";
  [%expect
    {|
    (Review (Reply_input (id c1) (text Thanks)))
    (Review (Reply_send c1))
    (Rpc (method_ review_reply) (params ((id c1) (text Thanks)))
     (tag (Review (Replied c1))))
    |}];
  H.click h ~selector:".review-thread-tools .btn";
  [%expect
    {|
    (Review (Set_status (id c1) (status Resolved)))
    (Rpc (method_ review_set_status) (params ((id c1) (status resolved)))
     (tag (Review (Changed (c1)))))
    |}]
;;

let%expect_test
    "a comment whose lines moved follows them; one whose lines are gone is \
     listed, folded, above"
  =
  let h = opened () in
  H.event
    h
    (review_event
       [ comment_json "c1" ~first:1 ~last:1 ~anchor:"let e = 5" "Moved?"
       ; comment_json "c2" ~first:2 ~last:2 ~anchor:"let z = 0" "Gone?"
       ]);
  H.text h ~selector:".review-code";
  [%expect
    {|
    (Complete_accept (run true))
    (1 outdated comment)
    @@ -2,3 +2,4 @@
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    c1 line 5
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Moved?
    []
    |}];
  H.text h ~selector:".review-file.current";
  [%expect {| (○ src/ main.ml 2 +2 −1) |}]
;;

let%expect_test "the whole file with the changes inline" =
  let h = opened () in
  H.click
    h
    ~selector:".review-controls .segmented:nth-child(2) .segment:nth-child(2)";
  H.text h ~selector:".review-code";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Set_layout Full))
    1 let a = 1
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    6 let f = 6
    |}];
  H.act h (Review (Set_mode Comment));
  H.text h ~selector:".review-code";
  [%expect
    {|
    1 let a = 1
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    6 let f = 6
    |}]
;;

let%expect_test
    "edit mode: the text, saved where the tools run unless it changed"
  =
  let h = opened () in
  H.key h "e" ~target:Review;
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Set_mode Edit))
    (Focus review-editor)
    |}];
  H.text h ~selector:".review-editor-bar";
  [%expect
    {|
    Saved on the tool host
    (Reload) (Save)
    |}];
  H.input h ~selector:"#review-editor" (main_ml ^ "let g = 7\n");
  H.text h ~selector:".review-editor-bar";
  [%expect
    {|
    (Review
     (Edit_input
       "let a = 1\
      \nlet b = 2\
      \nlet c = 3 * 2\
      \nlet d = 4\
      \nlet e = 5\
      \nlet f = 6\
      \nlet g = 7\
      \n"))
    Unsaved changes
    (Reload) (Save)
    |}];
  H.act h (Review Save);
  [%expect
    {|
    (Rpc (method_ fs_write)
     (params
      ((path /home/me/proj/src/main.ml)
       (text
         "let a = 1\
        \nlet b = 2\
        \nlet c = 3 * 2\
        \nlet d = 4\
        \nlet e = 5\
        \nlet f = 6\
        \nlet g = 7\
        \n")
       (expected m1) (review_root /home/me/proj)))
     (tag (Review (Saved src/main.ml))))
    |}];
  H.fail
    h
    "fs_write"
    "src/main.ml changed since you opened it: reload it (your edits are lost) \
     or save again to overwrite it";
  H.text h ~selector:".review-editor-bar";
  [%expect
    {|
    Unsaved changes src/main.ml changed since you opened it: reload it (your edits are lost) or save again to overwrite it
    (Reload) (Save)
    |}];
  (* Saving again overwrites it. *)
  H.act h (Review Save);
  H.reply h "fs_write" {|{"path":"/home/me/proj/src/main.ml","md5":"m2"}|};
  H.text h ~selector:".review-editor-bar";
  [%expect
    {|
    (Rpc (method_ fs_write)
     (params
      ((path /home/me/proj/src/main.ml)
       (text
         "let a = 1\
        \nlet b = 2\
        \nlet c = 3 * 2\
        \nlet d = 4\
        \nlet e = 5\
        \nlet f = 6\
        \nlet g = 7\
        \n")
       (review_root /home/me/proj)))
     (tag (Review (Saved src/main.ml))))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    Saved on the tool host
    (Reload) (Save)
    |}]
;;

let%expect_test
    "the file browser marks what changed and shows a changed file's diff"
  =
  let h = H.create ~state:(state ()) () in
  H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
  H.act h (Files Toggle);
  [%expect
    {|
    (Rpc (method_ fs_list) (params ((hidden false))) (tag (Files (Listed ""))))
    (Files (Focus files-list))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    |}];
  H.reply h "git_diff" (diff_json (main_ml_diff ^ util_diff));
  H.reply
    h
    "fs_list"
    {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
        {"name":"README.md","kind":"file","size":20,"mtime":1791100000000},
        {"name":"src","kind":"dir","mtime":1791190000000}],"total":2,"hidden":0}|};
  H.elements h ~selector:".files-row";
  H.names h ~selector:".files-change-count";
  [%expect
    {|
    <div class="changed dir files-row">
    <div class="files-row">
    span "2" -- 2 changed files inside
    |}];
  H.act h (Files (Go "/home/me/proj/src"));
  H.reply
    h
    "fs_list"
    {|{"path":"/home/me/proj/src","parent":"/home/me/proj","home":"/home/me","entries":[
        {"name":"main.ml","kind":"file","size":60,"mtime":1791190000000}],"total":1,"hidden":0}|};
  H.click h ~selector:".files-row";
  H.reply h "fs_preview" (preview_json "/home/me/proj/src/main.ml" main_ml);
  H.text h ~selector:".files-preview-body";
  [%expect
    {|
    (Rpc (method_ fs_list) (params ((path /home/me/proj/src) (hidden false)))
     (tag (Files (Listed /home/me/proj/src))))
    (Files (Select main.ml))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/main.ml)))
     (tag (Files (Previewed /home/me/proj/src/main.ml))))
    ○ +2 −1
    (Changes) (Whole file)
    (Review)
    @@ -2,3 +2,4 @@
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    |}];
  H.click h ~selector:".files-preview-body .review-open";
  [%expect
    {|
    (Review (Open (src/main.ml)))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    (Focus review-page)
    |}]
;;

let%expect_test
    "while nothing shows the changes, the agent's edits read nothing"
  =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|});
  H.event
    h
    {|{"type":"event","event":"tool_end","call":{"id":"e1","name":"edit","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"e1","tool_name":"edit","text":"ok","is_error":false}}|};
  [%expect {| |}]
;;

let%expect_test
    "removed lines: their old text; a mixed selection keeps the file's own"
  =
  let h = opened () in
  H.quiet h (fun () ->
    select h (point Old 3 4) (point Old 3 5) ~pop:{ left = 60; top = 70 };
    H.key h "c" ~target:Review);
  H.text h ~selector:".review-thread.draft";
  [%expect
    {|
    (Complete_accept (run true))
    (Review Start_comment)
    Comment on old line 3 old text
    c
    []
    Enter adds it · Shift+Enter a new line · Esc cancels (Cancel) (Add comment)
    |}];
  (* The box shows under the removed line. *)
  H.elements h ~selector:"tr[data-side='old'] + tr.review-insert";
  H.input h ~selector:"#review-draft" "Keep c?";
  H.act h (Review Draft_send);
  [%expect
    {|
    <tr class="review-insert">
    (Review (Draft_input "Keep c?"))
    (Rpc (method_ review_comment)
     (params
      ((root /home/me/proj) (path src/main.ml) (first 3) (last 3) (side old)
       (start_col 4) (end_col 5) (anchor "let c = 3") (text "Keep c?")))
     (tag (Review Comment_added)))
    |}];
  H.reply
    h
    "review_comment"
    (comment_json
       "c1"
       ~first:3
       ~last:3
       ~side:"old"
       ~columns:(4, 5)
       ~anchor:"let c = 3"
       "Keep c?");
  H.text h ~selector:".review-thread";
  H.elements h ~selector:"tr[data-side='old'] mark";
  [%expect
    {|
    c1 old line 3 old text
    (Resolve) (Delete the comment and its thread)
    c
    You 10:00 not sent yet
    Keep c?
    []
    <mark class="review-mark">
    |}];
  (* From the removed line into the added one: only the added one's part. *)
  select h (point Old 3 2) (point New 3 5);
  H.key h "c" ~target:Page;
  H.text h ~selector:".review-thread.draft .review-thread-head";
  H.text h ~selector:".review-thread.draft .review-quote";
  [%expect
    {|
    (Review Start_comment)
    (Focus review-draft)
    Comment on line 3
    let c
    |}];
  (* Esc drops it, nothing sent. *)
  H.key h "Escape" ~target:Review;
  H.elements h ~selector:".review-thread.draft";
  [%expect {| (Review Draft_cancel) |}]
;;

let%expect_test "a line's number selects it, Shift+click to another" =
  let h = opened () in
  H.click h ~selector:"tr[data-side='new'][data-line='3'] td.ln.pick";
  H.act
    h
    (Review
       (Select_line
          { path = "src/main.ml"; side = New; line = 4; extend = true }));
  H.text h ~selector:".review-select-bar";
  H.elements h ~selector:"tr.selected";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Select_line (path src/main.ml) (side New) (line 3) (extend false)))
    Clear_text_selection
    Clear_text_selection
    (Comment on lines 3–4) (Clear)
    <tr class="add sel-first selected">
    <tr class="add selected">
    |}];
  H.key h "c" ~target:Review;
  H.input h ~selector:"#review-draft" "Both?";
  H.act h (Review Draft_send);
  [%expect
    {|
    (Review Start_comment)
    (Focus review-draft)
    (Review (Draft_input Both?))
    (Rpc (method_ review_comment)
     (params
      ((root /home/me/proj) (path src/main.ml) (first 3) (last 4) (side new)
       (anchor  "let c = 3 * 2\
               \nlet d = 4")
       (text Both?)))
     (tag (Review Comment_added)))
    |}];
  (* Without a selection, c says how to make one. *)
  H.reply h "review_comment" (comment_json "c1" ~first:3 ~last:4 "Both?");
  H.key h "c" ~target:Review;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Review Start_comment)
    (Expire_toast (id 0) (after_ms 4420))
    Select the code to comment on first: drag across it, then Comment (or c).
    |}]
;;

let%expect_test "the comment and reply fields' keys" =
  let t = (H.model (opened ())).review in
  List.iter
    [ `Draft, "Enter", false, false
    ; `Draft, "Enter", true, false
    ; `Draft, "Enter", false, true
    ; `Draft, "Escape", false, false
    ; `Reply "c1", "Enter", false, false
    ; `Reply "c1", "Enter", true, false
    ; `Editor, "s", false, true
    ]
    ~f:(fun (field, key, shift, ctrl) ->
      print_s
        [%sexp
          (key : string)
        , ~~(shift : bool)
        , ~~(ctrl : bool)
        , (Prigh_web.Code_review.field_key
             t
             ~field
             ~key
             ~shift
             ~ctrl
             ~meta:false
           : Prigh_web.Code_review.Action.t option)]);
  [%expect
    {|
    (Complete_accept (run true))
    (Enter (shift false) (ctrl false) (Draft_send))
    (Enter (shift true) (ctrl false) ())
    (Enter (shift false) (ctrl true) (Draft_send))
    (Escape (shift false) (ctrl false) (Draft_cancel))
    (Enter (shift false) (ctrl false) ((Reply_send c1)))
    (Enter (shift true) (ctrl false) ())
    (s (shift false) (ctrl true) ())
    |}]
;;

let%expect_test "a comment saved before selections reads as whole lines" =
  let h = opened () in
  H.event
    h
    (review_event
       [ comment_json "c1" ~first:3 ~last:3 ~anchor:"let c = 3 * 2" "Old" ]);
  H.text h ~selector:".review-thread-head";
  H.elements h ~selector:"tr.commented";
  H.elements h ~selector:".review-diff mark";
  [%expect
    {|
    (Complete_accept (run true))
    c1 line 3
    (Resolve) (Delete the comment and its thread)
    <tr class="add commented">
    <mark class="review-mark">
    |}]
;;

let%expect_test "help lists the review's keys" =
  List.iter Prigh_web.Keys.help ~f:(fun (key, what) ->
    if String.is_substring key ~substring:"review" || String.equal key "Alt+R"
    then printf "%s: %s\n" key what);
  [%expect
    {|
    Alt+R: open or close the review page (/review): the changes file by file, beside the chat
    ] / [ (reviewing; also n p, j k): the next / previous changed file
    a / c / e / v / s (reviewing): approve the file; comment on the code selected (drag across it, in the review or a changed file's preview); edit or comment; the whole file or the changes; send the new comments to prigh
    r / b / d (reviewing): choose what to review (a worktree, or a branch read-only); what to compare it with (a branch, tag, commit or any revision typed); since the merge base or directly
    l / Shift+L (reviewing): what changed since your latest action on the file (or the hunks again); choose the action it compares with: Latest follows your newest
    Esc (reviewing): drop the comment or selection, then close the review
    Enter / Ctrl+S (reviewing): add the comment or reply (Shift+Enter: a new line); save the file being edited
    Ctrl+Enter (the review's commit message): commit the ticked files (approved files come ticked; untick to leave one out)
    |}]
;;

(* ---- where threads show *)

let three_threads () =
  let h = opened () in
  H.quiet h (fun () ->
    H.event
      h
      (review_event
         [ comment_json "c1" ~first:3 ~last:4 "Why double?"
         ; comment_json
             "c2"
             ~first:4
             ~last:4
             ~anchor:"let d = 4"
             ~status:"resolved"
             ~thread:
               [ {|{"author":"agent","text":"Fixed.","at_ms":1791194400000,"delivered":true}|}
               ]
             "Is d needed?"
         ; comment_json "c3" ~first:9 ~last:9 ~anchor:"let gone = 0" "Gone?"
         ]));
  h
;;

let%expect_test "on a wide window threads sit in a margin beside their lines" =
  let h = three_threads () in
  H.quiet h (fun () ->
    H.act h (Resize (Viewport { width = 1920; height = 1000 })));
  H.elements h ~selector:".review-code > *";
  H.elements h ~selector:".review-code-grid > *";
  H.show h ~selector:".review-margin";
  [%expect
    {|
    (Complete_accept (run true))
    <Vdom.Node.none-widget class="">
    <div class="review-asides">
    <div class="review-code-grid">
    <Vdom.Node.none-widget class="">
    <div class="review-code-main">
    <aside class="review-margin">
    <aside aria-label="Comments" class="review-margin">
      <div role="separator"
           tabindex="0"
           aria-label="Resize the review's comments"
           aria-orientation="vertical"
           aria-valuenow="340"
           aria-valuemin="240"
           aria-valuemax="640"
           title="Drag to resize, double-click for the default size"
           data-resize="review-margin"
           data-resize-var="--review-margin-width"
           data-resize-grows="-1"
           data-resize-target="#review-page"
           class="resizer resizer-review-margin"
           @on_dblclick> </div>
      <div @key=c1 data-side="new" data-line="3" class="review-margin-item thread">
        <article id="review-thread-c1"
                 data-comment="c1"
                 aria-label="Comment c1 on lines 3–4"
                 class="review-thread"
                 @on_mouseenter
                 @on_mouseleave>
          <div class="review-thread-head">
            <span class="review-id"> c1 </span>
            <span class="review-where"> lines 3–4 </span>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <div class="review-thread-tools">
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <button type="button" title="Mark it done" class="btn ghost small" @on_click> Resolve </button>
              <button type="button"
                      title="Delete the comment and its thread"
                      aria-label="Delete"
                      class="btn ghost icon"
                      @on_click>
                <icon class="trash"> </icon>
              </button>
            </div>
          </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <ul class="review-posts">
            <li class="review-post user">
              <div class="review-post-head">
                <span class="review-who"> You </span>
                <span class="review-time"> 10:00 </span>
                <span class="review-unsent"> not sent yet </span>
              </div>
              <div class="review-post-text">
                <div class="markdown">
                  <div class="md-part">
                    <p dir="auto"> Why double? </p>
                  </div>
                </div>
              </div>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </li>
          </ul>
          <div class="review-reply">
            <textarea id="review-reply-c1"
                      placeholder="Reply…"
                      rows="1"
                      aria-label="Reply to c1"
                      class="review-field"
                      #value=""
                      @on_input
                      @on_keydown> </textarea>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
        </article>
      </div>
      <div @key=c2 data-side="new" data-line="4" class="folded review-margin-item">
        <button type="button"
                id="review-thread-c2"
                data-comment="c2"
                aria-expanded="false"
                title="Resolved: c2 on line 4. Click to show its thread."
                class="review-folded-thread"
                @on_click
                @on_mouseenter
                @on_mouseleave>
          <icon class="check"> </icon>
          <span class="review-id"> c2 </span>
          <span class="review-folded-text"> Is d needed? </span>
          <span class="review-folded-count"> 2 </span>
        </button>
      </div>
    </aside>
    |}];
  (* The code has no threads between its lines: they are in the margin, at
     their first line; the resolved one folded to a mark. *)
  H.text h ~selector:".review-diff";
  H.elements h ~selector:".review-margin-item";
  H.text h ~selector:".review-margin";
  [%expect
    {|
    @@ -2,3 +2,4 @@
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    5 let e = 5
    <div class="review-margin-item thread">
    <div class="folded review-margin-item">
    c1 lines 3–4
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Why double?
    []
    (c2 Is d needed? 2)
    |}];
  (* The mark unfolds it; its fold button folds it again. *)
  H.click h ~selector:".review-folded-thread";
  H.text h ~selector:".review-margin-item.thread:last-child";
  H.click h ~selector:".review-fold";
  H.elements h ~selector:".review-margin-item";
  [%expect
    {|
    (Review (Unfold c2))
    c2 line 4 resolved
    (Fold it to its mark on the line) (Reopen) (Delete the comment and its thread)
    (Review (Unfold c2))
    <div class="review-margin-item thread">
    <div class="folded review-margin-item">
    |}];
  (* The comment whose code is gone: one folded line above the code. *)
  H.text h ~selector:".review-asides";
  H.click h ~selector:".review-aside-toggle";
  H.text h ~selector:".review-asides";
  [%expect
    {|
    (1 outdated comment)
    (Review (Unfold outdated:src/main.ml))
    (1 outdated comment)
    c3 line 9 its code changed
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Gone?
    []
    |}]
;;

let%expect_test "narrower, and on a phone, threads show under their lines" =
  let h = three_threads () in
  H.quiet h (fun () ->
    H.act h (Resize (Viewport { width = 1440; height = 900 })));
  H.text h ~selector:".review-code";
  [%expect
    {|
    (Complete_accept (run true))
    (1 outdated comment)
    @@ -2,3 +2,4 @@
    2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    c1 lines 3–4
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Why double?
    []
    (c2 Is d needed? 2)
    5 let e = 5
    |}];
  H.quiet h (fun () ->
    H.act h (Resize (Viewport { width = 1920; height = 1000 }));
    H.act h (Set_narrow true));
  H.elements h ~selector:".review-margin";
  [%expect {| |}]
;;
