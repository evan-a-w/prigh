open! Core
module H = Harness

(* What changed in a file since the user's last action on it (approving,
   commenting, ...): the review page's third view next to Changes / Whole
   file, chosen by default for a file that changed after its last action,
   with a badge in the file list, a menu of older actions, and comments
   made in it anchored to the file as it is now. *)

let state () =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String "/work"
      ; "git_branch", `String "main"
      ]
    ()
;;

let diff =
  "diff --git a/a.ml b/a.ml\n\
   index 1111111..2222222 100644\n\
   --- a/a.ml\n\
   +++ b/a.ml\n\
   @@ -1,2 +1,3 @@\n\
   -let x = 1\n\
   +let x = 2\n\
  \ let y = 3\n\
   +let z = 4\n\
   diff --git a/b.ml b/b.ml\n\
   index 1111111..2222222 100644\n\
   --- a/b.ml\n\
   +++ b/b.ml\n\
   @@ -1 +1 @@\n\
   -let b = 1\n\
   +let b = 2\n"
;;

let diff_json =
  Jsonaf.to_string
    (`Object
        [ "root", `String "/work"
        ; "against", `String "HEAD"
        ; "diff", `String diff
        ; "truncated", `False
        ; "branch", `String "main"
        ; "base", `String "HEAD"
        ; "main", `True
        ; "branches", `Array [ `String "main" ]
        ])
;;

let snapshot ?(by = "") ~path ~action ~at md5 =
  sprintf
    {|{"root":"/work","path":"%s","action":"%s"%s,"at_ms":%s,"content":{"kind":"text","md5":"%s"}}|}
    path
    action
    (if String.is_empty by then "" else sprintf {|,"by":"%s"|} by)
    at
    md5
;;

(* a.ml commented on at 9:00, approved at 10:00 by bob (another user); b.ml
   approved. *)
let review_json =
  sprintf
    {|{"comments":[],"approvals":[{"root":"/work","path":"b.ml","hash":"x"}],"snapshots":[%s,%s,%s]}|}
    (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791190800000" "m0")
    (snapshot
       ~path:"a.ml"
       ~action:"approved"
       ~by:"bob"
       ~at:"1791194400000"
       "m1")
    (snapshot ~path:"b.ml" ~action:"approved" ~at:"1791194400000" "m2")
;;

(* The review after one more action on a.ml, at 10:00 by you. *)
let review_after action =
  sprintf
    {|{"comments":[],"approvals":[{"root":"/work","path":"b.ml","hash":"x"}],"snapshots":[%s,%s,%s,%s]}|}
    (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791190800000" "m0")
    (snapshot
       ~path:"a.ml"
       ~action:"approved"
       ~by:"bob"
       ~at:"1791194400000"
       "m1")
    (snapshot ~path:"a.ml" ~action ~at:"1791194400000" "now")
    (snapshot ~path:"b.ml" ~action:"approved" ~at:"1791194400000" "m2")
;;

let review_event action =
  sprintf
    {|{"type":"event","event":"review","review":%s}|}
    (review_after action)
;;

let since_diff =
  "diff --git a/a.ml b/a.ml\n\
   --- a/a.ml\n\
   +++ b/a.ml\n\
   @@ -1,1 +1,2 @@\n\
  \ let x = 2\n\
   -let y = 3\n\
   +let y = 30\n\
   +let z = 4\n"
;;

let file
      ?(changed = true)
      ?(index = 1)
      ?(added = 2)
      ?(removed = 1)
      ?note
      path
      snapshot
      diff
  =
  sprintf
    {|{"path":"%s","index":%d,"snapshot":%s,"current":{"kind":"text","md5":"now"},"changed":%b,"added":%d,"removed":%d,"diff":%s%s}|}
    path
    index
    snapshot
    changed
    added
    removed
    (Jsonaf.to_string (`String diff))
    (match note with
     | Some n -> sprintf {|,"note":"%s"|} n
     | None -> "")
;;

let since_json =
  sprintf
    {|{"files":[%s,%s]}|}
    (file
       "a.ml"
       (snapshot
          ~path:"a.ml"
          ~action:"approved"
          ~by:"bob"
          ~at:"1791194400000"
          "m1")
       since_diff)
    (file
       ~changed:false
       ~index:0
       ~added:0
       ~removed:0
       "b.ml"
       (snapshot ~path:"b.ml" ~action:"approved" ~at:"1791194400000" "m2")
       "")
;;

let page h = H.text h ~selector:".review-page"

let opened () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.reply h "get_review" review_json;
    H.type_ h "/review";
    H.act h Send;
    H.reply h "git_diff" diff_json;
    H.reply
      h
      "fs_preview"
      {|{"path":"/work/a.ml","name":"a.ml","size":30,"kind":"text","text":"let x = 2\nlet y = 30\nlet z = 4\n","truncated":false,"md5":"now"}|});
  h
;;

let%expect_test "a file changed since its last action shows what changed since" =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () -> H.reply h "get_review" review_json);
  H.type_ h "/review";
  H.act h Send;
  H.reply h "git_diff" diff_json;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /review) (max 500)))
     (tag Ignore))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    (Focus review-page)
    (Rpc (method_ fs_preview) (params ((path /work/a.ml)))
     (tag (Review (Content a.ml))))
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    |}];
  (* Before the since-diff is in, the file shows its changes as before. *)
  H.text h ~selector:".review-controls";
  [%expect
    {|
    (Comment) (Edit)
    (Changes) (Whole file)
    (Since latest (approval · 10:00)) (Compare with another action on this file: Latest follows your newest (Shift+L))
    (Send comments) (Close the review (Esc, Alt+R))
    |}];
  H.reply h "review_since" since_json;
  page h;
  [%expect
    {|
    Review
    0/2 approved
    (Read the changes again (they also update as prigh edits))
    (Comment) (Edit)
    (Changes) (Whole file)
    (Since latest (approval · 10:00)) (Compare with another action on this file: Latest follows your newest (Shift+L))
    (Send comments) (Close the review (Esc, Alt+R))
    Reviewing
    (main /work)
    against
    (HEAD the uncommitted changes)
    (○ a.ml +2 −1 since approved +2 −1) (More (right-click, Shift+F10))
    (⚠ b.ml +1 −1) (More (right-click, Shift+F10))
    2 files changed, +3 −2
    a.ml
    Since bob's approval · 10:00: +2 −1 (removed lines are the file as it was then)
    @@ -1,1 +1,2 @@
    1 let x = 2
    2 - let y = 3
    2 + let y = 30
    3 + let z = 4
    (←) (Approve & next Space) (→) File 1 of 2
    |}];
  H.names h ~selector:".segment.on";
  H.names h ~selector:".review-since-menu option";
  [%expect
    {|
    button "Comment" -- Select code and comment on it (e)
    button "Since latest (approval \194\183 10:00)" -- What changed since your latest action on the file, whichever it is as you act: now Approved · 10:00 · by bob (l)
    |}];
  (* b.ml has not changed since it was approved: it shows its changes, and
     its since-view says nothing changed. *)
  H.key h "]" ~target:Review;
  H.text h ~selector:".review-current";
  H.key h "l" ~target:Review;
  H.text h ~selector:".review-current";
  [%expect
    {|
    (Review (Step 1))
    (Rpc (method_ fs_preview) (params ((path /work/b.ml)))
     (tag (Review (Content b.ml))))
    b.ml
    Changed since you approved it
    @@ -1 +1 @@
    1 - let b = 1
    1 + let b = 2
    (←) (Approve again Space) (→) File 2 of 2
    (Review (Set_layout Since))
    b.ml
    Changed since you approved it
    Nothing changed since your approval · 10:00 · the changes as they are, below
    @@ -1 +1 @@
    1 - let b = 1
    1 + let b = 2
    (←) (Approve again Space) (→) File 2 of 2
    |}];
  (* Changes takes a.ml back to its hunks; it stays so. *)
  H.key h "[" ~target:Review;
  H.act h (Review (Set_layout Unified));
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    (Review (Step -1))
    @@ -1,2 +1,3 @@
    1 - let x = 1
    1 + let x = 2
    2 let y = 3
    3 + let z = 4
    |}]
;;

let%expect_test "an older action from the menu" =
  let h = opened () in
  H.reply h "review_since" since_json;
  H.act h (Review (Pick_since { path = "a.ml"; index = 0 }));
  [%expect
    {|
    (Rpc (method_ review_since) (params ((root /work) (path a.ml) (index 0)))
     (tag (Review (Since_picked (root /work) (path a.ml) (index 0)))))
    |}];
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    Reading what changed since your comment · 09:00…
    @@ -1,2 +1,3 @@
    1 - let x = 1
    1 + let x = 2
    2 let y = 3
    3 + let z = 4
    |}];
  H.reply
    h
    "review_since"
    (sprintf
       {|{"files":[%s]}|}
       (file
          ~index:0
          ~added:1
          ~removed:0
          "a.ml"
          (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791190800000" "m0")
          "diff --git a/a.ml b/a.ml\n\
           --- a/a.ml\n\
           +++ b/a.ml\n\
           @@ -1,0 +1,1 @@\n\
           +let z = 4\n"));
  H.text h ~selector:".review-controls";
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    (Comment) (Edit)
    (Changes) (Whole file)
    (Since comment · 09:00) (Compare with another action on this file: Latest follows your newest (Shift+L))
    (Send comments) (Close the review (Esc, Alt+R))
    Since your comment · 09:00: +1 −0
    @@ -1,0 +1,1 @@
    1 + let z = 4
    |}];
  (* The last again: no request, the latest diff. *)
  H.act h (Review (Pick_since { path = "a.ml"; index = 1 }));
  H.text h ~selector:".review-since-note";
  [%expect
    {| Since bob's approval · 10:00: +2 −1 (removed lines are the file as it was then) |}]
;;

let%expect_test "comments in the since-view are on the file as it is now" =
  let h = opened () in
  H.reply h "review_since" since_json;
  (* A removed line here is the old snapshot's: refused. *)
  H.act
    h
    (Review
       (Select_line { path = "a.ml"; side = Old; line = 2; extend = false }));
  [%expect {| (Expire_toast (id 0) (after_ms 6000)) |}];
  H.act
    h
    (Review
       (Select_line { path = "a.ml"; side = New; line = 2; extend = false }));
  H.key h "c" ~target:Review;
  H.act h (Review (Draft_input "Why 30?"));
  H.act h (Review Draft_send);
  [%expect
    {|
    Clear_text_selection
    (Review Start_comment)
    (Focus review-draft)
    (Rpc (method_ review_comment)
     (params
      ((root /work) (path a.ml) (first 2) (last 2) (side new)
       (anchor "let y = 30") (text "Why 30?")))
     (tag (Review Comment_added)))
    |}];
  H.reply
    h
    "review_comment"
    {|{"id":"c1","root":"/work","path":"a.ml","first":2,"last":2,"anchor":"let y = 30","status":"open","thread":[{"author":"user","text":"Why 30?","at_ms":1791194400000,"delivered":false}]}|};
  [%expect {| |}];
  (* Before the review event brings the comment's snapshot, the since-view
     still compares with the approval, the comment on its current line. *)
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    Since bob's approval · 10:00: +2 −1 (removed lines are the file as it was then)
    @@ -1,1 +1,2 @@
    1 let x = 2
    2 - let y = 3
    2 + let y = 30
    c1 line 2
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Why 30?
    []
    3 + let z = 4
    |}];
  H.event h (review_event "commented");
  [%expect
    {|
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    |}];
  H.reply
    h
    "review_since"
    (sprintf
       {|{"files":[%s]}|}
       (file
          ~changed:false
          ~index:2
          ~added:0
          ~removed:0
          "a.ml"
          (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791194400000" "now")
          ""));
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    Nothing changed since your comment · 10:00 · the changes as they are, below
    @@ -1,2 +1,3 @@
    1 - let x = 1
    1 + let x = 2
    2 let y = 3
    3 + let z = 4
    |}]
;;

let%expect_test "actions read what changed again; the editor's save is one" =
  let h = opened () in
  H.reply h "review_since" since_json;
  H.key h "a" ~target:Review;
  H.reply h "review_approve" (review_after "approved");
  [%expect
    {|
    (Review (Toggle_approval a.ml))
    (Rpc (method_ review_approve)
     (params ((root /work) (path a.ml) (hash 185f77487f5bea33a200aac854de7910)))
     (tag (Review (Changed ()))))
    (Rpc (method_ fs_preview) (params ((path /work/b.ml)))
     (tag (Review (Content b.ml))))
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    |}];
  H.reply h "review_since" since_json;
  (* Approving went on to b.ml: back to a.ml. *)
  H.quiet h (fun () -> H.key h "ArrowLeft" ~target:Review);
  H.key h "e" ~target:Review;
  H.act h (Review (Edit_input "let x = 2\n"));
  H.act h (Review Save);
  [%expect
    {|
    (Review (Step -1))
    (Review (Set_mode Edit))
    (Focus review-editor)
    (Rpc (method_ fs_write)
     (params
      ((path /work/a.ml) (text "let x = 2\n") (expected now) (review_root /work)))
     (tag (Review (Saved a.ml))))
    |}];
  (* The save's snapshot comes with the review event; the diff is read
     again too, and with it what changed since. *)
  H.event h (review_event "edited");
  H.reply h "fs_write" {|{"path":"/work/a.ml","md5":"edited"}|};
  [%expect
    {|
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    (Rpc (method_ git_status) (params ()) (tag (Git Status)))
    |}];
  H.reply h "git_diff" diff_json;
  [%expect {| |}]
;;

let%expect_test "an older backend: no since-view" =
  let h = opened () in
  H.fail h "review_since" "unknown method \"review_since\"";
  H.text h ~selector:".review-files";
  [%expect
    {|
    (○ a.ml +2 −1) (More (right-click, Shift+F10))
    (⚠ b.ml +1 −1) (More (right-click, Shift+F10))
    2 files changed, +3 −2
    |}]
;;

let comment_c1 ?(replies = "") () =
  sprintf
    {|{"id":"c1","root":"/work","path":"a.ml","first":2,"last":2,"anchor":"let y = 30","status":"open","thread":[{"author":"user","text":"Why 30?","at_ms":1791194400000,"delivered":false}%s]}|}
    replies
;;

(* a.ml's review once commented on (at 10:00) and replied to (10:05). *)
let review_commented ?(replied = false) () =
  sprintf
    {|{"type":"event","event":"review","review":{"comments":[%s],"approvals":[{"root":"/work","path":"b.ml","hash":"x"}],"snapshots":[%s,%s,%s%s,%s]}}|}
    (comment_c1
       ~replies:
         (if replied
          then
            {|,{"author":"user","text":"Or 31?","at_ms":1791194700000,"delivered":false}|}
          else "")
       ())
    (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791190800000" "m0")
    (snapshot
       ~path:"a.ml"
       ~action:"approved"
       ~by:"bob"
       ~at:"1791194400000"
       "m1")
    (snapshot ~path:"a.ml" ~action:"commented" ~at:"1791194400000" "now")
    (if replied
     then
       "," ^ snapshot ~path:"a.ml" ~action:"replied" ~at:"1791194700000" "now"
     else "")
    (snapshot ~path:"b.ml" ~action:"approved" ~at:"1791194400000" "m2")
;;

let unchanged ~index ~action ~at =
  sprintf
    {|{"files":[%s]}|}
    (file
       ~changed:false
       ~index
       ~added:0
       ~removed:0
       "a.ml"
       (snapshot ~path:"a.ml" ~action ~at "now")
       "")
;;

let%expect_test
    "commenting shows the since-view of the latest action; Latest follows \
     newer ones; the menu names them"
  =
  let h = opened () in
  H.reply h "review_since" since_json;
  (* The hunks, chosen. *)
  H.act h (Review (Set_layout Unified));
  H.names h ~selector:".review-controls .segment.on";
  [%expect
    {|
    button "Comment" -- Select code and comment on it (e)
    button "Changes" -- Only the changed hunks (v)
    |}];
  H.quiet h (fun () ->
    H.act
      h
      (Review
         (Select_line { path = "a.ml"; side = New; line = 2; extend = false }));
    H.key h "c" ~target:Review;
    H.act h (Review (Draft_input "Why 30?"));
    H.act h (Review Draft_send));
  H.reply h "review_comment" (comment_c1 ());
  H.event h (review_commented ());
  H.reply
    h
    "review_since"
    (unchanged ~index:2 ~action:"commented" ~at:"1791194400000");
  [%expect
    {|
    (Review Start_comment)
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    |}];
  (* Since the comment, named by its line: nothing yet, so the changes
     under the note, the comment by its line. *)
  H.text h ~selector:".review-controls .segmented:last-of-type";
  H.text h ~selector:".review-scroll";
  [%expect
    {|
    (Changes) (Whole file)
    (Since latest (comment · 10:00)) (Compare with another action on this file: Latest follows your newest (Shift+L))
    Nothing changed since your comment on line 2 · 10:00 · the changes as they are, below
    @@ -1,2 +1,3 @@
    1 - let x = 1
    1 + let x = 2
    2 let y = 3
    c1 line 2
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Why 30?
    []
    3 + let z = 4
    |}];
  (* A reply: Latest follows it. *)
  H.quiet h (fun () ->
    H.act h (Review (Set_layout Full));
    H.act h (Review (Reply_input { id = "c1"; text = "Or 31?" }));
    H.act h (Review (Reply_send "c1")));
  H.reply h "review_reply" (comment_c1 ());
  H.event h (review_commented ~replied:true ());
  H.reply
    h
    "review_since"
    (unchanged ~index:3 ~action:"replied" ~at:"1791194700000");
  H.text h ~selector:".review-since";
  H.text h ~selector:".review-since-note";
  [%expect
    {|
    (Rpc (method_ review_since) (params ((root /work)))
     (tag (Review (Since /work))))
    (Since latest (reply · 10:05)) (Compare with another action on this file: Latest follows your newest (Shift+L))
    Nothing changed since your reply on line 2 · 10:05 · the changes as they are, below
    |}];
  (* Shift+L: the menu, Latest first, then the earlier actions by what was
     done where. *)
  H.key h "L" ~shift:true ~target:Review;
  H.names h ~selector:"#review-popover .review-pick-item";
  [%expect
    {|
    (Review_pick (Since a.ml))
    (Focus review-pick-filter)
    [option] div "Latest follows your newest action: Reply on line 2 \194\183 10:05"
    [option] div "Comment on line 2 \194\183 10:00"
    [option] div "Approved \194\183 10:00 \194\183 by bob"
    [option] div "Comment \194\183 09:00"
    |}];
  H.click h ~selector:"[data-id='since:2']";
  H.text h ~selector:".review-since";
  [%expect
    {|
    (Picker_choose since:2)
    (Rpc (method_ review_since) (params ((root /work) (path a.ml) (index 2)))
     (tag (Review (Since_picked (root /work) (path a.ml) (index 2)))))
    (Since comment on line 2 · 10:00) (Compare with another action on this file: Latest follows your newest (Shift+L))
    |}];
  (* Latest again: the newest, whichever it is. *)
  H.click h ~selector:"#review-since-button";
  H.click h ~selector:"[data-id='since:latest']";
  H.text h ~selector:".review-since";
  [%expect
    {|
    (Review_pick (Since a.ml))
    (Focus review-pick-filter)
    (Picker_choose since:latest)
    (Since latest (reply · 10:05)) (Compare with another action on this file: Latest follows your newest (Shift+L))
    |}]
;;
