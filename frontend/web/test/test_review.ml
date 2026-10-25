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
      id
      ~first
      ~last
      text
  =
  Jsonaf.to_string
    (`Object
        [ "id", `String id
        ; "root", `String "/home/me/proj"
        ; "path", `String "src/main.ml"
        ; "first", `Number (Int.to_string first)
        ; "last", `Number (Int.to_string last)
        ; "anchor", `String anchor
        ; "status", `String status
        ; ( "thread"
          , `Array
              (Jsonaf.of_string
                 (sprintf
                    {|{"author":"user","text":%S,"at_ms":1791194400000,"delivered":false}|}
                    text)
               :: List.map thread ~f:Jsonaf.of_string) )
        ])
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
    (○ src/ main.ml +2 −1)
    (○ src/ util.ml +2 −0)
    2 files changed, +4 −1
    (Previous file ([, p, k)) (Next file (], n, j))
    src/main.ml
    (Approve)
    @@ -2,3 +2,4 @@
    2 2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    4 5 let e = 5
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
     (params ((path src/main.ml) (hash 7f57d42d80a860a5b495b9ed7f7e860e)))
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
  [%expect {| (Rpc (method_ git_diff) (params ()) (tag (Review Diff))) |}];
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
    (Focus review-page)
    |}]
;;

let%expect_test
    "commenting on lines, sending, the agent's reply and its proposal"
  =
  let h = opened () in
  H.click h ~selector:"tr[data-line='3'] td.ln.pick";
  H.act
    h
    (Review (Select_line { path = "src/main.ml"; line = 4; extend = true }));
  H.text h ~selector:".review-select-bar";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Select_line (path src/main.ml) (line 3) (extend false)))
    (Comment on lines 3–4) (Clear)
    |}];
  H.elements h ~selector:"tr.selected";
  [%expect
    {|
    <tr class="add sel-first selected">
    <tr class="add selected">
    |}];
  H.key h "c" ~target:Review;
  H.input h ~selector:"#review-draft" "Why double it?";
  H.click h ~selector:".review-draft-actions .btn.primary";
  [%expect
    {|
    (Review Start_comment)
    (Focus review-draft)
    (Review (Draft_input "Why double it?"))
    (Review Draft_send)
    (Rpc (method_ review_comment)
     (params
      ((root /home/me/proj) (path src/main.ml) (first 3) (last 4)
       (anchor  "let c = 3 * 2\
               \nlet d = 4")
       (text "Why double it?")))
     (tag (Review Comment_added)))
    |}];
  H.reply
    h
    "review_comment"
    (comment_json "c1" ~first:3 ~last:4 "Why double it?");
  H.text h ~selector:".review-thread";
  [%expect
    {|
    c1 lines 3–4
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Why double it?
    []
    |}];
  H.text h ~selector:".review-controls .btn.primary";
  H.key h "s" ~target:Review;
  [%expect
    {|
    (Send 1 comment)
    (Review Submit)
    (Rpc (method_ review_submit) (params ()) (tag (Review Submitted)))
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
       "Why double it?"
       ~thread:
         [ {|{"author":"agent","text":"Or:","at_ms":1791194460000,"delivered":true,
              "proposal":{"edits":[{"old_text":"3 * 2","new_text":"3"}],"state":"accepted"}}|}
         ]);
  [%expect {| (Rpc (method_ git_diff) (params ()) (tag (Review Diff))) |}];
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
     (tag (Review (Changed (c1)))))
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
    "a comment whose lines moved follows them; one whose lines are gone shows \
     above"
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
    c2 line 2 its lines changed
    (Resolve) (Delete the comment and its thread)
    You 10:00 not sent yet
    Gone?
    []
    @@ -2,3 +2,4 @@
    2 2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    4 5 let e = 5
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
    1 1 let a = 1
    2 2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    4 5 let e = 5
    5 6 let f = 6
    |}];
  H.act h (Review (Set_mode Comment));
  H.text h ~selector:".review-code";
  [%expect
    {|
    1 1 let a = 1
    2 2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    4 5 let e = 5
    5 6 let f = 6
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
       (expected m1)))
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
        \n")))
     (tag (Review (Saved src/main.ml))))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
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
    2 2 let b = 2
    3 - let c = 3
    3 + let c = 3 * 2
    4 + let d = 4
    4 5 let e = 5
    |}];
  H.click h ~selector:".files-preview-body .review-open";
  [%expect
    {|
    (Review (Open (src/main.ml)))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
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

let%expect_test "help lists the review's keys" =
  List.iter Prigh_web.Keys.help ~f:(fun (key, what) ->
    if String.is_substring key ~substring:"review" || String.equal key "Alt+R"
    then printf "%s: %s\n" key what);
  [%expect
    {|
    Alt+R: open or close the review page (/review): the changes file by file, beside the chat
    ] / [ (reviewing; also n p, j k): the next / previous changed file
    a / c / e / v / s (reviewing): approve the file; comment on the selected lines (click a line number, Shift+click for a range); edit or comment; the whole file or the changes; send the new comments to prigh
    Esc (reviewing): drop the comment or selection, then close the review
    Ctrl+Enter / Ctrl+S (reviewing): add the comment or reply; save the file being edited
    |}]
;;
