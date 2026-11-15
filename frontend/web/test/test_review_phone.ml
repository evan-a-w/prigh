open! Core
open Prigh_web
module H = Harness
module Row = Diff_view.Row

(* Reading and reviewing code on a phone: wrapped lines hanging under their
   indent, the words that changed, the context folded around the changes,
   generated files and renames folded, the dock's change steps, the sheet
   that selects lines by tap and writes (or quickly asks) comments. *)

(* ---- the pure parts *)

let%expect_test "hanging indent: the line's own indent, plus 2" =
  List.iter
    [ "let x = 1"
    ; "    foo bar"
    ; "        deeply nested"
    ; "\tone tab"
    ; "\t\ttwo tabs"
    ; "  \tmixed"
    ; ""
    ; "      "
    ]
    ~f:(fun line -> printf "%-26S %d\n" line (Diff_view.hang line));
  [%expect
    {|
    "let x = 1"                2
    "    foo bar"              6
    "        deeply nested"    10
    "\tone tab"                8
    "\t\ttwo tabs"             16
    "  \tmixed"                8
    ""                         2
    "      "                   8
    |}]
;;

let show_words ~old ~new_ =
  match Word_diff.ranges ~old ~new_ with
  | None -> print_endline "(whole lines)"
  | Some (o, n) ->
    let mark text ranges =
      List.fold
        (List.rev ranges)
        ~init:text
        ~f:(fun text (r : Word_diff.Range.t) ->
          String.prefix text r.start
          ^ "["
          ^ String.sub text ~pos:r.start ~len:(r.end_ - r.start)
          ^ "]"
          ^ String.drop_prefix text r.end_)
    in
    printf "- %s\n+ %s\n" (mark old o) (mark new_ n)
;;

let%expect_test "word diff: only what changed within paired lines" =
  show_words
    ~old:"let greet name = \"Hello, \" ^ name"
    ~new_:"let salute name = \"Hi, \" ^ name";
  show_words ~old:"  if x > 0 then f x" ~new_:"  if x >= 0 then f x else g x";
  show_words ~old:"let c = 3" ~new_:"let c = 3 * 2";
  show_words ~old:"completely different" ~new_:"nothing alike here at all!";
  show_words ~old:"same" ~new_:"same";
  [%expect
    {|
    - let [greet] name = "[Hello], " ^ name
    + let [salute] name = "[Hi], " ^ name
    -   if x [>] 0 then f x
    +   if x [>=] 0 then f x[ else g x]
    - let c = 3
    + let c = 3[ * 2]
    (whole lines)
    (whole lines)
    |}]
;;

let line kind n text : Row.t =
  match kind with
  | `Same -> Line { kind; old_line = Some n; new_line = Some n; text }
  | `Added -> Line { kind; old_line = None; new_line = Some n; text }
  | `Removed -> Line { kind; old_line = Some n; new_line = None; text }
;;

let print_rows rows =
  List.iter rows ~f:(fun (row : Row.t) ->
    match row with
    | Hunk h -> print_endline h
    | Gap g -> printf "   ··· %s (id %d)\n" (Diff_view.gap_text g) g.id
    | Line { kind; new_line; old_line; text } ->
      printf
        "%3d %s %s\n"
        (Option.value (Option.first_some new_line old_line) ~default:0)
        (match kind with
         | `Same -> " "
         | `Added -> "+"
         | `Removed -> "-")
        text)
;;

let file_rows =
  List.init 30 ~f:(fun i -> line `Same (i + 1) (sprintf "line %d" (i + 1)))
  |> List.concat_mapi ~f:(fun i row ->
    if i = 14
    then [ line `Removed 15 "old 15"; line `Added 15 "new 15" ]
    else [ row ])
;;

let%expect_test "context: three lines around a change, the rest in gaps" =
  print_rows (Context_fold.fold Context_fold.empty file_rows);
  [%expect
    {|
       ··· 11 lines hidden (id 1)
     12   line 12
     13   line 13
     14   line 14
     15 - old 15
     15 + new 15
     16   line 16
     17   line 17
     18   line 18
       ··· 12 lines hidden (id 19)
    |}];
  (* Showing more: 20 under the rows above the second gap (all of it), then
     the first gap's lines from below. *)
  let gaps rows =
    List.filter_map rows ~f:(function
      | Row.Gap g -> Some g
      | Line _ | Hunk _ -> None)
  in
  let folded = Context_fold.fold Context_fold.empty file_rows in
  let first, second =
    match gaps folded with
    | [ a; b ] -> a, b
    | _ -> assert false
  in
  print_s [%sexp (first : Diff_view.Gap.t), (second : Diff_view.Gap.t)];
  let t = Context_fold.reveal Context_fold.empty second Down in
  let t = Context_fold.reveal t first Up in
  print_rows (Context_fold.fold t file_rows);
  [%expect
    {|
    (((id 1) (first 1) (hidden 11) (before false) (after true))
     ((id 19) (first 19) (hidden 12) (before true) (after false)))
      1   line 1
      2   line 2
      3   line 3
      4   line 4
      5   line 5
      6   line 6
      7   line 7
      8   line 8
      9   line 9
     10   line 10
     11   line 11
     12   line 12
     13   line 13
     14   line 14
     15 - old 15
     15 + new 15
     16   line 16
     17   line 17
     18   line 18
     19   line 19
     20   line 20
     21   line 21
     22   line 22
     23   line 23
     24   line 24
     25   line 25
     26   line 26
     27   line 27
     28   line 28
     29   line 29
     30   line 30
    |}]
;;

let%expect_test "context: short runs between changes stay" =
  let rows =
    [ line `Removed 1 "a"
    ; line `Added 1 "A"
    ; line `Same 2 "b"
    ; line `Same 3 "c"
    ; line `Same 4 "d"
    ; line `Same 5 "e"
    ; line `Same 6 "f"
    ; line `Same 7 "g"
    ; line `Same 8 "h"
    ; line `Added 9 "I"
    ]
  in
  print_rows (Context_fold.fold Context_fold.empty rows);
  [%expect
    {|
      1 - a
      1 + A
      2   b
      3   c
      4   d
      5   e
      6   f
      7   g
      8   h
      9 + I
    |}]
;;

let file
      ?(status = Prigh_ui.Diff_files.File.Status.Modified)
      ?(hunks = [])
      path
      ~added
      ~removed
  : Prigh_ui.Diff_files.File.t
  =
  { path; status; added; removed; hunks; hash = path }
;;

let%expect_test "generated files and pure renames are folded" =
  List.iter
    [ "flake.lock"
    ; "frontend/package-lock.json"
    ; "Cargo.lock"
    ; "prigh.opam.locked"
    ; "dist/app.min.js"
    ; "_build/default/x.ml"
    ; "web/node_modules/a/index.js"
    ; "src/lock.ml"
    ; "src/main.ml"
    ; "README.md"
    ]
    ~f:(fun p -> printf "%-28s %b\n" p (Folded_files.generated p));
  [%expect
    {|
    flake.lock                   true
    frontend/package-lock.json   true
    Cargo.lock                   true
    prigh.opam.locked            true
    dist/app.min.js              true
    _build/default/x.ml          true
    web/node_modules/a/index.js  true
    src/lock.ml                  false
    src/main.ml                  false
    README.md                    false
    |}];
  let files =
    [ file
        "src/old.ml"
        ~status:Deleted
        ~added:0
        ~removed:2
        ~hunks:[ "@@ -1,2 +0,0 @@\n-let a = 1\n-let b = 2" ]
    ; file
        "src/new.ml"
        ~status:Added
        ~added:2
        ~removed:0
        ~hunks:[ "@@ -0,0 +1,2 @@\n+let a = 1\n+let b = 2" ]
    ; file
        "src/other.ml"
        ~status:Added
        ~added:1
        ~removed:0
        ~hunks:[ "@@ -0,0 +1 @@\n+let a = 1" ]
    ; file "flake.lock" ~added:3 ~removed:3
    ; file "src/main.ml" ~added:1 ~removed:1
    ]
  in
  print_s
    [%sexp (Folded_files.classify files : Folded_files.Why.t String.Map.t)];
  [%expect
    {|
    ((flake.lock Generated) (src/new.ml (Renamed_from src/old.ml))
     (src/old.ml (Renamed_to src/new.ml)))
    |}]
;;

(* ---- the page on a phone *)

let state () =
  H.state_json
    ~fields:
      [ "session_id", `String "s1"
      ; "session_path", `String "/sessions/s1.jsonl"
      ; "cwd", `String "/home/me/proj"
      ]
    ()
;;

let long_file =
  String.concat
    (List.init 20 ~f:(fun i ->
       if i = 9
       then
         "        let result = compute_the_answer ~with_a_long_label:argument \
          ~and_another:(nested call here)\n"
       else sprintf "let v%d = %d\n" (i + 1) (i + 1)))
;;

let long_diff =
  "diff --git a/src/long.ml b/src/long.ml\n\
   index 1111111..2222222 100644\n\
   --- a/src/long.ml\n\
   +++ b/src/long.ml\n\
   @@ -7,7 +7,7 @@\n\
  \ let v7 = 7\n\
  \ let v8 = 8\n\
  \ let v9 = 9\n\
   -        let result = compute_an_answer ~with_a_long_label:argument \
   ~and_another:(nested call here)\n\
   +        let result = compute_the_answer ~with_a_long_label:argument \
   ~and_another:(nested call here)\n\
  \ let v11 = 11\n\
  \ let v12 = 12\n\
  \ let v13 = 13\n"
;;

let lock_diff =
  "diff --git a/flake.lock b/flake.lock\n\
   index 3333333..4444444 100644\n\
   --- a/flake.lock\n\
   +++ b/flake.lock\n\
   @@ -1 +1 @@\n\
   -{\"rev\": \"aaaa\"}\n\
   +{\"rev\": \"bbbb\"}\n"
;;

let rename_diff =
  "diff --git a/src/old_name.ml b/src/old_name.ml\n\
   deleted file mode 100644\n\
   index 5555555..0000000\n\
   --- a/src/old_name.ml\n\
   +++ /dev/null\n\
   @@ -1 +0,0 @@\n\
   -let moved = true\n\
   diff --git a/src/new_name.ml b/src/new_name.ml\n\
   new file mode 100644\n\
   index 0000000..5555555\n\
   --- /dev/null\n\
   +++ b/src/new_name.ml\n\
   @@ -0,0 +1 @@\n\
   +let moved = true\n"
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

let preview_json path text =
  Jsonaf.to_string
    (`Object
        [ "path", `String path
        ; "name", `String (Filename.basename path)
        ; "size", `Number (Int.to_string (String.length text))
        ; "kind", `String "text"
        ; "text", `String text
        ; "truncated", `False
        ; "md5", `String "m1"
        ])
;;

let opened () =
  let h = H.create ~state:(state ()) () in
  H.quiet h (fun () ->
    H.act h (Set_narrow true);
    H.reply h "get_review" {|{"comments":[],"approvals":[]}|};
    H.type_ h "/review";
    H.key h "Enter";
    H.reply h "git_diff" (diff_json (long_diff ^ lock_diff ^ rename_diff));
    H.reply h "fs_preview" (preview_json "/home/me/proj/src/long.ml" long_file));
  h
;;

let%expect_test "a phone: the file folded to the change's context, words marked"
  =
  let h = opened () in
  H.text h ~selector:".review-diff";
  [%expect
    {|
    (Complete_accept (run true))
    6 lines hidden (Show all)
    7 let v7 = 7
    8 let v8 = 8
    9 let v9 = 9
    10 - let result = compute_an_answer ~with_a_long_label:argument ~and_another:(nested call here)
    10 + let result = compute_the_answer ~with_a_long_label:argument ~and_another:(nested call here)
    11 let v11 = 11
    12 let v12 = 12
    13 let v13 = 13
    7 lines hidden (Show all)
    |}];
  H.elements h ~selector:".review-diff .wd";
  H.elements h ~selector:".review-diff tr.change-start";
  [%expect
    {|
    <span class="wd">
    <span class="wd">
    <tr class="change-start del">
    |}];
  H.click h ~selector:".review-gap-show";
  H.text h ~selector:".review-diff tr:first-child";
  [%expect
    {|
    (Review
     (Reveal (path src/long.ml)
      (gap ((id 1) (first 1) (hidden 6) (before false) (after true))) (how All)))
    1 let v1 = 1
    |}]
;;

let%expect_test
    "a phone: the dock steps through changes; w wraps; + and - size the code"
  =
  let h = opened () in
  H.text h ~selector:".review-dock";
  H.names h ~selector:".review-dock button";
  [%expect
    {|
    (Complete_accept (run true))
    (←) (Previous change (Shift+K)) (Approve & next Space) (Next change (Shift+J)) (→) File 1 of 4
    button "Previous file" -- Previous file (←, [, k)
    button "Previous change" -- Previous change (Shift+K)
    button "Approve & next Space" -- Approve this file as it is now and go on to the next (Space)
    button "Next change" -- Next change (Shift+J)
    button "Next file" -- Next file (→, ], j)
    |}];
  H.click h ~selector:".review-dock-step.change:nth-of-type(2)";
  H.key h "J" ~shift:true ~target:Review;
  H.key h "K" ~shift:true ~target:Review;
  [%expect
    {|
    (Review (Step_change -1))
    (Scroll_change -1)
    (Review (Step_change 1))
    (Scroll_change 1)
    (Review (Step_change -1))
    (Scroll_change -1)
    |}];
  H.key h "w" ~target:Review;
  H.elements h ~selector:".review-code";
  [%expect
    {|
    (Review (Toggle_wrap src/long.ml))
    <div class="review-code scrolls">
    |}];
  H.key h "+" ~target:Review;
  H.key h "+" ~target:Review;
  H.key h "-" ~target:Review;
  H.elements h ~selector:".app";
  [%expect
    {|
    (Code_size 1)
    (Cache_prefs (((key web.code-size) (value (large)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.code-size large)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 2940))
    (Code_size 1)
    (Cache_prefs (((key web.code-size) (value (huge)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.code-size huge)))))
     (tag Show_error))
    (Expire_toast (id 1) (after_ms 2900))
    (Code_size -1)
    (Cache_prefs (((key web.code-size) (value (large)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.code-size large)))))
     (tag Show_error))
    (Expire_toast (id 2) (after_ms 2940))
    <div class="app code-large narrow reviewing">
    |}];
  H.click h ~selector:".review-display-button";
  H.text h ~selector:".review-sheet";
  H.key h "Escape" ~target:Review;
  H.elements h ~selector:".review-sheet";
  [%expect
    {|
    (Review Toggle_display)
    Display (Done)
    Long lines
    (Wrap) (Scroll)
    Code size
    (A−) large (A+)
    (Review Toggle_display)
    |}]
;;

let%expect_test
    "a phone: a tap selects a line, another extends it, the sheet comments"
  =
  let h = opened () in
  H.click h ~selector:"tr[data-line='9']";
  H.text h ~selector:".review-sheet";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Tap_line (path src/long.ml) (side New) (line 9)))
    Clear_text_selection
    line 9 Tap another line to select to it (Clear the selection (Esc))
    (Explain this) (Why?) (Revert this) (Add a test)
    (Write a comment)
    |}];
  H.click h ~selector:"tr.add[data-line='10']";
  H.text h ~selector:".review-sheet-head";
  H.elements h ~selector:"tr.selected";
  [%expect
    {|
    (Review (Tap_line (path src/long.ml) (side New) (line 10)))
    Clear_text_selection
    lines 9–10 Tap another line to select to it (Clear the selection (Esc))
    <tr class="same sel-first selected">
    <tr class="add selected">
    |}];
  (* The only line selected, tapped again: unselected. *)
  H.click h ~selector:".review-sheet-close";
  H.click h ~selector:"tr[data-line='8']";
  H.click h ~selector:"tr[data-line='8']";
  H.elements h ~selector:".review-sheet";
  [%expect
    {|
    (Review Clear_selection)
    Clear_text_selection
    (Review (Tap_line (path src/long.ml) (side New) (line 8)))
    Clear_text_selection
    (Review (Tap_line (path src/long.ml) (side New) (line 8)))
    Clear_text_selection
    |}];
  H.click h ~selector:"tr[data-line='8']";
  H.click h ~selector:".review-sheet-comment";
  H.elements h ~selector:".review-sheet textarea";
  [%expect
    {|
    (Review (Tap_line (path src/long.ml) (side New) (line 8)))
    Clear_text_selection
    (Review Start_comment)
    (Focus review-draft)
    <textarea class="review-field">
    |}]
;;

let%expect_test "a phone: a quick ask comments and sends the review" =
  let h = opened () in
  H.quiet h (fun () -> H.click h ~selector:"tr.add[data-line='10']");
  H.names h ~selector:".review-chip";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Tap_line (path src/long.ml) (side New) (line 10)))
    button "Explain this" -- Explain this code: what it does and how it fits in. (sent to prigh at once)
    button "Why?" -- Why this change? What does it fix or enable? (sent to prigh at once)
    button "Revert this" -- Revert this change. (sent to prigh at once)
    button "Add a test" -- Add a test that covers this. (sent to prigh at once)
    |}];
  H.click h ~selector:".review-chip:nth-child(2)";
  [%expect
    {|
    (Review (Quick Why))
    (Rpc (method_ review_comment)
     (params
      ((root /home/me/proj) (path src/long.ml) (first 10) (last 10) (side new)
       (anchor
        "        let result = compute_the_answer ~with_a_long_label:argument ~and_another:(nested call here)")
       (text "Why this change? What does it fix or enable?")))
     (tag (Review Comment_added)))
    |}];
  H.reply
    h
    "review_comment"
    {|{"id":"c1","root":"/home/me/proj","path":"src/long.ml","first":10,"last":10,"anchor":"x","status":"open","thread":[{"author":"user","text":"Why this change? What does it fix or enable?","at_ms":1791194400000,"delivered":false}]}|};
  [%expect
    {|
    (Rpc (method_ review_submit) (params ((root /home/me/proj)))
     (tag (Review Submitted)))
    |}]
;;

let%expect_test "a phone: the lock file folded, approved with the others" =
  let h = opened () in
  H.quiet h (fun () -> H.key h "n" ~target:Review);
  H.text h ~selector:".review-code";
  [%expect
    {|
    (Complete_accept (run true))
    (Review (Step 1))
    generated A generated file (a lock file, build output or minified code): its changes are folded.
    (Show the changes) (Approve the 3 generated and renamed)
    |}];
  H.click h ~selector:".review-folded-actions button:nth-child(2)";
  H.text h ~selector:".review-dock";
  [%expect
    {|
    (Review
     (Approve_files (paths (flake.lock src/old_name.ml src/new_name.ml))
      (approve true)))
    (Rpc (method_ review_approve)
     (params
      ((root /home/me/proj) (path flake.lock)
       (hash e568c18f1cfd6e95a1b92713017c7d1f)))
     (tag (Review (Changed ()))))
    (Rpc (method_ review_approve)
     (params
      ((root /home/me/proj) (path src/old_name.ml)
       (hash dc7008bbf4d715a2af9a71126b5f1ab5)))
     (tag (Review (Changed ()))))
    (Rpc (method_ review_approve)
     (params
      ((root /home/me/proj) (path src/new_name.ml)
       (hash 59080f89a7c1855324c5d037486d7ea3)))
     (tag (Review (Changed ()))))
    (←) (Previous change (Shift+K)) (Approve & next Space) (Next change (Shift+J)) (→) File 2 of 4
    |}];
  H.click h ~selector:".review-folded-actions button";
  H.elements h ~selector:".review-diff";
  [%expect
    {|
    (Review (Unfold file:flake.lock))
    <table class="diff review-diff">
    |}]
;;
