open! Core
open Prigh_protocol
module Code_selection = Prigh_web.Code_selection
module Point = Code_selection.Point
module Comment_anchor = Prigh_ui.Comment_anchor

(* Selected code made into a comment's range: what the page reports (rows
   and UTF-16 offsets) as lines, columns and the side; and finding a
   comment's code again once the file changed. *)

let rows text = Prigh_web.Diff_view.unified_rows text

let diff =
  rows
    "@@ -2,3 +2,4 @@\n\
    \ let b = 2\n\
     -let c = 3\n\
     +let c = 3 * 2\n\
     +let d = \"é😀\"\n\
    \ let e = 5\n\
     @@ -10,2 +11,2 @@ let tail =\n\
     -let y = 0\n\
     +let y = 1\n\
    \ let z = 2\n"
;;

let file =
  [| "let a = 1"
   ; "let b = 2"
   ; "let c = 3 * 2"
   ; "let d = \"é😀\""
   ; "let e = 5"
   ; "let f = 6"
   ; "let g = 7"
   ; "let h = 8"
   ; "let i = 9"
   ; "let x = 10"
   ; "let y = 1"
   ; "let z = 2"
  |]
;;

let p side line offset = { Point.side; line; offset }

let show ?(lines = Some file) start end_ =
  match Code_selection.resolve diff ~lines ~start ~end_ with
  | None -> print_endline "nothing"
  | Some (range, anchor) ->
    print_s [%sexp (range : Review_comment.Range.t)];
    printf "%S\n" (Review_comment.Range.selected range ~anchor);
    if Option.is_some range.columns then printf "  of %S\n" anchor
;;

let%expect_test "within a line, across lines, backwards" =
  show (p New 3 4) (p New 3 5);
  show (p New 3 8) (p New 4 5);
  show (p New 4 5) (p New 3 8);
  [%expect
    {|
    ((first 3) (last 3) (side New) (columns ((start_col 4) (end_col 5))))
    "c"
      of "let c = 3 * 2"
    ((first 3) (last 4) (side New) (columns ((start_col 8) (end_col 5))))
    "3 * 2\nlet d"
      of "let c = 3 * 2\nlet d = \"\195\169\240\159\152\128\""
    ((first 3) (last 4) (side New) (columns ((start_col 8) (end_col 5))))
    "3 * 2\nlet d"
      of "let c = 3 * 2\nlet d = \"\195\169\240\159\152\128\""
    |}]
;;

let%expect_test "offsets are UTF-16 code units; columns are bytes" =
  (* é is one unit (two bytes), 😀 two (four bytes). *)
  show (p New 4 9) (p New 4 12);
  [%expect
    {|
    ((first 4) (last 4) (side New) (columns ((start_col 9) (end_col 15))))
    "\195\169\240\159\152\128"
      of "let d = \"\195\169\240\159\152\128\""
    |}]
;;

let%expect_test "whole lines: from a line's start to the next one's" =
  show (p New 3 0) (p New 4 0);
  show (p New 2 0) (p New 2 Int.max_value);
  (* Starting at a line's end is starting on the next. *)
  show (p New 2 9) (p New 3 5);
  [%expect
    {|
    ((first 3) (last 3) (side New))
    "let c = 3 * 2"
    ((first 2) (last 2) (side New))
    "let b = 2"
    ((first 3) (last 3) (side New) (columns ((start_col 0) (end_col 5))))
    "let c"
      of "let c = 3 * 2"
    |}]
;;

let%expect_test "removed lines: the old text; mixed, the removed ends go" =
  show (p Old 3 4) (p Old 3 5);
  show (p Old 3 2) (p New 3 5);
  show (p New 2 4) (p New 3 5);
  show (p New 2 4) (p Old 3 5);
  [%expect
    {|
    ((first 3) (last 3) (side Old) (columns ((start_col 4) (end_col 5))))
    "c"
      of "let c = 3"
    ((first 3) (last 3) (side New) (columns ((start_col 0) (end_col 5))))
    "let c"
      of "let c = 3 * 2"
    ((first 2) (last 3) (side New) (columns ((start_col 4) (end_col 5))))
    "b = 2\nlet c"
      of "let b = 2\nlet c = 3 * 2"
    ((first 2) (last 2) (side New) (columns ((start_col 4) (end_col 9))))
    "b = 2"
      of "let b = 2"
    |}]
;;

let%expect_test "across hunks: the lines between from the file" =
  show (p New 5 4) (p New 11 3);
  show ~lines:None (p New 5 4) (p New 11 3);
  (* Removed lines with kept ones between: on the new text. *)
  show (p Old 3 0) (p Old 10 3);
  (* Only removed lines, of two hunks: as far as the first runs on. *)
  (match
     Code_selection.resolve
       (rows "@@ -1,2 +1,0 @@\n-a\n-b\n@@ -5,1 +3,0 @@\n-e\n")
       ~lines:None
       ~start:(p Old 1 0)
       ~end_:(p Old 5 1)
   with
   | Some (range, anchor) ->
     print_s [%sexp (range : Review_comment.Range.t), (anchor : string)]
   | None -> print_endline "nothing");
  [%expect
    {|
    ((first 5) (last 11) (side New) (columns ((start_col 4) (end_col 3))))
    "e = 5\nlet f = 6\nlet g = 7\nlet h = 8\nlet i = 9\nlet x = 10\nlet"
      of "let e = 5\nlet f = 6\nlet g = 7\nlet h = 8\nlet i = 9\nlet x = 10\nlet y = 1"
    nothing
    ((first 3) (last 5) (side New))
    "let c = 3 * 2\nlet d = \"\195\169\240\159\152\128\"\nlet e = 5"
    (((first 1) (last 2) (side Old))  "a\
                                     \nb")
    |}]
;;

let%expect_test "nothing selected, or not in the rows" =
  show (p New 3 4) (p New 3 4);
  show (p New 3 4) (p New 99 0);
  show (p New 3 13) (p New 3 13);
  [%expect
    {|
    nothing
    nothing
    nothing
    |}]
;;

let%expect_test "whole lines by their numbers, either way round" =
  let lines from to_ =
    match Code_selection.whole_lines diff ~lines:(Some file) ~from ~to_ with
    | None -> print_endline "nothing"
    | Some (range, _) -> print_s [%sexp (range : Review_comment.Range.t)]
  in
  lines (p New 4 0) (p New 2 0);
  lines (p Old 3 0) (p Old 3 0);
  [%expect
    {|
    ((first 2) (last 4) (side New))
    ((first 3) (last 3) (side Old))
    |}]
;;

let%expect_test "what of each row a range covers" =
  let range =
    { Review_comment.Range.first = 3
    ; last = 4
    ; side = New
    ; columns = Some { start_col = 8; end_col = 5 }
    }
  in
  List.iter diff ~f:(fun row ->
    match Code_selection.key row, Code_selection.covered range row with
    | Some (side, n), Some (a, b) ->
      printf "%s %d: %d-%d\n" (Review_comment.Side.to_string side) n a b
    | Some (side, n), None ->
      printf "%s %d: -\n" (Review_comment.Side.to_string side) n
    | None, _ -> print_endline "(hunk)");
  [%expect
    {|
    (hunk)
    new 2: -
    old 3: -
    new 3: 8-13
    new 4: 0-5
    new 5: -
    (hunk)
    old 10: -
    new 11: -
    new 12: -
    |}]
;;

let%expect_test "a comment's code found again as the file changes" =
  let comment ?(side = Review_comment.Side.New) ?columns ~first ~last anchor =
    { Review_comment.id = "c1"
    ; root = "/r"
    ; path = "a.ml"
    ; range = { first; last; side; columns }
    ; anchor
    ; status = Open
    ; thread = []
    }
  in
  let locate c lines =
    let lines = Array.of_list lines in
    print_s
      [%sexp
        (Comment_anchor.locate
           c
           ~line:(fun n ->
             if n >= 1 && n <= Array.length lines
             then Some lines.(n - 1)
             else None)
           ~count:(Array.length lines)
         : Comment_anchor.Located.t)]
  in
  let c =
    comment
      ~first:2
      ~last:2
      ~columns:{ start_col = 4; end_col = 5 }
      "let c = 3 * 2"
  in
  (* Where it was; moved down by a line; its line changed elsewhere but the
     selected text is still there; gone. *)
  locate c [ "let a = 1"; "let c = 3 * 2" ];
  locate c [ "let a = 1"; "(* new *)"; "let c = 3 * 2" ];
  locate c [ "let a = 1"; "(* new *)"; "let c = 4 * 2" ];
  locate c [ "let a = 1"; "let d = 4" ];
  (* A selection over lines: its middle lines must be as they were. *)
  let c =
    comment
      ~first:1
      ~last:3
      ~columns:{ start_col = 8; end_col = 5 }
      "let a = 1 + 0\nlet b = 2\nlet c = 3"
  in
  locate c [ "(* x *)"; "let a = 1 + 1 + 0"; "let b = 2"; "let c = 3 + 3" ];
  locate c [ "let a = 1 + 0"; "let b = 3"; "let c = 3" ];
  (* Whole lines without columns are found by their text alone. *)
  locate (comment ~first:1 ~last:1 "let a = 1") [ "let a = 2" ];
  [%expect
    {|
    (At ((first 2) (last 2) (side New) (columns ((start_col 4) (end_col 5)))))
    (At ((first 3) (last 3) (side New) (columns ((start_col 4) (end_col 5)))))
    (At ((first 3) (last 3) (side New) (columns ((start_col 4) (end_col 5)))))
    Outdated
    (At ((first 2) (last 4) (side New) (columns ((start_col 12) (end_col 5)))))
    Outdated
    Outdated
    |}]
;;
