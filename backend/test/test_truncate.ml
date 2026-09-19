open! Core
open! Prigh

let show t = print_s [%sexp (t : Truncate.t)]

let%expect_test "count_lines" =
  print_s
    [%sexp
      (List.map [ ""; "a"; "a\n"; "a\nb"; "a\nb\n" ] ~f:Truncate.count_lines
       : int list)];
  [%expect {| (0 1 1 2 2) |}]
;;

let%expect_test "head/tail by lines" =
  let s = "1\n2\n3\n4\n5\n" in
  show (Truncate.head ~max_lines:2 s);
  show (Truncate.tail ~max_lines:2 s);
  show (Truncate.head ~max_lines:10 s);
  [%expect
    {|
    ((text  "1\
           \n2\
           \n") (truncated true) (total_lines 5) (total_bytes 10))
    ((text  "4\
           \n5\
           \n") (truncated true) (total_lines 5) (total_bytes 10))
    ((text  "1\
           \n2\
           \n3\
           \n4\
           \n5\
           \n")
     (truncated false) (total_lines 5) (total_bytes 10))
    |}]
;;

let%expect_test "head/tail by bytes cut at line boundaries" =
  let s = "aaaa\nbbbb\ncccc\n" in
  show (Truncate.head ~max_bytes:7 s);
  show (Truncate.tail ~max_bytes:7 s);
  show (Truncate.head ~max_bytes:3 "no-newline-here");
  [%expect
    {|
    ((text "aaaa\n") (truncated true) (total_lines 3) (total_bytes 15))
    ((text "cccc\n") (truncated true) (total_lines 3) (total_bytes 15))
    ((text no-) (truncated true) (total_lines 1) (total_bytes 15))
    |}]
;;
