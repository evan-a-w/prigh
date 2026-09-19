open! Core
open! Prigh

let file lines = String.concat lines ~sep:"\n" ^ "\n"
let numbered n = List.init n ~f:(fun i -> sprintf "line%02d" (i + 1))

let show label before after =
  printf "=== %s ===\n%s\n" label (Udiff.hunks ~path:"f" ~before ~after)
;;

let%expect_test "insertion" =
  show "insertion" (file [ "a"; "b"; "c" ]) (file [ "a"; "x"; "b"; "c" ]);
  [%expect
    {|
    === insertion ===
    --- a/f
    +++ b/f
    @@ -1,3 +1,4 @@
     a
    +x
     b
     c

    |}]
;;

let%expect_test "deletion" =
  show "deletion" (file [ "a"; "b"; "c" ]) (file [ "a"; "c" ]);
  [%expect
    {|
    === deletion ===
    --- a/f
    +++ b/f
    @@ -1,3 +1,2 @@
     a
    -b
     c

    |}]
;;

let%expect_test "change in the middle" =
  show "change" (file [ "a"; "b"; "c" ]) (file [ "a"; "B"; "c" ]);
  [%expect
    {|
    === change ===
    --- a/f
    +++ b/f
    @@ -1,3 +1,3 @@
     a
    -b
    +B
     c

    |}]
;;

let%expect_test "changes at both ends of a 30-line file" =
  let before = file (numbered 30) in
  let after =
    file
      ("changed-first"
       :: (List.drop (List.take (numbered 30) 29) 1 @ [ "changed-last" ]))
  in
  show "both ends" before after;
  [%expect
    {|
    === both ends ===
    --- a/f
    +++ b/f
    @@ -1,4 +1,4 @@
    -line01
    +changed-first
     line02
     line03
     line04
    @@ -27,4 +27,4 @@
     line27
     line28
     line29
    -line30
    +changed-last

    |}]
;;

let%expect_test "identical inputs" =
  let contents = file (numbered 10) in
  printf
    "empty=%b\n"
    (String.is_empty (Udiff.hunks ~path:"f" ~before:contents ~after:contents));
  [%expect {| empty=true |}]
;;
