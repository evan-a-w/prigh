open! Core
open! Prigh

let%expect_test "unified" =
  let show ~old ~new_ =
    let t = Text_diff.unified ~path:"a.ml" ~old ~new_ in
    printf "+%d -%d\n%s" t.added t.removed t.diff
  in
  show ~old:"same\n" ~new_:"same\n";
  [%expect {| +0 -0 |}];
  show
    ~old:"1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n"
    ~new_:"1\n2\nthree\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n";
  [%expect
    {|
    +2 -1
    diff --git a/a.ml b/a.ml
    --- a/a.ml
    +++ b/a.ml
    @@ -1,6 +1,6 @@
     1
     2
    -3
    +three
     4
     5
     6
    @@ -10,3 +10,4 @@
     10
     11
     12
    +13
    |}];
  show ~old:"" ~new_:"a\nb\n";
  [%expect
    {|
    +2 -0
    diff --git a/a.ml b/a.ml
    --- a/a.ml
    +++ b/a.ml
    @@ -0,0 +1,2 @@
    +a
    +b
    |}];
  show ~old:"a\nb\n" ~new_:"";
  [%expect
    {|
    +0 -2
    diff --git a/a.ml b/a.ml
    --- a/a.ml
    +++ b/a.ml
    @@ -1,2 +0,0 @@
    -a
    -b
    |}];
  show ~old:"a\nb" ~new_:"a\nb\n";
  [%expect
    {|
    +1 -1
    diff --git a/a.ml b/a.ml
    --- a/a.ml
    +++ b/a.ml
    @@ -1,2 +1,2 @@
     a
    -b
    \ No newline at end of file
    +b
    |}]
;;
