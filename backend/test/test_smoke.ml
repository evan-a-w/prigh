open! Core

let%expect_test "version" =
  print_endline Prigh.Version.to_string;
  [%expect {| 0.1.0 |}]
;;
