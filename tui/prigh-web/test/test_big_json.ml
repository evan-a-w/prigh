open! Core

(* js_of_ocaml's stack is small: a long session's replies must still parse. *)
let%expect_test "big JSON arrays parse in the browser" =
  List.iter [ 10_000; 200_000 ] ~f:(fun n ->
    let items =
      List.init n ~f:(fun i -> sprintf {|{"id":"e%d","text":"hello \u00e9"}|} i)
    in
    let json = sprintf {|[%s]|} (String.concat ~sep:"," items) in
    match Prigh_protocol.Json.parse json with
    | Ok (`Array l) -> printf "%d items\n" (List.length l)
    | Ok _ -> print_endline "not an array"
    | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    10000 items
    200000 items
    |}]
;;
