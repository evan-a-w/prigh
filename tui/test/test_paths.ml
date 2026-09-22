open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
module Paths = Prigh_ui_term.Paths

(* Path completion is relative to the session cwd, not the process cwd. *)
let%expect_test "paths: listed under the given cwd, skipping .git and _build" =
  with_temp_dir (fun dir ->
    let root = dir ^/ "proj" in
    List.iter
      [ "src"; "src/sub"; "docs"; ".git/objects"; "_build/default" ]
      ~f:(fun d -> Core_unix.mkdir_p (root ^/ d));
    List.iter
      [ "src/app.ml"
      ; "src/sub/deep.ml"
      ; "docs/README.md"
      ; ".git/HEAD"
      ; "_build/x"
      ]
      ~f:(fun f -> Out_channel.write_all (root ^/ f) ~data:"");
    let%bind all = Paths.list ~cwd:(Some root) ~prefix:"" in
    print_s [%sexp (all : Prigh_protocol.Json.t)];
    [%expect
      {| (docs/ docs/README.md src/ src/app.ml src/sub/ src/sub/deep.ml) |}];
    let%bind some = Paths.list ~cwd:(Some root) ~prefix:"APP" in
    print_s [%sexp (some : Prigh_protocol.Json.t)];
    [%expect {| (src/app.ml) |}];
    (* A different cwd sees different files. *)
    let%bind other = Paths.list ~cwd:(Some (root ^/ "docs")) ~prefix:"" in
    print_s [%sexp (other : Prigh_protocol.Json.t)];
    [%expect {| (README.md) |}];
    return ())
;;
