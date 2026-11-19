open! Core
open! Prigh

let%expect_test "Gemini coding/chat models resolve to distinct wire IDs" =
  List.iter Model.all ~f:(fun (m : Model.t) ->
    if Provider_id.equal m.provider Gemini
    then (
      let model = Model.resolve (Model.key m) |> Or_error.ok_exn in
      print_endline model.id));
  [%expect
    {|
    gemini-4-carbon
    gemini-4-argon
    gemini-4-neon
    gemini-4-hydrogen
    gemini-3.8-flash
    gemini-3.6-flash
    gemini-3.5-flash-lite
    gemini-3.1-flash-lite
    gemini-3.1-pro-preview
    gemini-3.1-pro-preview-customtools
    gemini-3-flash-preview
    |}]
;;

let%expect_test
    "Gemini default uses foreground streaming limits and supported levels"
  =
  let m = Model.default_for Gemini |> Option.value_exn in
  printf "%s %d %d\n" (Model.key m) m.context_window m.max_output;
  print_s [%sexp (m.thinking_levels : Thinking.Level.t list)];
  [%expect
    {|
    gemini/gemini-4-hydrogen 1048576 65536
    (Low Medium High Xhigh)
    |}]
;;
