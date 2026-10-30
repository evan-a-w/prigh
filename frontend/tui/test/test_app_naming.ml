open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let connected () =
  let h = Test_app.connected ~height:14 () in
  H.reply ~quiet:true h Models_catalog models_json;
  h
;;

let config ?model () =
  sprintf
    {|{"scoped_models":[],"naming_model":%s}|}
    (match model with
     | Some key -> P.Json.to_string (P.Json.str key)
     | None -> "null")
;;

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test "parsing /naming's arguments" =
  List.iter
    [ ""; "model claude-haiku-4-5"; "model off"; "model"; "colour blue" ]
    ~f:(fun rest ->
      print_s
        [%message rest ~_:(Naming_command.parse rest : Naming_command.Action.t)]);
  [%expect {| |}]
;;

let%expect_test "/naming shows, completes, sets, and a refusal" =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/naming";
  H.enter h;
  H.enter h;
  H.reply h Naming_shown (config ());
  H.show h;
  type_quietly h "/naming model ";
  H.show h;
  type_quietly h "haiku";
  H.show h;
  H.enter h;
  H.enter h;
  H.reply
    h
    (Naming_saved "/naming model anthropic/claude-haiku-4-5")
    (config ~model:"anthropic/claude-haiku-4-5" ());
  H.show h;
  type_quietly h "/naming model claude-haku";
  H.enter h;
  H.reply_error
    h
    (Naming_saved "/naming model claude-haku")
    "naming_model: unknown model \"claude-haku\"; did you mean: \
     anthropic/claude-haiku-4-5 (Claude Haiku 4.5)";
  H.show h;
  [%expect {| |}]
;;
