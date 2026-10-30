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
  [%expect
    {|
    ("" Show)
    ("model claude-haiku-4-5" (Set_model (claude-haiku-4-5)))
    ("model off" (Set_model ()))
    (model (Error "which model? usage: /naming or /naming model MODEL|off"))
    ("colour blue" (Error "usage: /naming or /naming model MODEL|off"))
    |}]
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
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ sessions and subagents are named by the generate model,  │
    │ else the session's model (/naming model MODEL|off)       │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off                         the generate model, else the …
      anthropic/claude-fable-5    Claude Fable 5
      anthropic/claude-fable-5-1  Claude Fable 5.1
      openai/gpt-5.5              GPT-5.5
      deepseek/deepseek-flash     DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    > /naming model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /naming model haiku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((naming_model haiku)))))
      (tag (Naming_saved "/naming model haiku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ sessions and subagents are named by                      │
    │ anthropic/claude-haiku-4-5 (/naming model MODEL|off)     │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((naming_model claude-haku)))))
      (tag (Naming_saved "/naming model claude-haku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ naming_model: unknown model "claude-haku"; did you mean: │
    │ anthropic/claude-haiku-4-5 (Claude Haiku 4.5)            │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /naming model claude-haku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
