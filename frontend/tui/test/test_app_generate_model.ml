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
    {|{"scoped_models":[],"generate_model":%s}|}
    (match model with
     | Some key -> P.Json.to_string (P.Json.str key)
     | None -> "null")
;;

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test "parsing /generate-model's arguments" =
  List.iter [ ""; "claude-haiku-4-5"; " off "; "model haiku" ] ~f:(fun rest ->
    print_s
      [%message
        rest
          ~_:
            (Generate_model_command.parse rest
             : Generate_model_command.Action.t)]);
  [%expect
    {|
    ("" Show)
    (claude-haiku-4-5 (Set_model (claude-haiku-4-5)))
    (" off " (Set_model ()))
    ("model haiku" (Error "usage: /generate-model or /generate-model MODEL|off"))
    |}]
;;

let%expect_test "/generate-model shows, completes, sets, clears, and a refusal" =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/generate-model";
  H.enter h;
  H.enter h;
  H.reply h Generate_model_shown (config ());
  H.show h;
  type_quietly h "/generate-model ";
  H.show h;
  type_quietly h "haiku";
  H.show h;
  H.enter h;
  H.enter h;
  H.reply
    h
    (Generate_model_saved "/generate-model anthropic/claude-haiku-4-5")
    (config ~model:"anthropic/claude-haiku-4-5" ());
  H.show h;
  type_quietly h "/generate-model off";
  H.enter h;
  H.enter h;
  H.reply h (Generate_model_saved "/generate-model off") (config ());
  H.show h;
  type_quietly h "/generate-model claude-haku";
  H.enter h;
  H.reply_error
    h
    (Generate_model_saved "/generate-model claude-haku")
    "generate_model: unknown model \"claude-haku\"; did you mean: \
     anthropic/claude-haiku-4-5 (Claude Haiku 4.5)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ commit messages, pull requests and branch names are      │
    │ drafted by the session's model (/generate-model          │
    │ MODEL|off)                                               │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off                         the session's model drafts th…
      anthropic/claude-fable-5    Claude Fable 5
      anthropic/claude-fable-5-1  Claude Fable 5.1
      openai/gpt-5.5              GPT-5.5
      deepseek/deepseek-flash     DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    > /generate-model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /generate-model haiku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((generate_model haiku)))))
      (tag (Generate_model_saved "/generate-model haiku")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ commit messages, pull requests and branch names are      │
    │ drafted by anthropic/claude-haiku-4-5 (/generate-model   │
    │ MODEL|off)                                               │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((generate_model null)))))
      (tag (Generate_model_saved "/generate-model off")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ commit messages, pull requests and branch names are      │
    │ drafted by the session's model (/generate-model          │
    │ MODEL|off)                                               │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((generate_model claude-haku)))))
      (tag (Generate_model_saved "/generate-model claude-haku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ generate_model: unknown model "claude-haku"; did you     │
    │ mean: anthropic/claude-haiku-4-5 (Claude Haiku 4.5)      │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /generate-model claude-haku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
