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
    {|{"scoped_models":[],"dictation_model":%s}|}
    (match model with
     | Some key -> P.Json.to_string (P.Json.str key)
     | None -> "null")
;;

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test "parsing /dictation's arguments" =
  List.iter
    [ ""; "model openai/whisper-1"; "model off"; "model"; "colour blue" ]
    ~f:(fun rest ->
      print_s
        [%message
          rest ~_:(Dictation_command.parse rest : Dictation_command.Action.t)]);
  [%expect
    {|
    ("" Show)
    ("model openai/whisper-1" (Set_model (openai/whisper-1)))
    ("model off" (Set_model ()))
    (model (Error "which model? usage: /dictation or /dictation model MODEL|off"))
    ("colour blue" (Error "usage: /dictation or /dictation model MODEL|off"))
    |}]
;;

let%expect_test "/dictation's completions" =
  List.iter [ ""; "mo"; "model "; "model whi"; "model a b" ] ~f:(fun rest ->
    print_s
      [%message
        rest
          ~_:
            (Dictation_command.complete rest
             : (int * (string * string) list) option)]);
  [%expect
    {|
    ("" ((0 ((model "the model that transcribes dictation")))))
    (mo ((0 ((model "the model that transcribes dictation")))))
    ("model " ((
      6 (
        (off "the default, openai/gpt-4o-mini-transcribe")
        (openai/gpt-4o-mini-transcribe "OpenAI, fast and cheap (the default)")
        (openai/gpt-4o-transcribe "OpenAI, most accurate")
        (openai/whisper-1         "OpenAI Whisper")))))
    ("model whi" ((
      6 (
        (off "the default, openai/gpt-4o-mini-transcribe")
        (openai/gpt-4o-mini-transcribe "OpenAI, fast and cheap (the default)")
        (openai/gpt-4o-transcribe "OpenAI, most accurate")
        (openai/whisper-1         "OpenAI Whisper")))))
    ("model a b" ())
    |}]
;;

let%expect_test "/dictation shows, completes, sets, and a refusal" =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/dictation";
  H.enter h;
  H.enter h;
  H.reply h Dictation_shown (config ());
  H.show h;
  type_quietly h "/dictation model ";
  H.show h;
  type_quietly h "whisper";
  H.show h;
  H.enter h;
  H.enter h;
  H.reply
    h
    (Dictation_saved "/dictation model openai/whisper-1")
    (config ~model:"openai/whisper-1" ());
  H.show h;
  type_quietly h "/dictation model anthropic/claude";
  H.enter h;
  H.reply_error
    h
    (Dictation_saved "/dictation model anthropic/claude")
    "dictation_model: Anthropic cannot transcribe: pick an OpenAI or \
     OpenAI-compatible model with /dictation model (e.g. \
     openai/gpt-4o-mini-transcribe)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ dictation is transcribed by                              │
    │ openai/gpt-4o-mini-transcribe, the default (/dictation   │
    │ model MODEL|off)                                         │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off                            the default, openai/gpt-4o…
      openai/gpt-4o-mini-transcribe  OpenAI, fast and cheap (th…
      openai/gpt-4o-transcribe       OpenAI, most accurate
      openai/whisper-1               OpenAI Whisper
    ────────────────────────────────────────────────────────────
    > /dictation model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ openai/whisper-1  OpenAI Whisper
    ────────────────────────────────────────────────────────────
    > /dictation model whisper▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    (Rpc
      (method_ set_config)
      (params ((config ((dictation_model openai/whisper-1)))))
      (tag (Dictation_saved "/dictation model openai/whisper-1")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ dictation is transcribed by openai/whisper-1 (/dictation │
    │ model MODEL|off)                                         │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((dictation_model anthropic/claude)))))
      (tag (Dictation_saved "/dictation model anthropic/claude")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ dictation_model: Anthropic cannot transcribe: pick an    │
    │ OpenAI or OpenAI-compatible model with /dictation model  │
    │ (e.g. openai/gpt-4o-mini-transcribe)                     │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
