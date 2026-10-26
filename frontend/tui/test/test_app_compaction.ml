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

let config ?(at = 200_000) ?model ?(mask = true) () =
  sprintf
    {|{"scoped_models":[],"compact_at_tokens":%d,"compaction_model":%s,"mask_observations":%b}|}
    at
    (match model with
     | Some key -> P.Json.to_string (P.Json.str key)
     | None -> "null")
    mask
;;

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test "parsing /compaction's arguments" =
  List.iter
    [ ""
    ; "at 150k"
    ; "at 150000"
    ; "at 1.5M"
    ; "at 200_000"
    ; "at off"
    ; "at 0"
    ; "at lots"
    ; "at -5"
    ; "model claude-haiku-4-5"
    ; "model off"
    ; "model"
    ; "mask on"
    ; "mask off"
    ; "mask"
    ; "mask maybe"
    ; "colour blue"
    ]
    ~f:(fun rest ->
      print_s
        [%message
          rest ~_:(Compaction_command.parse rest : Compaction_command.Action.t)]);
  List.iter
    [ 0; 999; 1_000; 150_000; 1_234; 1_500; 1_000_000; 1_500_000 ]
    ~f:(fun n -> printf "%d: %s\n" n (Compaction_command.tokens n));
  [%expect
    {|
    ("" Show)
    ("at 150k" (Set_at 150000))
    ("at 150000" (Set_at 150000))
    ("at 1.5M" (Set_at 1500000))
    ("at 200_000" (Set_at 200000))
    ("at off" (Set_at 0))
    ("at 0" (Set_at 0))
    ("at lots" (
      Error
      "\"lots\" is not a number of tokens (e.g. 150k or 200000; off: at 80% of the model's window only); usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    ("at -5" (
      Error
      "\"-5\" is not a number of tokens (e.g. 150k or 200000; off: at 80% of the model's window only); usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    ("model claude-haiku-4-5" (Set_model (claude-haiku-4-5)))
    ("model off" (Set_model ()))
    (model (
      Error
      "usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    ("mask on" (Set_mask true))
    ("mask off" (Set_mask false))
    (mask (
      Error
      "usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    ("mask maybe" (
      Error
      "usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    ("colour blue" (
      Error
      "usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off"))
    0: 0
    999: 999
    1000: 1k
    150000: 150k
    1234: 1234
    1500: 1.5k
    1000000: 1M
    1500000: 1.5M
    |}]
;;

let%expect_test "/compaction shows the settings, fetching them first" =
  let h = connected () in
  type_quietly h "/compaction";
  H.enter h;
  H.enter h;
  H.reply h Compaction_shown (config ());
  H.show h;
  type_quietly h "/compaction";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_config) (params ()) (tag Compaction_shown))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 200k tokens, or 80% of the model's window │
    │ if that is less; summaries by the session's model;       │
    │ observation masking on (/compaction at TOKENS|off,       │
    │ /compaction model MODEL|off, /compaction mask on|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 200k tokens, or 80% of the model's window │
    │ if that is less; summaries by the session's model;       │
    │ observation masking on (/compaction at TOKENS|off,       │
    │ /compaction model MODEL|off, /compaction mask on|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/compaction at and model: set, completed, and a refusal" =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/compaction at 150k";
  H.enter h;
  H.reply h (Compaction_saved "/compaction at 150k") (config ~at:150_000 ());
  H.show h;
  type_quietly h "/compaction ";
  H.show h;
  type_quietly h "model ";
  H.show h;
  type_quietly h "gpt";
  H.show h;
  H.enter h;
  H.enter h;
  H.reply
    h
    (Compaction_saved "/compaction model openai/gpt-5.5")
    (config ~at:150_000 ~model:"openai/gpt-5.5" ());
  H.show h;
  type_quietly h "/compaction at off";
  H.enter h;
  H.reply h (Compaction_saved "/compaction at off") (config ~at:0 ());
  H.show h;
  type_quietly h "/compaction model claude-haku";
  H.enter h;
  H.reply_error
    h
    (Compaction_saved "/compaction model claude-haku")
    "compaction_model: unknown model \"claude-haku\"; did you mean: \
     anthropic/claude-haiku-4-5 (Claude Haiku 4.5)";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((compact_at_tokens 150000)))))
      (tag (Compaction_saved "/compaction at 150k")))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 150k tokens, or 80% of the model's window │
    │ if that is less; summaries by the session's model;       │
    │ observation masking on (/compaction at TOKENS|off,       │
    │ /compaction model MODEL|off, /compaction mask on|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ at     compact once the context exceeds this many tokens …
      model  the model that writes the summaries
      mask   replace old tool outputs with placeholders first
    ────────────────────────────────────────────────────────────
    > /compaction ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off                         the session's model writes th…
      anthropic/claude-fable-5    Claude Fable 5
      anthropic/claude-fable-5-1  Claude Fable 5.1
      openai/gpt-5.5              GPT-5.5
      deepseek/deepseek-flash     DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    > /compaction model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ openai/gpt-5.5  GPT-5.5
    ────────────────────────────────────────────────────────────
    > /compaction model gpt▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    (Rpc
      (method_ set_config)
      (params ((config ((compaction_model openai/gpt-5.5)))))
      (tag (Compaction_saved "/compaction model openai/gpt-5.5")))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 150k tokens, or 80% of the model's window │
    │ if that is less; summaries by openai/gpt-5.5;            │
    │ observation masking on (/compaction at TOKENS|off,       │
    │ /compaction model MODEL|off, /compaction mask on|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((compact_at_tokens 0)))))
      (tag (Compaction_saved "/compaction at off")))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 80% of the model's window; summaries by   │
    │ the session's model; observation masking on (/compaction │
    │ at TOKENS|off, /compaction model MODEL|off, /compaction  │
    │ mask on|off)                                             │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((compaction_model claude-haku)))))
      (tag (Compaction_saved "/compaction model claude-haku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ compaction_model: unknown model "claude-haku"; did you   │
    │ mean: anthropic/claude-haiku-4-5 (Claude Haiku 4.5)      │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /compaction model claude-haku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/compaction with a bad argument says what to type" =
  let h = connected () in
  type_quietly h "/compaction at lots";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ "lots" is not a number of tokens (e.g. 150k or 200000;   │
    │ off: at 80% of the model's window only); usage:          │
    │ /compaction, /compaction at TOKENS|off, /compaction      │
    │ model MODEL|off or /compaction mask on|off               │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /compaction at lots▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/compaction mask: completed, set, and shown" =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/compaction mask ";
  H.show h;
  type_quietly h "off";
  H.enter h;
  H.reply h (Compaction_saved "/compaction mask off") (config ~mask:false ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ on   mask old tool outputs before summarising
      off  only summarise
    ────────────────────────────────────────────────────────────
    > /compaction mask ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    (Rpc
      (method_ set_config)
      (params ((config ((mask_observations false)))))
      (tag (Compaction_saved "/compaction mask off")))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ compaction: at 200k tokens, or 80% of the model's window │
    │ if that is less; summaries by the session's model;       │
    │ observation masking off (/compaction at TOKENS|off,      │
    │ /compaction model MODEL|off, /compaction mask on|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
