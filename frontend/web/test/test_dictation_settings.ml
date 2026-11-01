open! Core
module H = Harness

(* Settings' dictation model: what [/dictation] sets, shown and picked on
   the settings page. *)

let config ?model () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; ( "dictation_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ])
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.act h (Settings_section Models));
  h
;;

let%expect_test "dictation model: shown, picked, back to the default" =
  let h = settings () in
  H.text h ~selector:".setting.dictation-model";
  H.names h ~selector:".setting.dictation-model button";
  H.click h ~selector:".dictation-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    Dictation
    Turns what you say into text (the composer's microphone): an OpenAI or OpenAI-compatible transcription model. Without one, the browser's own speech recognition, where it has one.
    (openai/gpt-4o-mini-transcribe)
    button "Dictation model: openai/gpt-4o-mini-transcribe" -- Choose the model (/dictation model)
    (Run "/dictation model")
    (Focus picker-input)
    Dictation model
    (Close (Esc))
    []
    Default openai/gpt-4o-mini-transcribe ✓
    openai/gpt-4o-mini-transcribe OpenAI, fast and cheap (the default)
    openai/gpt-4o-transcribe OpenAI, most accurate
    openai/whisper-1 OpenAI Whisper
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "whisper"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ~model:"openai/whisper-1" ());
  H.text h ~selector:".setting.dictation-model";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((dictation_model openai/whisper-1)))))
     (tag Dictation_saved))
    (Expire_toast (id 0) (after_ms 4420))
    Dictation
    Turns what you say into text (the composer's microphone): an OpenAI or OpenAI-compatible transcription model. Without one, the browser's own speech recognition, where it has one.
    (openai/whisper-1)
    Dictation is transcribed by openai/whisper-1 (/dictation model MODEL|off)
    |}];
  H.quiet h (fun () -> H.click h ~selector:".dictation-model-choice");
  H.text h ~selector:".picker-item[aria-selected=true]";
  H.quiet h (fun () -> H.act h (Picker_query "default"));
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Run "/dictation model")
    openai/whisper-1 OpenAI Whisper ✓
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config) (params ((config ((dictation_model null)))))
     (tag Dictation_saved))
    |}]
;;

let%expect_test "a custom provider's model, typed, is listed as configured" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/dictation model aiproxy/whisper-large";
    H.act h Send);
  H.reply h "set_config" (config ~model:"aiproxy/whisper-large" ());
  H.quiet h (fun () ->
    H.type_ h "/dictation model";
    H.act h Send);
  H.text h ~selector:".modal";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4620))
    Dictation model
    (Close (Esc))
    []
    Default openai/gpt-4o-mini-transcribe
    aiproxy/whisper-large configured ✓
    openai/gpt-4o-mini-transcribe OpenAI, fast and cheap (the default)
    openai/gpt-4o-transcribe OpenAI, most accurate
    openai/whisper-1 OpenAI Whisper
    ↑↓ to move, Enter to choose, Esc to close
    |}]
;;

let%expect_test "/dictation from the composer, and a model the backend refuses" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/dictation";
    H.act h Send);
  H.text h ~selector:".toast:last-child";
  [%expect
    {| Dictation is transcribed by openai/gpt-4o-mini-transcribe, the default (/dictation model MODEL|off) |}];
  H.quiet h (fun () ->
    H.type_ h "/dictation model anthropic/claude-sonnet-5";
    H.act h Send);
  H.fail
    h
    "set_config"
    "dictation_model: Anthropic cannot transcribe: pick an OpenAI or \
     OpenAI-compatible model with /dictation model (e.g. \
     openai/gpt-4o-mini-transcribe)";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set the dictation model: dictation_model: Anthropic cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe) (Dismiss)
    |}]
;;
