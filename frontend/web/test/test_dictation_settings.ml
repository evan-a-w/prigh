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
    Turns what you say into text (the composer's microphone): an OpenAI or OpenAI-compatible transcription model. Without one, the browser's own speech recognition, where it has one. Free: Groq's whisper-large-v3-turbo. Add Groq with /login custom (README: Free dictation with Groq)
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
    Turns what you say into text (the composer's microphone): an OpenAI or OpenAI-compatible transcription model. Without one, the browser's own speech recognition, where it has one. Free: Groq's whisper-large-v3-turbo. Add Groq with /login custom (README: Free dictation with Groq)
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

(* Groq's free Whisper: a hint on the row, and in the list only once a
   custom provider named groq can run it. *)
let groq =
  {|{"provider":"groq","name":"groq","methods":[{"method":"api_key","label":"groq API key"}],"configured":{"method":"api_key","source":"key"},"expires_ms":null,"custom":{"base_url":"https://api.groq.com/openai/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}|}
;;

let%expect_test "Groq's free Whisper: hinted, listed once groq is added" =
  let h = settings () in
  H.text h ~selector:".setting.dictation-model .setting-note";
  H.quiet h (fun () -> H.click h ~selector:".dictation-model-choice");
  H.text h ~selector:".picker-item";
  [%expect
    {|
    Free: Groq's whisper-large-v3-turbo. Add Groq with /login custom (README: Free dictation with Groq)
    (Run "/dictation model")
    Default openai/gpt-4o-mini-transcribe ✓
    openai/gpt-4o-mini-transcribe OpenAI, fast and cheap (the default)
    openai/gpt-4o-transcribe OpenAI, most accurate
    openai/whisper-1 OpenAI Whisper
    |}];
  H.quiet h (fun () ->
    H.key h "Escape" ~target:Field;
    H.act
      h
      (Reply
         ( Auth_status Refresh
         , Ok
             (Jsonaf.of_string
                (String.chop_suffix_exn (String.strip H.auth_json) ~suffix:"]"
                 ^ ","
                 ^ groq
                 ^ "]")) )));
  H.text h ~selector:".setting.dictation-model .setting-note";
  H.quiet h (fun () -> H.click h ~selector:".dictation-model-choice");
  H.text h ~selector:".picker-item";
  [%expect {|
    Close_dialog
    Free: groq/whisper-large-v3-turbo, in the list
    (Run "/dictation model")
    Default openai/gpt-4o-mini-transcribe ✓
    openai/gpt-4o-mini-transcribe OpenAI, fast and cheap (the default)
    openai/gpt-4o-transcribe OpenAI, most accurate
    openai/whisper-1 OpenAI Whisper
    groq/whisper-large-v3-turbo Groq's Whisper, free (rate-limited)
    |}]
;;
