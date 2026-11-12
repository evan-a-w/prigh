open! Core
module H = Harness

(* Settings' drafting model: what [/generate-model] sets, shown and picked on
   the settings page and from the composer. *)

let config ?model () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; ( "generate_model"
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

let%expect_test "drafting model: shown, picked, back to the default" =
  let h = settings () in
  H.text h ~selector:".setting.generate-model";
  H.names h ~selector:".setting.generate-model button";
  H.click h ~selector:".generate-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    Drafting
    Drafts commit messages, pull request descriptions and branch names (✦ Generate). Default: the session's model.
    (Default)
    button "Drafting model: Default" -- Choose the model (/generate-model)
    (Run /generate-model)
    (Focus picker-input)
    Drafting model
    (Close (Esc))
    []
    Default the session's model ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ~model:"anthropic/claude-sonnet-5" ());
  H.text h ~selector:".setting.generate-model";
  H.names h ~selector:".setting.generate-model button";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((generate_model anthropic/claude-sonnet-5)))))
     (tag Generate_model_saved))
    (Expire_toast (id 0) (after_ms 6000))
    Drafting
    Drafts commit messages, pull request descriptions and branch names (✦ Generate). Default: the session's model.
    (Claude Sonnet 5)
    button "Drafting model: Claude Sonnet 5" -- Choose the model (/generate-model)
    Commit messages, pull requests and branch names are drafted by anthropic/claude-sonnet-5 (/generate-model MODEL|off)
    |}];
  H.quiet h (fun () -> H.click h ~selector:".generate-model-choice");
  H.text h ~selector:".picker-item[aria-selected=true]";
  H.quiet h (fun () -> H.act h (Picker_query "default"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ());
  H.text h ~selector:".setting.generate-model";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Run /generate-model)
    Claude Sonnet 5 anthropic, 200k ctx ✓
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config) (params ((config ((generate_model null)))))
     (tag Generate_model_saved))
    (Expire_toast (id 1) (after_ms 5900))
    Drafting
    Drafts commit messages, pull request descriptions and branch names (✦ Generate). Default: the session's model.
    (Default)
    Commit messages, pull requests and branch names are drafted by the session's model (/generate-model MODEL|off)
    |}]
;;

let%expect_test
    "/generate-model from the composer, and a model the backend refuses"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/generate-model ";
    H.act h Send);
  H.text h ~selector:".modal";
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  [%expect
    {|
    Drafting model
    (Close (Esc))
    []
    Default the session's model ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    Close_dialog
    |}];
  H.type_ h "/generate-model ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Drafting model ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    off the session's model drafts them
    openai/gpt-6 GPT-6
    anthropic/claude-opus-5-5 Claude Opus 5.5
    anthropic/claude-sonnet-5 Claude Sonnet 5
    deepseek/deepseek-chat DeepSeek Chat
    |}];
  H.quiet h (fun () ->
    H.type_ h "/generate-model sonnet";
    H.act h Send);
  H.reply h "set_config" (config ~model:"anthropic/claude-sonnet-5" ());
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 6000))
    Commit messages, pull requests and branch names are drafted by anthropic/claude-sonnet-5 (/generate-model MODEL|off)
    |}];
  H.quiet h (fun () ->
    H.type_ h "/generate-model off";
    H.act h Send);
  H.reply h "set_config" (config ());
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 5900))
    Commit messages, pull requests and branch names are drafted by the session's model (/generate-model MODEL|off)
    |}];
  H.quiet h (fun () ->
    H.type_ h "/generate-model nosuch";
    H.act h Send);
  H.fail
    h
    "set_config"
    {|generate_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5|};
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set the drafting model: generate_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5. Pick one with /generate-model (Dismiss)
    |}]
;;
