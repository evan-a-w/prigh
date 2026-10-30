open! Core
module H = Harness

(* Settings' naming model: what [/naming] sets, shown and picked on the
   settings page. *)

let config ?model () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; ( "naming_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ])
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  h
;;

let%expect_test "naming model: shown, picked, back to the default" =
  let h = settings () in
  H.text h ~selector:".setting.naming-model";
  H.names h ~selector:".setting.naming-model button";
  H.click h ~selector:".naming-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    Naming model The model that titles sessions and names subagents: a small, fast one is plenty. Default: the generate model, else the session's.
    (Default)
    button "Naming model: Default" -- Choose the model (/naming model)
    (Run "/naming model")
    (Focus picker-input)
    Naming model
    (Close (Esc))
    []
    Default the generate model, else the session's ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ~model:"anthropic/claude-sonnet-5" ());
  H.text h ~selector:".setting.naming-model";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((naming_model anthropic/claude-sonnet-5)))))
     (tag Naming_saved))
    (Expire_toast (id 0) (after_ms 4980))
    Naming model The model that titles sessions and names subagents: a small, fast one is plenty. Default: the generate model, else the session's.
    (Claude Sonnet 5)
    Sessions and subagents are named by anthropic/claude-sonnet-5 (/naming model MODEL|off)
    |}];
  H.quiet h (fun () -> H.click h ~selector:".naming-model-choice");
  H.text h ~selector:".picker-item[aria-selected=true]";
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Run "/naming model")
    Claude Sonnet 5 anthropic, 200k ctx ✓
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((naming_model anthropic/claude-sonnet-5)))))
     (tag Naming_saved))
    |}]
;;

let%expect_test "/naming from the composer, and a model the backend refuses" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/naming";
    H.act h Send);
  H.text h ~selector:".toast:last-child";
  [%expect
    {| Sessions and subagents are named by the generate model, else the session's model (/naming model MODEL|off) |}];
  H.quiet h (fun () ->
    H.type_ h "/naming model nosuch";
    H.act h Send);
  H.fail
    h
    "set_config"
    {|naming_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5|};
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set the naming model: naming_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5. Pick one with /naming model (Dismiss)
    |}]
;;
