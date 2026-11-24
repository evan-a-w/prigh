open! Core
module H = Harness

(* A provider's several logins: the settings' Models section lists them,
   each with its menu; another is added under a label; /model MODEL@LABEL
   pins the session to one, which the model chip and its menu show. *)

let logins_json =
  {|[{"provider":"openai","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true},{"ref":"openai/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":false}]},
     {"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":null}]|}
;;

let gpt =
  {|{"id":"gpt-6","provider":"openai","key":"openai/gpt-6","name":"GPT-6","context_window":200000,"max_output":64000,"supports_thinking":false,"cost":{"input":5,"output":25,"cache_read":0.5}}|}
;;

let create ?account () =
  let fields =
    ("model", Jsonaf.of_string gpt)
    :: Option.value_map account ~default:[] ~f:(fun a ->
      [ "account", `String a ])
  in
  let h = H.create ~state:(H.state_json ~fields ()) () in
  H.quiet h (fun () ->
    H.act h (Reply (Auth_status Refresh, Ok (Jsonaf.of_string logins_json)));
    H.act h (Reply (Models, Ok (Jsonaf.of_string H.models_json))));
  h
;;

let%expect_test "settings: each login, its menu, renaming one, adding one" =
  let h = create () in
  H.quiet h (fun () -> H.act h (Settings_section Models));
  H.text h ~selector:".provider-login";
  H.text h ~selector:".provider-login-add";
  [%expect
    {|
    ChatGPT: Personal
    Subscription, default, takes over at usage limits.
    (Manage)
    ChatGPT: Work
    Subscription.
    (Manage)
    Another ChatGPT login
    Another account, under a label: pin a session to it with /model MODEL@LABEL.
    (Add login)
    |}];
  H.act h (Run "/accounts menu openai/work");
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Focus picker-input)
    Use in this session this session's requests go to this login
    Make default sessions that pick no login use it
    Rename…
    Take over at usage limits when another login of the provider hits its limit
    Move up
    Log in again
    Log out deletes the stored credential
    |}];
  H.act h (Picker_query "rename");
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    (Focus dialog-input)
    Rename ChatGPT: Work
    (Close (Esc))
    The login's name in Settings, /accounts and beside the model (model@label).
    []
    (Cancel) (Save)
    |}];
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    Dialog_accept
    Type a label, e.g. Work (Esc cancels).
    |}];
  H.act h (Dialog_input "Day job");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ account_update) (params ((ref openai/work) (label "Day job")))
     (tag
      (Logins_changed (command "/accounts rename openai/work to Day job")
       (notice "renamed ChatGPT: Work to Day job"))))
    (Focus dialog)
    |}];
  (* Back on the settings page. *)
  print_s
    [%sexp
      ((match (H.model h).dialog with
        | Some Settings -> "settings"
        | _ -> "elsewhere")
       : string)];
  [%expect {| settings |}];
  H.act h (Run "/login openai as");
  H.act h (Dialog_input "Side");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Focus dialog-input)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ login) (params ((provider openai) (label Side)))
     (tag Login_started))
    |}]
;;

let%expect_test "/model MODEL@LABEL pins the login; the chip and menu show it" =
  let h = create () in
  H.text h ~selector:".model-chip";
  [%expect {| (GPT-6 @Personal) |}];
  H.type_ h "/model gpt-6@work";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model gpt-6@work") (max 500))) (tag Ignore))
    (Rpc (method_ set_model) (params ((model openai/gpt-6) (account work)))
     (tag Model_set))
    |}];
  H.type_ h "/model gpt-6@nope";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model gpt-6@nope") (max 500))) (tag Ignore))
    No openai login "nope": it has Personal, Work (/accounts lists them) (Dismiss)
    |}];
  let h = create ~account:"work" () in
  H.text h ~selector:".model-chip";
  H.act h (Open_chip Model);
  H.text h ~selector:".model-logins";
  [%expect
    {|
    (GPT-6 @Work)
    (Focus popover-filter)
    Place_popover
    Login ChatGPT
    (Personal) (Work ✓) (The provider's default)
    |}];
  H.click h ~selector:".model-logins .reasoning-row";
  [%expect
    {|
    (Run "/account openai/main")
    (Rpc (method_ set_model) (params ((model openai/gpt-6) (account main)))
     (tag
      (Logins_changed (command "/account openai/main")
       (notice "this session uses ChatGPT: Personal"))))
    |}]
;;
