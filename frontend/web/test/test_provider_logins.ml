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
    Personal
    Subscription, default, takes over at usage limits.
    (Manage)
    Work
    Subscription.
    (Manage)
    Another login
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

(* Alice may share Work (bob has it); dave shares his "Team" with her. *)
let sharing_json =
  {|[{"provider":"openai","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true,"shareable":true},{"ref":"openai/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":false,"shareable":true,"shared_with":[{"user":"bob","since_ms":1700000000000}]},{"ref":"~dave/openai/team","id":"~dave/team","label":"Team","method":"oauth","source":"shared by dave","default":false,"fallback":false,"shared_by":"dave"}]}]|}
;;

let%expect_test
    "sharing: Share… picks a user, grants show, revoking, a shared login"
  =
  let h = create () in
  (* Granted or revoked by its owner: the recipient's UI asks again. *)
  H.event h {|{"type":"event","event":"accounts_changed"}|};
  H.reply h "auth_status" sharing_json;
  H.reply h "account_share_targets" {|["bob","carol"]|};
  H.quiet h (fun () -> H.act h (Settings_section Models));
  H.text h ~selector:".provider-login";
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ account_share_targets) (params ()) (tag Share_targets))
    Personal
    Subscription, default, takes over at usage limits.
    (Share…) (Manage)
    Work
    Subscription, shared with bob.
    (Share…) (Manage)
    Team (from dave)
    Subscription, shared by dave.
    (Hide) (Manage)
    |}];
  H.click h ~nth:2 ~selector:".provider-login button";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Run "/accounts share openai/work")
    (Focus picker-input)
    Share ChatGPT: Work with
    (Close (Esc))
    []
    bob shared with them: pick to stop sharing ✓
    carol pick to share: they never see its key
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  List.iter (H.node h ~selector:".modal input" ()) ~f:(function
    | Element { attributes; _ } ->
      print_s
        [%sexp
          (List.Assoc.find attributes "placeholder" ~equal:String.equal
           : string option)]
    | _ -> ());
  [%expect {| ("Filter users\226\128\166") |}];
  H.act h (Picker_query "car");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ account_share) (params ((ref openai/work) (users (bob carol))))
     (tag
      (Logins_changed (command "/accounts share openai/work with carol")
       (notice "shared ChatGPT: Work with carol"))))
    (Focus dialog)
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
    Share… another user of this server may use it; they never see its key
    Stop sharing with bob their sessions on it go back to their default login
    Log in again
    Log out deletes the stored credential
    |}];
  H.act h (Picker_query "stop sharing");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ account_share) (params ((ref openai/work) (users ())))
     (tag
      (Logins_changed (command "/accounts unshare openai/work from bob")
       (notice "stopped sharing ChatGPT: Work with bob"))))
    (Focus dialog)
    |}];
  H.act h (Run "/accounts hide team");
  [%expect
    {|
    (Rpc (method_ account_hide) (params ((ref ~dave/openai/team) (hidden true)))
     (tag
      (Logins_changed (command "/accounts hide team")
       (notice
        "hid ChatGPT: Team (from dave): /accounts show ~dave/openai/team brings it back"))))
    |}];
  H.act h (Open_chip Model);
  H.text h ~selector:".model-logins";
  [%expect
    {|
    (Focus popover-filter)
    Place_popover
    Login ChatGPT
    (Personal) (Work) (Team (from dave)) (The provider's default ✓)
    |}]
;;

(* Each login's usage: Personal's limits, Work's requests (shared with bob:
   who used it), and the context card's line on the session's login. *)
let usage_json =
  let totals r =
    sprintf
      {|{"requests":%d,"input":%d,"output":%d,"cache_read":0,"cost_usd":0}|}
      r
      (r * 4000)
      (r * 300)
  in
  let periods a b c =
    sprintf
      {|"usage":{"today":%s,"last_7_days":%s,"last_30_days":%s}|}
      (totals a)
      (totals b)
      (totals c)
  in
  String.substr_replace_first
    sharing_json
    ~pattern:{|"shared_with":[{"user":"bob","since_ms":1700000000000}]|}
    ~with_:
      ({|"shared_with":[{"user":"bob","since_ms":1700000000000}],|}
       ^ periods 3 10 25)
;;

let limits_json =
  (* [H.now] is 1791194400000. *)
  {|{"fetched_at_ms":1791194400000,"providers":[{"provider":"openai","name":"ChatGPT","status":"limits","accounts":[
      {"ref":"openai/main","id":"main","label":"Personal","default":true,"status":"limits","plan":"pro","windows":[{"id":"primary","label":"5-hour session","used_percent":42,"resets_at_ms":1791199800000}]},
      {"ref":"openai/work","id":"work","label":"Work","default":false,"status":"error","error":"sign in again: /login openai as Work"}]}]}|}
;;

let%expect_test "settings and the context card show each login's usage" =
  let h = create () in
  H.quiet h (fun () ->
    H.act h (Clock H.now);
    H.act h (Reply (Auth_status Refresh, Ok (Jsonaf.of_string usage_json)));
    H.act h (Reply (Share_targets, Ok (Jsonaf.of_string {|["bob"]|}))));
  H.act h (Settings_section Models);
  H.reply h "usage_limits" limits_json;
  H.reply
    h
    "account_usage"
    {|[{"user":"alice","you":true,"today":{"requests":1,"input":4000,"output":300,"cache_read":0,"cost_usd":0},"last_7_days":{"requests":4,"input":16000,"output":1200,"cache_read":0,"cost_usd":0},"last_30_days":{"requests":5,"input":20000,"output":1500,"cache_read":0,"cost_usd":0}},
       {"user":"bob","today":{"requests":2,"input":8000,"output":600,"cache_read":0,"cost_usd":0},"last_7_days":{"requests":6,"input":24000,"output":1800,"cache_read":0,"cost_usd":0},"last_30_days":{"requests":20,"input":80000,"output":6000,"cache_read":0,"cost_usd":0}}]|};
  H.text h ~selector:".provider-login";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    (Rpc (method_ account_usage) (params ((ref openai/work)))
     (tag (Login_users openai/work)))
    Personal
    Subscription, default, takes over at usage limits. Pro plan.
    (Share…) (Manage)
    5-hour session Resets in 1h 30m · 11:30
    42% used
    Work
    Subscription, shared with bob.
    (Share…) (Manage)
    Limits unknown: sign in again: /login openai as Work
    Today 3 req 13k tok
    7 days 10 req 43k tok
    30 days 25 req 108k tok
    You today 1 req 4.3k tok · 30 days 5 req 22k tok
    bob today 2 req 8.6k tok · 30 days 20 req 86k tok
    Team (from dave)
    Subscription, shared by dave.
    (Hide) (Manage)
    |}];
  H.quiet h (fun () -> H.act h Close_dialog);
  H.act h Toggle_context_card;
  H.text h ~selector:".context-card .login-line";
  H.text h ~selector:".context-card .login-limit";
  [%expect
    {|
    Login Personal
    5-hour session 42% · 1h 30m
    |}]
;;

let%expect_test "a login's name drops the provider's name its label repeats" =
  let login ~provider label shared_by =
    {|{"provider":"p","name":"|}
    ^ provider
    ^ {|","methods":[],"configured":null,"accounts":[{"ref":"p/a","id":"a","label":"|}
    ^ label
    ^ {|","method":"oauth","source":"oauth","default":true,"fallback":false|}
    ^ Option.value_map shared_by ~default:"" ~f:(sprintf {|,"shared_by":"%s"|})
    ^ "}]}"
    |> Jsonaf.of_string
    |> Prigh_protocol.Auth_status.of_json
    |> Or_error.ok_exn
    |> List.return
    |> Prigh_ui.Provider_logins.all
    |> List.hd_exn
  in
  List.iter
    [ "Anthropic", "Anthropic (Claude Pro/Max)", None
    ; "ChatGPT", "ChatGPT Plus/Pro", None
    ; "ChatGPT", "ChatGPT API key", None
    ; "OpenAI API", "OpenAI API key", None
    ; "OpenAI API", "OpenAI OAuth", None
    ; "DeepSeek", "DeepSeek API key", Some "carol"
    ; "Anthropic", "Anthropic", None
    ; "Anthropic", "work", None
    ]
    ~f:(fun (provider, label, shared_by) ->
      let l = login ~provider label shared_by in
      printf
        "%-28s %-30s | %s\n"
        label
        (Prigh_ui.Provider_logins.name l)
        (Prigh_ui.Provider_logins.title l));
  [%expect
    {|
    Anthropic (Claude Pro/Max)   Claude Pro/Max                 | Anthropic: Claude Pro/Max
    ChatGPT Plus/Pro             Plus/Pro                       | ChatGPT: Plus/Pro
    ChatGPT API key              API key                        | ChatGPT: API key
    OpenAI API key               API key                        | OpenAI API: API key
    OpenAI OAuth                 OAuth                          | OpenAI API: OAuth
    DeepSeek API key             API key (from carol)           | DeepSeek: API key (from carol)
    Anthropic                    Anthropic                      | Anthropic: Anthropic
    work                         work                           | Anthropic: work
    |}]
;;

(* The backend's names: ChatGPT (the subscription) and OpenAI API (a key);
   a provider with only someone else's login hides it rather than log out. *)
let%expect_test "ChatGPT, OpenAI API and a login shared with the user" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.type_ h "/auth";
    H.act h Send;
    H.reply
      h
      "auth_status"
      {|[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Anthropic (Claude Pro/Max)"}],"configured":{"method":"oauth","source":"shared by lloyd"},"expires_ms":null,"accounts":[{"ref":"~lloyd/anthropic/main","id":"~lloyd/main","label":"Max","method":"oauth","source":"shared by lloyd","default":true,"fallback":false,"shared_by":"lloyd"}]},
         {"provider":"openai","name":"OpenAI API","methods":[{"method":"api_key","label":"OpenAI API key"}],"configured":{"method":"api_key","source":"stored api key"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"OpenAI API key","method":"api_key","source":"stored api key","default":true,"fallback":false}]},
         {"provider":"openai-codex","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai-codex/main","id":"main","label":"ChatGPT Plus/Pro","method":"oauth","source":"oauth","default":true,"fallback":false}]}]|});
  H.text h ~selector:".auth-row";
  [%expect
    {|
    Anthropic logged in with oauth (shared by lloyd)
    (Hide)
    OpenAI API logged in with api_key (stored api key)
    (Log out)
    ChatGPT logged in with oauth (oauth)
    (Log out)
    |}];
  H.click h ~selector:".auth-row button";
  [%expect
    {|
    (Run "/accounts hide ~lloyd/anthropic/main")
    (Rpc (method_ account_hide)
     (params ((ref ~lloyd/anthropic/main) (hidden true)))
     (tag
      (Logins_changed (command "/accounts hide ~lloyd/anthropic/main")
       (notice
        "hid Anthropic: Max (from lloyd): /accounts show ~lloyd/anthropic/main brings it back"))))
    |}];
  H.quiet h (fun () -> H.act h (Settings_section Models));
  H.text h ~selector:".settings-group-head";
  H.text h ~selector:".provider-login";
  [%expect
    {|
    Providers
    Anthropic
    OpenAI API
    ChatGPT
    Models prigh uses Besides the conversation's
    Compaction
    Max (from lloyd)
    Subscription, default, shared by lloyd.
    (Hide) (Manage)
    API key
    API key, default.
    (Manage)
    Plus/Pro
    Subscription, default.
    (Manage)
    |}]
;;
