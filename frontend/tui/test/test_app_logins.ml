open! Core
open! Expect_test_helpers_core
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* Two ChatGPT logins (Personal the default) and a DeepSeek key. *)
let logins_json =
  {|[{"provider":"openai","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true},{"ref":"openai/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":false}]},{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":{"method":"api_key","source":"auth.json"},"expires_ms":null,"accounts":[{"ref":"deepseek/main","id":"main","label":"DeepSeek API key","method":"api_key","source":"auth.json","default":true,"fallback":false}]}]|}
;;

let gpt = model_json ~provider:"openai" "gpt-5.5" "GPT-5.5"

let connected ?account () =
  let h = Test_app.connected ~width:72 ~model:gpt () in
  H.reply ~quiet:true h Auth_refresh logins_json;
  H.reply ~quiet:true h Models_catalog models_json;
  Option.iter account ~f:(fun account ->
    H.reply
      ~quiet:true
      h
      Initial_state
      (state_json
         ~model:gpt
         ~extra:[ "account", P.Json.to_string (P.Json.str account) ]
         ()));
  h
;;

let%expect_test "status line names the login when the provider has several" =
  let h = connected () in
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  let h = connected ~account:"work" () in
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  gpt-5.5@Work  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

(* One ChatGPT login: nothing to tell apart, so no @LABEL, pinned or not;
   an agent CLI on one of prigh's logins alike. *)
let one_login_json =
  {|[{"provider":"openai","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"desktop","method":"oauth","source":"oauth","default":true,"fallback":true}]}]|}
;;

let%expect_test "status line: @LABEL only when the provider has several logins" =
  let status ~auth extra =
    let h = Test_app.connected ~width:72 ~model:gpt () in
    H.reply ~quiet:true h Auth_refresh auth;
    H.reply ~quiet:true h Initial_state (state_json ~model:gpt ~extra ());
    Prigh_ui.Render.screen h.model
    |> Prigh_ui.Screen.to_plain ~show_cursor:false
    |> String.split_lines
    |> List.last_exn
    |> print_endline
  in
  let pinned = [ "account", {|"main"|} ] in
  let cli login =
    [ "engine", {|"composer"|}
    ; "adapter", {|"codex"|}
    ; "adapter_login", sprintf "%S" login
    ]
  in
  status ~auth:one_login_json pinned;
  status ~auth:one_login_json (cli "desktop");
  status ~auth:logins_json pinned;
  status ~auth:logins_json (cli "Work");
  [%expect
    {|
    /work  gpt-5.5  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    /work  Codex · gpt-5.5  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    /work  Codex · gpt-5.5@Work  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/accounts picks a login, then what to do with it" =
  let h = connected () in
  H.keys h "/accounts";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ usage_limits) (params ()) (tag (Usage_limits (show false))))
    ▌ earlier question
      earlier answer
    Logins  (5)
    / ▏
    ▸* ChatGPT: Personal     subscription, default, takes over at usage lim…
       ChatGPT: Work         subscription  openai/work
       DeepSeek: API key     API key, default  deepseek/main
       Add a ChatGPT login   another account, under a label you choose
       Add a DeepSeek login  another account, under a label you choose
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "work";
  H.enter h;
  H.show h;
  [%expect
    {|
    ChatGPT: Work  (7)
    / ▏
    ▸  Use in this session        this session's requests go to this login
       Make default               sessions that pick no login use it
       Rename…
       Take over at usage limits  when another login of the provider hits i…
       Move up
       Log in again
       Log out                    deletes the stored credential
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params (
        (model   openai/gpt-5.5)
        (account work)))
      (tag (
        Retry_in_editor
        (command "/account openai/work")
        (hint    "")
        (then_ (Notice_on_success "this session uses ChatGPT: Work")))))
    |}];
  (* Rename asks for the label in the editor. *)
  H.keys h "/accounts menu openai/work";
  H.enter h;
  H.keys h "Rena";
  H.enter h;
  H.keys h "Day job";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ account_update)
      (params (
        (ref   openai/work)
        (label "Day job")))
      (tag (
        Retry_in_editor
        (command "/accounts rename openai/work to Day job")
        (hint    "")
        (then_ (Notice_on_success "renamed ChatGPT: Work to Day job")))))
    |}]
;;

let%expect_test "/accounts subcommands" =
  let h = connected () in
  let run text =
    H.keys h text;
    H.enter h
  in
  run "/accounts default work";
  run "/accounts down personal";
  run "/accounts up work";
  run "/accounts fallback Work on";
  [%expect
    {|
    (Rpc
      (method_ account_update)
      (params (
        (ref     openai/work)
        (default true)))
      (tag (
        Retry_in_editor
        (command "/accounts default work")
        (hint    "")
        (then_ (Notice_on_success "Work is now ChatGPT's default login")))))
    (Rpc
      (method_ account_update)
      (params (
        (ref    openai/work)
        (before openai/main)))
      (tag (
        Retry_in_editor
        (command "/accounts down personal")
        (hint    "")
        (then_ (Notice_on_success "moved ChatGPT: Personal down")))))
    (Rpc
      (method_ account_update)
      (params (
        (ref    openai/work)
        (before openai/main)))
      (tag (
        Retry_in_editor
        (command "/accounts up work")
        (hint    "")
        (then_ (Notice_on_success "moved ChatGPT: Work up")))))
    (Rpc
      (method_ account_update)
      (params (
        (ref      openai/work)
        (fallback true)))
      (tag (
        Retry_in_editor
        (command "/accounts fallback Work on")
        (hint    "")
        (then_ (
          Notice_on_success
          "Work takes over when another ChatGPT login hits its usage limit")))))
    |}];
  (* Mistakes stay in the editor, with what to do. *)
  run "/accounts up personal";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────── ChatGPT: Personal is first already ───
    > /accounts up personal▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Prigh_ui.Key.ctrl 'c');
  run "/accounts default hobby";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────── no login "hobby"; /accounts lists them ───
    > /accounts default hobby▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Prigh_ui.Key.ctrl 'c');
  run "/accounts rename work";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────── /accounts rename ACCOUNT to LABEL: type the new label ───
    > /accounts rename work▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/login as, /logout PROVIDER@LABEL, /model MODEL@LABEL" =
  let h = connected () in
  let run text =
    H.keys h text;
    H.enter h
  in
  run "/login openai as Work";
  run "/login openai as Side project";
  [%expect
    {|
    (Rpc
      (method_ login)
      (params (
        (provider openai)
        (account  work)))
      (tag Show_error))
    (Rpc
      (method_ login)
      (params (
        (provider openai)
        (label    "Side project")))
      (tag Show_error))
    |}];
  run "/logout openai@work";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────┐
    │ Log out of ChatGPT: Work and delete its credential? (y/n)            │
    └──────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────
    ? ▏
    gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ logout)
      (params (
        (provider openai)
        (account  work)))
      (tag Show_error))
    |}];
  run "/model gpt-5.5@work";
  run "/model gpt-5.5@nope";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params (
        (model   openai/gpt-5.5)
        (account work)))
      (tag (Set_model_done openai/gpt-5.5)))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────┐
    │ no openai login "nope": it has Personal, Work (/accounts lists them) │
    └────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────
    > /model gpt-5.5@nope▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a pinned login carries over to the provider's other models" =
  let h = connected ~account:"work" () in
  H.keys h "/model gpt-5.5";
  H.enter h;
  H.keys h "/model deepseek-flash";
  H.enter h;
  H.keys h "/account";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params (
        (model   openai/gpt-5.5)
        (account work)))
      (tag (Set_model_done openai/gpt-5.5)))
    (Rpc
      (method_ set_model)
      (params ((model deepseek/deepseek-flash)))
      (tag (Set_model_done deepseek/deepseek-flash)))

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ChatGPT login for this session  (3)
    / ▏
       Personal                subscription, default, takes over at usage l…
    ▸* Work                    subscription
       The provider's default  follows /accounts default
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Work  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "default";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (
        Retry_in_editor
        (command "/account default")
        (hint    "")
        (then_ (Notice_on_success "this session uses ChatGPT's default login")))))
    |}]
;;

(* Alice may share Work with bob or carol (bob has it); dave shares his
   "Team" ChatGPT login with her. *)
let sharing_json =
  {|[{"provider":"openai","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":{"method":"oauth","source":"oauth"},"expires_ms":null,"accounts":[{"ref":"openai/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true,"shareable":true},{"ref":"openai/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":false,"shareable":true,"shared_with":[{"user":"bob","since_ms":1700000000000}]},{"ref":"~dave/openai/team","id":"~dave/team","label":"Team","method":"oauth","source":"shared by dave","default":false,"fallback":false,"shared_by":"dave"}]}]|}
;;

let%expect_test "sharing a login, and one shared with the user" =
  let h = connected () in
  H.reply ~quiet:true h Auth_refresh sharing_json;
  let run text =
    H.keys h text;
    H.enter h
  in
  run "/accounts menu work";
  H.show h;
  [%expect
    {|
    ChatGPT: Work  (1–7 of 9)
    / ▏
    ▸  Use in this session        this session's requests go to this login
       Make default               sessions that pick no login use it
       Rename…
       Take over at usage limits  when another login of the provider hits i…
       Move up
       Share…                     another user of this server may use it; t…
       Stop sharing with bob      their sessions on it go back to their def…
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "Share";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ account_share_targets)
      (params ())
      (tag (Share_targets openai/work)))
    |}];
  H.reply h (Share_targets "openai/work") {|["bob","carol"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Share ChatGPT: Work with  (2)
    / ▏
    ▸* bob    shared with them: pick to stop sharing
       carol  pick to share: they never see its key
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "car";
  H.enter h;
  run "/accounts unshare work from bob";
  run "/accounts unshare work from carol";
  [%expect
    {|
    (Rpc
      (method_ account_share)
      (params ((ref openai/work) (users (bob carol))))
      (tag (
        Retry_in_editor
        (command "/accounts share openai/work with carol")
        (hint    "")
        (then_ (Notice_on_success "shared ChatGPT: Work with carol")))))
    (Rpc
      (method_ account_share)
      (params ((ref openai/work) (users ())))
      (tag (
        Retry_in_editor
        (command "/accounts unshare work from bob")
        (hint    "")
        (then_ (Notice_on_success "stopped sharing ChatGPT: Work with bob")))))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────── ChatGPT: Work is not shared with carol: it is with bob ───
    > /accounts unshare work from carol▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Prigh_ui.Key.ctrl 'c');
  (* Dave's: use it, take over at limits, hide; not share or rename. *)
  run "/accounts menu team";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ChatGPT: Team (from dave)  (3)
    / ▏
    ▸  Use in this session        on dave's login: they see what it costs
       Take over at usage limits  when your own login of the provider hits …
       Hide                       out of your pickers; dave still shares it
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.key h (Prigh_ui.Key.plain Escape);
  run "/accounts share team with bob";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────── ChatGPT: Team (from dave) is dave's: only they can share it ───
    > /accounts share team with bob▏
    /work  gpt-5.5@Personal  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Prigh_ui.Key.ctrl 'c');
  run "/logout openai@Team";
  [%expect
    {|
    (Rpc
      (method_ account_hide)
      (params (
        (ref    ~dave/openai/team)
        (hidden true)))
      (tag (
        Retry_in_editor
        (command "/logout openai@Team")
        (hint    "")
        (then_ (
          Notice_on_success
          "hid ChatGPT: Team (from dave): /accounts show ~dave/openai/team brings it back")))))
    |}]
;;

let%expect_test "/accounts and /usage show each login's usage" =
  let h = connected () in
  let usage ~requests ~cost =
    sprintf
      {|{"requests":%d,"input":%d,"output":%d,"cache_read":0,"cost_usd":%.2f}|}
      requests
      (requests * 4000)
      (requests * 300)
      cost
  in
  let periods today week month =
    sprintf
      {|"usage":{"today":%s,"last_7_days":%s,"last_30_days":%s}|}
      today
      week
      month
  in
  let with_usage =
    String.substr_replace_first
      logins_json
      ~pattern:{|"default":true,"fallback":false}]}]|}
      ~with_:
        (sprintf
           {|"default":true,"fallback":false,%s}]}]|}
           (periods
              (usage ~requests:3 ~cost:0.42)
              (usage ~requests:10 ~cost:1.4)
              (usage ~requests:25 ~cost:3.5)))
    |> String.substr_replace_first
         ~pattern:{|"default":false,"fallback":false}|}
         ~with_:
           (sprintf
              {|"default":false,"fallback":false,%s}|}
              (periods
                 (usage ~requests:0 ~cost:0.)
                 (usage ~requests:2 ~cost:0.)
                 (usage ~requests:2 ~cost:0.)))
  in
  let limits =
    {|{"fetched_at_ms":1700000000000,"providers":[{"provider":"openai","name":"ChatGPT","status":"limits","accounts":[{"ref":"openai/main","id":"main","label":"Personal","default":true,"status":"limits","plan":"pro","windows":[{"id":"primary","label":"5h","used_percent":42,"resets_at_ms":1700005400000},{"id":"secondary","label":"week","used_percent":91,"resets_at_ms":1700259200000}]},{"ref":"openai/work","id":"work","label":"Work","default":false,"status":"error","error":"sign in again with /login openai"}]},{"provider":"deepseek","name":"DeepSeek","status":"api_key"}]}|}
  in
  H.keys h "/accounts";
  H.enter h;
  H.reply ~quiet:true h Auth_refresh with_usage;
  H.reply ~quiet:true h (Usage_limits { show = false }) limits;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ usage_limits) (params ()) (tag (Usage_limits (show false))))
    ▌ earlier question
      earlier answer
    Logins  (5)
    / ▏
    ▸* ChatGPT: Personal     5h 42%, resets in 1h 30m · subscription, defau…
       ChatGPT: Work         30d 2 req 8.6k tok · subscription  openai/work
       DeepSeek: API key     today 3 req 13k tok $0.42 · API key, default  …
       Add a ChatGPT login   another account, under a label you choose
       Add a DeepSeek login  another account, under a label you choose
    ────────────────────────────────────────────────────────────────────────
    >
    gpt-5.5@Personal  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.esc h;
  H.keys h "/usage";
  H.enter h;
  H.reply ~quiet:true h Auth_refresh with_usage;
  H.reply ~quiet:true h (Usage_limits { show = true }) limits;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ usage_limits) (params ()) (tag (Usage_limits (show true))))
    ┌─ Usage ──────────────────────────────────────────────────────────────┐
    │ ChatGPT: Personal  openai/main                                       │
    │   5h 42%, resets in 1h 30m                                           │
    │   week 91%, resets in 3d                                             │
    │   nothing in the last 30 days                                        │
    │ ChatGPT: Work  openai/work                                           │
    │   limits unknown: sign in again with /login openai                   │
    │   today    0 requests, 0 tokens                                      │
    └─────────────────────── 1–7 of 16 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ───────────────────────────────────────── reading the logins' usage… ───
    > ▏
    gpt-5.5@Personal  ↑↓ scroll · Esc closes · typing goes to the editor
    |}];
  H.key h (Prigh_ui.Key.plain Page_down);
  H.show h;
  [%expect
    {|
    ┌─ Usage ──────────────────────────────────────────────────────────────┐
    │   7 days   2 requests, 8.6k tokens                                   │
    │   30 days  2 requests, 8.6k tokens                                   │
    │ DeepSeek: API key  deepseek/main                                     │
    │   today    3 requests, 13k tokens, $0.42                             │
    │   7 days   10 requests, 43k tokens, $1.40                            │
    │   30 days  25 requests, 108k tokens, $3.50                           │
    │                                                                      │
    └────────────────────── 8–14 of 16 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────
    > ▏
    gpt-5.5@Personal  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test "a composer session on an agent CLI has no prigh login" =
  let h = connected () in
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json
       ~model:gpt
       ~extra:
         [ "engine", {|"composer"|}
         ; "adapter", {|"codex"|}
         ; "account", {|"work"|}
         ]
       ());
  H.keys h "/account";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ┌─ Notice ─────────────────────────────────────────────────────────────┐
    │ Codex uses its own login: /adapters login codex runs it on one of    │
    │ prigh's, or sign in through its CLI                                  │
    └────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────
    > ▏
    /work  Codex · gpt-5.5  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;
