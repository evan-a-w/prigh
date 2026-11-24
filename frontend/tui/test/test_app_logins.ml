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

let%expect_test "/accounts picks a login, then what to do with it" =
  let h = connected () in
  H.keys h "/accounts";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Logins  (5)
    / ▏
    ▸* ChatGPT: Personal           subscription, default, takes over at usa…
       ChatGPT: Work               subscription  openai/work
       DeepSeek: DeepSeek API key  API key, default  deepseek/main
       Add a ChatGPT login         another account, under a label you choose
       Add a DeepSeek login        another account, under a label you choose
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
