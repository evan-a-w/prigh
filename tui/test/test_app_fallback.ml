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

let config ?(fallback = []) ?default_cwd () =
  sprintf
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":true,"default_model":null,"default_thinking":null,"fallback_models":[%s],"default_cwd":%s}|}
    (String.concat
       ~sep:","
       (List.map fallback ~f:(fun key -> P.Json.to_string (P.Json.str key))))
    (match default_cwd with
     | Some dir -> P.Json.to_string (P.Json.str dir)
     | None -> "null")
;;

let handover ~from ~to_ =
  sprintf
    "[prigh: %s cannot continue (HTTP 429: The usage limit has been reached \
     (usage limit reached)), so %s takes over this conversation from here. \
     Carry on with the task where it left off.]"
    from
    to_
;;

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let%expect_test
    "/fallback shows the chain and the model in use, or how to set one"
  =
  let h = connected () in
  (* Not loaded yet: fetched first. *)
  H.keys h "/fallback";
  H.enter h;
  H.enter h;
  [%expect {| (Rpc (method_ get_config) (params ()) (tag Fallback_shown)) |}];
  H.reply h Fallback_shown (config ());
  H.keys h "/fallback";
  H.enter h;
  H.enter h;
  H.event
    h
    (Config_changed
       (Or_error.ok_exn
          (P.Config.of_json
             (Or_error.ok_exn
                (P.Json.parse
                   (config
                      ~fallback:
                        [ "anthropic/claude-fable-5-1"
                        ; "deepseek/deepseek-flash"
                        ]
                      ()))))));
  H.keys h "/fallback";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no fallback models: /fallback MODEL [MODEL...] sets the
      chain that takes over when a model's usage runs out
      no fallback models: /fallback MODEL [MODEL...] sets the
      chain that takes over when a model's usage runs out
      fallback: anthropic/claude-fable-5-1 →
      deepseek/deepseek-flash (now on deepseek/deepseek-flash)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/fallback MODEL...: completes each word, sends only fallback_models and \
     reports the resolved chain"
  =
  let h = connected () in
  H.keys h "/fallback ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /fallback ▏
    ▸ off                  no fallback: a model whose usage run…
      Claude Fable 5       anthropic/claude-fable-5
      Claude Fable 5.1     anthropic/claude-fable-5-1
      GPT-5.5              openai/gpt-5.5
      DeepSeek V4.1 Flash  deepseek/deepseek-flash
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  H.keys h "fable5.1";
  H.key h (Key.plain Tab);
  H.keys h " ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /fallback anthropic/claude-fable-5-1 ▏
    ▸ Claude Fable 5       anthropic/claude-fable-5
      GPT-5.5              openai/gpt-5.5
      DeepSeek V4.1 Flash  deepseek/deepseek-flash
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  (* Enter accepts the filtered model and runs the command. *)
  H.keys h "flash";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          fallback_models (anthropic/claude-fable-5-1 deepseek/deepseek-flash))))))
      (tag (
        Fallback_saved
        "/fallback anthropic/claude-fable-5-1 deepseek/deepseek-flash")))
    |}];
  H.reply
    h
    (Fallback_saved
       "/fallback anthropic/claude-fable-5-1 deepseek/deepseek-flash")
    (config
       ~fallback:[ "anthropic/claude-fable-5-1"; "deepseek/deepseek-flash" ]
       ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      fallback: anthropic/claude-fable-5-1 →
      deepseek/deepseek-flash (now on deepseek/deepseek-flash)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  (* Names and prefixes are the backend's to resolve, like /model's. *)
  H.keys h "/fallback gpt-5.5 zzz";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((fallback_models (gpt-5.5 zzz))))))
      (tag (Fallback_saved "/fallback gpt-5.5 zzz")))
    |}];
  H.reply_error
    h
    (Fallback_saved "/fallback gpt-5.5 zzz")
    {|fallback_models: unknown model "zzz"; did you mean: openai/gpt-5.5 (GPT-5.5)|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      fallback: anthropic/claude-fable-5-1 →
      deepseek/deepseek-flash (now on deepseek/deepseek-flash)
      fallback_models: unknown model "zzz"; did you mean:
      openai/gpt-5.5 (GPT-5.5)
    ────────────────────────────────────────────────────────────
    > /fallback gpt-5.5 zzz▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  print_s [%sexp (h.model.config : P.Config.t option)];
  [%expect
    {|
    ((
      (scoped_models (deepseek/deepseek-flash))
      (confirm_tools true)
      (default_model    ())
      (default_thinking ())
      (fallback_models (anthropic/claude-fable-5-1 deepseek/deepseek-flash))
      (default_cwd ())))
    |}]
;;

let%expect_test "/fallback off clears the chain" =
  let h = connected () in
  H.keys h "/fallback off";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((fallback_models ())))))
      (tag (Fallback_saved "/fallback off")))
    |}];
  H.reply h (Fallback_saved "/fallback off") (config ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no fallback models: /fallback MODEL [MODEL...] sets the
      chain that takes over when a model's usage runs out
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/default-dir: show, set with directory completion, off, and a refusal"
  =
  let h = connected () in
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/default-dir";
  H.enter h;
  H.enter h;
  type_quietly h "/default-dir ~/p";
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix "")
        (host   backend)))
      (tag (Dirs_for_autocomplete "")))
    |}];
  H.reply ~quiet:true h (Dirs_for_autocomplete "~/p") {|["~/proj/","~/play/"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no default directory: /default-dir PATH makes new sessions
      start there
    ────────────────────────────────────────────────────────────
    > /default-dir ~/p▏
    ▸ ~/proj/
      ~/play/
    …deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Esc close
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((default_cwd ~/proj/)))))
      (tag (Default_dir_saved "/default-dir ~/proj/")))
    |}];
  H.reply
    h
    (Default_dir_saved "/default-dir ~/proj/")
    (config ~default_cwd:"~/proj/" ());
  type_quietly h "/default-dir";
  H.enter h;
  H.enter h;
  type_quietly h "/default-dir off";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix "")
        (host   backend)))
      (tag (Dirs_for_autocomplete "")))
    (Rpc
      (method_ set_config)
      (params ((config ((default_cwd null)))))
      (tag (Default_dir_saved "/default-dir off")))
    |}];
  H.reply h (Default_dir_saved "/default-dir off") (config ());
  type_quietly h "/default-dir /no such";
  H.enter h;
  H.reply_error
    h
    (Default_dir_saved "/default-dir /no such")
    "cannot save /home/u/.prigh/config.json: Permission denied";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((default_cwd "/no such")))))
      (tag (Default_dir_saved "/default-dir /no such")))
    ▌ earlier question
      earlier answer
      no default directory: /default-dir PATH makes new sessions
      start there
      default directory: ~/proj/ (new sessions start there;
      /default-dir off clears it)
      default directory: ~/proj/ (new sessions start there;
      /default-dir off clears it)
      no default directory: /default-dir PATH makes new sessions
      start there
      cannot save /home/u/.prigh/config.json: Permission denied
    ────────────────────────────────────────────────────────────
    > /default-dir /no such▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/confirm and /scoped-models send only their own field, so they never \
     reset the fallback chain"
  =
  let h = connected () in
  H.keys h "/confirm on";
  H.enter h;
  H.keys h "/scoped-models";
  H.enter h;
  H.key h (Key.plain Tab);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((confirm_tools true)))))
      (tag (Notice_on_success "tool confirmation on")))
    (Rpc
      (method_ set_config)
      (params ((config ((scoped_models (deepseek/deepseek-flash))))))
      (tag Config_saved))
    |}]
;;

let%expect_test
    "a hand-over shows as one line, not a prompt; the status line follows the \
     new model"
  =
  let h = connected () in
  H.keys h "fix the bug";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "fix the bug"));
  H.event
    h
    (Message_end
       (Or_error.ok_exn
          (P.Message.of_json
             (Or_error.ok_exn
                (P.Json.parse
                   {|{"role":"assistant","content":[],"stop_reason":{"type":"error","message":"HTTP 429: The usage limit has been reached (usage limit reached)"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}|})))));
  H.event
    h
    (Notice
       "deepseek/deepseek-flash: HTTP 429: The usage limit has been reached \
        (usage limit reached); handing over to anthropic/claude-fable-5-1");
  let fable = model_json "claude-fable-5-1" "Claude Fable 5.1" in
  H.event h (State (state ~model:fable ~running:true ()));
  H.event
    h
    (Message_start
       (user
          (handover
             ~from:"deepseek/deepseek-flash"
             ~to_:"anthropic/claude-fable-5-1")));
  H.event h (Message_update { partial; delta = Text_delta "fixed it" });
  H.event h (Message_end (assistant "fixed it"));
  H.event h (State (state ~model:fable ()));
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "fix the bug"))) (tag Show_error))
    (Append_history "fix the bug")
      earlier answer

    ▌ fix the bug
      error: HTTP 429: The usage limit has been reached (usage
      limit reached)
      deepseek/deepseek-flash: HTTP 429: The usage limit has
      been reached (usage limit reached); handing over to
      anthropic/claude-fable-5-1
      ↪ handed over from deepseek/deepseek-flash to
      anthropic/claude-fable-5-1
      fixed it
    ────────────────────────────────────────────────────────────
    > ▏
    /work  claude-fable-5-1  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  (* Verbose adds the error; quiet keeps the line. *)
  H.keys h "/verbosity verbose";
  H.enter h;
  H.show h;
  H.keys h "/verbosity quiet";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ fix the bug
      error: HTTP 429: The usage limit has been reached (usage
      limit reached)
      deepseek/deepseek-flash: HTTP 429: The usage limit has
      been reached (usage limit reached); handing over to
      anthropic/claude-fable-5-1
      ↪ handed over from deepseek/deepseek-flash to
      anthropic/claude-fable-5-1 (HTTP 429: The usage limit has
      been reached (usage limit reached))
      fixed it
      view: verbose — everything is shown
    ────────────────────────────────────────────────────────────
    > ▏
    /work  claude-fable-5-1  think:off  ctx:0.1%/1.0M  $0.01
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the bug
      error: HTTP 429: The usage limit has been reached (usage
      limit reached)
      ↪ handed over from deepseek/deepseek-flash to
      anthropic/claude-fable-5-1
      fixed it
    ────────────────────────────────────────────────────────────
    > ▏
    /work  claude-fable-5-1  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a hand-over in the history and in /fork's list" =
  let h = connected () in
  let text =
    handover ~from:"deepseek/deepseek-flash" ~to_:"anthropic/claude-fable-5-1"
  in
  H.reply
    h
    Initial_messages
    (sprintf
       {|[{"role":"user","text":"fix the bug"},{"role":"user","text":%s},{"role":"assistant","content":[{"type":"text","text":"fixed it"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|}
       (P.Json.to_string (P.Json.str text)));
  H.show h;
  H.keys h "/fork";
  H.enter h;
  H.reply
    h
    Entries_for_fork
    (sprintf
       {|{"head":"u2","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"fix the bug"}},{"id":"u2","parent":"u1","kind":"message","message":{"role":"user","text":%s}}]}|}
       (P.Json.to_string (P.Json.str text)));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix the bug
      ↪ handed over from deepseek/deepseek-flash to
      anthropic/claude-fable-5-1
      fixed it
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))

    ▌ earlier question
      earlier answer

    ▌ fix the bug
      ↪ handed over from deepseek/deepseek-flash to
      anthropic/claude-fable-5-1
      fixed it
    Fork at  (2)
    / ▏
       fix the bug                     #1
    ▸* ↪ handed over from deepseek/deepseek-flash to anthropic/…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;
