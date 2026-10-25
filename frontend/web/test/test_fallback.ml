open! Core
open Prigh_web
module H = Harness

(* The model hand-over: [/fallback] sets the chain of models that take over
   when one's usage runs out, and the transcript shows a hand-over as a line
   rather than as a prompt of the user's. [/default-dir] sets where new
   sessions start. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let config ?(fallback = []) ?default_cwd () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; "confirm_tools", `False
        ; "default_model", `Null
        ; "default_thinking", `Null
        ; "fallback_models", `Array (List.map fallback ~f:(fun k -> `String k))
        ; ( "default_cwd"
          , Option.value_map default_cwd ~default:`Null ~f:(fun d -> `String d)
          )
        ])
;;

let print_config h =
  print_s [%sexp ((H.model h).config : Prigh_protocol.Config.t option)]
;;

let draft h =
  let m = H.model h in
  print_s [%message (m.draft : string) (m.cursor : int)]
;;

let gpt_6 =
  H.state_json
    ~fields:
      [ ( "model"
        , Jsonaf.of_string
            (H.model_json ~provider:"openai" ~id:"gpt-6" ~name:"GPT-6" ()) )
      ]
    ()
;;

let%expect_test "/fallback shows the chain, sets only it, and clears it" =
  let h = H.create () in
  (* An older backend's config has neither field. *)
  print_config h;
  run h "/fallback";
  toasts h;
  [%expect
    {|
    (((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (archive_after_days 7) (notify bell) (snippets ())))
    (Rpc (method_ push_pref) (params ((key history) (value /fallback) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 5780))
    No fallback models: /fallback MODEL [MODEL...] sets the chain that takes over when a model's usage runs out
    |}];
  (* The backend resolves names and prefixes; the toast shows its keys. *)
  run h "/fallback  gpt-6 Claude-Sonnet ";
  H.reply
    h
    "set_config"
    (config ~fallback:[ "openai/gpt-6"; "anthropic/claude-sonnet-5" ] ());
  toasts h;
  print_config h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/fallback  gpt-6 Claude-Sonnet") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((fallback_models (gpt-6 Claude-Sonnet))))))
     (tag Fallback_saved))
    (Expire_toast (id 1) (after_ms 4980))
    No fallback models: /fallback MODEL [MODEL...] sets the chain that takes over when a model's usage runs out
    Fallback: openai/gpt-6 → anthropic/claude-sonnet-5 (now on anthropic/claude-opus-5-5)
    (((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ())
      (fallback_models (openai/gpt-6 anthropic/claude-sonnet-5)) (default_cwd ())
      (archive_after_days 7) (notify bell) (snippets ())))
    |}];
  (* After a hand-over the status line's model moves on, and so does
     /fallback's "now on". *)
  H.event h (sprintf {|{"event":"state","state":%s}|} gpt_6);
  run h "/fallback";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fallback) (max 500)))
     (tag Ignore))
    (Expire_toast (id 2) (after_ms 4460))
    Fallback: openai/gpt-6 → anthropic/claude-sonnet-5 (now on openai/gpt-6)
    |}];
  (* Another client's change shows up. *)
  H.event
    h
    (sprintf
       {|{"event":"config_changed","config":%s}|}
       (config ~fallback:[ "deepseek/deepseek-chat" ] ()));
  run h "/fallback";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fallback) (max 500)))
     (tag Ignore))
    (Expire_toast (id 3) (after_ms 3660))
    Fallback: deepseek/deepseek-chat (now on openai/gpt-6)
    |}];
  run h "/fallback off";
  H.reply h "set_config" (config ());
  H.text h ~selector:".toast:last-child";
  print_config h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/fallback off") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((fallback_models ())))))
     (tag Fallback_saved))
    (Expire_toast (id 4) (after_ms 5260))
    Fallback cleared: a model whose usage runs out stops the run (/fallback MODEL... sets a chain)
    (((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (archive_after_days 7) (notify bell) (snippets ())))
    |}]
;;

let%expect_test
    "/fallback: an unknown model fails with the backend's suggestions"
  =
  let h = H.create () in
  run h "/fallback gpt-6 nope";
  H.fail
    h
    "set_config"
    {|fallback_models: unknown model "nope"; did you mean: openai/gpt-6 (GPT-6)|};
  toasts h;
  print_config h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/fallback gpt-6 nope") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((fallback_models (gpt-6 nope)))))) (tag Fallback_saved))
    Couldn't set the fallback models: fallback_models: unknown model "nope"; did you mean: openai/gpt-6 (GPT-6) (Dismiss)
    (((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (archive_after_days 7) (notify bell) (snippets ())))
    |}];
  (* Before the config has arrived there is nothing to show or keep. *)
  let m, cmds = App.update { App.init with draft = "/fallback gpt-6" } Send in
  print_s [%sexp (cmds : App.Command.t list)];
  print_s [%sexp (List.map m.toasts ~f:(fun t -> t.text) : string list)];
  [%expect
    {|
    ((Cache_prefs (((key history) (value (("/fallback gpt-6"))))))
     (Rpc (method_ push_pref)
      (params ((key history) (value "/fallback gpt-6") (max 500))) (tag Ignore)))
    ("Not connected yet: wait for the backend.")
    |}]
;;

let%expect_test "/fallback completes a model key per word" =
  let h = H.create () in
  H.type_ h "/fallback ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Fallback models, in order ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    Claude Opus 5.5 ✓ anthropic
    Claude Sonnet 5 anthropic
    GPT-6 openai
    DeepSeek Chat deepseek
    off no fallback: a model whose usage runs out stops
    |}];
  H.type_ h "/fallback gpt";
  H.key h "Tab";
  draft h;
  (* The next word: the models not listed yet, and no [off]. *)
  H.text h ~selector:".popup";
  [%expect
    {|
    (Complete_accept (run false))
    (Set_caret 23)
    ((m.draft "/fallback openai/gpt-6 ") (m.cursor 23))
    Fallback models, in order ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    Claude Opus 5.5 ✓ anthropic
    Claude Sonnet 5 anthropic
    DeepSeek Chat deepseek
    |}];
  (* Enter accepts the last one and runs the command. *)
  H.type_ h "/fallback openai/gpt-6 sonn";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params
      ((key history) (value "/fallback openai/gpt-6 anthropic/claude-sonnet-5")
       (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params
      ((config ((fallback_models (openai/gpt-6 anthropic/claude-sonnet-5))))))
     (tag Fallback_saved))
    |}];
  (* Enter before another word is begun sends what is there. *)
  H.type_ h "/fallback openai/gpt-6 ";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/fallback openai/gpt-6") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((fallback_models (openai/gpt-6)))))) (tag Fallback_saved))
    |}];
  (* A word in the middle completes in place. *)
  H.act h (Edit { text = "/fallback deep openai/gpt-6"; cursor = 14 });
  H.text h ~selector:".popup";
  H.key h "Tab" ~target:(Editor { cursor = 14 });
  draft h;
  [%expect
    {|
    Fallback models, in order ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    DeepSeek Chat deepseek
    Claude Opus 5.5 ✓ anthropic
    Claude Sonnet 5 anthropic
    (Complete_accept (run false))
    (Set_caret 32)
    ((m.draft "/fallback deepseek/deepseek-chat openai/gpt-6") (m.cursor 32))
    |}];
  H.type_ h "/fallback of";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/fallback off") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((fallback_models ())))))
     (tag Fallback_saved))
    |}]
;;

let%expect_test "/default-dir shows, sets and clears where new sessions start" =
  let h = H.create () in
  run h "/default-dir";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /default-dir) (max 500))) (tag Ignore))
    (Expire_toast (id 0) (after_ms 5620))
    No default directory: new sessions start in the backend's working directory; /default-dir PATH sets one
    |}];
  run h "/default-dir ~/proj";
  H.reply h "set_config" (config ~default_cwd:"~/proj" ());
  H.text h ~selector:".toast:last-child";
  print_config h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix ~/proj) (host backend)))
     (tag (Paths ~/proj)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir ~/proj") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd ~/proj)))))
     (tag Default_dir_saved))
    (Expire_toast (id 1) (after_ms 4700))
    Default directory: ~/proj (new sessions start there; /default-dir off clears it)
    (((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd (~/proj))
      (archive_after_days 7) (notify bell) (snippets ())))
    |}];
  run h "/default-dir";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /default-dir) (max 500))) (tag Ignore))
    (Expire_toast (id 2) (after_ms 4700))
    Default directory: ~/proj (new sessions start there; /default-dir off clears it) ×2
    |}];
  run h "/default-dir off";
  H.reply h "set_config" (config ());
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix off) (host backend)))
     (tag (Paths off)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir off") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd null)))))
     (tag Default_dir_saved))
    (Expire_toast (id 3) (after_ms 4700))
    Default directory cleared: new sessions start in the backend's working directory
    |}];
  run h "/default-dir /etc/passwd";
  H.fail h "set_config" "config.default_cwd must be a directory";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /etc/passwd) (host backend)))
     (tag (Paths /etc/passwd)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir /etc/passwd") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd /etc/passwd)))))
     (tag Default_dir_saved))
    Couldn't set the default directory: config.default_cwd must be a directory (Dismiss)
    |}]
;;

let%expect_test "/default-dir completes directories on the backend's host" =
  (* The tools run elsewhere, but new sessions start on the backend. *)
  let h =
    H.create ~state:(H.state_json ~fields:[ "active_host", `String "c7" ] ()) ()
  in
  H.type_ h "/default-dir /wo";
  H.reply h "list_dirs" {|["/work/","/world/"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix /wo) (host backend)))
     (tag (Paths /wo)))
    Directories on the backend ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    /work/
    /world/
    |}];
  (* Directories keep completing: Enter does not run yet. *)
  H.key h "Enter";
  draft h;
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ list_dirs) (params ((prefix /work/) (host backend)))
     (tag (Paths /work/)))
    (Set_caret 19)
    ((m.draft "/default-dir /work/") (m.cursor 19))
    |}]
;;

let%expect_test "/default-dir: a relative path is the backend host's" =
  let h = H.create () in
  run h "/default-dir proj/../src/";
  run h "/default-dir .";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix proj/../src/) (host backend)))
     (tag (Paths proj/../src/)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir proj/../src/") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd /work/src)))))
     (tag Default_dir_saved))
    (Rpc (method_ list_dirs) (params ((prefix .) (host backend)))
     (tag (Paths .)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir .") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd /work)))))
     (tag Default_dir_saved))
    |}];
  (* With the tools elsewhere, the backend's own directory; ~ is left to the
     backend. *)
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "active_host", `String "c7"
             ; "cwd", `String "/home/me/proj"
             ; ( "hosts"
               , Jsonaf.of_string
                   {|[{"id":"backend","name":"backend","cwd":"/srv/prigh"},
                      {"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null}]|}
               )
             ]
           ())
      ()
  in
  run h "/default-dir repos";
  run h "/default-dir ~/repos";
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix repos) (host backend)))
     (tag (Paths repos)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir repos") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((default_cwd /srv/prigh/repos)))))
     (tag Default_dir_saved))
    (Rpc (method_ list_dirs) (params ((prefix ~/repos) (host backend)))
     (tag (Paths ~/repos)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir ~/repos") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((default_cwd ~/repos)))))
     (tag Default_dir_saved))
    |}];
  (* Before the backend host is known there is nothing to resolve against. *)
  let h =
    H.create ~state:(H.state_json ~fields:[ "active_host", `String "c7" ] ()) ()
  in
  run h "/default-dir repos";
  toasts h;
  [%expect
    {|
    (Rpc (method_ list_dirs) (params ((prefix repos) (host backend)))
     (tag (Paths repos)))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-dir repos") (max 500)))
     (tag Ignore))
    Relative to what? The backend's directory isn't known yet: give an absolute path (/default-dir /path/to/repos) (Dismiss)
    |}]
;;

let handover_text =
  "[prigh: openai/gpt-6 cannot continue (HTTP 429: {\"error\": \"usage limit \
   reached (plan: pro), try later\"} (usage limit reached)), so \
   anthropic/claude-opus-5-5 takes over this conversation from here. Carry on \
   with the task where it left off.]"
;;

let handover_origin =
  {|"origin":{"source":{"kind":"handover","title":"Handed over from openai/gpt-6 to anthropic/claude-opus-5-5"}}|}
;;

let%expect_test "a hand-over: a line, not a prompt bubble" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"message_start","message":{"role":"user","text":"fix the build"}}|}
      ; sprintf
          {|{"event":"message_start","message":{"role":"user","text":%s,%s,"at":1791194280000}}|}
          (Jsonaf.to_string (`String handover_text))
          handover_origin
      ]
  in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <div data-entry="0" class="msg user">
        <div dir="auto" class="bubble"> fix the build </div>
      </div>
      <details data-kind="handover" class="note">
        <summary>
          <span aria-hidden="true" class="glyph"> ↪ </span>
          <span class="title"> Handed over from openai/gpt-6 to anthropic/claude-opus-5-5 </span>
          <time title="Monday 5 October 2026, 11:58:00" datetime="2026-10-05T09:58:00Z" class="time"> 11:58 </time>
        </summary>
        <pre class="note-text">
          [prigh: openai/gpt-6 cannot continue (HTTP 429: {"error": "usage limit reached (plan: pro), try later"} (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.]
        </pre>
      </details>
    </div>
    |}];
  Chat_harness.text chat;
  [%expect
    {| fix the build ↪ Handed over from openai/gpt-6 to anthropic/claude-opus-5-5 11:58 [prigh: openai/gpt-6 cannot continue (HTTP 429: {"error": "usage limit reached (plan: pro), try later"} (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.] |}]
;;

let%expect_test "a hand-over as it happens: notice, model, line" =
  let h = H.create ~state:gpt_6 () in
  H.text h ~selector:".composer-controls";
  H.event
    h
    {|{"event":"config_changed","config":{"fallback_models":["openai/gpt-6","anthropic/claude-opus-5-5"]}}|};
  H.event h {|{"event":"agent_start"}|};
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"fix the build"}}|};
  H.event
    h
    {|{"event":"notice","text":"openai/gpt-6: HTTP 429: quota (usage limit reached); handing over to anthropic/claude-opus-5-5"}|};
  H.event h (sprintf {|{"event":"state","state":%s}|} (H.state_json ()));
  H.event
    h
    (sprintf
       {|{"event":"message_start","message":{"role":"user","text":%s,%s}}|}
       (Jsonaf.to_string (`String handover_text))
       handover_origin);
  H.text h ~selector:".entries";
  H.text h ~selector:".toast";
  H.text h ~selector:".composer-controls";
  [%expect
    {|
    (GPT-6) (Thinking on) (Auto: never asks)
    (Expire_toast (id 0) (after_ms 5260))
    fix the build
    ↪ Handed over from openai/gpt-6 to anthropic/claude-opus-5-5
    [prigh: openai/gpt-6 cannot continue (HTTP 429: {"error": "usage limit reached (plan: pro), try later"} (usage limit reached)), so anthropic/claude-opus-5-5 takes over this conversation from here. Carry on with the task where it left off.]
    openai/gpt-6: HTTP 429: quota (usage limit reached); handing over to anthropic/claude-opus-5-5
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    |}];
  run h "/fallback";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fallback) (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 4980))
    Fallback: openai/gpt-6 → anthropic/claude-opus-5-5 (now on anthropic/claude-opus-5-5)
    |}]
;;

let%expect_test "a hand-over in /fork and /tree" =
  let h = H.create () in
  let entries =
    sprintf
      {|{"head":"e2","entries":[
         {"id":"e1","parent":null,"kind":"message","message":{"role":"user","text":"fix the build"}},
         {"id":"e2","parent":"e1","kind":"message","message":{"role":"user","text":%s,%s}}]}|}
      (Jsonaf.to_string (`String handover_text))
      handover_origin
  in
  run h "/fork";
  H.reply h "get_entries" entries;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fork) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    fix the build #1
    ↪ Handed over from openai/gpt-6 to anthropic/claude-opus-5-5 #2 ✓
    |}];
  H.key h "Escape" ~target:Field;
  run h "/tree";
  H.reply h "get_entries" entries;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ push_pref) (params ((key history) (value /tree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ((all true))) (tag (Entries Tree)))
    (Focus picker-input)
    > fix the build ✓
    > ↪ Handed over from openai/gpt-6 to anthropic/claude-opus-5-5 ✓
    |}]
;;

let%expect_test "/help fallback and default-dir" =
  let h = H.create () in
  run h "/help fallback";
  run h "/help /default-dir";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/help fallback") (max 500))) (tag Ignore))
    (Expire_toast (id 0) (after_ms 5500))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/help /default-dir") (max 500)))
     (tag Ignore))
    (Expire_toast (id 1) (after_ms 4380))
    /fallback [model...|off]: show or set the models that take over, in order, when one's usage runs out
    /default-dir [path|off]: show or set the directory new sessions start in
    |}]
;;
