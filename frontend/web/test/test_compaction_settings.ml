open! Core
module H = Harness

(* Settings' Compaction section: what [/compaction] sets, shown and set on
   the settings page. *)

let config ?(at = 200_000) ?model ?(mask = true) () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; "compact_at_tokens", `Number (Int.to_string at)
        ; ( "compaction_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ; ("mask_observations", if mask then `True else `False)
        ])
;;

let section h = H.text h ~selector:".settings-section:has(.compact-at)"

let field h =
  H.show h ~selector:".compact-at-input";
  H.text h ~selector:".setting.compact-at .setting-note"
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  h
;;

let%expect_test "the section: defaults, then each control" =
  let h = settings () in
  section h;
  H.names h ~selector:".settings-section:has(.compact-at) input";
  H.names h ~selector:".settings-section:has(.compact-at) button";
  [%expect
    {|
    Compaction
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window.
    [200k] tokens
    (Off)
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (The conversation's model)
    Mask old tool outputs Replace tool outputs older than the last 10 turns with placeholders first, which often saves summarising.
    (Keep old tool outputs (/compaction mask off))
    input "Compact at, in tokens" -- /compaction at TOKENS, e.g. 150k or 200000
    button "Off" -- Only at 80% of the model's window (/compaction at off)
    button "Summary model: The conversation's model" -- Choose the model (/compaction model)
    [switch] button "Mask old tool outputs" -- Keep old tool outputs (/compaction mask off)
    |}];
  (* A threshold: typed, applied with Enter, shown at once and saved. *)
  H.act h (Compact_at_input "150k");
  H.key h "Enter" ~target:Field;
  field h;
  H.reply h "set_config" (config ~at:150_000 ());
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_config) (params ((config ((compact_at_tokens 150000)))))
     (tag Compaction_saved))
    <input id="compact-at"
           type="text"
           placeholder="200k"
           title="/compaction at TOKENS, e.g. 150k or 200000"
           aria-label="Compact at, in tokens"
           aria-invalid="false"
           autocomplete="off"
           spellcheck="false"
           class="compact-at-input"
           #value="150k"
           @on_change
           @on_input/>
    (Expire_toast (id 0) (after_ms 6000))
    Compaction: at 150k tokens, or 80% of the model's window if that is less; summaries by the session's model; observation masking on (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    |}];
  (* Leaving the field ([change]) applies it too; the settings stay open. *)
  H.input h ~selector:".compact-at-input" " 120000 ";
  H.text h ~selector:".setting.compact-at .setting-control";
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    (Compact_at_input " 120000 ")
    Compact_at_commit
    (Rpc (method_ set_config) (params ((config ((compact_at_tokens 120000)))))
     (tag Compaction_saved))
    [120k] tokens
    (Off)
    true
    |}];
  H.quiet h (fun () -> H.reply h "set_config" (config ~at:120_000 ()));
  (* Not a number: refused, saying what to type; nothing is sent. *)
  H.act h (Compact_at_input "lots");
  H.key h "Enter" ~target:Field;
  field h;
  [%expect
    {|
    Dialog_accept
    <input id="compact-at"
           type="text"
           placeholder="200k"
           title="/compaction at TOKENS, e.g. 150k or 200000"
           aria-label="Compact at, in tokens"
           aria-invalid="true"
           autocomplete="off"
           spellcheck="false"
           class="compact-at-input"
           #value="lots"
           @on_change
           @on_input/>
    “lots” isn't a number of tokens: type one like 150k or 200000, or off to compact only at 80% of the window.
    |}];
  (* Typing again clears the error; Off (or typing off) turns it off. *)
  H.act h (Compact_at_input "15");
  field h;
  H.click h ~selector:".setting.compact-at .segment:not(.compact-at-field)";
  H.text h ~selector:".setting.compact-at";
  H.elements h ~selector:".setting.compact-at .segment";
  [%expect
    {|
    <input id="compact-at"
           type="text"
           placeholder="200k"
           title="/compaction at TOKENS, e.g. 150k or 200000"
           aria-label="Compact at, in tokens"
           aria-invalid="false"
           autocomplete="off"
           spellcheck="false"
           class="compact-at-input"
           #value="15"
           @on_change
           @on_input/>
    (Run "/compaction at off")
    (Rpc (method_ set_config) (params ((config ((compact_at_tokens 0)))))
     (tag Compaction_saved))
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window. Off: only at 80% of the window. Type a number to set one.
    [] tokens
    (Off)
    <label class="compact-at-field segment">
    <button class="on segment">
    |}];
  H.quiet h (fun () -> H.reply h "set_config" (config ~at:0 ()));
  H.act h (Compact_at_input "off");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_config) (params ((config ((compact_at_tokens 0)))))
     (tag Compaction_saved))
    |}];
  H.quiet h (fun () -> H.reply h "set_config" (config ~at:0 ()));
  (* With nothing typed, Enter accepts the page: it closes. *)
  H.key h "Enter" ~target:Field;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    false
    |}]
;;

let%expect_test "summary model: the model picker, then back to settings" =
  let h = settings () in
  H.click h ~selector:".summary-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Run "/compaction model")
    (Focus picker-input)
    Summary model
    (Close (Esc))
    []
    The conversation's model whichever model the conversation uses ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".setting.summary-model";
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((compaction_model anthropic/claude-sonnet-5)))))
     (tag Compaction_saved))
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (Claude Sonnet 5)
    true
    |}];
  H.reply
    h
    "set_config"
    (config ~model:"anthropic/claude-sonnet-5" ());
  (* Back to the conversation's model, from the same picker. *)
  H.quiet h (fun () -> H.click h ~selector:".summary-model-choice");
  H.text h ~selector:".picker-item[aria-selected=true]";
  H.text h ~selector:".modal";
  [%expect {|
    (Expire_toast (id 0) (after_ms 6000))
    (Run "/compaction model")
    Claude Sonnet 5 anthropic, 200k ctx ✓
    Summary model
    (Close (Esc))
    []
    The conversation's model whichever model the conversation uses
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx ✓
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "conversation"));
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".setting.summary-model";
  [%expect {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config) (params ((config ((compaction_model null)))))
     (tag Compaction_saved))
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (The conversation's model)
    |}]
;;

let%expect_test "mask: the switch" =
  let h = settings () in
  H.click h ~selector:".setting.mask .switch";
  H.elements h ~selector:".setting.mask .switch";
  H.reply h "set_config" (config ~mask:false ());
  H.names h ~selector:".setting.mask .switch";
  [%expect {|
    (Run "/compaction mask off")
    (Rpc (method_ set_config) (params ((config ((mask_observations false)))))
     (tag Compaction_saved))
    <button class="switch">
    (Expire_toast (id 0) (after_ms 6000))
    [switch] button "Mask old tool outputs" -- Mask old tool outputs (/compaction mask on)
    |}]
;;

let%expect_test "in sync with /compaction here and elsewhere; reset with the user" =
  let h = settings () in
  (* Another frontend's /compaction. *)
  H.event
    h
    (sprintf
       {|{"type":"event","event":"config_changed","config":%s}|}
       (config ~at:90_000 ~model:"openai/gpt-6" ~mask:false ()));
  section h;
  [%expect {|
    Compaction
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window.
    [90k] tokens
    (Off)
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (GPT-6)
    Mask old tool outputs Replace tool outputs older than the last 10 turns with placeholders first, which often saves summarising.
    (Mask old tool outputs (/compaction mask on))
    |}];
  (* This one's, typed in the composer behind the page. *)
  H.quiet h (fun () ->
    H.type_ h "/compaction at 250k";
    H.act h Send);
  H.text h ~selector:".setting.compact-at .setting-control";
  [%expect {|
    [250k] tokens
    (Off)
    |}];
  (* A half-typed threshold is the page's: Esc drops it. *)
  H.act h (Compact_at_input "33");
  H.key h "Escape" ~target:Field;
  H.quiet h (fun () -> H.act h Open_settings);
  H.text h ~selector:".setting.compact-at .setting-control";
  [%expect {|
    Close_dialog
    (Focus editor)
    [250k] tokens
    (Off)
    |}];
  (* Another account: its own settings, once the backend sends them. *)
  H.act h (Compact_at_input "lots");
  H.key h "Enter" ~target:Field;
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         ( User_switched
         , Ok
             (Jsonaf.of_string
                {|{"client_id":"c1","user":"root","namespace":"bob"}|}) ));
    H.act h Open_settings);
  section h;
  H.reply h "get_config" (config ());
  section h;
  [%expect {|
    Dialog_accept
    Compaction
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window. Waiting for the backend's settings…
    [] tokens
    (Off)
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (The conversation's model)
    Mask old tool outputs Replace tool outputs older than the last 10 turns with placeholders first, which often saves summarising.
    (Mask old tool outputs (/compaction mask on))
    Compaction
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window.
    [200k] tokens
    (Off)
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (The conversation's model)
    Mask old tool outputs Replace tool outputs older than the last 10 turns with placeholders first, which often saves summarising.
    (Keep old tool outputs (/compaction mask off))
    |}]
;;

let%expect_test "the picker: Esc goes back to settings; from the composer too" =
  let h = settings () in
  H.quiet h (fun () -> H.click h ~selector:".summary-model-choice");
  H.key h "Escape" ~target:Field;
  H.elements h ~selector:".modal";
  [%expect {|
    (Run "/compaction model")
    Close_dialog
    (Focus dialog)
    <div class="modal settings-dialog">
    |}];
  H.quiet h (fun () -> H.act h Close_dialog);
  H.quiet h (fun () -> H.type_ h "/compaction model");
  H.act h Send;
  H.quiet h (fun () -> H.act h (Picker_query "gpt"));
  H.key h "Enter" ~target:Field;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction model") (max 500))) (tag Ignore))
    (Focus picker-input)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_config)
     (params ((config ((compaction_model openai/gpt-6)))))
     (tag Compaction_saved))
    false
    |}]
;;
