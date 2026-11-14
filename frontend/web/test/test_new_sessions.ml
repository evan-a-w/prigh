open! Core
module H = Harness

(* Settings → New sessions: every default a new session starts with, each
   with its control and a way back to the built-in, saved with
   [defaults_only] so that open sessions keep theirs. *)

let hosts =
  `Array
    [ `Object
        [ "id", `String "backend"
        ; "name", `String "srv"
        ; "cwd", `String "/work"
        ; "home", `String "/home/prigh"
        ]
    ; `Object
        [ "id", `String "host-1"
        ; "name", `String "laptop"
        ; "cwd", `String "/Users/me"
        ; "home", `String "/Users/me"
        ]
    ]
;;

let config fields =
  Jsonaf.to_string (`Object (("scoped_models", `Array []) :: fields))
;;

let section () =
  let h = H.create ~state:(H.state_json ~fields:[ "hosts", hosts ] ()) () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.act h (Settings_section New_sessions));
  h
;;

let panel h = H.text h ~selector:".settings-panel"

let%expect_test "the section: the built-ins, every control" =
  let h = section () in
  panel h;
  H.names h ~selector:".settings-panel button";
  H.names h ~selector:".settings-panel input";
  [%expect
    {|
    New sessions
    What a new session starts with. A change applies from the next new session: open ones keep theirs. Kept on the backend.
    Start with From the next new session
    Model
    New sessions start with the backend's choice: a model of a provider you are logged in to.
    (The backend's choice) (This session’s)
    Thinking
    How hard new sessions think, on models that can. A model without the level takes its nearest one.
    (Off answer straight away) (Provider's default the provider picks the effort) (Minimal barely) (Low a little) (Medium some) (High more) (Xhigh a lot, for long agentic work) (Max as much as the model allows)
    Mode
    What asks before it runs: everything (Ask), all but file edits (Auto-edit) or nothing (Auto).
    (Ask) (Auto-edit) (Auto)
    Tool host
    New sessions you start here run their tools where the session you are on does.
    (Where you are)
    Starting directory
    New sessions start in their tool host's working directory. ~/ is its home.
    []
    Limits Every session without its own
    Fallback models
    When a model's usage runs out, the next in this list takes over the conversation. Every session follows it, open ones too. None: a model whose usage runs out stops.
    (Add)
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too. Off: no limit.
    (Stop sessions starting turns past a cost (/default-budget))
    button "New sessions' model: The backend's choice" -- Choose the model new sessions start with
    button "This session\226\128\153s" -- Start new sessions with Claude Opus 5.5 · thinking provider's default, its thinking level too (/change_default)
    button "Off" -- New sessions think at off
    button "Provider's default" -- New sessions think at provider's default
    button "Minimal" -- New sessions think at minimal
    button "Low" -- New sessions think at low
    button "Medium" -- New sessions think at medium
    button "High" -- New sessions think at high
    button "Xhigh" -- New sessions think at xhigh
    button "Max" -- New sessions think at max
    button "Ask" -- New sessions start in Ask mode
    button "Auto-edit" -- New sessions start in Auto-edit mode
    button "Auto" -- New sessions start in Auto mode
    button "New sessions' tool host: Where you are" -- Choose the tool host new sessions start on (/default-host)
    button "Add" -- Add a model to the end of the chain (/fallback MODEL…)
    [switch] button "Budget" -- Stop sessions starting turns past a cost (/default-budget)
    input "New sessions' starting directory" -- An absolute path, or ~/… under the host's home
    |}]
;;

let%expect_test "model: picked, shown, reset" =
  let h = section () in
  H.click h ~selector:".default-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Pick_default Model)
    (Focus picker-input)
    New sessions' model
    (Close (Esc))
    []
    The backend's choice one you are logged in to ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "set_config"
    (config [ "default_model", `String "anthropic/claude-sonnet-5" ]);
  H.text h ~selector:".setting.default-model";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params
      ((config ((default_model anthropic/claude-sonnet-5))) (defaults_only true)))
     (tag (Default_set (Model (anthropic/claude-sonnet-5)))))
    (Expire_toast (id 0) (after_ms 4180))
    Model
    New sessions start with Claude Sonnet 5.
    (Claude Sonnet 5) (This session’s) (Back to the built-in: the backend's choice)
    New sessions start with Claude Sonnet 5 (open sessions keep theirs)
    |}];
  H.click h ~selector:".setting.default-model .default-reset";
  H.reply h "set_config" (config []);
  H.text h ~selector:".setting.default-model";
  [%expect
    {|
    (Set_default (Model ()))
    (Rpc (method_ set_config)
     (params ((config ((default_model null))) (defaults_only true)))
     (tag (Default_set (Model ()))))
    (Expire_toast (id 1) (after_ms 4740))
    Model
    New sessions start with the backend's choice: a model of a provider you are logged in to.
    (The backend's choice) (This session’s)
    |}]
;;

let%expect_test "thinking and mode: segmented, for new sessions only" =
  let h = section () in
  H.click h ~selector:".setting.default-thinking .segment:nth-child(4)";
  H.reply h "set_config" (config [ "default_thinking", `String "high" ]);
  H.elements h ~selector:".setting.default-thinking .segment.on";
  H.text h ~selector:".setting.default-thinking";
  H.click h ~selector:".setting.default-thinking .default-reset";
  H.reply h "set_config" (config []);
  H.elements h ~selector:".setting.default-thinking .default-reset";
  [%expect
    {|
    (Set_default (Thinking (low)))
    (Rpc (method_ set_config)
     (params ((config ((default_thinking low))) (defaults_only true)))
     (tag (Default_set (Thinking (low)))))
    (Expire_toast (id 0) (after_ms 3620))
    <button class="on segment">
    Thinking
    How hard new sessions think, on models that can. A model without the level takes its nearest one.
    (Off answer straight away) (Provider's default the provider picks the effort) (Minimal barely) (Low a little) (Medium some) (High more) (Xhigh a lot, for long agentic work) (Max as much as the model allows)
    (Back to the built-in: provider's default)
    (Set_default (Thinking ()))
    (Rpc (method_ set_config)
     (params ((config ((default_thinking null))) (defaults_only true)))
     (tag (Default_set (Thinking ()))))
    (Expire_toast (id 1) (after_ms 5020))
    |}];
  H.click h ~selector:".setting.default-mode .segment:nth-child(1)";
  H.reply h "set_config" (config [ "approval_mode", `String "ask" ]);
  H.text h ~selector:".setting.default-mode";
  H.text h ~selector:".toast:last-child";
  (* This session keeps its mode. *)
  print_endline
    (Option.value_map (H.model h).state ~default:"?" ~f:(fun s ->
       Prigh_protocol.Approval_mode.to_string s.approval_mode));
  [%expect
    {|
    (Set_default (Mode Ask))
    (Rpc (method_ set_config)
     (params ((config ((approval_mode ask))) (defaults_only true)))
     (tag (Default_set (Mode Ask))))
    (Expire_toast (id 2) (after_ms 3820))
    Mode
    What asks before it runs: everything (Ask), all but file edits (Auto-edit) or nothing (Auto).
    (Ask) (Auto-edit) (Auto)
    (Back to the built-in: Auto)
    New sessions start in Ask mode (open sessions keep theirs)
    auto
    |}]
;;

let%expect_test "host: picked from the connected ones; one gone is said" =
  let h = section () in
  H.click h ~selector:".default-host-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Pick_default Host)
    (Focus picker-input)
    New sessions' host
    (Close (Esc))
    []
    Where you are the host of the session you are on ✓
    srv the backend, in /work
    laptop /Users/me
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "lap"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config [ "default_host", `String "host-1" ]);
  H.text h ~selector:".setting.default-host";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((default_host host-1))) (defaults_only true)))
     (tag (Default_set (Host (host-1)))))
    (Expire_toast (id 0) (after_ms 3740))
    Tool host
    New sessions you start here run their tools on laptop, while it is connected.
    (laptop) (Back to the built-in: where you are)
    New sessions start on laptop (open sessions keep theirs)
    |}];
  (* Another client chose a host that is not connected. *)
  H.event
    h
    (sprintf
       {|{"type":"event","event":"config_changed","config":%s}|}
       (config [ "default_host", `String "host-9" ]));
  H.text h ~selector:".setting.default-host";
  [%expect
    {|
    Tool host
    New sessions you start here run their tools on host-9, while it is connected. host-9 is not connected: until it is, new sessions start where you are.
    (host-9) (Back to the built-in: where you are)
    |}];
  H.click h ~selector:".setting.default-host .default-reset";
  [%expect
    {|
    (Set_default (Host ()))
    (Rpc (method_ set_config)
     (params ((config ((default_host null))) (defaults_only true)))
     (tag (Default_set (Host ()))))
    |}]
;;

let%expect_test
    "directory: typed, applied on Enter or leaving the field (relative: to \
     this session's), reset"
  =
  let h = section () in
  H.act h (Default_field_input (Dir, "~/proj"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config [ "default_cwd", `String "~/proj" ]);
  H.text h ~selector:".setting.default-dir";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_config)
     (params ((config ((default_cwd ~/proj))) (defaults_only true)))
     (tag (Default_set (Dir (~/proj)))))
    (Expire_toast (id 0) (after_ms 3740))
    Starting directory
    New sessions start in ~/proj, on their tool host.
    [~/proj] (Back to the built-in: the host's working directory)
    |}];
  (* Leaving the field applies it too. *)
  H.input h ~selector:"#default-dir" "rel";
  H.text h ~selector:".setting.default-dir";
  [%expect
    {|
    (Default_field_input Dir rel)
    (Default_field_commit Dir)
    (Rpc (method_ set_config)
     (params ((config ((default_cwd /work/rel))) (defaults_only true)))
     (tag (Default_set (Dir (/work/rel)))))
    Starting directory
    New sessions start in /work/rel, on their tool host.
    [/work/rel] (Back to the built-in: the host's working directory)
    |}];
  H.click h ~selector:".setting.default-dir .default-reset";
  H.reply h "set_config" (config []);
  H.text h ~selector:".setting.default-dir";
  [%expect
    {|
    (Default_field_input Dir "")
    (Default_field_commit Dir)
    (Rpc (method_ set_config)
     (params ((config ((default_cwd null))) (defaults_only true)))
     (tag (Default_set (Dir ()))))
    (Expire_toast (id 1) (after_ms 3860))
    Starting directory
    New sessions start in their tool host's working directory. ~/ is its home.
    []
    |}]
;;

let%expect_test "fallback: added from the picker, removed, cleared" =
  let h = section () in
  H.click h ~selector:".fallback-add";
  H.quiet h (fun () -> H.act h (Picker_query "gpt"));
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "set_config"
    (config [ "fallback_models", `Array [ `String "openai/gpt-6" ] ]);
  H.quiet h (fun () ->
    H.click h ~selector:".fallback-add";
    H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply
    h
    "set_config"
    (config
       [ ( "fallback_models"
         , `Array
             [ `String "openai/gpt-6"; `String "anthropic/claude-sonnet-5" ] )
       ]);
  H.text h ~selector:".setting.default-fallback";
  [%expect
    {|
    (Pick_default Fallback)
    (Focus picker-input)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((fallback_models (openai/gpt-6)))) (defaults_only true)))
     (tag (Default_set (Fallback (openai/gpt-6)))))
    (Expire_toast (id 0) (after_ms 3780))
    (Pick_default Fallback)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params
      ((config ((fallback_models (openai/gpt-6 anthropic/claude-sonnet-5))))
       (defaults_only true)))
     (tag (Default_set (Fallback (openai/gpt-6 anthropic/claude-sonnet-5)))))
    (Expire_toast (id 1) (after_ms 4980))
    Fallback models
    When a model's usage runs out, the next in this list takes over the conversation. Every session follows it, open ones too.
    1 GPT-6 (Take GPT-6 out of the chain)
    2 Claude Sonnet 5 (Take Claude Sonnet 5 out of the chain)
    (Add) (Back to the built-in: none)
    |}];
  H.click h ~selector:".fallback-remove";
  H.click h ~selector:".setting.default-fallback .default-reset";
  [%expect
    {|
    (Set_default (Fallback (anthropic/claude-sonnet-5)))
    (Rpc (method_ set_config)
     (params
      ((config ((fallback_models (anthropic/claude-sonnet-5))))
       (defaults_only true)))
     (tag (Default_set (Fallback (anthropic/claude-sonnet-5)))))
    (Set_default (Fallback ()))
    (Rpc (method_ set_config)
     (params ((config ((fallback_models ()))) (defaults_only true)))
     (tag (Default_set (Fallback ()))))
    |}]
;;

let%expect_test "budget: switched on, dollars, refused, off and on again" =
  let h = section () in
  let budget () = H.text h ~selector:".setting.default-budget" in
  let switch () = H.click h ~selector:".setting.default-budget .switch" in
  (* Off, the built-in: no field until switched on, which sets $5. *)
  budget ();
  switch ();
  H.reply h "set_config" (config [ "max_budget_usd", `Number "5" ]);
  budget ();
  [%expect
    {|
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too. Off: no limit.
    (Stop sessions starting turns past a cost (/default-budget))
    (Switch_number Budget true)
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd 5))) (defaults_only true)))
     (tag (Default_set (Budget (5)))))
    (Expire_toast (id 0) (after_ms 5220))
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too.
    [5.00] USD
    (No budget, the built-in (/default-budget off))
    |}];
  H.act h (Default_field_input (Budget, "$2.50"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config [ "max_budget_usd", `Number "2.5" ]);
  budget ();
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd 2.5))) (defaults_only true)))
     (tag (Default_set (Budget (2.5)))))
    (Expire_toast (id 1) (after_ms 5220))
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too.
    [2.50] USD
    (No budget, the built-in (/default-budget off))
    Sessions without their own /budget stop once they cost $2.50 (/default-budget off removes it)
    |}];
  H.act h (Default_field_input (Budget, "lots"));
  H.key h "Enter" ~target:Field;
  budget ();
  [%expect
    {|
    Dialog_accept
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too. “lots” isn't an amount of dollars: type one like 5 or 2.50, or clear the field for no budget.
    [lots] USD
    (No budget, the built-in (/default-budget off))
    |}];
  (* Off drops what was typed; on again restores $2.50, not $5. *)
  switch ();
  H.reply h "set_config" (config [ "max_budget_usd", `Null ]);
  budget ();
  switch ();
  [%expect
    {|
    (Switch_number Budget false)
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd null))) (defaults_only true)))
     (tag (Default_set (Budget ()))))
    (Expire_toast (id 2) (after_ms 5100))
    Budget
    Sessions without their own /budget start no turn once they have cost this much, open ones too. Off: no limit.
    (Stop sessions starting turns past a cost (/default-budget))
    (Switch_number Budget true)
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd 2.5))) (defaults_only true)))
     (tag (Default_set (Budget (2.5)))))
    |}];
  (* /default-budget off is remembered the same way. *)
  H.reply h "set_config" (config [ "max_budget_usd", `Number "2.5" ]);
  H.quiet h (fun () -> H.act h (Run "/default-budget 7"));
  H.reply h "set_config" (config [ "max_budget_usd", `Number "7" ]);
  H.quiet h (fun () -> H.act h (Run "/default-budget off"));
  H.reply h "set_config" (config [ "max_budget_usd", `Null ]);
  switch ();
  [%expect
    {|
    (Expire_toast (id 3) (after_ms 5220))
    (Expire_toast (id 4) (after_ms 5220))
    (Expire_toast (id 5) (after_ms 5100))
    (Switch_number Budget true)
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd 7))) (defaults_only true)))
     (tag (Default_set (Budget (7)))))
    |}]
;;

let%expect_test "a refused change shows the backend's again" =
  let h = section () in
  H.click h ~selector:".setting.default-thinking .segment:nth-child(5)";
  H.fail h "set_config" "the disk is full";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Set_default (Thinking (medium)))
    (Rpc (method_ set_config)
     (params ((config ((default_thinking medium))) (defaults_only true)))
     (tag (Default_set (Thinking (medium)))))
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set the default thinking level: the disk is full (Dismiss)
    |}]
;;

let%expect_test "/default-host and /default-budget" =
  let h = section () in
  H.quiet h (fun () -> H.act h Close_dialog);
  H.type_ h "/default-host desk";
  H.act h Send;
  H.text h ~selector:".toast:last-child";
  H.type_ h "/default-host laptop";
  H.act h Send;
  H.type_ h "/default-budget 2.5";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-host desk") (max 500)))
     (tag Ignore))
    No connected tool host is called “desk”: /default-host lists them. (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-host laptop") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((default_host host-1))) (defaults_only true)))
     (tag (Default_set (Host (host-1)))))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/default-budget 2.5") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config)
     (params ((config ((max_budget_usd 2.5))) (defaults_only true)))
     (tag (Default_set (Budget (2.5)))))
    |}]
;;
