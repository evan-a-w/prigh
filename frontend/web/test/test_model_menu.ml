open! Core
module H = Harness

(* The composer's row, the model's chip and menu, and the context ring. *)

let%expect_test
    "the composer's row: attach and the ring at the left; host, model, send"
  =
  let h = H.create () in
  H.elements h ~selector:".composer-row > *";
  H.names h ~selector:".composer-row button";
  [%expect
    {|
    <div class="composer-plus">
    <div class="context-ring-anchor flex relative shrink-0">
    <Vdom.Node.none-widget class="">
    <div class="composer-controls">
    <div class="composer-buttons">
    button "Add files or images" -- Add files or images (or paste, or drop them here)
    button "Context 0% full, $0.00: details"
    button "Details" -- The session's details (/session)
    button "backend" -- Tools run on the backend (/host switches)
    button "Model: Claude Opus 5.5, Medium" -- Claude Opus 5.5, reasoning Medium: models, reasoning and access (Ctrl+L)
    button "Send" -- Send (Enter)
    |}]
;;

let%expect_test "the model chip: the provider's mark, the short name, the level"
  =
  let h = H.create () in
  H.elements h ~selector:".model-chip > *";
  H.text h ~selector:".model-chip";
  [%expect
    {|
    <icon class="anthropic">
    <span class="chip-label">
    (Opus 5.5)
    |}];
  (* The level dims beside the name; a pending one shows at once. *)
  H.quiet h (fun () -> H.act h (Set_thinking "high"));
  H.text h ~selector:".model-chip";
  [%expect {| (Opus 5.5) |}]
;;

let menu h =
  H.elements h ~selector:".model-rail-tab.on";
  H.text h ~selector:".model-list"
;;

(* Eight Anthropic models (one legacy) and GPT-6, OpenAI signed in too. *)
let many () =
  let h = H.create () in
  let models =
    List.map
      [ "claude-opus-5-5", "Claude Opus 5.5", false
      ; "claude-sonnet-5", "Claude Sonnet 5", false
      ; "claude-haiku-5", "Claude Haiku 5", false
      ; "claude-opus-5", "Claude Opus 5", false
      ; "claude-sonnet-4-5", "Claude Sonnet 4.5", false
      ; "claude-haiku-4-5", "Claude Haiku 4.5", false
      ; "claude-opus-4-1", "Claude Opus 4.1", false
      ; "claude-sonnet-3-7", "Claude Sonnet 3.7", true
      ]
      ~f:(fun (id, name, legacy) ->
        match
          Jsonaf.of_string (H.model_json ~provider:"anthropic" ~id ~name ())
        with
        | `Object fields ->
          `Object (("legacy", if legacy then `True else `False) :: fields)
        | json -> json)
    @ [ Jsonaf.of_string
          (H.model_json ~provider:"openai" ~id:"gpt-6" ~name:"GPT-6" ())
      ]
  in
  H.quiet h (fun () -> H.act h (Reply (Models, Ok (`Array models))));
  let auth =
    String.substr_replace_first
      H.auth_json
      ~pattern:{|"configured":null|}
      ~with_:{|"configured":{"method":"api_key","source":"env"}|}
  in
  H.quiet h (fun () ->
    H.act h (Reply (Auth_status Refresh, Ok (Jsonaf.of_string auth))));
  h
;;

let%expect_test "the model menu: the rail, Alt+N labels, Show more, ← →" =
  let h = many () in
  H.key h "l" ~ctrl:true;
  H.names h ~selector:".model-rail-tab";
  menu h;
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    [tab] button "Favourites"
    [tab] button "Anthropic"
    [tab] button "OpenAI API"
    <button class="model-rail-tab on">
    Claude Opus 5.5 Alt+1 (Star: into the favourites (Alt+F)) ✓
    Claude Sonnet 5 Alt+2 (Star: into the favourites (Alt+F))
    Claude Haiku 5 Alt+3 (Star: into the favourites (Alt+F))
    Claude Opus 5 Alt+4 (Star: into the favourites (Alt+F))
    Claude Sonnet 4.5 Alt+5 (Star: into the favourites (Alt+F))
    Claude Haiku 4.5 Alt+6 (Star: into the favourites (Alt+F))
    Show 1 more
    |}];
  (* Show more: the older ones, the legacy one left out. *)
  H.click h ~selector:".model-more";
  menu h;
  [%expect
    {|
    Model_menu_more
    (Focus popover-filter)
    <button class="model-rail-tab on">
    Claude Opus 5.5 Alt+1 (Star: into the favourites (Alt+F)) ✓
    Claude Sonnet 5 Alt+2 (Star: into the favourites (Alt+F))
    Claude Haiku 5 Alt+3 (Star: into the favourites (Alt+F))
    Claude Opus 5 Alt+4 (Star: into the favourites (Alt+F))
    Claude Sonnet 4.5 Alt+5 (Star: into the favourites (Alt+F))
    Claude Haiku 4.5 Alt+6 (Star: into the favourites (Alt+F))
    Claude Opus 4.1 Alt+7 (Star: into the favourites (Alt+F))
    |}];
  (* ← → go along the rail, round. *)
  H.key h "ArrowRight" ~target:Field;
  menu h;
  H.key h "ArrowRight" ~target:Field;
  menu h;
  H.key h "ArrowLeft" ~target:Field;
  menu h;
  [%expect
    {|
    (Model_menu_step 1)
    <button class="model-rail-tab on">
    GPT-6 Alt+1 (Star: into the favourites (Alt+F))
    (Model_menu_step 1)
    <button class="favourites model-rail-tab on">
    (Model_menu_step -1)
    <button class="model-rail-tab on">
    GPT-6 Alt+1 (Star: into the favourites (Alt+F))
    |}];
  (* A provider's tab by click. *)
  H.click h ~selector:".model-rail-tab" ~nth:2;
  menu h;
  [%expect
    {|
    (Model_menu_tab (Provider openai))
    (Focus popover-filter)
    <button class="model-rail-tab on">
    GPT-6 Alt+1 (Star: into the favourites (Alt+F))
    |}]
;;

let%expect_test "Alt+F stars into the favourites; Alt+N switches" =
  let h = many () in
  H.key h "l" ~ctrl:true;
  (* The highlighted one, the current model: starred, saved as a pref. *)
  H.key h "f" ~alt:true ~target:Field;
  H.elements h ~selector:".model-row.starred";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    (Toggle_favourite ())
    (Cache_prefs
     (((key favourite-models) (value ((anthropic/claude-opus-5-5))))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((favourite-models (anthropic/claude-opus-5-5))))))
     (tag Show_error))
    (Focus popover-filter)
    <div class="marked model-row selected starred">
    |}];
  (* A star by click, too. *)
  H.click h ~selector:".model-star" ~nth:1;
  H.click h ~selector:".model-rail-tab.favourites";
  menu h;
  [%expect
    {|
    (Toggle_favourite (anthropic/claude-sonnet-5))
    (Cache_prefs
     (((key favourite-models)
       (value ((anthropic/claude-opus-5-5 anthropic/claude-sonnet-5))))))
    (Rpc (method_ set_prefs)
     (params
      ((prefs
        ((favourite-models (anthropic/claude-opus-5-5 anthropic/claude-sonnet-5))))))
     (tag Show_error))
    (Focus popover-filter)
    (Model_menu_tab Favourites)
    (Focus popover-filter)
    <button class="favourites model-rail-tab on">
    Claude Opus 5.5 Alt+1 (Unstar: out of the favourites (Alt+F)) ✓
    Claude Sonnet 5 Alt+2 (Unstar: out of the favourites (Alt+F))
    |}];
  (* Alt+2: the list's second, at once. *)
  H.key h "2" ~alt:true ~code:"Digit2" ~target:Field;
  H.elements h ~selector:"#chip-popover";
  H.text h ~selector:".model-chip";
  [%expect
    {|
    (Model_menu_nth 2)
    (Focus editor)
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Model_set))
    (Sonnet 5)
    |}]
;;

let%expect_test "searching finds models across the providers" =
  let h = many () in
  H.key h "l" ~ctrl:true;
  H.input h ~selector:"#popover-filter" "6";
  H.text h ~selector:".model-list";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    (Picker_query 6)
    GPT-6 OpenAI API Alt+1 (Star: into the favourites (Alt+F))
    |}];
  H.input h ~selector:"#popover-filter" "zzzz";
  H.text h ~selector:"#chip-popover .model-menu-main";
  [%expect
    {|
    (Picker_query zzzz)
    [zzzz]
    All providers
    Nothing matches “zzzz”: Backspace widens the search.
    |}]
;;

let big =
  {|{"id":"claude-fable-5-1","provider":"anthropic","key":"anthropic/claude-fable-5-1","name":"Claude Fable 5.1","context_window":1000000,"max_output":128000,"supports_thinking":true,"thinking_options":["default","low","medium","high"],"thinking_default":"high","cost":{"input":10,"output":50,"cache_read":0.25}}|}
;;

let ring h =
  H.names h ~selector:".context-ring";
  H.elements h ~selector:".context-ring-fill";
  H.text h ~selector:".context-card .context-card-sub"
;;

let%expect_test "the ring: compaction against the window, the fill from 0" =
  (* A 200k window: at 80% (the 200k compact_at_tokens is past it). *)
  let h = H.create () in
  ring h;
  [%expect
    {|
    button "Context 0% full, $0.00: details"
    0% used · compacts at 160k (80%)
    |}];
  (* Half full: the fill is drawn. *)
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "context_tokens", `Number "100000" ] ()));
  ring h;
  H.show h ~selector:".context-ring-fill";
  [%expect
    {|
    button "Context 50% full, $0.00: details"
    <circle class="context-ring-fill">
    50% used · compacts at 160k (80%)
    <circle class="context-ring-fill"
            cx="15"
            cy="15"
            r="12"
            stroke-dasharray="37.7 75.4"
            transform="rotate(-90 15 15)"> </circle>
    |}];
  (* A 1M window: at compact_at_tokens, 200k, a fifth of it. *)
  let h =
    H.create
      ~state:(H.state_json ~fields:[ "model", Jsonaf.of_string big ] ())
      ()
  in
  ring h;
  [%expect
    {|
    button "Context 0% full, $0.00: details"
    0% used · compacts at 200k (20%)
    |}]
;;

let%expect_test "the ring opens its card; Esc closes it" =
  let h = H.create () in
  H.click h ~selector:".context-ring";
  H.elements h ~selector:".context-ring-anchor";
  H.names h ~selector:".context-card button";
  [%expect
    {|
    Toggle_context_card
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    <div class="context-ring-anchor flex open relative shrink-0 z-(--z-menu-anchor)">
    button "Details" -- The session's details (/session)
    |}];
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".context-ring-anchor";
  [%expect
    {|
    Toggle_context_card
    <div class="context-ring-anchor flex relative shrink-0">
    |}];
  H.act h Toggle_context_card;
  H.elements h ~selector:".context-ring-anchor";
  H.act h Toggle_context_card;
  H.elements h ~selector:".context-ring-anchor";
  [%expect
    {|
    <div class="context-ring-anchor flex open relative shrink-0 z-(--z-menu-anchor)">
    <div class="context-ring-anchor flex relative shrink-0">
    |}]
;;

let%expect_test "the session's model is listed even when it is an old one" =
  let h = many () in
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ ( "model"
              , Jsonaf.of_string
                  (H.model_json
                     ~provider:"anthropic"
                     ~id:"claude-opus-4-1"
                     ~name:"Claude Opus 4.1"
                     ()) )
            ]
          ()));
  H.key h "l" ~ctrl:true;
  menu h;
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    <button class="model-rail-tab on">
    Claude Opus 5.5 Alt+1 (Star: into the favourites (Alt+F))
    Claude Sonnet 5 Alt+2 (Star: into the favourites (Alt+F))
    Claude Haiku 5 Alt+3 (Star: into the favourites (Alt+F))
    Claude Opus 5 Alt+4 (Star: into the favourites (Alt+F))
    Claude Sonnet 4.5 Alt+5 (Star: into the favourites (Alt+F))
    Claude Haiku 4.5 Alt+6 (Star: into the favourites (Alt+F))
    Claude Opus 4.1 Alt+7 (Star: into the favourites (Alt+F)) ✓
    |}]
;;

let%expect_test "short names: only a maker's name goes" =
  List.iter
    [ "Claude Opus 4.6"
    ; "Claude Opus 4.6 (Thinking)"
    ; "GPT-6.2 Sol"
    ; "GPT-6.2-Sol"
    ; "GPT-OSS 120B (Medium)"
    ; "Gemini 3.8 Flash"
    ; "Opus 5.5 (1M context)"
    ; "DeepSeek V4 Pro"
    ; "Default"
    ]
    ~f:(fun name ->
      printf "%s -> %s\n" name (Prigh_web.Model_menu.short_name name));
  [%expect
    {|
    Claude Opus 4.6 -> Opus 4.6
    Claude Opus 4.6 (Thinking) -> Opus 4.6 (Thinking)
    GPT-6.2 Sol -> 6.2 Sol
    GPT-6.2-Sol -> GPT-6.2-Sol
    GPT-OSS 120B (Medium) -> GPT-OSS 120B (Medium)
    Gemini 3.8 Flash -> Gemini 3.8 Flash
    Opus 5.5 (1M context) -> Opus 5.5 (1M context)
    DeepSeek V4 Pro -> V4 Pro
    Default -> Default
    |}]
;;

let%expect_test "newest first, a CLI's models in its own order where they stand"
  =
  let model provider id name : Prigh_protocol.Model.t =
    { id
    ; provider
    ; key = provider ^ "/" ^ id
    ; name
    ; context_window = 0
    ; max_output = 0
    ; supports_thinking = false
    ; thinking_options = []
    ; thinking_default = None
    ; cost = { input = 0.; output = 0.; cache_read = 0. }
    ; image_limit = None
    ; legacy = false
    }
  in
  Prigh_web.Model_menu.newest_first
    [ model "anthropic" "opus-4" "Opus 4"
    ; model "codex" "gpt-5" "GPT-5"
    ; model "codex" "gpt-6" "GPT-6"
    ; model "anthropic" "opus-5" "Opus 5"
    ; model "openai" "o3" "o3"
    ; model "codex" "gpt-4" "GPT-4"
    ; model "anthropic" "haiku" "Haiku"
    ]
  |> List.iter ~f:(fun (m : Prigh_protocol.Model.t) -> print_endline m.key);
  List.iter [ "acp:opencode"; "acp:goose"; "custom" ] ~f:(fun p ->
    printf "%s: %s\n" p (Prigh_web.Provider_mark.letter p));
  [%expect
    {|
    anthropic/opus-5
    codex/gpt-5
    codex/gpt-6
    anthropic/opus-4
    openai/o3
    codex/gpt-4
    anthropic/haiku
    acp:opencode: O
    acp:goose: G
    custom: C
    |}]
;;
