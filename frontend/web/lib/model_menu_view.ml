open! Core
open! Import
open Html
module Action = App.Action

let option_id i = sprintf "popover-option-%d" i

let provider_name (m : App.Model.t) id =
  List.find m.auth ~f:(fun (s : Auth_status.t) -> String.equal s.provider id)
  |> Option.value_map ~default:id ~f:(fun s -> s.name)
;;

let access_label (mode : Session_mode.t) =
  if mode.chat
  then "Chat"
  else if mode.plan
  then "Plan"
  else (
    match mode.approval with
    | Ask -> "Ask first"
    | Auto_edit -> "Auto-edit"
    | Smart -> "Smart"
    | Auto -> "Full access")
;;

let rail (m : App.Model.t) ~offered ~inject =
  let tab (tab : Model_menu.Tab.t) =
    let on = Model_menu.Tab.equal tab m.model_menu.tab in
    let glyph, name =
      match tab with
      | Favourites -> Html.icon Star, "Favourites"
      | Provider p -> Provider_mark.view p, provider_name m p
    in
    Node.button
      ~attrs:
        [ classes
            [ "model-rail-tab" ]
            [ "on", on
            ; ( "favourites"
              , match tab with
                | Favourites -> true
                | Provider _ -> false )
            ]
        ; Attr.type_ "button"
        ; Attr.role "tab"
        ; Attr.title name
        ; Attr.create "aria-label" name
        ; Attr.create "aria-selected" (Bool.to_string on)
        ; Attr.tabindex (-1)
        ; Attr.on_click (fun _ -> inject (Action.Model_menu_tab tab))
        ]
      [ glyph ]
  in
  match Model_menu.tabs offered with
  | favourites :: providers ->
    Node.div
      ~attrs:
        [ Attr.class_ "model-rail"
        ; Attr.role "tablist"
        ; Attr.create "aria-label" "Providers"
        ; Attr.create "aria-orientation" "vertical"
        ]
      (tab favourites
       :: div ~cls:"model-rail-rule" []
       :: List.map providers ~f:tab)
  | [] -> Node.none
;;

let row (m : App.Model.t) ~searching ~selected ~inject i (item : Picker.Item.t) =
  if String.equal item.id App.more_id
  then
    Node.div
      ~attrs:
        [ classes [ "model-row"; "model-more" ] [ "selected", i = selected ]
        ; Attr.role "option"
        ; Attr.id (option_id i)
        ; Attr.create "aria-selected" (Bool.to_string (i = selected))
        ; Attr.on_click (fun _ -> inject Action.Model_menu_more)
        ]
      [ icon ~cls:"model-more-icon" Chevron; span ~cls:"model-name" item.label ]
  else (
    let model = List.find m.models ~f:(fun l -> String.equal l.key item.id) in
    let starred = List.mem m.favourite_models item.id ~equal:String.equal in
    Node.div
      ~attrs:
        [ classes
            [ "model-row" ]
            [ "selected", i = selected
            ; "marked", item.marked
            ; "starred", starred
            ]
        ; Attr.role "option"
        ; Attr.id (option_id i)
        ; Attr.create "aria-selected" (Bool.to_string (i = selected))
        ; Attr.create "aria-checked" (Bool.to_string item.marked)
        ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
        ]
      [ (match model with
         | Some l -> Provider_mark.view l.provider
         | None -> Node.none)
      ; Node.span
          ~attrs:[ Attr.class_ "model-name" ]
          [ Node.text item.label
          ; (match model with
             | Some l when searching ->
               span ~cls:"model-provider" (provider_name m l.provider)
             | _ -> Node.none)
          ]
      ; (if i < 9
         then
           Node.kbd
             ~attrs:
               [ Attr.class_ "model-key"
               ; Attr.title (sprintf "Alt+%d switches to it" (i + 1))
               ]
             [ Node.textf "Alt+%d" (i + 1) ]
         else Node.none)
      ; Node.button
          ~attrs:
            [ classes [ "model-star" ] [ "on", starred ]
            ; Attr.type_ "button"
            ; Attr.tabindex (-1)
            ; Attr.title
                (if starred
                 then "Unstar: out of the favourites (Alt+F)"
                 else "Star: into the favourites (Alt+F)")
            ; Attr.create
                "aria-label"
                ((if starred then "Unstar " else "Star ") ^ item.label)
            ; Attr.create "aria-pressed" (Bool.to_string starred)
            ; Attr.on_click (fun _ ->
                Effect.Many
                  [ Effect.Stop_propagation
                  ; inject (Action.Toggle_favourite (Some item.id))
                  ])
            ]
          [ icon Star ]
      ; (if item.marked
         then Html.mark ~cls:"model-check" Check
         else Node.span ~attrs:[ Attr.class_ "model-check" ] [])
      ])
;;

let reasoning (m : App.Model.t) ~inject =
  let current_key =
    match m.pending.model with
    | Some key -> Some key
    | None -> Option.map m.state ~f:(fun s -> s.model.key)
  in
  (* The session's own model when it is the one: its levels are the ones
     [set_thinking] accepts. *)
  let model =
    Option.bind current_key ~f:(fun key ->
      match m.state with
      | Some state when String.equal state.model.key key -> Some state.model
      | _ -> List.find m.models ~f:(fun l -> String.equal l.key key))
  in
  match model, m.state with
  | Some model, Some state when model.supports_thinking ->
    let current = Option.value m.pending.thinking ~default:state.thinking in
    Node.div
      ~attrs:
        [ Attr.class_ "model-reasoning"
        ; Attr.role "group"
        ; Attr.create "aria-label" ("Reasoning for " ^ model.name)
        ]
      (div
         ~cls:"model-reasoning-head"
         [ span ~cls:"model-reasoning-title" "Reasoning"
         ; span ~cls:"model-reasoning-model" (Model_menu.short_name model.name)
         ]
       :: List.map
            (Reasoning.rows model ~current)
            ~f:(fun (r : Reasoning.Row.t) ->
              Node.button
                ~attrs:
                  [ classes [ "reasoning-row" ] [ "on", r.current ]
                  ; Attr.type_ "button"
                  ; Attr.tabindex (-1)
                  ; Attr.title
                      (sprintf
                         "Reasoning: %s (Alt+T cycles, /thinking %s)"
                         r.label
                         r.id)
                  ; Attr.create "aria-pressed" (Bool.to_string r.current)
                  ; Attr.on_click (fun _ -> inject (Action.Set_thinking r.id))
                  ]
                [ span ~cls:"reasoning-label" r.label
                ; (if r.default
                   then span ~cls:"reasoning-default" "Default"
                   else Node.none)
                ; (if r.current
                   then Html.mark ~cls:"reasoning-check" Check
                   else Node.span ~attrs:[ Attr.class_ "reasoning-check" ] [])
                ]))
  | Some model, Some _ ->
    div
      ~cls:"model-reasoning none"
      [ div
          ~cls:"model-reasoning-head"
          [ span ~cls:"model-reasoning-title" "Reasoning"
          ; span ~cls:"model-reasoning-model" (Model_menu.short_name model.name)
          ]
      ; div
          ~cls:"model-reasoning-note"
          [ Node.text "This model does not reason." ]
      ]
  | _ -> Node.none
;;

let access (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    let current =
      Option.value m.pending.mode ~default:(Session_mode.of_state state)
    in
    let open_ = m.model_menu.access_open in
    Node.div
      ~attrs:[ classes [ "model-access" ] [ "open", open_ ] ]
      [ (if open_
         then
           Node.div
             ~attrs:
               [ Attr.class_ "model-access-menu menu-surface"
               ; Attr.role "menu"
               ; Attr.create "aria-label" "When tools ask first"
               ]
             (List.map
                (Session_mode.choices
                   ~plan:(Session_engine.supports state.engine Plan_mode)
                   ~current
                   ())
                ~f:(fun mode ->
                  let on = Session_mode.equal mode current in
                  Node.button
                    ~attrs:
                      [ classes [ "model-access-item" ] [ "on", on ]
                      ; Attr.type_ "button"
                      ; Attr.role "menuitemradio"
                      ; Attr.create "aria-checked" (Bool.to_string on)
                      ; Attr.title (Session_mode.describe mode)
                      ; Attr.on_click (fun _ ->
                          Effect.Many
                            [ inject Action.Model_menu_access
                            ; inject (Action.Set_mode mode)
                            ])
                      ]
                    [ icon (if mode.chat then Message else Shield)
                    ; Node.span
                        ~attrs:[ Attr.class_ "menu-text" ]
                        [ span ~cls:"menu-label" (access_label mode)
                        ; span ~cls:"menu-detail" (Session_mode.title mode)
                        ]
                    ; (if on
                       then Html.mark ~cls:"model-check" Check
                       else Node.span ~attrs:[ Attr.class_ "model-check" ] [])
                    ]))
         else Node.none)
      ; Node.button
          ~attrs:
            [ classes
                [ "model-access-button" ]
                [ ( "never-asks"
                  , (not (current.plan || current.chat))
                    && Approval_mode.equal current.approval Auto )
                ]
            ; Attr.type_ "button"
            ; Attr.title
                (sprintf
                   "%s (Alt+M cycles, /mode picks)"
                   (String.capitalize (Session_mode.describe current)))
            ; Attr.create "aria-haspopup" "menu"
            ; Attr.create "aria-expanded" (Bool.to_string open_)
            ; Attr.on_click (fun _ -> inject Action.Model_menu_access)
            ]
          [ icon (if current.chat then Message else Shield)
          ; span ~cls:"chip-label" (access_label current)
          ; icon ~cls:"chevron" Chevron
          ]
      ]
;;

let view (m : App.Model.t) (p : Picker.t) ~inject =
  let offered = App.offered_models m in
  let visible = Picker.visible p in
  let selected = Picker.selected p in
  let query = Picker.query p in
  let active =
    if List.is_empty visible
    then Attr.empty
    else Attr.create "aria-activedescendant" (option_id selected)
  in
  let empty =
    match visible, m.model_menu.tab with
    | [], _ when not (String.is_empty query) ->
      Some (sprintf "Nothing matches “%s”: Backspace widens the search." query)
    | [], Favourites ->
      Some "No favourites yet: star a model (its ☆, or Alt+F) to keep it here."
    | [], Provider _ -> Some "No models here."
    | _ :: _, _ -> None
  in
  let heading =
    if not (String.is_empty query)
    then "All providers"
    else (
      match m.model_menu.tab with
      | Favourites -> "Favourites"
      | Provider p -> provider_name m p)
  in
  Node.div
    ~attrs:
      [ Attr.class_
          "loon chip-popover menu-surface model-menu bg-popover \
           text-popover-foreground rounded-lg shadow-md ring-1 \
           ring-foreground/10"
      ; Attr.create "data-slot" "popover-content"
      ; Attr.id "chip-popover"
      ; Attr.create "data-chip" "model"
      ; Attr.role "dialog"
      ; Attr.create "aria-label" "Model"
      ]
    [ div
        ~cls:"model-menu-body"
        [ rail m ~offered ~inject
        ; div
            ~cls:"model-menu-main"
            [ Node.div
                ~attrs:
                  [ Attr.class_ "popover-search"
                  ; Attr.create "data-slot" "command-input-wrapper"
                  ]
                [ icon Search
                ; Node.input
                    ~attrs:
                      [ Attr.id "popover-filter"
                      ; Attr.type_ "text"
                      ; Attr.create "aria-label" "Search models"
                      ; Attr.role "combobox"
                      ; Attr.create "aria-controls" "popover-list"
                      ; Attr.create "aria-expanded" "true"
                      ; Attr.create "aria-autocomplete" "list"
                      ; active
                      ; Attr.placeholder "Search models..."
                      ; Attr.value query
                      ; Attr.create "autocomplete" "off"
                      ; Attr.create "spellcheck" "false"
                      ; Attr.on_input (fun _ text ->
                          inject (Action.Picker_query text))
                      ]
                    ()
                ]
            ; div ~cls:"model-list-head" [ Node.text heading ]
            ; (match empty with
               | Some text -> div ~cls:"popover-empty" [ Node.text text ]
               | None ->
                 Node.div
                   ~key:
                     (query
                      ^ "|"
                      ^ Sexp.to_string (Model_menu.sexp_of_t m.model_menu))
                   ~attrs:
                     [ Attr.class_ "popover-items model-list"
                     ; Attr.create "data-slot" "command-list"
                     ; Attr.id "popover-list"
                     ; Attr.role "listbox"
                     ; Attr.create "aria-label" heading
                     ]
                   (List.mapi
                      visible
                      ~f:
                        (row
                           m
                           ~searching:(not (String.is_empty query))
                           ~selected
                           ~inject)))
            ]
        ; reasoning m ~inject
        ]
    ; div
        ~cls:"model-menu-foot"
        [ span
            ~cls:"menu-keys"
            "↑ ↓ models · ← → providers · Enter switches · Esc closes"
        ; access m ~inject
        ]
    ]
;;
