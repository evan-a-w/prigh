open! Core
open! Import
open Html
module Action = App.Action

let title : Chip.t -> string = function
  | Model -> "Model"
  | Host -> "Where tools run"
  | Attach -> "Add to your message"
;;

let shortcut : Chip.t -> string = function
  | Model -> "Ctrl+L"
  | Host -> "/host"
  | Attach -> "or paste, or drop"
;;

let option_id i = sprintf "popover-option-%d" i

let row ~chip ~selected ~inject i (item : Picker.Item.t) =
  Node.div
    ~attrs:
      [ classes
          [ "menu-item"; "popover-item" ]
          [ "selected", i = selected
          ; "marked", item.marked
          ; "dimmed", item.dimmed
          ]
      ; Attr.role "option"
      ; Attr.id (option_id i)
      ; Attr.create "aria-selected" (Bool.to_string (i = selected))
      ; Attr.create "aria-checked" (Bool.to_string item.marked)
      ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
      ]
    [ (if Chip.equal chip Attach
       then
         Html.mark
           ~cls:"popover-check attach-icon"
           (if String.equal item.id "image" then Image else Paperclip)
       else if item.marked
       then Html.mark ~cls:"picked popover-check" Check
       else Node.span ~attrs:[ Attr.class_ "mark popover-check" ] [])
    ; Node.span
        ~attrs:[ Attr.class_ "menu-text" ]
        [ span ~cls:"menu-label" item.label
        ; (if String.is_empty item.detail
           then Node.none
           else span ~cls:"menu-detail" item.detail)
        ]
    ]
;;

let rows ~chip (p : Picker.t) ~inject =
  List.mapi
    (Picker.visible p)
    ~f:(row ~chip ~selected:(Picker.selected p) ~inject)
;;

(* With nowhere else for tools to run, how to add somewhere. *)
let host_note (m : App.Model.t) =
  match m.state with
  | Some state when List.length state.hosts <= 1 ->
    Some
      (div
         ~cls:"popover-hint"
         [ Node.text
             (if List.is_empty state.hosts
              then "No tool host is connected: "
              else "The only host: ")
         ; Node.text "run "
         ; Node.code [ Node.text "prigh tool-host -connect SERVER" ]
         ; Node.text
             " on a machine, or a prigh TUI connected to this backend, to add \
              one."
         ])
  | _ -> None
;;

(* The transcript's verbosity as a row of choices, and the usage: on a
   phone, in the ⋯ sheet. *)
let session_sections (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    let section name ~glyph ?(note = "") body =
      Node.div
        ~attrs:
          [ Attr.class_ "session-section"
          ; Attr.role "group"
          ; Attr.create "aria-label" name
          ]
        [ div
            ~cls:"session-section-head"
            [ icon glyph
            ; span ~cls:"session-section-title" name
            ; span ~cls:"session-section-note" note
            ]
        ; body
        ]
    in
    let choice ~current ~label ~title ~on_click =
      Node.button
        ~attrs:
          [ classes [ "session-choice" ] [ "on", current ]
          ; Attr.type_ "button"
          ; Attr.title title
          ; Attr.create "aria-pressed" (Bool.to_string current)
          ; Attr.on_click (fun _ -> on_click)
          ]
        [ Node.text label ]
    in
    let verbosity =
      section
        "Transcript"
        ~glyph:Lines
        (div
           ~cls:"session-choices"
           (List.map Prigh_ui.Verbosity.all ~f:(fun v ->
              let name = Prigh_ui.Verbosity.name v in
              choice
                ~current:(Prigh_ui.Verbosity.equal v m.verbosity)
                ~label:name
                ~title:(sprintf "Transcript: %s (/verbosity %s)" name name)
                ~on_click:(inject (Action.Run ("/verbosity " ^ name))))))
    in
    let usage =
      section
        "Usage"
        ~glyph:Ring
        (div
           ~cls:"session-usage"
           [ (match Status_view.context_percent state with
              | Some percent ->
                Node.span
                  ~attrs:[ Attr.class_ "session-context" ]
                  [ Status_view.meter percent
                  ; span ~cls:"percent" (sprintf "%.0f%%" percent)
                  ; span
                      ~cls:"session-of"
                      (sprintf
                         "%s / %s"
                         (Status_view.tokens state.context_tokens)
                         (Status_view.tokens state.model.context_window))
                  ]
              | None -> Node.none)
           ; Status_view.cost state ~inject
           ])
    in
    div ~cls:"session-sections" [ verbosity; usage ]
;;

let other (m : App.Model.t) ~(chip : Chip.t) (p : Picker.t) ~inject =
  let visible = Picker.visible p in
  let active =
    if List.is_empty visible
    then Attr.empty
    else Attr.create "aria-activedescendant" (option_id (Picker.selected p))
  in
  let list =
    match visible with
    | [] when String.is_empty (Picker.query p) -> Node.none
    | [] ->
      div
        ~cls:"popover-empty"
        [ Node.textf
            "Nothing matches “%s”: Backspace widens the search."
            (Picker.query p)
        ]
    | _ ->
      (* A new list for each filter, scrolled to its top. *)
      Node.div
        ~key:(Picker.query p)
        ~attrs:
          ([ Attr.class_ "popover-items"
           ; Attr.id "popover-list"
           ; Attr.role "listbox"
           ; Attr.create "aria-label" (title chip)
           ]
           @
           match chip with
           | Model | Host -> []
           | Attach -> [ Attr.tabindex 0; active ])
        (rows ~chip p ~inject)
  in
  Node.div
    ~attrs:
      [ Attr.class_ "chip-popover menu-surface"
      ; Attr.id "chip-popover"
      ; Attr.create
          "data-chip"
          (match chip with
           | Model -> "model"
           | Host -> "host"
           | Attach -> "attach")
      ; Attr.role "dialog"
      ; Attr.create "aria-label" (title chip)
      ]
    ([ (match chip with
        | Model | Host ->
          let what =
            match chip with
            | Host -> "hosts"
            | Model | Attach -> "models"
          in
          div
            ~cls:"popover-search"
            [ icon Search
            ; Node.input
                ~attrs:
                  [ Attr.id "popover-filter"
                  ; Attr.type_ "text"
                  ; Attr.create "aria-label" ("Filter " ^ what)
                  ; Attr.role "combobox"
                  ; Attr.create "aria-controls" "popover-list"
                  ; Attr.create "aria-expanded" "true"
                  ; Attr.create "aria-autocomplete" "list"
                  ; active
                  ; Attr.placeholder (sprintf "Filter %s…" what)
                  ; Attr.value (Picker.query p)
                  ; Attr.create "autocomplete" "off"
                  ; Attr.create "spellcheck" "false"
                  ; Attr.on_input (fun _ text ->
                      inject (Action.Picker_query text))
                  ]
                ()
            ; span ~cls:"popover-shortcut" (shortcut chip)
            ]
        | Attach ->
          div
            ~cls:"popover-head"
            [ span ~cls:"popover-title" (title chip)
            ; span ~cls:"popover-shortcut" (shortcut chip)
            ])
     ; list
     ]
     @ Option.to_list
         (match chip with
          | Host -> host_note m
          | Model | Attach -> None)
     @ [ div
           ~cls:"menu-keys"
           [ Node.text
               (match chip with
                | Attach -> "Enter chooses, Esc closes"
                | Model | Host ->
                  "Enter chooses, ← → the next setting, Esc closes")
           ]
       ])
;;

let view (m : App.Model.t) ~(chip : Chip.t) (p : Picker.t) ~inject =
  match chip with
  | Model -> Model_menu_view.view m p ~inject
  | Host | Attach -> other m ~chip p ~inject
;;

(* Over the page, under the composer's chips: a click elsewhere closes the
   menu, one on another chip opens that one's. *)
let backdrop ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "menu-backdrop popover-backdrop"
      ; Attr.on_click (fun _ -> inject Action.Close_dialog)
      ]
    []
;;
