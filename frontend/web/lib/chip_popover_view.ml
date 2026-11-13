open! Core
open! Import
open Html
module Action = App.Action

let title : Chip.t -> string = function
  | Model -> "Model"
  | Thinking -> "Thinking"
  | Mode -> "When tools ask first"
  | Verbosity -> "Transcript"
  | Host -> "Where tools run"
  | Attach -> "Add to your message"
;;

let shortcut : Chip.t -> string = function
  | Model -> "Ctrl+L"
  | Thinking -> "Alt+T cycles"
  | Mode -> "Alt+M cycles"
  | Verbosity -> "Ctrl+O cycles"
  | Host -> "/host"
  | Attach -> "or paste, or drop"
;;

let option_id i = sprintf "popover-option-%d" i

(* How hard the model thinks at each setting: a bar per effort level it
   takes, as many lit as the setting's (the provider's default lights its
   level when the model says which, else none). *)
let meter (m : App.Model.t) level =
  let llm = Option.map m.state ~f:(fun s -> s.model) in
  let levels =
    List.filter (App.thinking_options m) ~f:(fun l ->
      not (List.mem [ "off"; "default" ] l ~equal:String.equal))
  in
  let level =
    match
      Thinking.normalize level, Option.bind llm ~f:(fun l -> l.thinking_default)
    with
    | "default", Some known -> Thinking.normalize known
    | level, _ -> level
  in
  let on =
    List.findi levels ~f:(fun _ l -> String.equal l level)
    |> Option.value_map ~default:0 ~f:(fun (i, _) -> i + 1)
  in
  let steps = List.length levels in
  Node.span
    ~attrs:[ Attr.class_ "thinking-meter"; Attr.create "aria-hidden" "true" ]
    (List.init steps ~f:(fun i ->
       Node.span
         ~attrs:
           [ classes [ "bar" ] [ "on", i < on ]
           ; Attr.style
               (Css_gen.height
                  (`Px_float
                      (4.
                       +. (8.
                           *. Float.of_int i
                           /. Float.of_int (max 1 (steps - 1))))))
           ]
         []))
;;

let provider_name (m : App.Model.t) id =
  List.find m.auth ~f:(fun (s : Auth_status.t) -> String.equal s.provider id)
  |> Option.value_map ~default:id ~f:(fun s -> s.name)
;;

let row
      (m : App.Model.t)
      ~chip
      ~filtering
      ~selected
      ~inject
      i
      (item : Picker.Item.t)
  =
  let model =
    match (chip : Chip.t) with
    | Model -> List.find m.models ~f:(fun l -> String.equal l.key item.id)
    | Thinking | Mode | Verbosity | Host | Attach -> None
  in
  let detail =
    match model with
    | Some l ->
      if filtering
      then
        String.concat
          ~sep:", "
          (List.filter_opt
             [ Some (provider_name m l.provider)
             ; Option.some_if item.dimmed "not logged in"
             ])
      else ""
    | None -> item.detail
  in
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
        ; (if String.is_empty detail
           then Node.none
           else span ~cls:"menu-detail" detail)
        ]
    ; (match chip, model with
       | Model, Some l ->
         span
           ~cls:"popover-aside"
           ~attrs:[ Attr.title "Context window, in tokens" ]
           (App.compact_tokens l.context_window)
       | Thinking, _ -> meter m item.id
       | (Model | Mode | Verbosity | Host | Attach), _ -> Node.none)
    ]
;;

(* Each provider's models under its name, unless filtered (best first). *)
let rows (m : App.Model.t) ~chip (p : Picker.t) ~inject =
  let filtering = not (String.is_empty (Picker.query p)) in
  let selected = Picker.selected p in
  let row = row m ~chip ~filtering ~selected ~inject in
  let provider (item : Picker.Item.t) =
    List.find m.models ~f:(fun l -> String.equal l.key item.id)
    |> Option.map ~f:(fun l -> l.provider)
  in
  match (chip : Chip.t) with
  | Model when not filtering ->
    List.mapi (Picker.visible p) ~f:(fun i item -> i, item)
    |> List.group ~break:(fun (_, a) (_, b) ->
      not (Option.equal String.equal (provider a) (provider b)))
    |> List.map ~f:(fun group ->
      let name =
        match
          Option.bind (List.hd group) ~f:(fun (_, item) -> provider item)
        with
        | None -> "Other"
        | Some id -> provider_name m id
      in
      Node.div
        ~attrs:
          [ Attr.class_ "popover-group"
          ; Attr.role "group"
          ; Attr.create "aria-label" name
          ]
        (div
           ~cls:"menu-heading"
           [ Node.text name
           ; (if
                List.for_all group ~f:(fun (_, (item : Picker.Item.t)) ->
                  item.dimmed)
              then span ~cls:"popover-note" "not logged in: /login"
              else Node.none)
           ]
         :: List.map group ~f:(fun (i, item) -> row i item)))
  | _ -> List.mapi (Picker.visible p) ~f:row
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

(* The session's settings as rows of choices, each leaving the rest open:
   the thinking level, the mode, the transcript's verbosity, and the usage.
   On a phone they are in the ⋯ sheet (the composer's row has no room for
   their chips). *)
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
    let thinking =
      if not state.model.supports_thinking
      then Node.none
      else (
        let level =
          Thinking.normalize
            (Option.value m.pending.thinking ~default:state.thinking)
        in
        section
          "Thinking"
          ~glyph:Brain
          ~note:(Thinking.label level)
          (div
             ~cls:"session-choices thinking-choices"
             (List.map (App.thinking_options m) ~f:(fun l ->
                choice
                  ~current:(String.equal l level)
                  ~label:l
                  ~title:
                    (sprintf "Thinking: %s (/thinking %s)" (Thinking.label l) l)
                  ~on_click:(inject (Action.Set_thinking l))))))
    in
    let mode =
      let current =
        Option.value m.pending.mode ~default:(Session_mode.of_state state)
      in
      section
        "Mode"
        ~glyph:Shield
        ~note:(Session_mode.describe current)
        (div
           ~cls:"session-choices"
           (List.map (Session_mode.choices ~current) ~f:(fun mode ->
              choice
                ~current:(Session_mode.equal mode current)
                ~label:(Session_mode.label mode)
                ~title:(Session_mode.describe mode)
                ~on_click:(inject (Action.Set_mode mode)))))
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
    div ~cls:"session-sections" [ thinking; mode; verbosity; usage ]
;;

let view (m : App.Model.t) ~(chip : Chip.t) (p : Picker.t) ~inject =
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
           | Thinking | Mode | Verbosity | Attach -> [ Attr.tabindex 0; active ]
          )
        (rows m ~chip p ~inject)
  in
  Node.div
    ~attrs:
      [ Attr.class_ "chip-popover menu-surface"
      ; Attr.id "chip-popover"
      ; Attr.create
          "data-chip"
          (match chip with
           | Model -> "model"
           | Thinking -> "thinking"
           | Mode -> "mode"
           | Verbosity -> "verbosity"
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
            | Model | Thinking | Mode | Verbosity | Attach -> "models"
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
        | Thinking | Mode | Verbosity | Attach ->
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
          | Model | Thinking | Mode | Verbosity | Attach -> None)
     @ [ div
           ~cls:"menu-keys"
           [ Node.text
               (match chip with
                | Attach -> "Enter chooses, Esc closes"
                | Model | Thinking | Mode | Verbosity | Host ->
                  "Enter chooses, ← → the next setting, Esc closes")
           ]
       ])
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
