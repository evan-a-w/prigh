open! Core
open! Import
open Html
module Action = App.Action

let title : Chip.t -> string = function
  | Model -> "Model"
  | Thinking -> "Thinking"
  | Mode -> "When tools ask first"
  | Host -> "Where tools run"
;;

let shortcut : Chip.t -> string = function
  | Model -> "Ctrl+L"
  | Thinking -> "Alt+T cycles"
  | Mode -> "Alt+M cycles"
  | Host -> "/host"
;;

let option_id i = sprintf "popover-option-%d" i

(* How hard the model thinks at each level: one bar per step above off. *)
let meter level =
  let steps = List.length App.thinking_levels - 1 in
  let on =
    List.findi App.thinking_levels ~f:(fun _ l -> String.equal l level)
    |> Option.value_map ~default:0 ~f:fst
  in
  Node.span
    ~attrs:[ Attr.class_ "thinking-meter"; Attr.create "aria-hidden" "true" ]
    (List.init steps ~f:(fun i ->
       Node.span ~attrs:[ classes [ "bar" ] [ "on", i < on ] ] []))
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
    | Thinking | Mode | Host -> None
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
    [ (if item.marked
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
       | Thinking, _ -> meter item.id
       | (Model | Mode | Host), _ -> Node.none)
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
           | Thinking | Mode -> [ Attr.tabindex 0; active ])
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
           | Host -> "host")
      ; Attr.role "dialog"
      ; Attr.create "aria-label" (title chip)
      ]
    ([ (match chip with
        | Model | Host ->
          let what =
            match chip with
            | Host -> "hosts"
            | Model | Thinking | Mode -> "models"
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
        | Thinking | Mode ->
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
          | Model | Thinking | Mode -> None)
     @ [ div
           ~cls:"menu-keys"
           [ Node.text "Enter chooses, ← → the next setting, Esc closes" ]
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
