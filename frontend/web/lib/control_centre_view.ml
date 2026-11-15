open! Core
open! Import
open Html
module Action = App.Action

let marked ~cls text ranges =
  Node.span
    ~attrs:[ Attr.class_ cls; Attr.create "dir" "auto" ]
    (List.map (Prigh_ui.Session_search.pieces text ranges) ~f:(function
       | `Plain, s -> Node.text s
       | `Match, s -> Node.create "mark" [ Node.text s ]))
;;

let option_id i = sprintf "control-option-%d" i

let row_attrs ~inject ~selected ~marked i =
  [ classes [ "control-row" ] [ "selected", selected; "marked", marked ]
  ; Attr.role "option"
  ; Attr.id (option_id i)
  ; Attr.create "aria-selected" (Bool.to_string selected)
  ; (if marked then Attr.create "aria-checked" "true" else Attr.empty)
  ; Attr.on_click (fun _ -> inject (Action.Control_choose i))
  ]
;;

let dot marked =
  Node.span
    ~attrs:
      [ classes [ "control-dot" ] [ "on", marked ]
      ; Attr.create "aria-hidden" "true"
      ]
    []
;;

let root_rows ~inject (root : Control_centre.t) rows =
  let row i (r : Control_centre.Row.t) =
    Node.div
      ~attrs:(row_attrs ~inject ~selected:(i = root.selected) ~marked:r.marked i)
      [ div
          ~cls:"control-main"
          [ (match r.target with
             | Value _ -> dot r.marked
             | Setting _ | Session _ | Command _ -> Node.none)
          ; marked ~cls:"control-label" r.label r.matched
          ; (match r.snippet with
             | Some s -> marked ~cls:"control-snippet" s.text s.ranges
             | None -> Node.none)
          ]
      ; (if String.is_empty r.detail
         then Node.none
         else span ~cls:"control-detail" r.detail)
      ; (match r.keys with
         | Some k -> Node.create "kbd" ~attrs:[ Attr.class_ "control-keys" ] [ Node.text k ]
         | None -> Node.none)
      ]
  in
  (* A heading before each group's first row. *)
  List.concat_mapi rows ~f:(fun i (r : Control_centre.Row.t) ->
    let first =
      i = 0
      ||
      match List.nth rows (i - 1) with
      | Some prev -> not (Control_centre.Group.equal prev.group r.group)
      | None -> true
    in
    (if first
     then
       [ div
           ~cls:"control-group"
           ~attrs:[ Attr.role "presentation" ]
           [ Node.text (Control_centre.Group.title r.group) ]
       ]
     else [])
    @ [ row i r ])
;;

let option_rows ~inject (o : Dialog.Control_options.t) =
  let selected = Picker.selected o.picker in
  List.mapi (Picker.visible o.picker) ~f:(fun i (item : Picker.Item.t) ->
    Node.div
      ~attrs:
        (row_attrs ~inject ~selected:(i = selected) ~marked:item.marked i
         @ [ (if item.dimmed then Attr.class_ "dimmed" else Attr.empty) ])
      [ div
          ~cls:"control-main"
          [ dot item.marked
          ; span ~cls:"control-label" item.label
          ; (if String.is_empty item.detail
             then Node.none
             else span ~cls:"control-about" item.detail)
          ]
      ; (if item.marked then span ~cls:"control-detail" "current" else Node.none)
      ])
;;

let hint key what =
  Node.span
    ~attrs:[ Attr.class_ "control-hint" ]
    [ Node.create "kbd" [ Node.text key ]; Node.text (" " ^ what) ]
;;

let view (m : App.Model.t) ~(root : Control_centre.t) ~options ~inject =
  let close = inject Action.Close_dialog in
  let query, selected, body, count =
    match (options : Dialog.Control_options.t option) with
    | Some o ->
      let rows = option_rows ~inject o in
      Picker.query o.picker, Picker.selected o.picker, rows, List.length rows
    | None ->
      let rows = App.control_rows m root in
      root.query, root.selected, root_rows ~inject root rows, List.length rows
  in
  Node.div
    ~attrs:
      [ Attr.class_ "modal-backdrop control-backdrop"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ "control-centre"
          ; Attr.id "control-centre"
          ; Attr.role "dialog"
          ; Attr.create "aria-modal" "true"
          ; Attr.create "aria-label" "Control centre"
          ; Attr.tabindex (-1)
          ]
        [ div
            ~cls:"control-input-row"
            [ icon Search
            ; (match options with
               | Some o ->
                 button
                   ~cls:"control-crumb"
                   ~title:"Back (⌫ in an empty filter)"
                   ~on_click:(inject Action.Control_back)
                   [ Node.text (Control_setting.short o.setting) ]
               | None -> Node.none)
            ; Node.input
                ~attrs:
                  [ Attr.id "control-input"
                  ; Attr.type_ "text"
                  ; Attr.create
                      "aria-label"
                      "Search sessions, settings and commands"
                  ; Attr.role "combobox"
                  ; Attr.create "aria-controls" "control-list"
                  ; Attr.create "aria-expanded" "true"
                  ; Attr.create "aria-autocomplete" "list"
                  ; (if count = 0
                     then Attr.empty
                     else
                       Attr.create "aria-activedescendant" (option_id selected))
                  ; Attr.placeholder
                      (match options with
                       | Some o ->
                         sprintf
                           "Filter %s…"
                           (String.lowercase (Control_setting.short o.setting))
                       | None -> "Search sessions, settings and commands…")
                  ; Attr.value query
                  ; Attr.create "autocomplete" "off"
                  ; Attr.create "spellcheck" "false"
                  ; Attr.on_input (fun _ text -> inject (Action.Control_query text))
                  ]
                ()
            ; button
                ~cls:"icon ghost control-close"
                ~title:"Close (Esc)"
                ~label:"Close"
                ~on_click:close
                [ icon Close ]
            ]
        ; (if count = 0
           then
             div
               ~cls:"control-empty"
               [ Node.textf
                   "Nothing matches “%s”: Backspace widens the search."
                   (String.strip query)
               ]
           else
             div
               ~cls:"control-list"
               ~attrs:
                 [ Attr.role "listbox"
                 ; Attr.id "control-list"
                 ; Attr.create "aria-label" "Results"
                 ]
               body)
        ; Node.footer
            ~attrs:[ Attr.class_ "control-foot" ]
            [ hint "↑↓" "move"
            ; hint "↵" (if Option.is_some options then "set" else "open")
            ; hint "⌫" "back"
            ; hint "esc" "close"
            ]
        ]
    ]
;;
