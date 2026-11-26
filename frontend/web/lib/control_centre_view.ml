open! Core
open! Import
open Html
module Action = App.Action

let marked ~cls text ranges =
  Node.span
    ~attrs:[ Attr.class_ cls; Attr.create "dir" "auto" ]
    (List.map (Prigh_ui.Session_search.pieces text ranges) ~f:(function
       | `Plain, s -> Node.text s
       | `Match, s ->
         Node.create
           "mark"
           ~attrs:[ Attr.class_ "bg-transparent font-semibold text-primary" ]
           [ Node.text s ]))
;;

let label_cls = "control-label min-w-0 max-w-full shrink-0 truncate"
let faint = "min-w-0 flex-1 truncate text-xs text-muted-foreground"
let snippet_cls = "control-snippet narrow:basis-full " ^ faint

let detail_cls =
  "control-detail max-w-[45%] shrink truncate text-xs text-muted-foreground"
;;

let option_id i = sprintf "control-option-%d" i

let row_attrs ~inject ~selected ~marked i =
  [ classes
      [ "control-row group/command-item relative flex min-h-8 min-w-0 \
         cursor-default items-center gap-3 rounded-lg px-2 py-1.5 text-sm \
         outline-hidden select-none hover:bg-muted \
         data-[selected=true]:bg-muted narrow:min-h-11"
      ]
      [ "selected", selected; "marked", marked ]
  ; Attr.create "data-slot" "command-item"
  ; Attr.create "data-selected" (Bool.to_string selected)
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
      [ classes
          [ "control-dot size-1.5 shrink-0 self-center rounded-full" ]
          [ "on bg-primary", marked; "border border-border", not marked ]
      ; Attr.create "aria-hidden" "true"
      ]
    []
;;

let root_rows ~inject (root : Control_centre.t) rows =
  let row i (r : Control_centre.Row.t) =
    Node.div
      ~attrs:
        (row_attrs ~inject ~selected:(i = root.selected) ~marked:r.marked i)
      [ div
          ~cls:
            "control-main flex min-w-0 flex-1 items-baseline gap-2.5 \
             narrow:flex-wrap narrow:gap-y-0.5 narrow:py-1"
          [ (match r.target with
             | Value _ -> dot r.marked
             | Setting _ | Session _ | Command _ -> Node.none)
          ; marked ~cls:label_cls r.label r.matched
          ; (match r.snippet with
             | Some s -> marked ~cls:snippet_cls s.text s.ranges
             | None -> Node.none)
          ]
      ; (if String.is_empty r.detail
         then Node.none
         else span ~cls:detail_cls r.detail)
      ; (match r.keys with
         | Some k ->
           Loon.Kbd.view ~class_:"control-keys narrow:hidden" [ Node.text k ]
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
           ~cls:
             "control-group px-2 pt-2.5 pb-1.5 text-xs font-medium \
              text-muted-foreground"
           ~attrs:
             [ Attr.role "presentation"
             ; Attr.create "data-slot" "command-group"
             ]
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
         @ [ (if item.dimmed
              then Attr.class_ "dimmed opacity-50"
              else Attr.empty)
           ])
      [ div
          ~cls:"control-main flex min-w-0 flex-1 items-baseline gap-2.5"
          [ dot item.marked
          ; span ~cls:label_cls item.label
          ; (if String.is_empty item.detail
             then Node.none
             else span ~cls:("control-about " ^ faint) item.detail)
          ]
      ; (if item.marked then span ~cls:detail_cls "current" else Node.none)
      ])
;;

let hint key what =
  Node.span
    ~attrs:[ Attr.class_ "control-hint whitespace-nowrap" ]
    [ Loon.Kbd.view [ Node.text key ]; Node.text (" " ^ what) ]
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
      [ Attr.class_
          "modal-backdrop control-backdrop pt-[12vh] narrow:items-stretch \
           narrow:p-0"
      ; Attr.create "data-slot" "dialog-overlay"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_
              "loon control-centre flex max-h-[72vh] w-[min(640px,100%)] flex-col \
               overflow-hidden rounded-xl bg-popover text-popover-foreground \
               shadow-lg ring-1 ring-foreground/10 outline-none animate-in \
               fade-in-0 zoom-in-95 narrow:h-(--vvh,100dvh) narrow:max-h-none \
               narrow:w-full narrow:rounded-none narrow:pt-(--safe-t) \
               narrow:pb-(--safe-b) narrow:ring-0 narrow:zoom-in-100 \
               narrow:slide-in-from-bottom-4"
          ; Attr.create "data-slot" "command"
          ; Attr.id "control-centre"
          ; Attr.role "dialog"
          ; Attr.create "aria-modal" "true"
          ; Attr.create "aria-label" "Control centre"
          ; Attr.tabindex (-1)
          ]
        [ div
            ~cls:
              "control-input-row flex min-h-12 items-center gap-2 border-b \
               border-border pr-1.5 pl-3.5 narrow:min-h-(--strip-h,52px) \
               narrow:pl-[max(14px,var(--safe-l))] \
               narrow:pr-[max(4px,var(--safe-r))] [&>.icon]:size-4 \
               [&>.icon]:shrink-0 [&>.icon]:opacity-50"
            ~attrs:[ Attr.create "data-slot" "command-input-wrapper" ]
            [ icon Search
            ; (match options with
               | Some o ->
                 button
                   ~cls:
                     "control-crumb max-w-[45%] shrink-0 truncate narrow:h-11"
                   ~variant:Secondary
                   ~size:Xs
                   ~title:"Back (⌫ in an empty filter)"
                   ~on_click:(inject Action.Control_back)
                   [ Node.text (Control_setting.short o.setting) ]
               | None -> Node.none)
            ; Node.input
                ~attrs:
                  [ Attr.id "control-input"
                  ; Attr.class_
                      "min-w-0 flex-1 bg-transparent py-3 text-base \
                       outline-none placeholder:text-muted-foreground \
                       md:text-sm narrow:text-base"
                  ; Attr.create "data-slot" "command-input"
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
                  ; Attr.on_input (fun _ text ->
                      inject (Action.Control_query text))
                  ]
                ()
            ; button
                ~cls:"control-close shrink-0 narrow:size-11"
                ~variant:Ghost
                ~size:Icon_sm
                ~title:"Close (Esc)"
                ~label:"Close"
                ~on_click:close
                [ icon Close ]
            ]
        ; (if count = 0
           then
             div
               ~cls:
                 "control-empty px-4 py-6 text-center text-sm \
                  text-muted-foreground"
               ~attrs:[ Attr.create "data-slot" "command-empty" ]
               [ Node.textf
                   "Nothing matches “%s”: Backspace widens the search."
                   (String.strip query)
               ]
           else
             div
               ~cls:
                 "control-list min-h-0 flex-1 overflow-y-auto \
                  overscroll-contain p-1 narrow:pl-[max(4px,var(--safe-l))] \
                  narrow:pr-[max(4px,var(--safe-r))]"
               ~attrs:
                 [ Attr.create "data-slot" "command-list"
                 ; Attr.role "listbox"
                 ; Attr.id "control-list"
                 ; Attr.create "aria-label" "Results"
                 ]
               body)
        ; Node.footer
            ~attrs:
              [ Attr.class_
                  "control-foot flex gap-4 border-t border-border bg-muted/50 \
                   px-3.5 py-2 text-xs text-muted-foreground narrow:hidden"
              ]
            [ hint "↑↓" "move"
            ; hint "↵" (if Option.is_some options then "set" else "open")
            ; hint "⌫" "back"
            ; hint "esc" "close"
            ]
        ]
    ]
;;
