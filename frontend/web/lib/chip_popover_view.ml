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

let head_cls =
  "flex shrink-0 items-center gap-2 border-b border-border px-3 pt-2 pb-[7px]"
;;

let search_cls =
  "popover-search "
  ^ head_cls
  ^ " [&>.icon]:size-3.5 [&>.icon]:shrink-0 [&>.icon]:opacity-50"
;;

let shortcut_cls =
  "popover-shortcut text-xs whitespace-nowrap text-muted-foreground \
   pointer-coarse:hidden"
;;

let row ~chip ~selected ~inject i (item : Picker.Item.t) =
  Node.div
    ~attrs:
      [ classes
          [ "menu-item popover-item"
          ; Menu_view.item_look
          ; "gap-2 px-2 py-1.5 max-[760px]:py-2.5"
          ]
          [ "selected", i = selected
          ; "marked", item.marked
          ; "dimmed [&_.menu-text]:opacity-55", item.dimmed
          ]
      ; Attr.create "data-slot" "command-item"
      ; (if i = selected then Attr.create "data-highlighted" "" else Attr.empty)
      ; Attr.role "option"
      ; Attr.id (option_id i)
      ; Attr.create "aria-selected" (Bool.to_string (i = selected))
      ; Attr.create "aria-checked" (Bool.to_string item.marked)
      ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
      ]
    [ (if Chip.equal chip Attach
       then
         Html.mark
           ~cls:"popover-check attach-icon w-4 [--mark-size:14px]"
           (if String.equal item.id "image" then Image else Paperclip)
       else if item.marked
       then Html.mark ~cls:"picked popover-check w-4 [--mark-size:14px]" Check
       else
         Node.span
           ~attrs:[ Attr.class_ "mark popover-check w-4 [--mark-size:14px]" ]
           [])
    ; Node.span
        ~attrs:
          [ Attr.class_ "menu-text flex min-w-0 flex-1 flex-col leading-tight" ]
        [ span ~cls:(Menu_view.label_look ^ " text-[13.5px]") item.label
        ; (if String.is_empty item.detail
           then Node.none
           else
             span
               ~cls:"menu-detail truncate text-xs text-muted-foreground"
               item.detail)
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
         ~cls:
           "popover-hint shrink-0 border-t border-border px-3 py-2 text-xs \
            leading-normal text-muted-foreground [&_code]:font-mono \
            [&_code]:text-foreground"
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
          [ Attr.class_ "session-section flex flex-col gap-1.5 px-2 pt-1.5 pb-1"
          ; Attr.role "group"
          ; Attr.create "aria-label" name
          ]
        [ div
            ~cls:
              "session-section-head flex min-w-0 items-center gap-2 \
               text-[13px] text-muted-foreground [&>.icon]:size-4 \
               [&>.icon]:flex-none"
            [ icon glyph
            ; span
                ~cls:"session-section-title font-semibold text-foreground"
                name
            ; span
                ~cls:"session-section-note ml-auto min-w-0 truncate text-xs"
                note
            ]
        ; body
        ]
    in
    let choice ~current ~label ~title ~on_click =
      Node.button
        ~attrs:
          [ classes
              [ "session-choice min-h-11 min-w-0 flex-1 basis-0 truncate \
                 rounded-md px-1 text-sm font-medium text-muted-foreground \
                 hover:text-foreground focus-visible:outline-2 \
                 focus-visible:-outline-offset-2 focus-visible:outline-ring \
                 data-[state=on]:bg-background data-[state=on]:text-foreground \
                 data-[state=on]:shadow-sm max-[359px]:min-w-11 \
                 max-[359px]:flex-auto max-[359px]:px-0.5 \
                 max-[359px]:text-[13px]"
              ]
              [ "on", current ]
          ; Attr.create "data-slot" "toggle-group-item"
          ; Attr.create "data-state" (if current then "on" else "off")
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
           ~cls:"session-choices flex gap-0.5 rounded-lg bg-muted p-[3px]"
           ~attrs:[ Attr.create "data-slot" "toggle-group"; Attr.role "group" ]
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
           ~cls:
             "session-usage flex items-center gap-3 px-0.5 text-[13px] \
              text-muted-foreground [&>.status-item.cost]:font-mono \
              [&>.status-item.cost]:text-[12.5px] \
              [&>.status-item.cost]:text-foreground"
           [ (match Status_view.context_percent state with
              | Some percent ->
                Node.span
                  ~attrs:
                    [ Attr.class_
                        "session-context flex min-w-0 flex-1 items-center \
                         gap-2 [&_.meter]:h-[5px] [&_.meter]:flex-1"
                    ]
                  [ Status_view.meter percent
                  ; span
                      ~cls:
                        "percent font-mono text-[12.5px] text-foreground \
                         tabular-nums"
                      (sprintf "%.0f%%" percent)
                  ; span
                      ~cls:"session-of font-mono text-xs text-muted-foreground"
                      (sprintf
                         "%s / %s"
                         (Status_view.tokens state.context_tokens)
                         (Status_view.tokens state.model.context_window))
                  ]
              | None -> Node.none)
           ; Status_view.cost state ~inject
           ])
    in
    div
      ~cls:
        "session-sections mb-1.5 flex flex-col gap-1 border-b border-border \
         px-1 pt-0.5 pb-2.5"
      [ verbosity; usage ]
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
        ~cls:"popover-empty px-3 py-3.5 text-xs text-muted-foreground"
        [ Node.textf
            "Nothing matches “%s”: Backspace widens the search."
            (Picker.query p)
        ]
    | _ ->
      (* A new list for each filter, scrolled to its top. *)
      Node.div
        ~key:(Picker.query p)
        ~attrs:
          ([ Attr.class_
               "popover-items min-h-0 flex-1 overflow-y-auto \
                overscroll-contain p-1 outline-none"
           ; Attr.create "data-slot" "command-list"
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
      [ Attr.class_
          "loon chip-popover menu-surface bg-popover text-popover-foreground \
           rounded-lg shadow-md ring-1 ring-foreground/10"
      ; Attr.create "data-slot" "popover-content"
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
            ~cls:search_cls
            [ icon Search
            ; Node.input
                ~attrs:
                  [ Attr.id "popover-filter"
                  ; Attr.class_
                      "min-w-0 flex-1 bg-transparent text-[13.5px] \
                       outline-none placeholder:text-muted-foreground \
                       narrow:text-base"
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
            ; span ~cls:shortcut_cls (shortcut chip)
            ]
        | Attach ->
          div
            ~cls:("popover-head " ^ head_cls)
            [ span
                ~cls:
                  "popover-title flex-1 text-xs font-medium \
                   text-muted-foreground"
                (title chip)
            ; span ~cls:shortcut_cls (shortcut chip)
            ])
     ; list
     ]
     @ Option.to_list
         (match chip with
          | Host -> host_note m
          | Model | Attach -> None)
     @ [ div
           ~cls:
             "menu-keys shrink-0 border-t border-border px-3 pt-1.5 pb-[7px] \
              text-xs text-muted-foreground pointer-coarse:hidden"
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
