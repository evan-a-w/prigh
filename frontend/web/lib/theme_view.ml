open! Core
open! Import
open Html
module Action = App.Action

(* The CSS gives each preset its colour (per theme); a custom one comes as
   it is. *)
let accent_attrs : Theme.Accent.t -> Attr.t list = function
  | Preset p -> [ Attr.create "data-accent" (Theme.Accent.Preset.to_string p) ]
  | Custom hex -> [ Attr.style (Css_gen.create ~field:"--sodium" ~value:hex) ]
;;

(* In a picker's row (the /accent and /theme pickers) they are small. *)
let swatch accent =
  Node.span
    ~attrs:
      ([ Attr.class_
           "swatch block size-[22px] shrink-0 rounded-full bg-[var(--sodium)] \
            shadow-[inset_0_0_0_1px_#0000001f] [.picker-item_&]:size-3"
       ; Attr.create "aria-hidden" "true"
       ]
       @ accent_attrs accent)
    []
;;

(* A small picture of the page in the theme's colours (its [data-theme]
   rescopes them), the user's accent on it. *)
let sample name =
  let bar cls = Node.span ~attrs:[ Attr.class_ cls ] [] in
  let line ?(cls = "w-[70%] bg-[var(--faint-line)]") () =
    bar ("sample-line block h-[3px] rounded-xs [.picker-item_&]:h-0.5 " ^ cls)
  in
  let pane =
    "flex flex-col gap-[5px] rounded-[3px] p-[5px] [.picker-item_&]:gap-0.5 \
     [.picker-item_&]:p-0.5"
  in
  Node.span
    ~attrs:
      [ Attr.class_
          "theme-sample flex h-16 w-full gap-1 overflow-hidden rounded-md \
           border border-[var(--rule)] bg-[var(--graphite)] p-1 narrow:h-12 \
           [.picker-item_&]:h-7 [.picker-item_&]:w-11 \
           [.picker-item_&]:shrink-0 [.picker-item_&]:gap-0.5 \
           [.picker-item_&]:rounded [.picker-item_&]:p-0.5"
      ; Attr.create "data-theme" (Theme.Name.to_string name)
      ; Attr.create "aria-hidden" "true"
      ]
    [ Node.span
        ~attrs:[ Attr.class_ ("sample-side w-[30%] bg-[var(--slate)] " ^ pane) ]
        [ line (); line ~cls:"current w-[85%] bg-[var(--sodium)]" (); line () ]
    ; Node.span
        ~attrs:[ Attr.class_ ("sample-main min-w-0 flex-1 " ^ pane) ]
        [ bar
            "sample-bubble block h-2 w-[60%] rounded-r-xs border-l-2 \
             border-[var(--sodium)] bg-[var(--slate-2)] [.picker-item_&]:h-1"
        ; line ~cls:"wide w-[90%] bg-[var(--muted-text)]" ()
        ; line ()
        ; bar
            "sample-box block flex-1 rounded-xs border border-[var(--rule)] \
             bg-[var(--well)]"
        ]
    ]
;;

let themes (m : App.Model.t) ~inject =
  let current = (App.Model.look m).name in
  Node.div
    ~attrs:
      [ Attr.class_ "theme-choices grid w-full grid-cols-3 gap-2.5"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Theme"
      ]
    (List.map Theme.Name.all ~f:(fun n ->
       let name = Theme.Name.to_string n in
       let on = Theme.Name.equal n current in
       Node.button
         ~attrs:
           [ Attr.class_
               (Loon.Cn.cn
                  [ "theme-choice"
                  ; (if on then "on" else "")
                  ; "flex min-w-0 cursor-pointer flex-col gap-1.5 rounded-lg \
                     border bg-transparent px-1.5 pt-1.5 pb-[7px] text-left \
                     text-[13px] text-muted-foreground outline-none \
                     transition-colors hover:border-ring/60 \
                     hover:text-foreground focus-visible:ring-[3px] \
                     focus-visible:ring-ring/50"
                  ; (if on
                     then "border-primary text-foreground ring-1 ring-primary"
                     else "")
                  ])
           ; Attr.type_ "button"
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string on)
           ; Attr.title (sprintf "%s (/theme %s)" (Theme.Name.describe n) name)
           ; Attr.on_click (fun _ -> inject (Action.Run ("/theme " ^ name)))
           ]
         [ sample n
         ; span ~cls:"theme-name pl-0.5 font-medium" (String.capitalize name)
         ]))
;;

let accent_choice_cls on =
  Loon.Cn.cn
    [ "accent-choice"
    ; (if on then "on" else "")
    ; "group/accent relative inline-flex cursor-pointer items-center gap-2 \
       rounded-full border-0 bg-transparent p-[3px] text-[13px] \
       text-muted-foreground outline-none focus-visible:ring-[3px] \
       focus-visible:ring-ring/50 narrow:size-11 narrow:justify-center \
       [&>.swatch]:motion-safe:transition-transform \
       hover:[&>.swatch]:scale-[1.08]"
    ; (if on
       then
         "[&>.swatch]:shadow-[0_0_0_2px_var(--background),0_0_0_4px_var(--foreground)]"
       else "")
    ]
;;

let accents (m : App.Model.t) ~inject =
  let current = (App.Model.look m).accent in
  let presets =
    List.map Theme.Accent.Preset.all ~f:(fun p ->
      let name = Theme.Accent.Preset.to_string p in
      let on = Theme.Accent.equal current (Preset p) in
      Node.button
        ~attrs:
          [ Attr.class_ (accent_choice_cls on)
          ; Attr.type_ "button"
          ; Attr.role "radio"
          ; Attr.create "aria-checked" (Bool.to_string on)
          ; Attr.create "aria-label" (String.capitalize name)
          ; Attr.title (sprintf "%s (/accent %s)" (String.capitalize name) name)
          ; Attr.on_click (fun _ -> inject (Action.Run ("/accent " ^ name)))
          ]
        [ swatch (Preset p) ])
  in
  let custom_on =
    match current with
    | Custom _ -> true
    | Preset _ -> false
  in
  let custom =
    Node.label
      ~attrs:
        [ Attr.class_
            (Loon.Cn.cn
               [ accent_choice_cls custom_on
               ; "accent-custom border pr-2.5 hover:text-foreground \
                  focus-within:ring-[3px] focus-within:ring-ring/50 \
                  narrow:w-auto narrow:px-3"
               ; (if custom_on
                  then "border-solid text-foreground"
                  else
                    "border-dashed before:block before:size-[22px] \
                     before:shrink-0 before:rounded-full \
                     before:bg-[conic-gradient(#f06d6d,#f0c04c,#7fd36b,#4cc9c0,#6a8ff8,#c084f5,#f06d6d)] \
                     before:content-['']")
               ])
        ; Attr.title "Any colour (/accent #rrggbb)"
        ]
      [ (if custom_on then swatch current else Node.none)
      ; Node.input
          ~attrs:
            [ Attr.type_ "color"
            ; Attr.class_
                "absolute inset-0 size-full cursor-pointer border-0 p-0 \
                 opacity-0"
            ; Attr.id "accent-input"
            ; Attr.create "aria-label" "Custom accent colour"
            ; Attr.value (Theme.Accent.hex current)
            ; Attr.on_input (fun _ value ->
                inject (Action.Preview_accent value))
            ; Attr.on_change (fun _ value ->
                inject (Action.Run ("/accent " ^ value)))
            ]
          ()
      ; span ~cls:"accent-custom-label" "Custom"
      ]
  in
  Node.div
    ~attrs:
      [ Attr.class_ "accent-choices flex flex-wrap items-center gap-2"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Accent colour"
      ]
    (presets @ [ custom ])
;;

(* One of [all] (by its [to_string]), set with [/command NAME]: loon's
   toggle group. *)
let choice ~label ~command ~all ~to_string ~describe ~current ~inject =
  Node.div
    ~attrs:[ Attr.class_ "setting-segments loon narrow:w-full" ]
    [ Loon.Toggle_group.single
        ~class_:"narrow:w-full"
        ~attrs:[ Attr.create "aria-label" label ]
        ~variant:Outline
        ~size:Sm
        ~value:(Some (to_string current))
        ~on_change:(function
          | Some name -> inject (Action.Run (sprintf "/%s %s" command name))
          | None -> Effect.Ignore)
        (fun group ->
           List.map all ~f:(fun v ->
             let name = to_string v in
             Loon.Toggle_group.item
               group
               ~value:name
               ~attrs:
                 [ Attr.class_ "narrow:h-11 narrow:flex-1 narrow:px-3"
                 ; Attr.title (sprintf "%s (/%s %s)" (describe v) command name)
                 ]
               [ Node.text (String.capitalize name) ]))
    ]
;;

let rows (m : App.Model.t) ~inject =
  let look = App.Model.look m in
  let note =
    match Option.bind m.hello ~f:Hello_reply.acting_as with
    | Some other ->
      Some
        (sprintf
           "Acting as %s: changes stay in this browser until you are back."
           other
         |> fun text -> "warm", text)
    | None -> None
  in
  [ Settings_row.row
      ~cls:"theme"
      ~wide:true
      ~name:"Theme"
      ~about:"The page's colours."
      ?note
      [ themes m ~inject ]
  ; Settings_row.row
      ~cls:"accent"
      ~wide:true
      ~name:"Accent"
      ~about:"Your turns, the selection, focus and work in progress."
      [ accents m ~inject ]
  ; Settings_row.row
      ~cls:"corners"
      ~name:"Corners"
      ~about:"How round buttons, cards and dialogs are."
      [ choice
          ~label:"Corners"
          ~command:"corners"
          ~all:Theme.Radius.all
          ~to_string:Theme.Radius.to_string
          ~describe:Theme.Radius.describe
          ~current:look.radius
          ~inject
      ]
  ; Settings_row.row
      ~cls:"font"
      ~name:"Font"
      ~about:"Prose and the interface."
      [ choice
          ~label:"Font"
          ~command:"font"
          ~all:Theme.Sans.all
          ~to_string:Theme.Sans.to_string
          ~describe:Theme.Sans.describe
          ~current:look.sans
          ~inject
      ]
  ; Settings_row.row
      ~cls:"code-font"
      ~name:"Code font"
      ~about:"Code, commands and their output."
      [ choice
          ~label:"Code font"
          ~command:"code-font"
          ~all:Theme.Mono.all
          ~to_string:Theme.Mono.to_string
          ~describe:Theme.Mono.describe
          ~current:look.mono
          ~inject
      ]
  ]
;;
