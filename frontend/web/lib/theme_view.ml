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

let swatch accent =
  Node.span
    ~attrs:
      ([ Attr.class_ "swatch"; Attr.create "aria-hidden" "true" ]
       @ accent_attrs accent)
    []
;;

let sample name =
  let bar cls = Node.span ~attrs:[ Attr.class_ cls ] [] in
  Node.span
    ~attrs:
      [ Attr.class_ "theme-sample"
      ; Attr.create "data-theme" (Theme.Name.to_string name)
      ; Attr.create "aria-hidden" "true"
      ]
    [ Node.span
        ~attrs:[ Attr.class_ "sample-side" ]
        [ bar "sample-line"; bar "sample-line current"; bar "sample-line" ]
    ; Node.span
        ~attrs:[ Attr.class_ "sample-main" ]
        [ bar "sample-bubble"
        ; bar "sample-line wide"
        ; bar "sample-line"
        ; bar "sample-box"
        ]
    ]
;;

let themes (m : App.Model.t) ~inject =
  let current = (App.Model.look m).name in
  Node.div
    ~attrs:
      [ Attr.class_ "theme-choices"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Theme"
      ]
    (List.map Theme.Name.all ~f:(fun n ->
       let name = Theme.Name.to_string n in
       let on = Theme.Name.equal n current in
       Node.button
         ~attrs:
           [ classes [ "theme-choice" ] [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string on)
           ; Attr.title (sprintf "%s (/theme %s)" (Theme.Name.describe n) name)
           ; Attr.on_click (fun _ -> inject (Action.Run ("/theme " ^ name)))
           ]
         [ sample n; span ~cls:"theme-name" (String.capitalize name) ]))
;;

let accents (m : App.Model.t) ~inject =
  let current = (App.Model.look m).accent in
  let presets =
    List.map Theme.Accent.Preset.all ~f:(fun p ->
      let name = Theme.Accent.Preset.to_string p in
      let on = Theme.Accent.equal current (Preset p) in
      Node.button
        ~attrs:
          [ classes [ "accent-choice" ] [ "on", on ]
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
        [ classes [ "accent-choice"; "accent-custom" ] [ "on", custom_on ]
        ; Attr.title "Any colour (/accent #rrggbb)"
        ]
      [ (if custom_on then swatch current else Node.none)
      ; Node.input
          ~attrs:
            [ Attr.type_ "color"
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
      [ Attr.class_ "accent-choices"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Accent colour"
      ]
    (presets @ [ custom ])
;;

(* One of [all] (by its [to_string]), set with [/command NAME]: loon's
   toggle group. *)
let choice ~label ~command ~all ~to_string ~describe ~current ~inject =
  Node.div
    ~attrs:[ Attr.class_ "loon" ]
    [ Loon.Toggle_group.single
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
                 [ Attr.class_ "narrow:h-11 narrow:px-3"
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
