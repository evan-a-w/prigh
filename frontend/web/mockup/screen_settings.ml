open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

(* A miniature of the page in a theme: the sidebar, a reply and a bubble. *)
let preview = function
  | `Light ->
    div
      "flex h-full bg-[#f7f9fb]"
      [ div "w-1/4 bg-[#eef2f6]" []
      ; div
          "flex flex-1 flex-col gap-1.5 p-2.5"
          [ div "h-1.5 w-3/4 rounded-full bg-[#c9d2db]" []
          ; div "h-1.5 w-1/2 rounded-full bg-[#c9d2db]" []
          ; div "mt-auto ml-auto h-3 w-2/5 rounded-md bg-[#dfe8f1]" []
          ]
      ]
  | `Dark ->
    div
      "flex h-full bg-[#131a22]"
      [ div "w-1/4 bg-[#18202a]" []
      ; div
          "flex flex-1 flex-col gap-1.5 p-2.5"
          [ div "h-1.5 w-3/4 rounded-full bg-[#33404d]" []
          ; div "h-1.5 w-1/2 rounded-full bg-[#33404d]" []
          ; div "mt-auto ml-auto h-3 w-2/5 rounded-md bg-[#2b3f53]" []
          ]
      ]
;;

let theme_choice ~value ~on_change =
  let card group ~id ~label preview =
    let selected = [%equal: string option] value (Some id) in
    Node.label
      ~attrs:
        [ Attr.for_ ("theme-" ^ id)
        ; Attr.class_
            (Cn.cn
               [ "group flex cursor-pointer flex-col gap-2 rounded-lg"
               ; "[&_[data-slot=radio-group-item]]:sr-only"
               ])
        ]
      [ div
          (Cn.cn
             [ "relative aspect-[16/10] overflow-hidden rounded-lg border ring-offset-2 \
                ring-offset-card transition-shadow group-hover:border-ring"
             ; (if selected then "ring-2 ring-primary border-transparent" else "")
             ])
          [ preview ]
      ; div
          "flex items-center gap-2 text-sm"
          [ Radio_group.item group ~id:("theme-" ^ id) ~value:id
          ; span
              (if selected then "text-foreground" else "text-muted-foreground")
              [ Node.text label ]
          ; (if selected then Icon.view ~class_:"ml-auto size-3.5 text-primary" Check
             else Node.none)
          ]
      ]
  in
  Radio_group.view
    ~class_:"grid grid-cols-3 gap-3 max-sm:gap-2"
    ~orientation:Horizontal
    ~value
    ~on_change
    (fun g ->
      [ card g ~id:"light" ~label:"Light" (preview `Light)
      ; card g ~id:"dark" ~label:"Dark" (preview `Dark)
      ; card
          g
          ~id:"system"
          ~label:"System"
          (div
             "relative h-full"
             [ preview `Light
             ; div
                 "absolute inset-0 [clip-path:polygon(100%_0,100%_100%,0_100%)]"
                 [ preview `Dark ]
             ])
      ])
;;

let accents =
  [ "slate", "Slate", "bg-[#a9bccf]"
  ; "blue", "Blue", "bg-[#6ea8fe]"
  ; "violet", "Violet", "bg-[#a78bfa]"
  ; "green", "Green", "bg-[#6fcf97]"
  ; "amber", "Amber", "bg-[#f2c94c]"
  ; "rose", "Rose", "bg-[#f2849e]"
  ]
;;

let accent_choice ~value ~on_change =
  div
    "flex items-center gap-2"
    (List.map accents ~f:(fun (id, label, bg) ->
       let selected = String.equal value id in
       Node.button
         ~attrs:
           [ Attr.type_ "button"
           ; Attr.title label
           ; Attr.create "aria-label" label
           ; Attr.create "aria-pressed" (Bool.to_string selected)
           ; Attr.on_click (fun _ -> on_change id)
           ; Attr.class_
               (Cn.cn
                  [ "flex size-6 items-center justify-center rounded-full ring-offset-2 \
                     ring-offset-card outline-none transition-shadow \
                     focus-visible:ring-2 focus-visible:ring-ring max-md:size-8"
                  ; bg
                  ; (if selected then "ring-2 ring-foreground/70" else "hover:ring-2 hover:ring-border")
                  ])
           ]
         [ (if selected
            then Icon.view ~class_:"size-3.5 text-[#131a22]" Check
            else Node.none)
         ]))
;;

let switch_row ~id ~title ~description (checked, set) =
  Settings_page.row
    ~for_:id
    ~title
    ~description
    (Switch.view ~id ~checked ~on_change:set ())
;;

let toggle init (local_ graph) =
  let v, set = Bonsai.state init graph in
  Bonsai.both v set
;;

let component ~tip:_ (local_ graph) =
  let theme, set_theme = Bonsai.state "dark" graph in
  let accent, set_accent = Bonsai.state "slate" graph in
  let density, set_density = Bonsai.state "compact" graph in
  let font_size, set_font_size = Bonsai.state [ 14. ] graph in
  let timestamps = toggle true graph in
  let tool_output = toggle false graph in
  let reduce_motion = toggle false graph in
  let notify = toggle true graph in
  let verbosity =
    Select.component
      ~default_value:"normal"
      (Bonsai.return
         (Select.trigger ~id:"verbosity" ~size:Sm ~class_:"w-36" ()))
      (Bonsai.return
         (Select.content
            ~position:Popper
            ~align:End
            [ Select.item ~value:"quiet" [ Node.text "Quiet" ]
            ; Select.item ~value:"normal" [ Node.text "Normal" ]
            ; Select.item ~value:"verbose" [ Node.text "Verbose" ]
            ; Select.item ~value:"everything" [ Node.text "Everything" ]
            ]))
      graph
  in
  let code_font =
    Select.component
      ~default_value:"geist-mono"
      (Bonsai.return (Select.trigger ~id:"code-font" ~size:Sm ~class_:"w-36" ()))
      (Bonsai.return
         (Select.content
            ~position:Popper
            ~align:End
            [ Select.item ~value:"geist-mono" [ Node.text "Geist Mono" ]
            ; Select.item ~value:"jetbrains" [ Node.text "JetBrains Mono" ]
            ; Select.item ~value:"system" [ Node.text "System monospace" ]
            ]))
      graph
  in
  let body =
    let%arr theme
    and set_theme
    and accent
    and set_accent
    and density
    and set_density
    and font_size
    and set_font_size
    and timestamps
    and tool_output
    and reduce_motion
    and notify
    and _, verbosity = verbosity
    and _, code_font = code_font in
    let size = List.hd font_size |> Option.value ~default:14. in
    div
      "flex flex-col gap-8"
      [ Settings_page.group
          ~title:"Theme"
          ~description:"System follows your device's light or dark setting."
          [ div
              "p-4"
              [ theme_choice ~value:(Some theme) ~on_change:set_theme ]
          ; Settings_page.row
              ~stack:true
              ~title:"Accent"
              ~description:"Your messages, the send button and focus rings."
              (accent_choice ~value:accent ~on_change:set_accent)
          ]
      ; Settings_page.group
          ~title:"Layout"
          [ Settings_page.row
              ~title:"Density"
              ~description:"Spacing in the transcript and the sidebar."
              (Toggle_group.single
                 ~variant:Outline
                 ~size:Sm
                 ~class_:
                   "[&>[data-state=on]]:bg-accent [&>[data-state=on]]:text-accent-foreground"
                 ~value:(Some density)
                 ~on_change:(fun v -> set_density (Option.value v ~default:density))
                 (fun g ->
                   [ Toggle_group.item g ~value:"compact" [ Node.text "Compact" ]
                   ; Toggle_group.item g ~value:"comfortable" [ Node.text "Comfortable" ]
                   ]))
          ; Settings_page.row
              ~stack:true
              ~title:"Text size"
              ~description:"The transcript and the composer; menus keep theirs."
              (div
                 "flex items-center gap-4"
                 [ span "text-xs text-muted-foreground" [ Node.text "A" ]
                 ; Slider.view
                     ~class_:"flex-1"
                     ~min:12.
                     ~max:18.
                     ~step:1.
                     ~value:font_size
                     ~on_change:set_font_size
                     ()
                 ; span "text-base text-muted-foreground" [ Node.text "A" ]
                 ; span
                     "w-10 text-right text-xs tabular-nums text-muted-foreground"
                     [ Node.textf "%.0fpx" size ]
                 ])
          ; Settings_page.row
              ~for_:"code-font"
              ~title:"Code font"
              ~description:"Code blocks, diffs and the terminal."
              code_font
          ]
      ; Settings_page.group
          ~title:"Transcript"
          [ Settings_page.row
              ~for_:"verbosity"
              ~title:"Verbosity"
              ~description:"How much of each tool call shows. Ctrl+O cycles it."
              verbosity
          ; switch_row
              ~id:"tool-output"
              ~title:"Expand tool output"
              ~description:"Show each tool's output under it, not folded away."
              tool_output
          ; switch_row
              ~id:"timestamps"
              ~title:"Timestamps"
              ~description:"The time beside each message."
              timestamps
          ; switch_row
              ~id:"reduce-motion"
              ~title:"Reduce motion"
              ~description:"No shimmer or animated spinners."
              reduce_motion
          ]
      ; Settings_page.group
          ~title:"Notifications"
          [ switch_row
              ~id:"notify"
              ~title:"When a session needs you"
              ~description:
                "A notification when a tool waits for confirmation or a question is asked, \
                 and the tab isn't focused."
              notify
          ]
      ]
  in
  Settings_page.component
    ~start_on_list:true
    ~section:Appearance
    ~description:"How prigh looks on this device."
    ~body
    graph
;;
