open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let providers =
  [ ( "Anthropic"
    , [ "Opus 4.5", "200k", "$5 / $25"; "Sonnet 4.5", "1M", "$3 / $15"; "Haiku 4.5", "200k", "$1 / $5" ] )
  ; "OpenAI", [ "GPT-5.1", "400k", "$1.25 / $10"; "GPT-5.1 mini", "400k", "$0.25 / $2" ]
  ; "Google", [ "Gemini 3 Pro", "1M", "$2 / $12" ]
  ]
;;

let levels = [ "Off"; "Low"; "Medium"; "High"; "Max" ]

let picker ~state ~set_state ~thinking ~set_thinking =
  div
    ""
    [ Command.view
        ~id:"model-picker"
        ~class_:"bg-transparent [&_[data-slot=command-item]]:w-full"
        ~state
        ~on_change:set_state
        [ Command.input ~placeholder:"Search models…" ~auto_focus:true ()
        ; Command.list
            ~class_:"max-h-[min(320px,50svh)]"
            (Command.empty [ Node.text "No model matches. /login adds providers." ]
             :: List.map providers ~f:(fun (provider, models) ->
               Command.group
                 ~heading:provider
                 (List.map models ~f:(fun (name, context, cost) ->
                    let current = String.equal name Fake.User.model in
                    Command.item
                      ~value:name
                      ~keywords:[ provider ]
                      [ Icon.view
                          ~class_:(if current then "" else "invisible")
                          Check
                      ; span (if current then "font-medium" else "") [ Node.text name ]
                      ; Command.shortcut
                          ~class_:"flex gap-3 tabular-nums tracking-normal"
                          [ span "w-10 text-right" [ Node.text context ]
                          ; span "w-20 text-right" [ Node.text cost ]
                          ]
                      ]))))
        ]
    ; div
        "flex items-center gap-2 border-t px-3 py-2.5"
        [ span "flex items-center gap-1.5 text-xs text-muted-foreground"
            [ Icon.view ~class_:"size-3.5" Brain; Node.text "Thinking" ]
        ; Toggle_group.single
            ~class_:"ml-auto"
            ~variant:Outline
            ~size:Sm
            ~value:(Some thinking)
            ~on_change:(fun v -> set_thinking (Option.value v ~default:thinking))
            (fun g ->
              List.map levels ~f:(fun l ->
                Toggle_group.item g ~value:l ~class_:"px-2 text-xs" [ Node.text l ]))
        ]
    ; Overlay_parts.footer_hints
        [ [ "↑"; "↓" ], "model"; [ "Tab" ], "thinking"; [ "↵" ], "use"; [ "esc" ], "close" ]
    ]
;;

let component ~tip (local_ graph) =
  let open_, set_open = Bonsai.state true graph in
  let state, set_state = Bonsai.state Command.State.empty graph in
  let thinking, set_thinking = Bonsai.state Fake.User.thinking graph in
  let is_mobile = Use_mobile.component graph in
  let composer =
    let%arr open_ and set_open and state and set_state and thinking and set_thinking
    and is_mobile in
    let body = picker ~state ~set_state ~thinking ~set_thinking in
    let wrap_chip = function
      | "model" when is_mobile ->
        Some
          (fun chip ->
            Drawer.view ~id:"model-drawer" ~open_ ~on_open_change:set_open (fun d ->
              [ Drawer.trigger d chip
              ; Drawer.content
                  d
                  [ Drawer.header
                      ~class_:"pb-0 text-left"
                      [ Drawer.title d [ Node.text "Model" ] ]
                  ; body
                  ]
              ]))
      | "model" ->
        Some
          (fun chip ->
            Popover.view ~id:"model-popover" ~open_ ~on_open_change:set_open (fun p ->
              [ Popover.trigger p chip
              ; Popover.content
                  p
                  ~side:Top
                  ~align:Start
                  ~side_offset:8.
                  ~class_:"w-[400px] gap-0 p-0"
                  [ body ]
              ]))
      | _ -> None
    in
    fun tip -> Composer.view ~wrap_chip ~tip ()
  in
  Shell.component ~tip ~main:(Screen_chat.component ~composer ~tip graph) graph
;;
