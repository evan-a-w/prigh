open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

module Provider = struct
  type t =
    { name : string
    ; glyph : string
    }
end

let providers : Provider.t list =
  [ { name = "Anthropic"; glyph = "A" }
  ; { name = "OpenAI"; glyph = "O" }
  ; { name = "Google"; glyph = "G" }
  ; { name = "Mistral"; glyph = "M" }
  ; { name = "OpenRouter"; glyph = "R" }
  ]
;;

(* name, favourite *)
let models =
  [ "Opus 4.5", true
  ; "Sonnet 4.5", true
  ; "Haiku 4.5", false
  ; "Opus 4.1", false
  ; "Sonnet 4", false
  ; "Haiku 3.5", false
  ]
;;

let levels = [ "Low"; "Medium"; "High"; "Extra High"; "Max" ]

let glyph ?(class_ = "") g =
  span
    (Cn.cn
       [ "inline-flex size-4 shrink-0 items-center justify-center font-serif text-[13px] \
          font-bold leading-none text-muted-foreground"
       ; class_
       ])
    [ Node.text g ]
;;

let rail_button ~active ~label content =
  Node.button
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "relative flex size-9 shrink-0 items-center justify-center rounded-md \
                text-muted-foreground hover:bg-accent hover:text-foreground"
             ; (if active
                then
                  "bg-accent text-foreground before:absolute before:rounded-full \
                   before:bg-sky-400 md:before:top-2 md:before:bottom-2 md:before:-left-2 \
                   md:before:w-[3px] max-md:before:-bottom-2 max-md:before:left-2 \
                   max-md:before:right-2 max-md:before:h-[3px]"
                else "")
             ])
      ; Attr.create "aria-label" label
      ; Attr.title label
      ]
    [ content ]
;;

let rail =
  div
    "flex shrink-0 gap-1 p-2 max-md:border-b md:flex-col md:border-r"
    (rail_button ~active:false ~label:"Favourites" (Icon.view ~class_:"size-4" Star)
     :: div "bg-border max-md:mx-1 max-md:w-px md:my-1 md:h-px" []
     :: List.mapi providers ~f:(fun i (p : Provider.t) ->
       rail_button
         ~active:(i = 0)
         ~label:p.name
         (glyph ~class_:(if i = 0 then "text-foreground" else "") p.glyph)))
;;

let models_column ~state ~set_state =
  Command.view
    ~id:"model-picker"
    ~class_:
      "min-w-0 flex-1 rounded-none bg-transparent md:border-r \
       [&_[data-slot=command-item]]:w-full"
    ~state
    ~on_change:set_state
    [ Command.input ~placeholder:"Search models…" ~auto_focus:true ()
    ; Command.list
        ~class_:"max-h-[min(300px,40svh)]"
        [ Command.empty [ Node.text "No model matches. /login adds providers." ]
        ; Command.group
            ~heading:"Anthropic"
            (List.mapi models ~f:(fun i (name, favourite) ->
               let current = String.equal name Fake.User.model in
               Command.item
                 ~value:name
                 ~class_:"h-8"
                 [ glyph "A"
                 ; span (if current then "font-medium" else "") [ Node.text ("Claude " ^ name) ]
                 ; Command.shortcut
                     ~class_:"flex items-center gap-2 tracking-normal"
                     [ Kbd.view ~class_:"max-md:hidden" [ Node.textf "Alt+%d" (i + 1) ]
                     ; Icon.view
                         ~class_:
                           (if favourite
                            then "size-3.5 fill-sky-400 text-sky-400"
                            else "size-3.5 opacity-0")
                         Star
                     ; Icon.view
                         ~class_:(if current then "size-4 text-sky-400" else "size-4 opacity-0")
                         Check
                     ]
                 ]))
        ; Command.static
            (Node.button
               ~attrs:
                 [ Attr.class_
                     "flex w-full items-center gap-2 rounded-md px-2 py-1.5 text-[13px] \
                      text-muted-foreground hover:bg-accent hover:text-foreground"
                 ]
               [ Icon.view ~class_:"size-4" Chevron_down; Node.text "Show 5 more" ])
        ]
    ]
;;

let reasoning ~thinking ~set_thinking =
  div
    "flex shrink-0 flex-col gap-0.5 p-2 max-md:border-t md:w-[210px]"
    [ div
        "flex items-baseline gap-1.5 px-2 pt-1 pb-1.5"
        [ span "text-[13px] font-semibold" [ Node.text "Reasoning" ]
        ; span "truncate text-xs text-muted-foreground" [ Node.text Fake.User.model ]
        ]
    ; div
        "flex gap-0.5 max-md:overflow-x-auto md:flex-col"
        (List.map levels ~f:(fun l ->
           let selected = String.equal l thinking in
           Node.button
             ~attrs:
               [ Attr.class_
                   (Cn.cn
                      [ "flex h-8 shrink-0 items-center gap-2 rounded-md px-2 text-left \
                         text-[13px] hover:bg-accent max-md:h-10 max-md:px-3"
                      ; (if selected then "bg-accent/60 text-foreground" else "text-foreground/85")
                      ])
               ; Attr.on_click (fun _ -> set_thinking l)
               ]
             [ span "" [ Node.text l ]
             ; (if String.equal l "Medium"
                then span "ml-auto text-xs text-muted-foreground max-md:hidden" [ Node.text "Default" ]
                else Node.none)
             ; (if selected
                then
                  Icon.view
                    ~class_:
                      (Cn.cn
                         [ "size-4 text-sky-400"
                         ; (if String.equal l "Medium" then "" else "ml-auto")
                         ])
                    Check
                else Node.none)
             ]))
    ]
;;

let footer =
  div
    "flex items-center gap-3 border-t px-3 py-1.5 text-xs text-muted-foreground"
    [ span
        "truncate max-md:hidden"
        [ Node.text "↑↓ models · ←→ providers · Enter switches · Esc closes" ]
    ; Button.view
        ~variant:Ghost
        ~size:Xs
        ~class_:"ml-auto gap-1.5 font-normal text-sky-300 hover:text-sky-200 max-md:h-9"
        [ Icon.view Shield; Node.text Fake.User.mode; Icon.view Chevron_up ]
    ]
;;

let picker ~state ~set_state ~thinking ~set_thinking =
  div
    "flex flex-col"
    [ div
        "flex min-h-0 max-md:flex-col"
        [ rail; models_column ~state ~set_state; reasoning ~thinking ~set_thinking ]
    ; footer
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
                      ~class_:"sr-only"
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
                  ~align:End
                  ~side_offset:10.
                  ~class_:"w-[720px] max-w-[calc(100vw-2rem)] gap-0 overflow-hidden p-0"
                  [ body ]
              ]))
      | _ -> None
    in
    fun tip -> Composer.view ~wrap_chip ~tip ()
  in
  Shell.component ~tip ~main:(Screen_chat.component ~composer ~tip graph) graph
;;
