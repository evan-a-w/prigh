open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children
let kbd s = Kbd.view ~class_:"ml-1 max-sm:hidden h-4 min-w-4 px-1 text-[10px]" [ Node.text s ]

let card children =
  div "mx-auto w-full max-w-[760px] px-4 pb-2"
    [ div "flex flex-col gap-3 rounded-2xl border border-amber-500/30 bg-card p-3.5 text-sm shadow-lg shadow-black/20" children ]
;;

let confirm_card =
  card
    [ div "flex items-center gap-2"
        [ span "flex size-6 items-center justify-center rounded-md bg-amber-500/15 text-amber-300 [&_svg]:size-3.5" [ Icon.view Terminal ]
        ; span "font-medium" [ Node.text "Run this command?" ]
        ; span "text-xs text-muted-foreground" [ Node.text "bash · desktop" ]
        ; span "ml-auto text-xs text-muted-foreground max-sm:hidden" [ Node.text "Mode: Auto (asks for risky tools)" ]
        ]
    ; Node.pre ~attrs:[ Attr.class_ "whitespace-pre-wrap break-all rounded-lg border bg-muted/40 px-3 py-2 font-mono text-[12px] leading-relaxed" ]
        [ span "text-muted-foreground select-none" [ Node.text "$ " ]; Node.text "git push --force-with-lease origin feat/composer-chips" ]
    ; div "text-xs text-muted-foreground" [ Node.text "Rewrites the remote branch: 2 commits on origin would be replaced." ]
    ; div "flex flex-wrap items-center gap-1.5"
        [ Button.view ~size:Sm [ Node.text "Allow once"; kbd "↵" ]
        ; Button.view ~variant:Outline ~size:Sm [ Node.text "Always allow git push"; kbd "A" ]
        ; Button.view ~variant:Ghost ~size:Sm ~class_:"text-muted-foreground" [ Node.text "Deny"; kbd "Esc" ]
        ; Button.view ~variant:Link ~size:Sm ~class_:"ml-auto text-xs text-muted-foreground max-sm:hidden" [ Node.text "Deny and tell prigh why…" ]
        ]
    ]
;;

let options =
  [ "sheet", "One row, overflow in a sheet", "Model and thinking stay; mode and host move to the ⋯ sheet."
  ; "wrap", "Wrap onto two rows", "Every chip stays visible; the composer grows by 32px."
  ; "icons", "Icons only", "Chips shrink to icons with tooltips; labels show on a long press."
  ]
;;

let question_card ~choice ~set_choice =
  card
    [ div "flex items-center gap-2"
        [ span "flex size-6 items-center justify-center rounded-md bg-sky-500/15 text-sky-300 [&_svg]:size-3.5" [ Icon.view Circle_help ]
        ; span "font-medium" [ Node.text "Which layout should the composer use on phones?" ]
        ; span "ml-auto text-xs tabular-nums text-muted-foreground" [ Node.text "1 of 2" ]
        ]
    ; Radio_group.view ~class_:"gap-1.5" ~value:(Some choice) ~on_change:set_choice (fun g ->
        List.map options ~f:(fun (v, title, desc) ->
          Node.label
            ~attrs:[ Attr.for_ ("q-" ^ v); Attr.class_ "flex cursor-pointer items-start gap-3 rounded-lg border px-3 py-2 hover:bg-accent/40 has-[[data-state=checked]]:border-ring has-[[data-state=checked]]:bg-accent/40" ]
            [ Radio_group.item ~id:("q-" ^ v) ~class_:"mt-0.5" g ~value:v
            ; div "flex flex-col gap-0.5" [ span "text-[13px] font-medium" [ Node.text title ]; span "text-xs text-muted-foreground" [ Node.text desc ] ]
            ]))
    ; Input.view ~class_:"h-8 text-base md:text-sm" ~placeholder:"Or answer in your own words…" ()
    ; div "flex items-center justify-end gap-1.5"
        [ Button.view ~variant:Ghost ~size:Sm ~class_:"text-muted-foreground" [ Node.text "Skip"; kbd "Esc" ]
        ; Button.view ~size:Sm [ Node.text "Submit"; kbd "↵" ]
        ]
    ]
;;

let banner =
  div "px-4 pt-2"
    [ Alert.root ~class_:"mx-auto max-w-[760px] border-amber-500/30 bg-amber-500/8 py-2 text-amber-200 *:data-[slot=alert-description]:text-amber-200/70"
        [ Icon.view Refresh_cw
        ; Alert.title [ Node.text "Reconnecting to desktop…" ]
        ; Alert.description [ Node.text "Lost the connection 12s ago. Retrying in 3s; your message is kept." ]
        ; Alert.action [ Button.view ~variant:Outline ~size:Xs [ Node.text "Retry now" ] ]
        ]
    ]
;;

let component ~tip (local_ graph) =
  let toaster = Sonner.toaster graph in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (Bonsai.return
         (Sonner.toast ~duration:Sonner.infinity ~description:"“Flaky tmux-test on posix backend”"
            ~action:("Undo", Effect.Ignore) "Session deleted"))
    graph;
  let overlay = toaster in
  Shell.component ~overlay ~tip ~main:(Screen_chat.component ~banner:(Bonsai.return banner) ~above_composer:(Bonsai.return confirm_card) ~queued:[] ~tip graph) graph
;;

let question ~tip (local_ graph) =
  let choice, set_choice = Bonsai.state "sheet" graph in
  let main =
    let card =
      let%arr choice and set_choice in
      question_card ~choice ~set_choice
    in
    Screen_chat.component ~above_composer:card ~queued:[] ~tip graph
  in
  Shell.component ~tip ~main graph
;;
