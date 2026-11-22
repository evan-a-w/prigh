open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

type status = Running | Waiting | Done | Failed

let badge = function
  | Running ->
    Badge.view ~variant:Secondary ~class_:"gap-1 bg-sky-500/15 text-sky-300"
      [ Spinner.view ~class_:"size-3" (); Node.text "Running" ]
  | Waiting -> Badge.view ~variant:Secondary ~class_:"bg-amber-500/15 text-amber-300" [ Node.text "Needs you" ]
  | Done -> Badge.view ~variant:Secondary ~class_:"bg-emerald-500/15 text-emerald-300" [ Node.text "Done" ]
  | Failed -> Badge.view ~variant:Secondary ~class_:"bg-red-500/15 text-red-300" [ Node.text "Failed" ]
;;

let meta parts =
  div "flex items-center gap-1.5 text-[11px] tabular-nums text-muted-foreground/80"
    (List.intersperse ~sep:(span "opacity-50" [ Node.text "·" ]) (List.map parts ~f:(fun p -> span "" [ Node.text p ])))
;;

let agent ?(expanded = []) ~name ~task ~status ~meta:m () =
  div
    (Cn.cn [ "flex flex-col gap-1.5 rounded-lg border px-3 py-2.5"; (if List.is_empty expanded then "" else "bg-card") ])
    ([ div "flex items-center gap-2"
         [ Icon.view ~class_:"size-3.5 text-muted-foreground" Bot
         ; span "truncate text-[13px] font-medium" [ Node.text name ]
         ; div "ml-auto" [ badge status ]
         ]
     ; div "line-clamp-2 text-xs leading-relaxed text-muted-foreground" [ Node.text task ]
     ; meta m
     ]
     @ expanded)
;;

let activity =
  [ Separator.view ~class_:"my-1" ()
  ; div "text-[11px] font-medium uppercase tracking-wide text-muted-foreground/70" [ Node.text "Latest" ]
  ; div "flex flex-col gap-1 font-mono text-[11.5px] text-muted-foreground"
      [ div "flex items-center gap-2" [ Icon.view ~class_:"size-3 text-emerald-400" Check; span "truncate" [ Node.text "edit  composer.ml (+48 −21)" ] ]
      ; div "flex items-center gap-2" [ Icon.view ~class_:"size-3 text-emerald-400" Check; span "truncate" [ Node.text "bash  dune build ./web/mockup" ] ]
      ; div "flex items-center gap-2 text-foreground/90" [ Spinner.view ~class_:"size-3 text-sky-400" (); span "truncate" [ Node.text "bash  node e2e/mobile.mjs --width 320" ] ]
      ]
  ; div "flex items-center gap-1.5 pt-1"
      [ Button.view ~variant:Outline ~size:Xs [ Icons.view Arrow_up_right; Node.text "Open" ]
      ; Button.view ~variant:Ghost ~size:Xs [ Icon.view Message_square; Node.text "Message" ]
      ; Button.view ~variant:Ghost ~size:Xs ~class_:"ml-auto text-red-400 hover:text-red-300" [ Icon.view Square; Node.text "Stop" ]
      ]
  ]
;;

let section title count = div "flex items-center gap-2 px-1 pt-2 text-xs font-medium text-muted-foreground" [ Node.text title; span "tabular-nums opacity-70" [ Node.textf "%d" count ] ]

let panel ~tip =
  Panel.view
    ~title:[ span "font-medium" [ Node.text "Agents" ]; Badge.view ~variant:Secondary ~class_:"h-4 px-1.5 text-[10px]" [ Node.text "3 running" ] ]
    ~actions:[ Panel.icon_button tip ~id:"agents-new" "New agent" (Icon.view Plus); Panel.icon_button tip ~id:"agents-close" "Close" (Icon.view X) ]
    [ div "flex min-h-0 flex-1 flex-col gap-2 overflow-y-auto p-3"
        [ section "Running" 3
        ; agent ~name:"Fix chip touch targets" ~task:"Make the composer chips 44px tall on phones and rerun e2e/mobile.mjs at 320px and 390px." ~status:Running ~meta:[ "2m 14s"; "$0.38"; "Sonnet 4.5"; "worktree" ] ~expanded:activity ()
        ; agent ~name:"Review composer diff" ~task:"Review feat/composer-chips against main: correctness, phone layout, and the e2e." ~status:Running ~meta:[ "48s"; "$0.09"; "Opus 4.5" ] ()
        ; agent ~name:"Port Resizable keyboard" ~task:"Arrow keys and Home/End on the handle; tests in test_resizable.ml." ~status:Waiting ~meta:[ "5m 02s"; "$0.61"; "desktop" ] ()
        ; section "Previous agents" 2
        ; agent ~name:"Inventory prigh-web UI" ~task:"List every surface and its loon match." ~status:Done ~meta:[ "49s"; "$0.26" ] ()
        ; agent ~name:"Flaky tmux test" ~task:"Find why tmux-test hangs on the posix backend." ~status:Failed ~meta:[ "12m"; "$1.12"; "out of turns" ] ()
        ]
    ]
;;

let component ~tip (local_ graph) =
  Shell.component ~right:(Bonsai.map tip ~f:(fun tip -> panel ~tip)) ~right_size:32. ~right_title:"Agents" ~tip ~main:(Screen_chat.component ~tip graph) graph
;;
