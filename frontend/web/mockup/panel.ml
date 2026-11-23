open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let view ?(class_ = "") ~title ~actions body =
  div
    (Cn.cn [ "flex h-full min-h-0 flex-col bg-background text-sm"; class_ ])
    [ div
        "flex h-10 shrink-0 items-center gap-2 border-b px-3"
        [ div "flex min-w-0 flex-1 items-center gap-2" title
        ; div "flex shrink-0 items-center gap-0.5" actions
        ]
    ; div "flex min-h-0 flex-1 flex-col" body
    ]
;;

let icon_button tip ~id label icon =
  Tip.wrap
    tip
    ~id
    label
    (Button.view
       ~variant:Ghost
       ~size:Icon_xs
       ~class_:"text-muted-foreground hover:text-foreground"
       ~attrs:[ Attr.create "aria-label" label ]
       [ icon ])
;;

let counts added removed =
  span
    "flex shrink-0 items-center gap-1.5 font-mono text-[11px] tabular-nums"
    [ span "text-emerald-600 dark:text-emerald-400" [ Node.textf "+%d" added ]
    ; span "text-red-600 dark:text-red-400" [ Node.textf "−%d" removed ]
    ]
;;
