open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let view ?(class_ = "") (status : Fake.Status.t) ~age =
  let cls, text =
    match status with
    | Thinking -> "thinking-shimmer text-foreground/80", "thinking…"
    | Working -> "thinking-shimmer text-foreground/80", "working…"
    | Idle -> "tabular-nums", age
  in
  Node.span
    ~attrs:[ Attr.class_ (Cn.cn [ "shrink-0"; cls; class_ ]) ]
    [ Node.text text ]
;;
