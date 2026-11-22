(** What every component opens: Core, Vdom, Bonsai and small helpers. *)

include Core
module Vdom = Virtual_dom.Vdom
module Node = Vdom.Node
module Attr = Vdom.Attr
module Effect = Bonsai_web.Effect
module Bonsai = Bonsai_web.Bonsai

(** The component's classes, then the caller's attributes (whose classes and
    styles merge with ours). *)
let attrs ?(attrs = []) classes = Attr.classes classes :: attrs

let data name value = Attr.create ("data-" ^ name) value
let aria name value = Attr.create ("aria-" ^ name) value
let aria_bool name b = aria name (Bool.to_string b)
