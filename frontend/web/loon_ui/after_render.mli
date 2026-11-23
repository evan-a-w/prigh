(** Running DOM code on an element after the page shows it: measuring it,
    or setting styles from measurements. *)

open! Import
open Js_of_ocaml

(** Calls [f] with the element once it is in the page, then after every
    render that keeps it (in the next animation frame). [name] tells
    hooks apart: an element takes one hook of each name. *)
val attr : name:string -> (Dom_html.element Js.t -> unit) -> Attr.t

(** The window's [setTimeout] and [clearTimeout] (nothing outside a
    browser): [schedule key ms effect] runs [effect] after [ms]
    milliseconds unless [cancel key] (or another [schedule key]) comes
    first. *)
val schedule : string -> int -> unit Effect.t -> unit Effect.t

val cancel : string -> unit Effect.t

(** The element with this id, in a browser. *)
val by_id : string -> Dom_html.element Js.t option

val in_browser : unit -> bool
