(** Small DOM effects the components share: moving focus and reading keys. *)

open! Import
open Js_of_ocaml

(** The key a keyboard event is for (its [code], or its [keyCode] where it
    has none). *)
val key : Dom_html.keyboardEvent Js.t -> Dom_html.Keyboard_code.t

(** Focuses the first element in the document matching [selector] (after
    the next render, so that it may be one this render creates). *)
val focus_selector : string -> unit Effect.t

(** Focuses the first element matching [selector] at once (moving focus off
    an element before the render that follows). *)
val focus_selector_now : string -> unit Effect.t

(** Stops the event's default action (e.g. scrolling on an arrow key). *)
val prevent_default : #Dom_html.event Js.t -> unit Effect.t

(** Stops the event's default action and its propagation. *)
val stop : #Dom_html.event Js.t -> unit Effect.t
