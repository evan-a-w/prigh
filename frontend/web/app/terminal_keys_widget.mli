open! Core
open Js_of_ocaml

(** The terminal's extra keys ([Prigh_web.Terminal_keys]) as a bar of
    buttons, under the shell and over a phone's keyboard (terminal.css shows
    it on touch screens while the shell has the focus). A tap acts on
    release (a swipe scrolls the bar instead), an arrow held repeats, and no
    key takes the focus from the shell, so the phone's keyboard stays up. *)

type t

(** [press] sends bytes to the shell as typed; [app_cursor] is its cursor
    keys' mode; [refocus] gives the shell back the focus if it lost it. *)
val create
  :  press:(string -> unit)
  -> app_cursor:(unit -> bool)
  -> refocus:(unit -> unit)
  -> t

val element : t -> Dom_html.element Js.t

(** Text typed on the keyboard with the latched Ctrl and Alt applied. *)
val transform : t -> string -> string
