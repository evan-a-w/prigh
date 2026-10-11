open! Core

(** Older messages load as the user nears the top of the transcript (where
    the page marks a button with [data-older]), and stay out of the way once
    added above: what showed stays in place ([keep_position]). A click on an
    [Output_view.load_attribute] button fetches an elided output whole. *)

val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** Before the older messages are added: the next change of the
    transcript's height goes above what shows. *)
val keep_position : unit -> unit
