open! Core

(** Maps a terminal key press to the platform-neutral key. *)
val key : Bonsai_term.Event.t -> Prigh_ui.Key.t option
