open! Core

(** The theme on the document ([data-theme], [data-accent] and a custom
    accent's variables on [<html>]), kept for the next page load's first
    paint ([Pref_cache.save_theme]), and on the terminal. *)

val apply : Prigh_web.Theme.t -> unit
