open! Core

(** Intents exercised by the scenario tests. A global mutable set, populated by
    the [H] harness in [test_app.ml], then reported at the bottom of that file
    so every keymap binding has a scenario. *)
val hit : Prigh_ui.Intent.t Hash_set.t

val record : Prigh_ui.Intent.t -> unit
val record_key : Prigh_ui.Key.t -> unit
val covered : Prigh_ui.Intent.t -> bool
