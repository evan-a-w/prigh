open! Core

(** Lays out the whole frame from the model. Pure. *)
val screen : App.Model.t -> Screen.t

(** The status line alone (also used by tests). *)
val status : App.Model.t -> Content.Line.t

val format_tokens : int -> string
