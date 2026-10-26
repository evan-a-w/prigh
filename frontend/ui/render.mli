open! Core

(** Lays out the whole frame from the model (see [Layout]). Pure. *)
val screen : App.Model.t -> Screen.t

(** What a left click on the cell at [row], [col] of [screen] does: the jump
    indicator, shown while the transcript is scrolled back, goes to the
    bottom. *)
val click : App.Model.t -> row:int -> col:int -> Intent.t option

(** The status bar alone (also used by tests). *)
val status : App.Model.t -> Content.Line.t

val format_tokens : int -> string
