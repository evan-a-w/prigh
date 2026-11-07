open! Core
open! Import

(** A [render_ui] call in the transcript: its components as the page shows
    them ({!Ui_spec}), or a skeleton while its arguments stream in. *)

(** The call's components, parsed once per call (and again when its
    arguments change); [None] while they do not parse. *)
val spec : Tool_call.t -> Ui_spec.t option

val view : Ui_context.t -> Ui_spec.t -> Node.t

(** What shows while the arguments arrive. *)
val skeleton : unit -> Node.t
