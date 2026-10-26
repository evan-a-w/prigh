open! Core
open! Import

(** The ✦ Generate button in a description field's corner ({!Generate}):
    Stop while its draft streams in, and Undo once one replaced the text. *)
val button
  :  ?label:string (** what it drafts, e.g. "Generate title and description" *)
  -> Generate.t
  -> Generate.Target.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
