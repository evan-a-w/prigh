open! Core

(** What a key in the prompt editor does (the rest is typing). *)

type t =
  { key : string (** [KeyboardEvent.key] *)
  ; shift : bool
  ; alt : bool
  ; ctrl : bool
  ; meta : bool
  }

val editor_action : t -> running:bool -> App.Action.t option
