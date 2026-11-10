open! Core

(** The user's memory: the notes the model saves when asked to remember
    something ([get_memory]'s reply and the [memory_changed] event), and
    whether it is on. *)

module Note : sig
  type t =
    { id : string
    ; title : string option
    ; text : string
    ; saved : string (** UTC, [YYYY-MM-DD HH:MM:SS...Z] *)
    }
  [@@deriving sexp_of, equal]

  (** [saved]'s date, [YYYY-MM-DD]. *)
  val date : t -> string
end

type t =
  { enabled : bool
  ; notes : Note.t list (** oldest first *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [save_memory]: edits note [id], or adds one (replacing the note with
    the same title). Its reply is [{note}]. *)
val save_params
  :  ?id:string
  -> ?title:string
  -> string
  -> (string * Json.t) list

val note_of_reply : Json.t -> Note.t Or_error.t

(** [forget_memory] *)
val forget_params : id:string -> (string * Json.t) list

(** [set_memory_enabled] *)
val set_enabled_params : bool -> (string * Json.t) list
