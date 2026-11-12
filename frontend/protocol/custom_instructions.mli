open! Core

(** The user's custom instructions: standing instructions in the context of
    every session, which the model may also change when the user lets it
    ([get_custom_instructions]' reply and the [custom_instructions_changed]
    event). *)

(** Ids are [ci1], [ci2], ... *)
module Note = Memory.Note

module Limits : sig
  type t =
    { text : int (** bytes of one *)
    ; title : int
    ; count : int (** how many there can be *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { enabled : bool (** the model may change them *)
  ; notes : Note.t list (** oldest first *)
  ; limits : Limits.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** About how many tokens they add to every session's context (bytes / 4,
    with the heading and ids as the model sees them). *)
val tokens : Note.t list -> int

(** [save_custom_instruction]: edits [id] (without a [title], removing
    it), or adds one. Its reply is [{note}]. *)
val save_params
  :  ?id:string
  -> ?title:string
  -> string
  -> (string * Json.t) list

val note_of_reply : Json.t -> Note.t Or_error.t

(** [remove_custom_instruction] *)
val remove_params : id:string -> (string * Json.t) list

(** [set_custom_instructions_enabled] *)
val set_enabled_params : bool -> (string * Json.t) list
