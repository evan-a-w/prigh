open! Core

(** What the user wrote about themselves, given to the model in every
    session ([get_user_info]'s and [set_user_info]'s reply and the
    [user_info_changed] event). *)

type t =
  { text : string (** empty: none *)
  ; max : int (** the longest text the backend takes, in bytes *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [set_user_info]; an empty text removes it. *)
val set_params : string -> (string * Json.t) list
