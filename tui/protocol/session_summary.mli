open! Core

type t =
  { id : string
  ; path : string
  ; name : string option
  ; description : string option
  ; cwd : string
  ; created_at : string
  ; updated_at : string option
  ; first_prompt : string option
  ; message_count : int
  ; parent : string option
  ; live : bool (** loaded in the backend *)
  ; running : bool
  ; clients : int (** frontends attached to it *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** One line for lists: the description if there is one, else the first prompt,
    else ["(empty)"]. *)
val blurb : t -> string
