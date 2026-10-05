open! Core

(** The result of [hello] (and of [set_user]): our client id and, when the
    backend has users ([prigh serve -tokens]), the one whose sessions we see
    ([namespace]) and the one we signed in as ([user]); they differ after a
    superuser's [/setusr]. *)
type t =
  { client_id : string
  ; namespace : string option
  ; user : string option
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [namespace] when it is not [user]'s. *)
val acting_as : t -> string option
