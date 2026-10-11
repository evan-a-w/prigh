open! Core

(** One question of an [ask_user] call. *)

module Choice : sig
  type t =
    { label : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { question : string
  ; header : string option
  ; choices : Choice.t list
  ; multiple : bool (** several choices may be picked *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
