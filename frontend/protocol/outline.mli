open! Core

(** A session's landmarks ([get_outline]): the user's messages, compactions
    and replies that ended with an error, also where no page of it is
    loaded. *)

module Kind : sig
  type t =
    | User
    | Compaction
    | Error
  [@@deriving sexp_of, equal]
end

module Point : sig
  type t =
    { index : int (** the message's position, as {!Message_page.older} counts *)
    ; kind : Kind.t
    ; text : string (** at most 240 bytes *)
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]
end

type t = Point.t list [@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
