open! Core

(** What [search_sessions] found in the sessions' messages: per session, how
    well it matched and the passage that matched best. *)

module Snippet : sig
  type t =
    { text : string
    ; ranges : (int * int) list
      (** [(start, length)] in bytes of [text]: the matched words, in order *)
    }
  [@@deriving sexp_of, equal]
end

module Hit : sig
  type t =
    { id : string
    ; path : string
    ; score : int
    ; snippet : Snippet.t option (** [None] when only its name matched *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { query : string (** the query searched, as sent *)
  ; hits : Hit.t list (** best first *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
val params : query:string -> limit:int -> (string * Json.t) list
