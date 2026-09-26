open! Core

module Kind : sig
  type t =
    | Message of Message.t
    | Model of
        { model : string
        ; thinking : string
        }
    | Compaction of
        { summary : string
        ; kept_from : string
        }
    | Name of { name : string }
    | Description of { text : string }
    | Cwd of { cwd : string }
    | System_prompt
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; parent : string option
  ; kind : Kind.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
