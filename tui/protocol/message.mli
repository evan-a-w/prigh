open! Core

module Assistant : sig
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Tool_result : sig
  type t =
    { tool_call_id : string
    ; tool_name : string
    ; text : string
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

type t =
  | User of string
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
