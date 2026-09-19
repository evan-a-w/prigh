open! Core

module User : sig
  type t = { text : string } [@@deriving sexp, jsonaf, equal]
end

module Assistant : sig
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    }
  [@@deriving sexp, jsonaf, equal]

  val text : t -> string
  val thinking : t -> string
  val tool_calls : t -> Content.Tool_call.t list
end

module Tool_result : sig
  type t =
    { tool_call_id : string
    ; tool_name : string
    ; text : string
    ; is_error : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp, jsonaf, equal]

val user : string -> t
