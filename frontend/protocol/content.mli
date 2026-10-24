open! Core

(** A tool the provider runs itself (web search): no tool call or result, just
    a block in the assistant's content. *)
module Server_tool : sig
  type t =
    { id : string
    ; name : string
    ; input : string (** JSON text, like a tool call's arguments *)
    ; output : string option (** [None] while it runs *)
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Text of string
  | Thinking of string
  | Tool_call of Tool_call.t
  | Server_tool of Server_tool.t
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
