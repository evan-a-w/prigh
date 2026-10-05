open! Core

module User : sig
  type t =
    { text : string
    ; images : Image.t list [@sexp.list]
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Assistant : sig
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
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
    ; images : Image.t list [@sexp.list]
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

(** [at]: when the backend recorded the message (absent for older sessions'
    messages, the compaction summary, and streaming partials); sexps show it
    in UTC. *)
type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
