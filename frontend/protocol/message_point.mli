open! Core

(** A message of the client's session's context, with what trimming before
    it, deleting it or trimming after it takes out ([get_message_points]). *)

module Role : sig
  type t =
    | User
    | Assistant
    | Tool_result
  [@@deriving sexp_of, equal]
end

type t =
  { message : string (** the entry id, for {!Request.Delete}, {!Request.Trim_after} *)
  ; index : int (** its position, as {!Outline.Point.index} counts *)
  ; role : Role.t
  ; text : string (** its start: what was written, or the tool calls *)
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
  ; trim_before : Trim.Freed.t option [@sexp.option]
    (** what trimming before it takes out; [None] for the first message, a
        tool result, and where a call would be parted from its result *)
  ; delete : Trim.Freed.t option [@sexp.option]
    (** what deleting it takes out; [None] for a tool result *)
  ; trim_after : Trim.Freed.t option [@sexp.option]
    (** what trimming after it takes out; [None] where that is refused *)
  }
[@@deriving sexp_of, equal]

(** [get_message_points]' answer, oldest first. *)
val list_of_json : Json.t -> t list Or_error.t
