open! Core

module User : sig
  type t =
    { text : string
    ; images : Image.t list
    ; origin : Injected.t option
      (** how [text] is made up, when the harness added to it; [None] for the
        user's own text and for messages recorded before it *)
    }
  [@@deriving sexp, jsonaf, equal]

  (** What the user wrote: [text] without the harness's notes and attached
      files (recorded or recognised). *)
  val body : t -> string
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
    ; images : Image.t list
    }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp, jsonaf, equal]

val user : ?images:Image.t list -> ?origin:Injected.t -> string -> t

(** Each image becomes a line of text saying it was left out, for models that
    cannot see images. *)
val omit_images : t -> t
