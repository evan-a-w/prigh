open! Core

(** How a user message is made up when the harness added to it (the backend's
    [Injected]): environment notes in front of the user's text, attached
    files behind it, or a message the harness wrote in the user's place. The
    message's [text] stays exactly what the model saw. *)

module Kind : sig
  type t =
    | Host (** tools now run on another host *)
    | Cwd (** another working directory *)
    | Tools
    (** a model change added or removed tools ([editor] from older backends) *)
    | Plan_mode (** plan mode turned on or off *)
    | Delivery (** reports of finished subagents and jobs *)
    | Handover (** another model takes over *)
    | Goal (** [/goal] starts *)
    | Goal_continuation
    | Reminder (** one of the session's reminders ([/remind]) *)
    | Resume (** after a usage limit reset *)
    | Shell (** [!cmd]'s output added to the context *)
    | Summary (** the compaction summary *)
    | Review (** [/review] *)
    | Init (** [/init] *)
    | Review_comments (** the user's review comments, sent together *)
    | Other of string
  [@@deriving sexp_of, equal]

  val of_string : string -> t
end

module Note : sig
  type t =
    { kind : Kind.t
    ; title : string (** one line for the user *)
    ; text : string (** exactly what the model saw *)
    }
  [@@deriving sexp_of, equal]
end

module Source : sig
  type t =
    { kind : Kind.t
    ; title : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { body : string option (** what the user wrote; [None] with a [source] *)
  ; notes : Note.t list
  ; attachments : string list (** paths *)
  ; source : Source.t option (** the whole message is the harness's *)
  }
[@@deriving sexp_of, equal]

val empty : t
val of_json : Json.t -> t Or_error.t
