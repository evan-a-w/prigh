open! Core

(** What the harness puts into a user message, recorded with the message so
    that frontends can show it as a note rather than as something the user
    wrote. A message's [text] is always exactly what the model sees: this
    only says how it is made up, so providers and prompt caches are not
    affected. *)

module Note : sig
  (** Prefixed to the next user message when something changed between turns
      ([Told_environment.notes]). *)
  module Kind : sig
    type t =
      | Host of { name : string }
      | Cwd of { dir : string }
      | Tools of
          { added : string list
          ; removed : string list
          }
      (** a model change changed the tools ([System_prompt.tools_changed_note]) *)
      | Editor of
          { before : string
          ; after : string
          }
      (** sessions from before [Tools]: the file editor changed (wire [tools]) *)
      | Plan_mode of { on : bool }
      (** plan mode turned on or off ([System_prompt.plan_mode_note]), also
          as a message of its own between turns *)
    [@@deriving sexp, jsonaf, equal]
  end

  type t =
    { kind : Kind.t
    ; text : string (** exactly as prefixed *)
    }
  [@@deriving sexp, jsonaf, equal]

  (** One line for the user, e.g. [Tools now run on laptop]. *)
  val title : t -> string
end

module Attachment : sig
  (** An [@path] file appended as a [<file>] block of [length] bytes. *)
  type t =
    { path : string
    ; length : int
    }
  [@@deriving sexp, jsonaf, equal]
end

module Source : sig
  (** A message the harness writes in the user's place. *)
  type t =
    | Delivery of { reports : string list }
    (** finished background work; [reports] are the reports' header lines
        without their brackets, e.g. [job j1 exited 0] *)
    | Handover of
        { from : string
        ; to_ : string
        ; error : string
        }
    | Goal_start of { goal : string }
    | Goal_continuation of
        { continuation : int
        ; max : int
        }
    | Resume (** after a usage limit reset *)
    | Shell of { command : string } (** [!cmd]'s output *)
    | Summary (** the compaction summary standing for older messages *)
    | Review of { target : string } (** [/review] *)
    | Init (** [/init] *)
  [@@deriving sexp, jsonaf, equal]

  (** The wire name: [delivery], [handover], [goal], [goal_continuation],
      [resume], [shell], [summary], [review], [init]. *)
  val kind : t -> string

  val title : t -> string
end

type t =
  { notes : Note.t list
  ; attachments : Attachment.t list
  ; source : Source.t option
  }
[@@deriving sexp, jsonaf, equal]

val empty : t
val is_empty : t -> bool

(** [text] with [notes] in front, separated by a blank line. *)
val prefix_notes : Note.t list -> string -> string

(** What the user wrote of [text], a message made up as [t] says: without the
    notes in front and the attached files behind (empty for a message of
    notes only); all of it for a message of the harness's. *)
val body : t -> string -> string

(** Recognises the harness's text in messages recorded without [t] (sessions
    from before it): environment and plan mode notes in front (or making up
    the whole message), [<file>] blocks behind, and whole messages (reports
    of background work, hand-overs, goals, the resume after a usage limit,
    the compaction summary), possibly after notes. [None] for a plain
    message. *)
val infer : string -> t option

(** Recorded, else inferred. *)
val of_message : origin:t option -> string -> t option

(** The RPC's [origin] object: [source: {kind, title}] for a message of the
    harness's, otherwise [body] (what the user wrote), with [notes: [{kind,
    title, text}]] and [attachments: [{path}]] when there are any. *)
val to_json : t -> text:string -> Jsonaf.t
