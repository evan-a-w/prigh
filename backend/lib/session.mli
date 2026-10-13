open! Core
open! Import

(** Append-only JSONL session log forming a tree of entries. The active
    conversation is the path from the root to [head]; rewinding moves [head]
    to an earlier entry so new messages branch from there. *)

(** The tool host a session runs on (see [Agent]). *)
module Host : sig
  type t =
    { id : string
    ; name : string (** as last seen, for when it is disconnected *)
    ; pinned : bool (** chosen explicitly rather than by default *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Entry : sig
  module Payload : sig
    type t =
      | Message of Message.t
      | Model of
          { model : string
          ; thinking : Thinking.t
          }
      | Compaction of
          { summary : string
          ; kept_from : string
            (** id of the first entry retained after the summary *)
          ; pinned : string option
            (** a message before [kept_from] kept verbatim ahead of the
                summary: the user's latest, when the compaction falls in the
                middle of the work on it *)
          }
      | Name of { name : string }
      | Description of { text : string }
      (** model-written one-line summary, see [Session_description] *)
      | Cwd of
          { cwd : string
          ; host : Host.t option
            (** where [cwd] is: from here on the session runs there;
                absent before a session has a host, and in files written
                before hosts were recorded *)
          }
      | System_prompt of { text : string }
      | Goal of { goal : Goal.t option } (** [None]: cleared *)
      | Budget of { usd : float option }
      (** the session's cost budget; [None]: none, overriding the default *)
      | Approval of
          { mode : string (** an [Approval_mode] *)
          ; plan : bool
          } (** the session's approval mode and whether it is in plan mode *)
      | Allow_rule of { rule : string }
      (** an [Approval_rule] the user allowed for the session *)
      | Archived of { archived : bool }
      (** archived or unarchived by hand (see {!Summary.is_archived}); not
          copied by [fork] *)
      | Spend of
          { usage : Usage.t
          ; cost_usd : float
          }
      (** spend the messages do not record: a subagent's response, a
          compaction, a [btw] answer *)
      | Resume_at of { at : float option }
      (** a run scheduled to resume when a usage limit resets (seconds since
          the epoch); [None]: no longer *)
      | Told_environment of Told_environment.t
      (** what the model was last told about its environment, see
          {!told_environment} *)
    [@@deriving sexp, jsonaf]
  end

  type t =
    { id : string
    ; parent : string option
    ; at : float option
      (** when it was appended (seconds since the epoch); absent in files
          written before entries recorded it *)
    ; payload : Payload.t
    }
  [@@deriving sexp, jsonaf]
end

type t

(** Nothing is written until the session has a message or a name; an
    abandoned empty session leaves no file. *)
val create : dir:string -> cwd:string -> ?parent:string -> unit -> t

(** Lines it cannot read (a torn last line, kinds of entries from a newer
    prigh) are skipped, with a warning each in {!skipped}; the entries after
    an unreadable entry stay on the path through it, and the next append
    starts on a new line. Fails only without a readable header. *)
val load : string -> t Or_error.t

(** Why [load] skipped lines, one warning each. *)
val skipped : t -> string list

(** Whether its file was written and has been deleted since. *)
val gone : t -> bool

(** Whether the file at [path] exists yet. *)
val persisted : t -> bool

val id : t -> string
val path : t -> string
val cwd : t -> string

(** The host of the last [cwd] entry that has one. *)
val host : t -> Host.t option

(** The last [cwd] on each host, by [Host.id]. *)
val host_cwds : t -> string String.Map.t

val parent : t -> string option
val head : t -> string option
val entries : t -> Entry.t list
val created_at : t -> string
val updated_at : t -> string

(** Seconds from [created_at] to the last modification (or now, when the file
    has not been written since creation). *)
val duration_seconds : t -> float

(** Last [name] entry written to the file. *)
val name : t -> string option

(** Last [description] entry written to the file. *)
val description : t -> string option

(** Entries on the active path, root first. *)
val active_path : t -> Entry.t list

(** Messages for the next model request: those on the active path, with a
    compaction summary (if any) replacing everything before [kept_from],
    preceded by the compaction's pinned message. *)
val messages : t -> Message.t list

(** The entry id of each of [messages] ([None] for a compaction summary). *)
val context_ids : t -> string option list

(** A compaction summary as it appears in [messages]. *)
val summary_message : string -> Message.t

val is_summary_message : Message.t -> bool

(** [messages] with the times they were appended (none for the summary). *)
val timed_messages : t -> Timed_message.t list

(** [timed_messages], each with the id of its entry ([None] for the
    summary). *)
val identified_messages : t -> (string option * Timed_message.t) list

val model : t -> (string * Thinking.t) option

(** The system prompt fixed for this conversation, if one was recorded on the
    active path. *)
val system_prompt : t -> string option

val set_system_prompt : t -> text:string -> Entry.t

(** The last [goal] entry on the active path. *)
val goal : t -> Goal.t option

val set_goal : t -> Goal.t option -> Entry.t

(** The last [budget] entry on the active path: [None] when there is none
    (the default applies), [Some None] when it was turned off. *)
val budget : t -> float option option

val set_budget : t -> usd:float option -> Entry.t

(** The last [approval] entry on the active path. *)
val approval : t -> (string * bool) option

val set_approval : t -> mode:string -> plan:bool -> Entry.t

(** The last [told_environment] entry on the active path: what the model
    was last told about its environment. [None] in sessions from before it
    was recorded. *)
val told_environment : t -> Told_environment.t option

val set_told_environment : t -> Told_environment.t -> Entry.t

(** The [allow_rule] entries on the active path, oldest first. *)
val allow_rules : t -> string list

val add_allow_rule : t -> rule:string -> Entry.t

(** [at] defaults to now. *)
val append_message : t -> ?at:float -> Message.t -> Entry.t

val set_model : t -> model:string -> thinking:Thinking.t -> Entry.t
val set_name : t -> name:string -> Entry.t
val set_description : t -> text:string -> Entry.t
val set_archived : t -> archived:bool -> Entry.t
val add_spend : t -> usage:Usage.t -> cost_usd:float -> Entry.t

(** The [Spend] entries' total, on every branch. *)
val spend : t -> Usage.t * float

(** The last [Resume_at] entry's time, on any branch. *)
val resume_at : t -> float option

(** Appends a [Resume_at] entry unless [resume_at] already says so. *)
val set_resume_at : t -> float option -> unit

(** Records a [cwd] entry; a subsequent [load] restores it. *)
val set_cwd : t -> ?host:Host.t -> cwd:string -> unit -> Entry.t

val append_compaction
  :  ?pinned:string
  -> t
  -> summary:string
  -> kept_from:string
  -> Entry.t

val rewind : t -> to_:string -> unit Or_error.t

(** Copy [src_path] into [dir] under a fresh stamp. The session id is kept
    unless it already exists in [dir]. *)
val import : dir:string -> string -> t Or_error.t

(** A new session in [dir] containing a copy of the active path up to and
    including [at] (default: head). *)
val fork : ?at:string -> t -> dir:string -> t Or_error.t

module Export_format : sig
  type t =
    | Markdown
    | Jsonl
  [@@deriving sexp_of]

  val of_string : string -> t Or_error.t
  val extension : t -> string
end

(** Markdown transcript of the active path; message headings carry their
    time (UTC) when known. *)
val to_markdown : t -> string

module Summary : sig
  type t =
    { id : string
    ; path : string
    ; name : string option
    ; description : string option
    ; cwd : string
    ; created_at : string
    ; updated_at : string
    ; first_prompt : string option
    ; message_count : int
    ; parent : string option
    ; archived : bool option
      (** the last [Archived] entry, when it comes after the last message *)
    ; last_activity : float
      (** seconds since the epoch: the last message's time (else the file's
          mtime), or a later unarchiving *)
    ; resume_at : float option (** {!resume_at} *)
    ; bytes : int (** {!disk_usage} *)
    }
  [@@deriving sexp_of]

  (** Never while [active] (a run or background work going on); else
      archived by hand, or automatically when the last activity is more than
      [archive_after_days] days before [now] (seconds; [0] days: never). *)
  val is_archived
    :  t
    -> now:float
    -> archive_after_days:float
    -> active:bool
    -> bool
end

(** Most recently updated first. Files are only parsed again when their size
    or mtime changed since the last call. *)
val list : dir:string -> Summary.t list

(** A session's own directory next to its file, [<stamp>_<id>/] for
    [<stamp>_<id>.jsonl], for per-session data (it may not exist). *)
val data_dir : string -> string

(** The session file [path] and its {!data_dir}, those that exist. *)
val files : string -> string list

(** Bytes used by {!files}. *)
val disk_usage : string -> int

(** Removes {!files}. *)
val delete_files : string -> unit

(** The summary whose id is [key], else the only one whose id starts with
    it; errors say whether none or several matched. *)
val find_summary : Summary.t list -> string -> Summary.t Or_error.t

(** Which saved sessions [prune] should delete; every given field must
    match. [cwd] is compared verbatim, so resolve it first. *)
module Filter : sig
  type t =
    { max_messages : int option
    ; cwd : string option
    ; older_than_days : float option
    ; prompt : string option (** substring of the first prompt *)
    }
  [@@deriving sexp_of]

  val matches : t -> now:Time_float.t -> Summary.t -> bool
end

(** Removes the session's files. *)
val delete : Summary.t -> unit

val default_dir : home:string -> string
