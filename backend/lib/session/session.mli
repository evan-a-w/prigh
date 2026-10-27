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

(** Where a session was before a worktree switch moved it into a worktree
    ({!home}), for switching it back. *)
module Home : sig
  type t =
    { cwd : string
    ; host : Host.t option (** [None]: the session had no host yet *)
    ; repo : Git_location.t option
      (** where [cwd] is in git, when known: its branch names it *)
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
          ; pinned : Pinned_messages.t
            (** messages before [kept_from] carried ahead of the summary:
                the user's own, and their latest as it is, when the
                compaction falls in the middle of the work on it *)
          }
      | Masked of { through : string }
      (** observation masking ({!Observation_mask}): the messages of the
          active path up to the entry [through] are sent masked from here
          on; the last one on the path counts *)
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
      | Reminders of { reminders : Reminder.t list }
      (** the session's reminders from here on, all of them *)
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
      | Review of Review_state.Change.t
      (** a change to the review of the working tree: a comment, an
          approval; see {!review} *)
      | Repo of
          { cwd : string
          ; repo : Git_location.t option (** [None]: not in a repository *)
          } (** where [cwd] is in git, found on its host (see {!repo}) *)
      | Agent_worktree of Agent_worktree.t
      (** a worktree made for one of its subagents; not copied by [fork] *)
      | Worktree of Session_worktree.t
      (** a worktree it made or checked a pull request out in (those it
          moves into are its [Repo] entries') *)
      | Worktree_removed of { path : string }
      (** the worktree at [path] was removed: none of its worktrees
          ({!worktrees}) any more *)
      | Home of { home : Home.t option }
      (** where a worktree switch took it from; [None]: cleared (it went
          back, or elsewhere). Like [Cwd], the last one on any branch
          counts. *)
      | Trim of { before : string }
      (** from here on the model's context starts at the message [before]
          (one of the user's): what comes before it is dropped (see
          {!messages}) *)
      | Untrim (** cancels the latest [Trim] or [Trim_after] in effect *)
      | Trim_after of
          { after : string
          ; through : string
          }
      (** from here on the messages after [after] up to [through] (the
          newest when it was made) are dropped from the model's context;
          those that come later stay in it *)
      | Delete of { message : string }
      (** from here on the message [message] is out of the model's context,
          and so are the results of its tool calls *)
      | Restore of { entry : string }
      (** cancels the [Trim], [Trim_after] or [Delete] entry [entry] *)
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
    starts on a new line. Fails only without a readable header. [yield] is
    called every few hundred kilobytes parsed: a server passes
    [Fiber.yield], so that a large file does not keep its other fibers
    waiting. *)
val load : ?yield:(unit -> unit) -> string -> t Or_error.t

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

(** Messages for the next model request: those on the active path, folded
    in order: a compaction's summary replaces everything before its
    [kept_from] in the context so far, preceded by the compaction's pinned
    messages; a trim drops everything before its message (the summary and
    the pinned messages too, when it cuts into a compaction's kept part), a
    trim after a message what comes after it up to its [through], a
    deletion its message and the results of that message's tool calls. An
    untrim cancels the latest trim in effect, a restore the entry it names:
    what they took out comes back, in front of the context as it is now for
    a trim before a message (a compaction after a trim summarised only what
    came after it), else where it was; but for what another range in effect
    would have taken out, which stays out. Those up to the last [Masked]
    entry's [through] are masked ({!Observation_mask.mask}). The others
    ([timed_messages], [identified_messages]) are what the user sees: not
    masked. *)
val messages : t -> Message.t list

module Trim : sig
  type t =
    { before : string
      (** the entry id of the message the context starts at, the latest
          trim's *)
    ; messages : int
    ; tokens : int (** estimated ({!Message.estimate_tokens}) *)
    ; at : float option (** when the latest trim was made *)
    }
  [@@deriving sexp_of]
end

(** What is out of the model's context: a range of messages that a trim
    (before or after a message) or a deletion took out. *)
module Hidden : sig
  module Kind : sig
    type t =
      | Trim_before of { before : string }
      | Trim_after of
          { after : string
          ; through : string
          }
      | Deleted of { message : string }
    [@@deriving sexp_of, equal]

    val is_trim : t -> bool
  end

  type t =
    { entry : string (** the entry that made it, for [undelete]/[untrim] *)
    ; kind : Kind.t
    ; messages : int (** the messages it keeps out of the context now *)
    ; tokens : int (** their estimated tokens *)
    ; at : float option
    ; divider : int option
      (** for a trim after a message: where its divider goes, as the number
          of messages of {!identified_messages} before it *)
    }
  [@@deriving sexp_of]
end

(** The trims before a message in effect on the active path: the latest
    one's point, and every one's messages and tokens. *)
val trim : t -> Trim.t option

(** The ranges in effect on the active path, oldest first. *)
val hidden : t -> Hidden.t list

(** What the trims before a message in effect dropped from the context,
    oldest first. *)
val trimmed_messages : t -> (string option * Timed_message.t) list

(** What the range [entry] (see {!Hidden.t.entry}) keeps out of the context,
    oldest first; empty if it is not in effect. *)
val hidden_messages : t -> entry:string -> (string option * Timed_message.t) list

(** Drops everything before the message [before] from the context: it must
    be a user message in {!messages} other than the first (a cut there never
    parts a tool call from its result). Answers what it dropped. *)
val append_trim : t -> before:string -> Trim.t Or_error.t

(** Drops everything after the message [after] from the context, up to the
    newest message: messages appended later stay in it. [after] must be in
    {!messages}, not the newest, after any compaction summary, and not a
    message whose tool calls are answered after it. *)
val append_trim_after : t -> after:string -> Hidden.t Or_error.t

(** Drops the message [message] from the context: a user or assistant
    message in {!messages} (a tool result goes with its call), with the
    results of its tool calls. *)
val append_delete : t -> message:string -> Hidden.t Or_error.t

(** Cancels the latest trim (before or after a message) in effect, or the
    one [entry] made, answering what it brought back. *)
val append_untrim : ?entry:string -> t -> Hidden.t Or_error.t

(** Cancels the latest deletion in effect, or the one [entry] made,
    answering what it brought back. *)
val append_undelete : ?entry:string -> t -> Hidden.t Or_error.t

(** The input tokens of the latest reply on the active path's request,
    adjusted by the estimated tokens the trims, deletions and their undoing
    since took out or brought back; 0 before any reply. *)
val context_tokens : t -> int

(** The entry id of each of [messages] ([None] for a compaction summary). *)
val context_ids : t -> string option list

(** The cwd each of [messages] was appended in: the last [cwd] entry written
    before it, on any branch (the current cwd for the summary). *)
val context_cwds : t -> string list

(** A compaction summary as it appears in [messages]. *)
val summary_message : string -> Message.t

val is_summary_message : Message.t -> bool

(** The summary a {!summary_message} carries. *)
val summary_of_message : Message.t -> string option

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

(** The last [reminders] entry on the active path. *)
val reminders : t -> Reminder.t list

val set_reminders : t -> Reminder.t list -> Entry.t

(** Every reminder name in the file, on any branch, also those removed. *)
val reminder_names : t -> string list

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

(** The [review] entries of every branch, in the order written: rewinding
    keeps comments and approvals. *)
val review : t -> Review_state.t

val record_review : t -> Review_state.Change.t -> Entry.t
(** Where the session's cwd is in git, as last recorded for that cwd:
    [None] when nothing was (an older session, a host that cannot tell),
    [Some None] outside a repository. *)
val repo : t -> Git_location.t option option

val set_repo : t -> cwd:string -> Git_location.t option -> Entry.t

(** The worktrees that matter to it and were not removed since, by when
    they first did: those it made ({!add_worktree}), moved into (its [Repo]
    entries away from the main checkout) or made for its subagents. A fork
    keeps them but its parent's subagents'. *)
val worktrees : t -> Session_worktree.t list

val add_agent_worktree : t -> Agent_worktree.t -> Entry.t
val add_worktree : t -> Session_worktree.t -> Entry.t
val worktree_removed : t -> path:string -> Entry.t

(** The last [Home] entry, on any branch; [fork] copies it. *)
val home : t -> Home.t option

val set_home : t -> Home.t option -> Entry.t

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
  :  ?pinned:Pinned_messages.t
  -> t
  -> summary:string
  -> kept_from:string
  -> Entry.t

(** Masks the messages up to the entry [through] (on the active path) from
    now on. *)
val append_masked : t -> through:string -> Entry.t

(** Moves [head] to [to_]. The trims and deletions in effect stay: each is
    recorded again while its message is still in the context (rewinding to
    before it drops it; a trim after a message then reaches as far as the
    path does), and those on the new path that were not in effect are
    cancelled. *)
val rewind : t -> to_:string -> unit Or_error.t

(** Copy [src_path] into [dir] under a fresh stamp. The session id is kept
    unless it already exists in [dir]. *)
val import : dir:string -> string -> t Or_error.t

(** A new session in [dir] containing a copy of the active path up to and
    including [at] (default: head), with the trims and deletions in effect,
    as {!rewind} keeps them. *)
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
    ; host : string option
      (** the id of the tool host [cwd] is on, when the session records it *)
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
    ; repo : Git_location.t option option (** {!repo} *)
    ; worktrees : Session_worktree.t list (** {!worktrees} *)
    ; home : Home.t option (** {!home} *)
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

(** [list], parsing with [load], except the files of [live] sessions (by
    path: the sessions open in memory, which write them), summed up from
    memory instead. *)
val list_with
  :  live:(string -> t option)
  -> load:(string -> t Or_error.t)
  -> dir:string
  -> Summary.t list

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
