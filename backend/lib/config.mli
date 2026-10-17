open! Core
open! Import

(** How the terminal frontend tells the user that something needs them while
    they look elsewhere ([/notify]); prigh-web keeps its own setting in the
    browser. *)
module Notify : sig
  type t =
    | Off
    | Bell (** the terminal bell *)
    | Desktop (** a desktop notification (OSC 9 / OSC 777) *)
  [@@deriving sexp_of, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t Or_error.t
end

type t =
  { scoped_models : string list
  ; approval_mode : Approval_mode.t
    (** the mode new sessions start in; [confirm_tools] (true: [ask], false:
        [auto]) in files written before it, replaced by it on save *)
  ; permissions : Approval.Rules.t
    (** [permissions]: allow/ask/deny rules for every session *)
  ; default_model : string option
    (** model key for new sessions, set by [/change_default] *)
  ; default_thinking : Thinking.t option
  ; fallback_models : string list
    (** model keys, in order: when a model's usage runs out (or it has no
        credentials), a run hands over to the next one after it *)
  ; default_cwd : string option
    (** where new sessions start on the backend host *)
  ; max_budget_usd : float option
    (** the cost budget of sessions that set none ([/budget]); the CLI's
        [-max-budget-usd] overrides it *)
  ; goal_max_continuations : int
    (** automatic continuations a [/goal] gets before it stops *)
  ; reminders : Reminder.t list
    (** [reminders]: sent to the main agent when it stops or every so many
        turns; edited by hand *)
  ; resume_on_limit_reset : bool
    (** resume a run stopped by a usage limit when the limit resets *)
  ; archive_after_days : float
    (** a session inactive for this long is archived (default 7; 0: never),
        see {!Session.Summary.is_archived} *)
  ; notify : Notify.t (** default [Bell] *)
  ; auto_background_seconds : float
    (** a foreground [bash] call of the main agent still running after this
        long is moved to the background as a job, and [job_wait]/
        [subagent_wait] return after at most this long (default 30; 0:
        never, waits unbounded) *)
  }
[@@deriving sexp_of]

val default : t

(** [auto_background_seconds], [None] when off. *)
val auto_background : t -> float option

(** The model for new sessions: [default_model], else the first of
    [fallback_models]. *)
val start_model : t -> string option

(** Reads [~/.prigh/config.json] under [home]. A missing file yields
    {!default}; unknown fields are ignored. *)
val load : home:string -> t Or_error.t

(** What is wrong with the file, each saying where and what to do: settings
    of the wrong type (which make {!load} fail) and unknown top-level fields.
    Unreadable or invalid JSON is {!read_fields}'s error, not repeated here. *)
val problems : home:string -> string list

(** Writes [~/.prigh/config.json], creating parent directories, as pretty
    JSON. Fields it does not own (such as [providers]) are kept, and so are
    [permissions] and [reminders], which are edited by hand; [confirm_tools]
    is dropped in favour of [approval_mode]. *)
val save : home:string -> t -> unit Or_error.t

(** The file's top-level fields ([[]] when it is missing), for the parts of
    the file other modules own. *)
val read_fields : home:string -> (string * Json.t) list Or_error.t

(** Replaces the file atomically. *)
val write_fields : home:string -> (string * Json.t) list -> unit Or_error.t

val path : home:string -> string
val to_json : t -> Json.t
val of_json : Json.t -> t Or_error.t
