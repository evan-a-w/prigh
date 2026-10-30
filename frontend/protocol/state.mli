open! Core

module Subagent : sig
  (** A background subagent that is running or whose report has not been
      delivered to the main agent yet. *)
  type t =
    { id : string
    ; task : string
    ; running : bool
    }
  [@@deriving sexp_of, equal]
end

module Job : sig
  (** A background shell job that is running or whose report has not been
      delivered to the main agent yet. *)
  type t =
    { id : string
    ; command : string
    ; running : bool
    ; exit : string option (** e.g. [exited 0], [killed], once finished *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { session_id : string
  ; session_path : string
  ; session_name : string option
  ; session_description : string option
  ; cwd : string
  ; home : Session_home.t option
    (** where a worktree switch took it from, for [/worktree back] *)
  ; git_branch : string option
  ; model : Model.t
  ; thinking : string
  ; running : bool
  ; message_count : int
  ; usage : Usage.t
  ; cost_usd : float
  ; context_tokens : int
  ; active_host : string (** [Host.id]; "backend" or a client id *)
  ; active_host_name : string option
    (** its name as last seen, also while it is disconnected (absent from
        [hosts]); [None] when unknown or from older backends *)
  ; hosts : Host.t list
    (** backend first, then connected tool-capable clients *)
  ; subagents : Subagent.t list
  ; jobs : Job.t list
  ; budget_usd : float option (** no turn starts once [cost_usd] reaches it *)
  ; goal : Goal.t option
  ; reminders : Reminder.t list (** [/remind] *)
  ; resume_at : Time_ns.t option
    (** when a run stopped by a usage limit resumes by itself *)
  ; approval_mode : Approval_mode.t
  ; plan_mode : bool (** read-only planning, see [exit_plan_mode] *)
  ; plan : Plan.t option (** the model's [update_plan] *)
  ; session_rules : string list
    (** rules the user allowed for the session ("don't ask again") *)
  ; trim : Trim.t option (** what is trimmed from the model's context *)
  ; hidden : Hidden.t list
    (** what trims and deletions keep out of the model's context, oldest
        first *)
  ; transcript_filter : string option
    (** what prigh-web shows of the session's transcript
        ([Transcript_filter.to_string]); [None]: everything, also from
        backends without it *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
