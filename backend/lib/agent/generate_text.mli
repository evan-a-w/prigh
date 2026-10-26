open! Core
open! Import

(** Drafts the text of a description field with one tool-less model call
    ([generate_text]): a commit message, a pull request's title and body, a
    branch name, a session's title. Each kind gathers its own context (the
    staged diff on the tool host, the branch's commits, the repository's
    conventions, the session's goal and plan, the conversation) and cleans
    the reply; adding a kind is a constructor and its {!Spec}. The model is
    the config's [generate_model], else the session's
    ({!Agent.generate_model}), with thinking off; the cost counts towards
    the session's. *)

module Kind : sig
  type t =
    | Commit_message
    (** from the staged diff, the session's goal and plan,
                         and the recent commits' style *)
    | Pr
    (** title and body, from the commits and diff against the base and
             the repository's pull request guidance and template *)
    | Branch_name (** from a task *)
    | Session_title (** from the conversation *)
  [@@deriving sexp_of, equal, enumerate]

  (** [commit_message], [pr], [branch_name], [session_title]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Target : sig
  (** What to draft for. *)
  type t =
    { dir : string option
      (** the worktree (relative to the session's directory; default: the
          session's) *)
    ; base : string option (** [Pr]: the branch it merges into *)
    ; task : string option
      (** [Branch_name]: what the branch is for (default: the session's
          goal, else its last request) *)
    }
  [@@deriving sexp_of]

  val empty : t
end

module Prompt : sig
  type t =
    { system : string
    ; user : string
    ; max_tokens : int
    }
  [@@deriving sexp_of]
end

module Result : sig
  type t =
    { text : string (** cleaned *)
    ; title : string option (** [Pr]: the first line *)
    ; body : string option (** [Pr]: the rest *)
    ; model : string (** its key *)
    ; cost_usd : float
    }
  [@@deriving sexp_of]

  (** [{text, title?, body?, model, cost_usd}] *)
  val to_json : t -> Json.t
end

(** The prompt for [kind], with its context gathered. Errors say what to do
    (nothing staged, no commits on the branch, nothing said yet). *)
val prompt : Agent.t -> Kind.t -> Target.t -> Prompt.t Or_error.t

(** A reply made fit for the field: code fences and quotes dropped; a branch
    name made a valid one; a title one line. *)
val clean : Kind.t -> string -> string

(** A pull request's text: the title (its first line, without a leading [#]
    or [Title:]) and the body (the rest). *)
val split_pr : string -> string * string

(** Drafts it, streaming the reply's text to [on_delta]. *)
val run
  :  Agent.t
  -> Kind.t
  -> Target.t
  -> cancel:Cancellation.t
  -> on_delta:(string -> unit)
  -> Result.t Or_error.t
