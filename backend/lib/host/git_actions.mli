open! Core
open! Import

(** The review's git actions on a worktree of a tool host, as the
    pseudo-tool [op]: its status, staging and unstaging files, committing,
    pushing, and the context a model drafts a commit message or a pull
    request from ({!Generate_text}). The worktree is the session's
    directory's, or [dir] (relative to it). Paths are relative to the
    worktree's top directory and must stay inside it; git runs with literal
    pathspecs and never prompts for credentials (the tool host's own are
    used). Refusals say what to do instead. *)

module File : sig
  (** A changed file, as [git status] says: [index] and [worktree] are its
      staged and unstaged changes, a letter each ([M]odified, [A]dded,
      [D]eleted, [R]enamed, [T]ype changed, [U]nmerged; [?] in [worktree]:
      untracked). *)
  type t =
    { path : string
    ; orig : string option (** a rename's old path *)
    ; index : string option
    ; worktree : string option
    }
  [@@deriving sexp, jsonaf, equal]
end

module Status : sig
  type t =
    { root : string
    ; branch : string option (** [None]: detached *)
    ; head : string option (** HEAD's commit; [None] before the first *)
    ; upstream : string option (** e.g. [origin/main] *)
    ; ahead : int (** commits not on the upstream *)
    ; behind : int (** the upstream's commits not here *)
    ; files : File.t list
    }
  [@@deriving sexp, jsonaf, equal]

  val parse : root:string -> string -> t
end

module Commit : sig
  type t =
    { sha : string
    ; subject : string
    ; status : Status.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Push : sig
  type t =
    { remote : string
    ; branch : string
    ; status : Status.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Commit_context : sig
  (** What a commit message is drafted from. *)
  type t =
    { branch : string option
    ; diff : string (** the staged changes, {!Diff_digest}ed *)
    ; recent : string list (** the latest commits' subjects, newest first *)
    ; conventions : (string * string) list
      (** the repository's guidance on commits: sections of AGENTS.md,
          CONTRIBUTING.md... *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Pr_context : sig
  (** What a pull request is drafted from. *)
  type t =
    { branch : string option
    ; base : string
    ; commits : (string * string) list (** subject, body; oldest first *)
    ; diff : string (** [base...HEAD], {!Diff_digest}ed *)
    ; conventions : (string * string) list
      (** the repository's guidance on pull requests *)
    ; template : (string * string) option
      (** its pull request template (path, text) *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Request : sig
  type t =
    | Status
    | Stage of string list
    | Unstage of string list
    | Commit of
        { message : string
        ; amend : bool
        ; paths : string list (** staged first *)
        }
    | Push
    | Commit_context
    | Pr_context of { base : string option }
  [@@deriving sexp_of, equal]

  (** [dir]: the worktree's directory (default: the session's). *)
  val to_json : ?dir:string -> t -> Json.t
end

val op : string

(** On the host. *)
val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Json.t
  -> Tool_result.t

(** The diff a model is shown at most (see {!Diff_digest}). *)
val max_diff_bytes : int

(** The sections of a markdown file whose headings mention [topic] (e.g.
    "pull request"), each with its subsections. *)
val sections : topic:string list -> string -> string option
