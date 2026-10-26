open! Core
open! Import

(** A worktree's changes on a tool host, for [/diff] and [/review]: [git
    diff] against [HEAD] (or, with a base, against its merge base with
    [HEAD]: what was committed since as well) plus the untracked files as
    new files, optionally limited to one path. The worktree is the one of
    the session's directory, or [dir] (any worktree of any repository on
    the host). With [default_base], a linked worktree (not the repository's
    main checkout) is compared with its base when none is given: the branch
    it was made from as {!Worktree.create} recorded it, else its remote's
    default branch ([origin/HEAD]), else [main] or [master]. Runs on the
    host as the pseudo-tool [op]. *)

module Result : sig
  type t =
    { root : string (** the repository's top directory *)
    ; against : string
      (** what the working tree is compared with: [HEAD], a merge base's
        commit, or [empty tree] before the first commit *)
    ; diff : string (** paths relative to [root] *)
    ; truncated : bool (** cut at [max_bytes] *)
    ; branch : string option (** checked out; [None]: detached *)
    ; base : string
      (** what [against] was made from: [HEAD], or the base's name *)
    ; main : bool (** the repository's main checkout, not a linked worktree *)
    ; branches : string list
      (** the repository's local, then remote branches ([origin/main]), to
          pick another base from *)
    }
  [@@deriving sexp, jsonaf, equal]

  (** Files changed, lines added and removed. *)
  val stats : t -> int * int * int
end

val op : string
val max_bytes : int

(** The pseudo-tool's arguments. *)
val args
  :  ?path:string
  -> ?base:string
  -> ?dir:string
  -> ?default_base:bool
  -> unit
  -> Jsonaf.t

(** On the host: [Ok] carries the [Result.t] as JSON; errors say what to do
    (not a repository, git missing, an unknown base). *)
val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Jsonaf.t
  -> Tool_result.t

(** Whether an error of [run] says the base is not a revision. *)
val is_unknown_revision : string -> bool

val of_tool_result : Tool_result.t -> Result.t Or_error.t
