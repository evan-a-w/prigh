open! Core
open! Import

(** The working tree's changes on a tool host, for [/diff] and [/review]:
    [git diff] against [HEAD] (or, with a base, against its merge base with
    [HEAD]) plus the untracked files as new files, optionally limited to one
    path. Runs on the host as the pseudo-tool [op]. *)

module Result : sig
  type t =
    { root : string (** the repository's top directory *)
    ; against : string
      (** what the working tree is compared with: [HEAD], a merge base's
        commit, or [empty tree] before the first commit *)
    ; diff : string (** paths relative to [root] *)
    ; truncated : bool (** cut at [max_bytes] *)
    }
  [@@deriving sexp, jsonaf, equal]

  (** Files changed, lines added and removed. *)
  val stats : t -> int * int * int
end

val op : string
val max_bytes : int

(** The pseudo-tool's arguments. *)
val args : ?path:string -> ?base:string -> unit -> Jsonaf.t

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
