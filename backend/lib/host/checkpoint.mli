open! Core
open! Import

(** Snapshots of a git working tree, kept as hidden commits under
    [refs/prigh/checkpoints/]: what lets a composer session (whose agent
    edits files itself) rewind its files with its conversation.

    A snapshot is the working tree as [git add -A] sees it (untracked files
    included, ignored ones not), written through a temporary index: the
    user's branch, index, stash and [HEAD] are never touched. Restoring
    writes the snapshot's files back into the working tree, and removes
    those it did not have, again through a temporary index.

    Everything runs git on the tool host, in the repository of [cwd]; the
    pseudo-tool {!op} runs it there ({!run}), and {!Client} calls it. *)

val ref_prefix : string

(** [refs/prigh/checkpoints/<session>/]: where a session's checkpoints go. *)
val session_prefix : session:string -> string

module Snapshot : sig
  type t =
    { commit : string option (** [None] when it was not kept under a ref *)
    ; tree : string
    ; root : string (** the repository's top *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Captured : sig
  type t =
    | Snapshot of Snapshot.t
    | Not_git of string (** why there is no snapshot: not a repository, ... *)
  [@@deriving sexp, jsonaf, equal]
end

(** The working tree of [cwd]'s repository now, kept as a commit at [ref]
    when one is given (it must be under {!ref_prefix}). *)
val capture
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> ref:string option
  -> Captured.t Or_error.t

(** The files that differ between the snapshots (commits or trees) [from]
    and [to_]; [to_] defaults to the working tree now. Sorted. *)
val changed
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> from:string
  -> to_:string option
  -> string list Or_error.t

(** Puts the working tree back as the snapshot [to_] has it: files it has
    are written, files it lacks are removed (with directories left empty).
    Refuses, changing nothing, unless the working tree is still the
    snapshot [expect] (nothing changed since it was taken), naming the files
    that differ. Returns the files it changed. *)
val restore
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> to_:string
  -> expect:string
  -> string list Or_error.t

(** Deletes the refs under [prefix] (which must be under {!ref_prefix});
    returns how many there were. Nothing to do outside a repository. *)
val drop
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> prefix:string
  -> int Or_error.t

(** {2 On a tool host} *)

val op : string

val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Json.t
  -> Tool.Result.t

(** The calls, as the backend makes them: [exec] runs {!op} on the session's
    tool host. *)
module Client : sig
  type exec := Json.t -> Tool.Result.t

  val capture : exec -> ref:string option -> Captured.t Or_error.t

  val changed
    :  exec
    -> from:string
    -> to_:string option
    -> string list Or_error.t

  val restore : exec -> to_:string -> expect:string -> string list Or_error.t
  val drop : exec -> prefix:string -> int Or_error.t
end
