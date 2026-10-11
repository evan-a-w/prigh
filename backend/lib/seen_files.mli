open! Core
open! Import

(** What the agent has seen of the files it may overwrite: for each file it
    read or wrote, a digest of the contents it saw and when.

    [write] (and [apply_patch]'s [*** Add File:]) refuse to replace an
    existing file unless its current contents are what the agent last read
    or wrote, so a file it never looked at, or one changed behind its back,
    is not clobbered. Edits match text exactly and are not blocked; they get
    a note when the file changed since.

    The record lives on the tool host, where the files are (this process's
    table, in the backend for its own host and in each [prigh tool-host]),
    keyed by a scope the backend passes with the call: the session id, so a
    session's subagents share it. A host that restarts, a host switch to a
    machine with no record, or an evicted entry all mean "not seen", so the
    agent is asked to read the file again: losing the record fails safe.
    Calls without a scope (an older backend, direct use) are not checked. *)

type t

(** The hidden argument carrying the scope. *)
val scope_arg : string

(** Whether calls of the tool [name] take a scope ([read], [write], [edit],
    [apply_patch]). *)
val applies_to : string -> bool

(** [arguments] with [scope_arg] set. *)
val with_scope : Json.t -> scope:string -> Json.t

(** The record of the arguments' scope, created on first use. *)
val of_args : Json.t -> t option

(** The agent has seen [contents] as the contents of [path] (absolute). *)
val record : t -> path:string -> contents:string -> unit

val forget : t -> path:string -> unit

(** Whether the agent may replace [path], an existing file whose contents are
    now [current]. The error starts [refusing to WHAT] and ends [then RETRY]
    after saying why and to read the file. *)
val check_overwrite
  :  t option
  -> path:string
  -> current:string
  -> what:string
  -> retry:string
  -> (unit, string) Result.t

(** A note for an edit of [path] whose contents before it were [current],
    when they are not what the agent last saw. *)
val changed_note : t option -> path:string -> current:string -> string option

(** At most this many files per scope and this many scopes are kept; the
    least recently used go first. *)
val max_files : int

val max_scopes : int
