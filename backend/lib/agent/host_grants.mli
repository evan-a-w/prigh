open! Core
open! Import

(** Which other tool hosts the session may work on: starting a subagent
    there, or managing worktrees there ([subagent]'s and [worktree]'s
    [host]). That is the user's choice: a host they named in their own
    messages, or one they approved when asked, which is then granted for
    the rest of the session (in memory: a reloaded session asks again). *)

type t

val create : unit -> t

(** Forgets every grant: another session was loaded. *)
val clear : t -> unit

val grant : t -> host:string -> unit

(** [host] (its id) was granted, or the user named it ([name], or the id)
    as a word in one of their [requests] (see {!Approval_judge.requests}),
    ignoring case. *)
val allows : t -> host:string -> name:string -> requests:string list -> bool

(** Whether [text] has [word] in it, ignoring case, not as part of a longer
    word (letters, digits, [-] and [_]). *)
val names : string -> word:string -> bool
