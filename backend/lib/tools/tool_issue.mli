open! Core
open! Import

(** [issue]: the issues of the working directory's repository on its git
    host, through the user's sign-in ({!Forges}): list, view, create,
    comment, edit, close, reopen, label, unlabel, assign, unassign. Every
    action but [list] and [view] changes the host, so asks in ask mode. *)

val name : string

(** Whether [action] changes anything (approvals, plan mode). *)
val changes : string -> bool

val create : forges:Forges.t -> unit -> Tool.t
