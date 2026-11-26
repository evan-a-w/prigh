open! Core
open! Import

(** Where the session (in a subagent's chat, the subagent) works, in the
    line under the composer: its cwd (under the tool host's home as ~) on the
    left, never the host (the host chip names it), and its branch on the
    right. Each is a button: the folder runs [/cd], the branch [/worktree];
    a subagent's say why they cannot change. *)

type t

(** [None] before the session has a cwd, and for temporary sessions. *)
val of_model : App.Model.t -> t option

val view : t -> inject:(App.Action.t -> unit Effect.t) -> Vdom.Node.t
