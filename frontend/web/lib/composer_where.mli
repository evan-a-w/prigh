open! Core
open! Import

(** Where the session works, in the line under the composer: its cwd (under
    the tool host's home as ~) on the left, with the host first when its
    tools run elsewhere than where they would by default, and its branch on
    the right. Each is a button: the folder runs [/cd], the branch
    [/worktree]. *)

type t

(** [None] before the session has a cwd, and for temporary sessions. *)
val of_model : App.Model.t -> t option

val view : t -> inject:(App.Action.t -> unit Effect.t) -> Vdom.Node.t
