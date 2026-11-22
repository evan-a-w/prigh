open! Core

(** The worktrees each session shows (those its fold leaves open), one line
    a session: [(feat/login here ● 3 ↑2 ↓1) (docs)]. *)
val print : Harness.t -> unit
