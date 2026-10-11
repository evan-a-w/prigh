open! Core
open! Import

(** [/diff]'s dialog: the working tree's changes file by file, each folded
    open with its hunks in [Diff_view]. *)
val view : Git_diff.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
