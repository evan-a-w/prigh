open! Core
open! Import

(** [/diff]'s dialog: the working tree's changes file by file, each folded
    open with its hunks in [Diff_view] (a long one folded to 400 lines, as
    [fold] says). *)
val view
  :  fold:Fold.t
  -> Git_diff.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
