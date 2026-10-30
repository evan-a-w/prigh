open! Core
open! Import

(** The review page's git bar ({!Git_bar}): the branch and how it stands
    with its upstream, what is staged, and Commit…, Push and Create pull
    request ([strip]); while composing a commit, its files, the message with
    ✦ Generate, Amend and Commit ([commit_box]). None of them shows for a
    branch reviewed without a checkout. *)

val strip : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
val commit_box : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
