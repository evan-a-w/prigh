open! Core
open! Import

(** The review page's git bar ({!Git_bar}): the branch and how it stands
    with its upstream, what is staged, and Stage approved, Push and Create
    pull request ([strip]); a file's stage box in the file list ([tick]);
    the commit message with ✦ Generate, Amend and Commit ([commit_box]). *)

val strip : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

val tick
  :  App.Model.t
  -> string (** the file's path *)
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

val commit_box
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
