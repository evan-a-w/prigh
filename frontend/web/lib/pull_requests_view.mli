open! Core
open! Import

(** The pull request dialogs: the list ([/pr], the session card's branch), one
    pull request with its checks and diff, the create form ([/pr new]) and
    the offer to work in a checked-out pull request. *)

val list : Pull_requests.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

val page
  :  App.Model.t
  -> Pr_page.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

val create : Pr_create.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

val checked_out
  :  number:int
  -> Pr_checkout.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
