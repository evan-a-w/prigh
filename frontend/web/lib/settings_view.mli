open! Core
open! Import

(** The settings page ([Dialog.Settings], the sidebar's gear or the bar's, [/settings]):
    notifications and the transcript's detail in this browser, tool
    confirmation and the providers on the backend, the git hosts, the Nix
    store (for superusers) and the account. Each
    control runs the command that changes it, so the page stays open. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The git hosts' rows (connect, connect with a token, disconnect, another
    Forgejo host), also in [/auth]. *)
val git_hosts
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t list
