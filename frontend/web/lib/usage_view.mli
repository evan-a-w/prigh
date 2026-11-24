open! Core
open! Import

(** Settings → Usage: when the limits were read, with Refresh; then, by
    provider, a card per login with usage to show (its row as in Settings →
    Models: limit windows, requests per period, users), or for a provider
    with none how to sign in or why its limits are unknown. *)
val groups
  :  App.Model.t
  -> inject:(App.Action.t -> unit Vdom.Effect.t)
  -> Vdom.Node.t list
