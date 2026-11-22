open! Core
open! Import

(** Settings → Usage: when the limits were read, with Refresh; then a card
    per provider, a row per limit window (its bar, the share used, and when
    it resets, from now and on the clock), or how to sign in. *)
val groups
  :  App.Model.t
  -> inject:(App.Action.t -> unit Vdom.Effect.t)
  -> Vdom.Node.t list

(** ["2h 14m"], ["3d 4h"], ["12m"], ["under a minute"]. *)
val until : Time_ns.Span.t -> string
