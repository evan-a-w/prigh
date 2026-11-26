open! Core
open! Import

(** A usage limit window's pieces, as Settings → Usage and each login's row
    show them. *)

(** When the limits [u] were read, or the clock's time since. *)
val now : App.Model.t -> Subscription_usage.t -> Time_ns.t

(** ["Resets in 2h 14m · 15:30"]. *)
val resets
  :  App.Model.t
  -> now:Time_ns.t
  -> Subscription_usage.Window.t
  -> string

(** A window's bar and the share used. *)
val meter : Subscription_usage.Window.t -> Vdom.Node.t
