open! Core
open! Import

(** Settings → Usage: the subscriptions' usage limits ([usage_limits]), asked
    for when the section opens and on Refresh, never polled. They belong to
    the user (switching starts over). *)

type t =
  { usage : (Subscription_usage.t, string) Result.t option
    (** the last answer; [None] before the first *)
  ; loading : bool (** asked, no answer yet (the last one stays shown) *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t
val loaded : t -> (Subscription_usage.t, string) Result.t -> t
