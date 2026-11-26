open! Core
open! Import

(** What a login has used, as text for every frontend: its subscription's
    limits ([usage_limits]) and what ran on it ([auth_status]'s [usage],
    [account_usage]'s users). *)

(** ["2h 14m"], ["3d 4h"], ["12m"], ["under a minute"]. *)
val until : Time_ns.Span.t -> string

(** ["42%"], ["2.5%"]. *)
val percent : float -> string

(** ["12k"], ["1.2M"]. *)
val tokens : int -> string

(** The limits' answer for the login [ref_] (as [auth_status] names it). *)
val limits_of
  :  P.Subscription_usage.t option
  -> ref_:string
  -> P.Subscription_usage.Status.t option

(** ["5-hour session 42%, resets in 2h 14m"]. *)
val window : now:Time_ns.t -> P.Subscription_usage.Window.t -> string

(** The limit windows ([max]: the first so many), joined by [" · "]; [None]
    when there are none to show (an API key, not signed in). *)
val limits
  :  ?max:int
  -> now:Time_ns.t
  -> P.Subscription_usage.Status.t
  -> string option

(** ["3 requests, 12k tokens, $0.40"]; the cost only with [cost] (an API
    key's, billed per token). *)
val totals : cost:bool -> P.Login_usage.Totals.t -> string

(** ["3 req 12k tok $0.40"]: [totals], shorter. *)
val compact : cost:bool -> P.Login_usage.Totals.t -> string

(** A login row's few words: its first limit window when the provider
    reported it, else what ran on it today or else in 30 days (["today 3 req
    12k tok $0.42"]); [None]: nothing. *)
val brief
  :  now:Time_ns.t
  -> limits:P.Subscription_usage.Status.t option
  -> P.Auth_status.Account.t
  -> string option

(** One line per period (today, 7 and 30 days), or that nothing ran. *)
val period_lines : cost:bool -> P.Login_usage.t option -> string list

(** Whether a login is billed per token: what ran on it shows its cost. *)
val billed : P.Auth_status.Account.t -> bool
