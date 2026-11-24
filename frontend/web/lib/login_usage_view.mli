open! Core
open! Import

(** A login's usage, as Settings → Models → Providers and the status line's
    card show it: its subscription's limit windows (bar, share used, when
    each resets) when the provider reports them, what ran on it today and
    over 7 and 30 days (an API key's cost too), and for a login the user
    shares, each user's part ([account_usage]). *)

(** Under a login's row in Settings. *)
val details : App.Model.t -> Prigh_ui.Provider_logins.Login.t -> Vdom.Node.t

(** The context card's lines on the session's login (the pinned one, else
    its provider's default); none without a stored login. *)
val card_lines
  :  App.Model.t
  -> State.t
  -> line:(?cls:string -> string -> string -> Vdom.Node.t)
  -> Vdom.Node.t list
