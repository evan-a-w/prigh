open! Core
open! Import

(** A login's usage, as Settings → Models → Providers and the status line's
    card show it: its subscription's limit windows (bar, share used, when
    each resets) when the provider reports them, what ran on it today and
    over 7 and 30 days (an API key's cost too), and for a login the user
    shares, each user's part ([account_usage]). *)

(** A login's row, as Settings → Models → Providers and Settings → Usage
    list them under its provider's name: its label ("Team (from dave)",
    "Claude Pro/Max" for "Anthropic (Claude Pro/Max)"), what it is and its
    plan, Share… (Hide or Show for one shared with the user) and Manage,
    and under them its usage. *)
val row
  :  App.Model.t
  -> inject:(App.Action.t -> unit Vdom.Effect.t)
  -> Prigh_ui.Provider_logins.Login.t
  -> Vdom.Node.t

(** Whether the login has usage to show: limits (or why they are unknown),
    requests in 30 days, or users. *)
val shown : App.Model.t -> Prigh_ui.Provider_logins.Login.t -> bool

(** The context card's lines on the session's login (the pinned one, else
    its provider's default); none without a stored login. *)
val card_lines
  :  App.Model.t
  -> State.t
  -> line:(?cls:string -> string -> string -> Vdom.Node.t)
  -> Vdom.Node.t list
