open! Core
open! Import

(** The user's preferences and data that prigh-web keeps on the backend
    ([get_prefs], [set_prefs], [push_pref], [prefs_changed]), so that they
    follow the user from browser to browser and change live in the others:
    the theme and accent, notifications, the transcript's verbosity, the
    prompt history (shared with the TUI), the panels' sizes, whether the
    terminal is open and which of the sidebar's projects are folded. The
    browser caches them ([Command.Cache_prefs]) for the first paint and for
    backends that keep none.

    What stays in the browser only: the sign-ins (needed before there is a
    user to ask), the notes a reload passes to the next page load (adding an
    account, a refused sign-in) and, per tab, the draft and the chat's
    scroll position. *)

module Key : sig
  val theme : string
  val accent : string
  val notify : string
  val verbosity : string
  val terminal_open : string

  (** The sidebar's sessions whose worktrees are folded away: their ids. *)
  val folded : string

  (** What the review page compared last ({!Review_memory}). *)
  val review : string

  (** ["history"]: the prompts sent, newest first, shared with the TUI. *)
  val history : string

  (** Set once the browser's preferences from before were uploaded (or the
      backend had its own). *)
  val migrated : string

  (** Those [get_prefs] asks for. *)
  val all : string list
end

(** How many prompts the backend keeps in [Key.history]. *)
val history_max : int

module Remote : sig
  type t =
    | Unknown (** not asked yet *)
    | Kept (** the backend keeps them *)
    | Local (** a backend that keeps none: in this browser only *)
  [@@deriving sexp_of, equal]
end

type t =
  { remote : Remote.t
  ; legacy : Prefs.Change.t list
    (** what the browser kept before the backend did: uploaded once *)
  }
[@@deriving sexp_of]

val init : t

(** Whether changes go to the backend (also not while acting as another
    user, whose are not ours: see [App]). *)
val saving : t -> bool

(** [get_prefs]' reply arrived: the changes to apply, and to upload (the
    legacy ones, when the backend has never had this browser's). *)
val loaded
  :  t
  -> Prefs.Change.t list
  -> t * Prefs.Change.t list * Prefs.Change.t list
