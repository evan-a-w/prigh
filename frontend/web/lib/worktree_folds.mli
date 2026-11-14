open! Core
open! Import

(** Whether the sidebar shows a session's worktrees under it or folds them
    behind its [⑂ N] button.

    By default they show when the session was used in the last [hours]
    ([0]: never), and fold otherwise. A session is used when it changes
    (its [updated_at]), while it runs, while the user is in it, when the
    user goes into it (in any of their browsers: [opened]), and whenever
    any of those happens to a session working in one of its worktrees (its
    cwd inside one). A pinned session's always show (unless [hours] is
    [0]).

    A fold or unfold by hand wins until the user next goes into the
    session, or into a session working in one of its worktrees; then the
    default holds again.

    All four are the user's preferences ({!Pref_sync.Key.folded},
    [unfolded], [opened], [worktree_hours]). *)

type t [@@deriving sexp_of, equal]

(** [hours] [default_hours]; nothing folded, unfolded or opened. *)
val empty : t

(** [12]. *)
val default_hours : int

(** [720] (30 days). *)
val max_hours : int

val hours : t -> int

(** [12], [12h], [12 hours], [off] or [never] ([0]): whole hours from [0] to
    [max_hours], else why not. *)
val parse_hours : string -> (int, string) Result.t

(** ["12 hours"], ["1 hour"], ["off"]. *)
val hours_string : int -> string

module Current : sig
  (** The session the user is in, and where it works now. *)
  type t =
    { id : string
    ; cwd : string
    }
end

(** The ids of [sessions] whose worktrees show at [now] ([None]: not known
    yet, when all of them do). *)
val shown
  :  t
  -> now:Time_ns.t option
  -> current:Current.t option
  -> Session_summary.t list
  -> String.Set.t

(** The user's click on a session's [⑂ N] at [now]: the opposite of whether
    it is [shown], kept until the session is next opened. [listed]: the
    sessions there are, whose choices are worth keeping. *)
val toggle
  :  t
  -> now:Time_ns.t option
  -> listed:String.Set.t
  -> shown:bool
  -> string
  -> t

(** The user went into the session at [now]. *)
val open_ : t -> now:Time_ns.t -> listed:String.Set.t -> string -> t

(** The preferences, as the backend keeps them ([None]: the default):
    [folded] and [unfolded] map session ids to when the user chose,
    [opened] to when they last went into it. *)
val folded_pref : t -> Json.t option

val unfolded_pref : t -> Json.t option
val opened_pref : t -> Json.t option
val hours_pref : t -> Json.t option

(** A list of ids (before choices expired) is no choice. *)
val set_folded : t -> Json.t option -> t

val set_unfolded : t -> Json.t option -> t

(** Merged with what this browser knows: the later of each session's. *)
val merge_opened : t -> Json.t option -> t

(** This browser knows of a later opening than [pref] has. *)
val opened_ahead : t -> Json.t option -> bool

val set_hours_pref : t -> Json.t option -> t
val set_hours : t -> int -> t
