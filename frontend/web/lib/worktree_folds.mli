open! Core
open! Import

(** Whether the sidebar shows a session's worktrees under it or folds them
    behind its [⑂ N] button. By default, shown when the session was used in
    the last [hours] (its last change, as the sidebar's age says; [0]:
    never), folded otherwise; once the user folds or unfolds one, that
    choice wins whatever its age. All three are the user's preferences
    ({!Pref_sync.Key.folded}, [unfolded], [worktree_hours]). *)

type t [@@deriving sexp_of, equal]

(** [hours] [default_hours]; nothing folded or unfolded by hand. *)
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

(** Used in the last [hours] at [now] ([None]: not known yet, taken as
    recent). *)
val recent : t -> now:Time_ns.t option -> Session_summary.t -> bool

(** Its worktrees show. *)
val shown : t -> now:Time_ns.t option -> Session_summary.t -> bool

(** The user's click on its [⑂ N]: the opposite of what shows, kept. *)
val toggle : t -> now:Time_ns.t option -> Session_summary.t -> t

(** The preferences, as the backend keeps them ([None]: the default):
    [folded], [unfolded] and [hours]. *)
val folded_pref : t -> Json.t option

val unfolded_pref : t -> Json.t option
val hours_pref : t -> Json.t option
val set_folded : t -> Json.t option -> t
val set_unfolded : t -> Json.t option -> t
val set_hours_pref : t -> Json.t option -> t
val set_hours : t -> int -> t
