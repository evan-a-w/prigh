open! Core
open! Import

(** The [/sessions] picker's data: active sessions first, each with its
    repository and branch and its worktrees ({!Session_worktrees}), then
    the archived ones after a divider, dimmed. Also what its keys, [/delete] and
    [/delete-archived] act on and the confirmations they ask. *)

type t =
  { named_only : bool
  ; sessions : P.Session_summary.t list
  }
[@@deriving sexp_of, equal]

val create : ?named_only:bool -> P.Session_summary.t list -> t
val title : t -> string

(** The rows ([current]: the path of the session we are in, marked [*]),
    keeping the query, marks (also on sessions the filter hides) and highlight
    of [previous] (a hidden one moves to its nearest neighbour shown), or
    highlighting [select]. *)
val picker
  :  ?previous:Picker.t
  -> ?select:string
  -> t
  -> current:string option
  -> Picker.t

val find : t -> string -> P.Session_summary.t option

(** What Ctrl+D deletes: the marked sessions, else the highlighted one. *)
val targets : t -> Picker.t -> P.Session_summary.t list

(** As if archived (or unarchived) already, so the picker shows it at once. *)
val set_archived : t -> path:string -> archived:bool -> t

val remove : t -> paths:string list -> t

(** ["session \"build fix\""], or by id when unnamed. *)
val describe : P.Session_summary.t -> string

(** ["Delete 3 sessions (1.2 MB)? Enter deletes, Esc keeps them"], with
    where deleting them [moving] takes us (["You'll move to session
    \"build fix\"."]) before the last sentence. *)
val delete_question
  :  ?moving:P.Switch_to.t
  -> t
  -> P.Session_summary.t list
  -> string

(** [Error] says what to do when they include the [current] session (its
    path) and the backend would keep it: it is running or open in another
    frontend. *)
val deletable
  :  P.Session_summary.t list
  -> current:string option
  -> (P.Session_summary.t list, string) Result.t

(** Where deleting [deleting] moves us when the [current] session is among
    them: the next session down the list (as the picker orders it) not being
    deleted, else the nearest above, else a new session. *)
val moving_to
  :  t
  -> deleting:P.Session_summary.t list
  -> current:string option
  -> P.Switch_to.t option

(** The archived sessions, but the [current] one when it would be kept. *)
val archived_to_delete : t -> current:string option -> P.Session_summary.t list

(** [None] when there are none to delete. *)
val delete_archived_question : t -> current:string option -> string option

(** ["2 marked (1.2 MB)"] while sessions are marked. *)
val marked_summary : t -> Picker.t -> string option

(** Each argument as a path, an id, a unique id prefix or a name; an error says
    which matched nothing or several, and how to find the right one. *)
val resolve
  :  P.Session_summary.t list
  -> string list
  -> (P.Session_summary.t list, string) Result.t
