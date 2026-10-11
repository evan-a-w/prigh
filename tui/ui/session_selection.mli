open! Core
open! Import

(** The [/sessions] picker's data: active sessions first, then the archived
    ones after a divider, dimmed. Also what its keys, [/delete] and
    [/delete-archived] act on and the confirmations they ask. *)

type t =
  { named_only : bool
  ; sessions : P.Session_summary.t list
  }
[@@deriving sexp_of, equal]

val create : ?named_only:bool -> P.Session_summary.t list -> t
val title : t -> string

(** The rows ([current]: the path of the session we are in, marked [*]),
    keeping the query, marks and highlight of [previous] (or highlighting
    [select]). *)
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
val archived : t -> P.Session_summary.t list

(** ["session \"build fix\""], or by id when unnamed. *)
val describe : P.Session_summary.t -> string

(** ["Delete 3 sessions (1.2 MB)? Enter deletes, Esc keeps them"]. *)
val delete_question : P.Session_summary.t list -> string

(** [None] when nothing is archived. *)
val delete_archived_question : t -> string option

(** ["2 marked (1.2 MB)"] while sessions are marked. *)
val marked_summary : t -> Picker.t -> string option

(** Each argument as a path, an id, a unique id prefix or a name; an error says
    which matched nothing or several, and how to find the right one. *)
val resolve
  :  P.Session_summary.t list
  -> string list
  -> (P.Session_summary.t list, string) Result.t
