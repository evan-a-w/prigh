open! Core
open! Import

(** The sidebar's view of [list_sessions]: the pinned sessions, then the
    other active ones, then the archived ones, each most recent first. Each
    says where it works (its repository and branch) and has its worktrees
    under it ({!Prigh_ui.Session_worktrees}). Searching them is the control
    centre's ({!Control_centre}). *)

(** The name, else the description or first prompt. *)
val title : Session_summary.t -> string

(** The DOM id of a session's row in the sidebar (focused from the keyboard). *)
val dom_id : string -> string

module Sections : sig
  type t =
    { pinned : Prigh_ui.Session_search.Result.t list
    ; active : Prigh_ui.Session_search.Result.t list
      (** the others not archived *)
    ; archived : Prigh_ui.Session_search.Result.t list
    }
end

(** Pinned, and not archived (archiving unpins). *)
val is_pinned : Session_summary.t -> bool

(** The sessions in their groups, each in the backend's order. *)
val sections : Session_summary.t list -> Sections.t

(** The rows the sidebar shows, in its order: the pinned sessions, the other
    active ones, then the archived ones when [archived_open]. *)
val shown
  :  Session_summary.t list
  -> archived_open:bool
  -> Session_summary.t list
