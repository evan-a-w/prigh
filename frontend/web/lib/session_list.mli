open! Core
open! Import

(** The sidebar's view of [list_sessions]: the pinned sessions, then the
    other active ones, then the archived ones, each most recent first. Each says where it works (its repository
    and branch) and has its worktrees under it
    ({!Prigh_ui.Session_worktrees}). Searching it is
    {!Prigh_ui.Session_search}: the titles and places here, merged with the
    backend's search of the messages ([content]). *)

(** The name, else the description or first prompt. *)
val title : Session_summary.t -> string

(** The sessions matching [query], best first, with what matched; all of
    them, in the backend's order, when it is empty. *)
val results
  :  ?content:Session_hits.t
  -> Session_summary.t list
  -> query:string
  -> Prigh_ui.Session_search.Result.t list

(** [results]' sessions. *)
val filter
  :  ?content:Session_hits.t
  -> Session_summary.t list
  -> query:string
  -> Session_summary.t list

(** The DOM id of a session's row in the sidebar (focused from the keyboard). *)
val dom_id : string -> string

module Sections : sig
  type t =
    { pinned : Prigh_ui.Session_search.Result.t list
    ; active : Prigh_ui.Session_search.Result.t list (** the others not archived *)
    ; archived : Prigh_ui.Session_search.Result.t list
    }
end

(** Pinned, and not archived (archiving unpins). *)
val is_pinned : Session_summary.t -> bool

(** [results], then split into its groups, each in [results]' order (best
    first while searching). *)
val sections
  :  ?content:Session_hits.t
  -> Session_summary.t list
  -> query:string
  -> Sections.t

(** Whether the archived sessions show: when the user unfolded them, or while
    searching. *)
val archived_shown : query:string -> archived_open:bool -> bool

(** The rows the sidebar shows, in its order: the pinned sessions, the other
    active ones, then the archived ones when [archived_shown]. *)
val shown
  :  ?content:Session_hits.t
  -> Session_summary.t list
  -> query:string
  -> archived_open:bool
  -> Session_summary.t list
