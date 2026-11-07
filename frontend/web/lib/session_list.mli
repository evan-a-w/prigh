open! Core
open! Import

(** The sidebar's view of [list_sessions]: the pinned sessions, then the
    other active ones, then the archived ones, each most recent first. Each says where it works (its repository
    and branch) and has its worktrees under it
    ({!Prigh_ui.Session_worktrees}). *)

(** The name, else the description or first prompt. *)
val title : Session_summary.t -> string

(** The sessions matching [query] (title, first prompt, cwd, repository,
    branch and its worktrees' branches), best first; all of them, in the
    backend's order, when it is empty. *)
val filter : Session_summary.t list -> query:string -> Session_summary.t list

(** The DOM id of a session's row in the sidebar (focused from the keyboard). *)
val dom_id : string -> string

module Sections : sig
  type t =
    { pinned : Session_summary.t list
    ; active : Session_summary.t list (** the others not archived *)
    ; archived : Session_summary.t list
    }
end

(** Pinned, and not archived (archiving unpins). *)
val is_pinned : Session_summary.t -> bool

(** [filter], then split into its groups, each in [filter]'s order. *)
val sections : Session_summary.t list -> query:string -> Sections.t

(** Whether the archived sessions show: when the user unfolded them, or while
    searching. *)
val archived_shown : query:string -> archived_open:bool -> bool

(** The rows the sidebar shows, in its order: the pinned sessions, the other
    active ones, then the archived ones when [archived_shown]. *)
val shown
  :  Session_summary.t list
  -> query:string
  -> archived_open:bool
  -> Session_summary.t list
