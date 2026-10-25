open! Core
open! Import

(** The sidebar's view of [list_sessions]: the active sessions as a tree of
    projects ({!Prigh_ui.Project_tree}), then the archived ones. *)

(** The name, else the description or first prompt. *)
val title : Session_summary.t -> string

(** The sessions matching [query] (title, first prompt, cwd, project and
    branch), best first; all of them, in the backend's order, when it is
    empty. *)
val filter : Session_summary.t list -> query:string -> Session_summary.t list

(** The DOM id of a session's row in the sidebar (focused from the keyboard). *)
val dom_id : string -> string

(** [filter], then split: the active sessions, then the archived ones. *)
val sections
  :  Session_summary.t list
  -> query:string
  -> Session_summary.t list * Session_summary.t list

(** Whether the archived sessions show: when the user unfolded them, or while
    searching. *)
val archived_shown : query:string -> archived_open:bool -> bool

(** Whether [query] searches. *)
val searching : query:string -> bool

(** The active sessions' tree. While searching, only the matches, without
    [listing]'s worktrees or the archived sessions' subagents' worktrees. *)
val tree
  :  Session_summary.t list
  -> query:string
  -> listing:Worktree.Listing.t option
  -> Prigh_ui.Project_tree.t

(** The session rows the sidebar shows, in its order: the tree's, but those
    [folded] away (not while searching), then the archived ones when
    [archived_shown]. *)
val shown
  :  Session_summary.t list
  -> query:string
  -> archived_open:bool
  -> folded:String.Set.t
  -> listing:Worktree.Listing.t option
  -> Session_summary.t list
