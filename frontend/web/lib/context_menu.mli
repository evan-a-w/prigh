open! Core
open! Import

(** The sidebar's context menus: right-click (or Shift+F10, the context
    menu key, a long press, the row's ⋯ button) on a session or one of its
    worktrees. Each item does what a slash command or a key does elsewhere,
    which its detail names. Shown as a [Picker] dialog ([Dialog.Picker_kind.Menu])
    floating at the pointer or under the row. *)

module Target : sig
  type t =
    | Session of string (** its path *)
    | Worktree of
        { session : string (** the path of the session it is under *)
        ; path : string
        }
  [@@deriving sexp_of, equal]
end

module Anchor : sig
  (** Where it opens: the menu's top left corner, kept in the window. *)
  type t =
    | Point of
        { x : int
        ; y : int
        } (** at the pointer *)
    | Element of string (** under the element with this id (the keyboard) *)
  [@@deriving sexp_of, equal]
end

type t =
  { target : Target.t
  ; anchor : Anchor.t
  }
[@@deriving sexp_of, equal]

module Item : sig
  type t =
    | Open (** switch to the session *)
    | Open_tab (** the session in a new browser tab *)
    | Rename
    | Fork
    | New_worktree
    | Switch_back (** the session goes back to its home ([/worktree back]) *)
    | Switch_here (** the session moves into the worktree *)
    | New_session_here
    | Review
    | Files
    | Terminal
    | Create_pr
    | Copy_path
    | Copy_id
    | Archive
    | Unarchive
    | Delete
    | Remove (** the worktree *)
  [@@deriving sexp_of, equal, enumerate]

  val id : t -> string
  val of_id : string -> t option

  (** Destructive: shown in red. *)
  val danger : t -> bool
end

(** As a [Picker]: ids from {!Item.id}, the label, the detail (the slash
    command that does the same). A session with a home ({!Session_summary.home}:
    pass the current session's from its state) offers to switch back first. *)
val picker : Target.t -> Session_summary.t -> current:bool -> Picker.t

(** The worktree's path, for a worktree's menu. *)
val worktree : t -> string option

(** The session's path. *)
val session : t -> string

(** The DOM id of a worktree's row under a session (by their ids). *)
val worktree_dom_id : session:string -> string -> string

module Plan : sig
  (** What an item does to a session that may not be the one on show:
      switch to it, move it into a directory, then run a command there.
      Each step waits for the backend's answer before the next. *)
  type t =
    { switch : string option (** the session (its path) to switch to *)
    ; enter : string option (** the directory it moves into *)
    ; run : string option (** a slash command, once there *)
    ; notice : string option (** said once there, when nothing runs *)
    }
  [@@deriving sexp_of, equal]
end
