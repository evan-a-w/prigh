open! Core
open! Import

(** Context menus: right-click (or Shift+F10, the context menu key, a long
    press, the ⋯ button) on a session or one of its worktrees in the
    sidebar, or on a message of the transcript. Each item does what a slash
    command or a key does elsewhere, which its detail names. Shown as a
    [Picker] dialog ([Dialog.Picker_kind.Menu]) floating at the pointer or
    under the row. *)

module Target : sig
  type t =
    | Session of string (** its path *)
    | Worktree of
        { session : string (** the path of the session it is under *)
        ; path : string
        }
    | Message of int
    (** a message of the transcript, by its entry: a tool call's is its
        reply's *)
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
    | Copy_message (** its text (a reply's markdown) *)
    | Trim_before
    | Trim_after
  [@@deriving sexp_of, equal, enumerate]

  val id : t -> string
  val of_id : string -> t option

  (** Destructive: shown in red. *)
  val danger : t -> bool
end

(** As a [Picker]: ids from {!Item.id}, the label, the detail (the slash
    command that does the same). A session with a home ({!Session_summary.home}:
    pass the current session's from its state) offers to switch back first;
    [host_home] is the home directory of the tool host its cwd is on. *)
val picker
  :  ?host_home:string
  -> Target.t
  -> Session_summary.t
  -> current:bool
  -> Picker.t

(** What a message's menu offers ({!message_picker}). *)
module Message : sig
  type t =
    { reply : bool
    ; calls : int (** a reply's tool calls, deleted with it *)
    ; text : string (** what Copy copies *)
    ; first : bool (** the context starts with it: nothing before to trim *)
    ; newest : bool (** nothing after it to trim *)
    ; running : bool (** a run is in progress: it cannot change *)
    }
  [@@deriving sexp_of, equal]

  (** "Your message" or "Reply", with its tool calls. *)
  val title : t -> string
end

(** Copy; Trim everything before this, Trim everything after this; Delete.
    Those that do not apply are [dimmed]: disabled, their detail says why
    (otherwise it is the slash command that does the same). *)
val message_picker : Message.t -> Picker.t

(** The worktree's path, for a worktree's menu. *)
val worktree : t -> string option

(** The session's path, for a session's or worktree's menu. *)
val session : t -> string option

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
