open! Core

(** The sizes of the page's panels, which the user drags (or moves with the
    keyboard) from a handle on their inner edge: each one's preference, kept
    with the user's preferences, and the size it gets in the window it is in.
    Side panels docked beside the chat share the window's width so that the
    chat keeps [chat_min] pixels (or, while reviewing, the review page keeps
    [review_min] beside the chat, which is then one of them): when the
    window shrinks, they shrink (towards their minimums) in proportion, and
    grow back to their preferences with it. The review's columns fit in what
    the review page has. *)

module Handle : sig
  type t =
    | Sidebar (** the sessions sidebar's width, from its right edge *)
    | Files
    (** the file browser's list's width, from its left edge (with the
        preview alone, the preview's) *)
    | Files_preview
    (** the file browser's preview beside its list, from its left edge *)
    | Agents (** the agents panel's width, from its left edge *)
    | Terminal (** the terminal panel's height, from its top edge *)
    | Review_chat
    (** the chat's width beside the review page, from its left edge *)
    | Review_files
    (** the review's list of changed files, from its right edge *)
    | Review_margin
    (** the review's column of comments right of the code, from its left
        edge: [--review-margin-width]; place it on that column with
        [Resize_view.handle m ~inject Review_margin] *)
  [@@deriving sexp_of, equal, compare, enumerate]

  (** As in [data-resize]: ["sidebar"], ["files-preview"]... *)
  val name : t -> string

  val of_name : string -> t option

  (** The user's preference's key ([web.sidebar-width]...), see
      {!Pref_sync}. *)
  val pref_key : t -> string

  (** The CSS variable ([--sidebar-width]...), in pixels. *)
  val var : t -> string

  (** "Resize the sessions sidebar" *)
  val label : t -> string

  (** Whether it moves left and right (a vertical edge) rather than up and
      down. *)
  val vertical : t -> bool

  (** The direction the edge moves to make the panel bigger: [1] right or
      down, [-1] left or up. *)
  val grows : t -> int
end

module Viewport : sig
  type t =
    { width : int
    ; height : int
    }
  [@@deriving sexp_of, equal]
end

module Shown : sig
  (** The side panels open (and the file browser's preview, the review
      page), and whether the page is phone-sized (no handles: the panels are
      sheets). *)
  type t =
    { sidebar : bool
    ; files : bool
    ; preview : bool
    ; agents : bool
    ; reviewing : bool
    ; narrow : bool
    }
  [@@deriving sexp_of]
end

module Bounds : sig
  type t =
    { min : int
    ; max : int
    ; value : int (** the size it has now *)
    }
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

(** No preferences, a 1280x800 window until the page says. *)
val empty : t

val chat_min : int

(** What the review page keeps beside the chat docked by it. *)
val review_min : int

(** Below this width the file browser floats over the chat; below
    [agents_floats_below], the agents panel (as in the CSS). *)
val files_floats_below : int

val agents_floats_below : int
val viewport : t -> Viewport.t

(** The preference, if the user set one. *)
val preference : t -> Handle.t -> int option

val bounds : t -> Shown.t -> Handle.t -> Bounds.t

(** Whether the file browser's preview takes the list's place rather than
    sitting left of it: on a phone, floating over the chat, or without room
    for both beside the chat. *)
val preview_alone : t -> Shown.t -> bool

(** The width of the review page's file view, beside its file list (the
    page's own padding included). *)
val review_room : t -> Shown.t -> int

(** The file browser's preview's width. *)
val preview_width : t -> Shown.t -> int

(** Every size, as CSS variables for the page ([--sidebar-width: 280px]). *)
val vars : t -> Shown.t -> (string * string) list

module Action : sig
  type t =
    | Set of Handle.t * int (** dragged to this size *)
    | Nudge of Handle.t * int (** by steps of 16px, [+] bigger *)
    | Smallest of Handle.t
    | Largest of Handle.t
    | Reset of Handle.t (** back to the default *)
    | Viewport of Viewport.t (** the window resized *)
    | Restore of (Handle.t * int option) list
    (** the preferences kept ([None]: none, the default) *)
  [@@deriving sexp_of]
end

module Save : sig
  (** A preference to keep ([None]: forget it), by its [Handle.pref_key]. *)
  type t =
    { key : string
    ; value : int option
    }
  [@@deriving sexp_of, equal]
end

val update : t -> Shown.t -> Action.t -> t * Save.t list
