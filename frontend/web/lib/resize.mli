open! Core

(** The sizes of the page's panels, which the user drags (or moves with the
    keyboard) from a handle on their inner edge: each one's preference, kept
    with the user's preferences, and the size it gets in the window it is in. Side panels
    docked beside the chat share the window's width so that the chat keeps
    [chat_min] pixels: when the window shrinks, they shrink (towards their
    minimums) in proportion, and grow back to their preferences with it. *)

module Handle : sig
  type t =
    | Sidebar (** the sessions sidebar's width, from its right edge *)
    | Files (** the file browser's width, from its left edge *)
    | Files_split (** the file browser's list over its preview, in percent *)
    | Agents (** the agents panel's width, from its left edge *)
    | Terminal (** the terminal panel's height, from its top edge *)
  [@@deriving sexp_of, equal, compare, enumerate]

  (** As in [data-resize]: ["sidebar"], ["files"], ["files-split"]... *)
  val name : t -> string

  val of_name : string -> t option

  (** The user's preference's key ([web.sidebar-width]...), see
      {!Pref_sync}. *)
  val pref_key : t -> string

  (** The CSS variable ([--sidebar-width]...). *)
  val var : t -> string

  (** "Resize the sessions sidebar" *)
  val label : t -> string

  (** Whether it moves left and right (a vertical edge) rather than up and
      down. *)
  val vertical : t -> bool

  (** The direction the edge moves to make the panel bigger: [1] right or
      down, [-1] left or up. *)
  val grows : t -> int

  (** ["px"] or ["%"]. *)
  val unit : t -> string
end

module Viewport : sig
  type t =
    { width : int
    ; height : int
    }
  [@@deriving sexp_of, equal]
end

module Shown : sig
  (** The side panels open, and whether the page is phone-sized (no handles:
      the panels are sheets). *)
  type t =
    { sidebar : bool
    ; files : bool
    ; agents : bool
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

(** Below this width the file browser floats over the chat; below
    [agents_floats_below], the agents panel (as in the CSS). *)
val files_floats_below : int

val agents_floats_below : int
val viewport : t -> Viewport.t

(** The preference, if the user set one. *)
val preference : t -> Handle.t -> int option

val bounds : t -> Shown.t -> Handle.t -> Bounds.t

(** Every size, as CSS variables for the page ([--sidebar-width: 280px]). *)
val vars : t -> Shown.t -> (string * string) list

module Action : sig
  type t =
    | Set of Handle.t * int (** dragged to this size *)
    | Nudge of Handle.t * int (** by steps (16px or 5%), [+] bigger *)
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
