open! Core
open! Import

(** A text field's or the terminal's context menu in a browser's app window
    (the desktop app's browser shell, see
    {!Desktop_link.browser_shell_of_hash}), where the browser's own menu is
    suppressed because it gives away that the window is Chrome: Cut, Copy,
    Paste, Select all (the terminal: Copy, Paste, Select all). Its own layer,
    over any dialog (whose fields it serves), owning the keyboard until it
    closes. *)

module Kind : sig
  type t =
    | Text_field (** a textarea or a text input *)
    | Terminal (** the terminal panel's xterm.js *)
  [@@deriving sexp_of, equal]
end

module Field : sig
  (** The right-clicked field as it stood. *)
  type t =
    { kind : Kind.t
    ; selection : bool (** text is selected in it *)
    ; editable : bool (** neither read-only nor disabled *)
    ; secret : bool (** a password: what it holds is not copied out *)
    }
  [@@deriving sexp_of, equal]
end

module Item : sig
  type t =
    | Cut
    | Copy
    | Paste
    | Select_all
  [@@deriving sexp_of, equal, enumerate]

  val label : t -> string

  (** Its key in that kind of field: [Ctrl+X], the terminal's
      [Ctrl+Shift+C] and so on. *)
  val key : Kind.t -> t -> string option
end

type t [@@deriving sexp_of, equal]

val create : Field.t -> anchor:Context_menu.Anchor.t -> t
val field : t -> Field.t
val anchor : t -> Context_menu.Anchor.t

(** The items it shows, in groups: the terminal has no Cut. *)
val sections : t -> Item.t list list

(** Shown, and: Cut and Copy need a selection (and not a password), Cut and
    Paste an editable field. *)
val enabled : t -> Item.t -> bool

(** The highlighted item: the first enabled one when it opens. *)
val selected : t -> Item.t

(** The highlight moved by [delta] among the enabled items, wrapping. *)
val move : t -> int -> t

(** Moved to this item ([None] when it is disabled). *)
val select : t -> Item.t -> t option

module Decision : sig
  type t =
    | Native (** the browser's own menu *)
    | Suppress (** no menu at all (but prigh's own, where it has one) *)
    | Open of Field.t
  [@@deriving sexp_of, equal]
end

(** What a right-click does: in the browser shell the browser's menu never
    shows, a text field ([Some]) gets this menu; elsewhere it is the
    browser's. *)
val decide : browser_shell:bool -> Field.t option -> Decision.t
