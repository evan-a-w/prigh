open! Core

(** The terminal's extra keys on a touch screen (Termux's, Blink's): the keys
    a phone's keyboard lacks, in a bar over it. Pure: which keys, the sticky
    Ctrl and Alt, and the bytes each key sends; app/terminal_widget.ml draws
    the bar and feeds the bytes to xterm.js. *)

module Modifier : sig
  type t =
    | Ctrl
    | Alt
  [@@deriving sexp_of, equal]
end

module Key : sig
  type t =
    | Esc
    | Tab
    | Modifier of Modifier.t
    | Left
    | Up
    | Down
    | Right
    | Char of char (** awkward on a phone's keyboard: [| ~ / -] *)
    | Home
    | End
    | Page_up
    | Page_down
  [@@deriving sexp_of, equal]

  (** In the bar's order: the most used first, the rest scrolled to. *)
  val all : t list

  (** What the key shows. *)
  val label : t -> string

  (** What a screen reader says. *)
  val name : t -> string

  (** Held down, it repeats (the arrows). *)
  val repeats : t -> bool
end

module Latch : sig
  type t =
    | Off
    | Once (** for the next key only *)
    | Locked (** until tapped again *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val empty : t
val latch : t -> Modifier.t -> Latch.t

(** Two taps of a modifier within this many milliseconds lock it. *)
val double_tap_ms : float

(** [key] pressed at [now] (milliseconds): the bytes for the shell (none for a
    modifier, which latches) and the modifiers after it (a key releases a
    [Once] modifier). Arrows, Home and End follow the cursor keys' mode
    (DECCKM: [app_cursor], which vim and less set), as xterm sends them. *)
val press : t -> Key.t -> app_cursor:bool -> now:float -> t * string

(** Text from the phone's keyboard with the latched modifiers applied to its
    first character: Ctrl+c is [\x03], Alt+x is [ESC x]. Without a modifier
    latched, [data] as it is. *)
val type_ : t -> string -> t * string
