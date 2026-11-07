open! Core
open! Import

(** What the [computer] tool does on the screen: the tool's arguments and
    the action [prigh-computer] is sent (the same JSON). Coordinates are in
    the screenshot's (scaled) space. A superset of Anthropic's and OpenAI's
    computer-use actions, so their native tools map onto it. *)

module Button : sig
  type t =
    | Left
    | Right
    | Middle
    | Back
    | Forward
  [@@deriving sexp_of, equal, enumerate]

  val to_string : t -> string
end

module Point : sig
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of, equal]
end

module Region : sig
  type t =
    { x0 : int
    ; y0 : int
    ; x1 : int
    ; y1 : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Screenshot
  | Click of
      { at : Point.t option (** [None]: where the pointer is *)
      ; button : Button.t
      ; count : int (** 1 to 3 *)
      ; keys : string option (** modifiers held, e.g. ["ctrl+shift"] *)
      }
  | Move of Point.t
  | Drag of
      { path : Point.t list (** at least two points *)
      ; button : Button.t
      ; keys : string option
      }
  | Mouse_down of
      { at : Point.t option
      ; button : Button.t
      }
  | Mouse_up of
      { at : Point.t option
      ; button : Button.t
      }
  | Scroll of
      { at : Point.t option
      ; dx : int (** wheel notches, positive to the right *)
      ; dy : int (** positive down *)
      ; keys : string option
      }
  | Type of string
  | Key of string
  (** combos like ["ctrl+shift+t"], several separated by spaces *)
  | Hold_key of
      { combo : string
      ; duration : float (** seconds, at most 10 *)
      }
  | Wait of float (** seconds, at most 30 *)
  | Zoom of Region.t
  | Cursor_position
[@@deriving sexp_of, equal]

(** The tool's arguments. Errors say what is wrong and what to send. *)
val of_json : Json.t -> t Or_error.t

(** The canonical JSON (defaults filled in), what the helper is sent. *)
val to_json : t -> Json.t

(** "click (640, 312)", "double-click (1, 2)", "type “hello”", "key
    ctrl+shift+t", "scroll ↓3 at (640, 312)", "drag (1, 2) → (3, 4)",
    "screenshot", "zoom [0, 0, 100, 50]", "wait 2s", "cursor position" *)
val describe : t -> string

(** The point it acts on (the last of a drag), if any. *)
val point : t -> Point.t option

(** Whether a screenshot follows it: all but [Wait]. *)
val takes_screenshot : t -> bool

(** The JSON schema of the tool's parameters. *)
val parameters : Json.t
