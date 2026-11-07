open! Core
open! Import

(** The [computer] tool's call (an action on the user's screen, coordinates
    in the screenshot's scaled space) as the frontends show it. *)

module Point : sig
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Screenshot
  | Click of
      { at : Point.t option
      ; button : string
      ; count : int
      ; keys : string option
      }
  | Move of Point.t option
  | Drag of Point.t list
  | Mouse_down of
      { at : Point.t option
      ; button : string
      }
  | Mouse_up of
      { at : Point.t option
      ; button : string
      }
  | Scroll of
      { at : Point.t option
      ; dx : int
      ; dy : int
      }
  | Type of string
  | Key of string
  | Hold_key of
      { combo : string
      ; duration : float option
      }
  | Wait of float option
  | Zoom of int list
  | Cursor_position
  | Other of string (** an action this frontend does not know *)
[@@deriving sexp_of]

(** [None] while the arguments stream in without an [action] yet. A call
    still streaming gives what it has so far. *)
val of_call : P.Tool_call.t -> t option

(** As prigh-web's tool card says it: [click (640, 312)], [double-click
    (…)], [type “hello”], [key ctrl+shift+t], [scroll ↓3 at (640, 312)],
    [drag (10, 10) → (200, 40)], [screenshot], [zoom [0, 0, 100, 80]],
    [wait 2s], [cursor position]. *)
val describe : t -> string

(** The same, compact, for a terminal's one line: [click 640,312]. *)
val compact : t -> string

(** Where the action points, to mark on its screenshot: the click, the
    move, the end of a drag, the scroll's point. *)
val point : t -> Point.t option

(** [W×H] from the result's last line ([screenshot 1280×800]). *)
val screenshot_size : string -> (int * int) option
