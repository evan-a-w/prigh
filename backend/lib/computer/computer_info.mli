open! Core
open! Import

(** What a tool host with computer use enabled says about the screen it
    controls ([prigh-computer --probe], [hello]'s [computer]): the helper's
    backend, the controlled output's real size in pixels and the size of the
    screenshots the model sees (whose coordinates it gives). *)
type t =
  { backend : string (** [wlroots], [x11], [fake], ... *)
  ; detail : string option (** the compositor or server, e.g. [Hyprland] *)
  ; platform : string (** [linux], ... *)
  ; output : string option (** the output (monitor) controlled *)
  ; width : int
  ; height : int
  ; scale : float (** the output's HiDPI scale factor *)
  ; scaled_width : int
  ; scaled_height : int
  }
[@@deriving sexp_of, equal]

(** Lenient: unknown fields are ignored, [detail] and [output] may be
    missing, [scale] defaults to 1. *)
val of_json : Json.t -> t Or_error.t

val to_json : t -> Json.t

(** Where a point of the screenshot (the model's coordinates) is on the
    screen: scaled by [width / scaled_width] and [height / scaled_height],
    rounded, within the screen. *)
val to_real : t -> x:int -> y:int -> int * int

(** The screenshot's size for a screen of [width] x [height]: the longest
    side at most [max_long_side] and at most [max_pixels] pixels, keeping
    the aspect ratio, never enlarged. *)
val scaled_size
  :  max_long_side:int
  -> max_pixels:int
  -> width:int
  -> height:int
  -> int * int

(** "wlroots (Hyprland), 2560×1440 shown as 1280×720" *)
val to_string_hum : t -> string
