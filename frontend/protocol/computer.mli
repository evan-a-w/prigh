open! Core

(** What a tool host that offers computer use says about the screen it
    controls ([hosts[].computer]): the helper's backend and the controlled
    output's size, real and as the model sees it. *)
type t =
  { backend : string (** [wlroots], [x11], [fake], ... *)
  ; detail : string option (** e.g. [Hyprland] *)
  ; platform : string option
  ; width : int
  ; height : int
  ; scaled_width : int
  ; scaled_height : int
  ; scale : float (** the compositor's HiDPI factor *)
  }
[@@deriving sexp_of, equal]

(** Lenient: missing sizes are 0, a missing scale 1. *)
val of_json : Json.t -> t Or_error.t
