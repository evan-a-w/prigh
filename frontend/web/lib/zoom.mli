open! Core

(** The image viewer's (and the drawing dialog's) zoom and pan: pure maths,
    the page applies it as a CSS transform (app/viewer_widget.ml,
    app/annotate_widget.ml).

    An image of [image] pixels is shown in a stage of [stage] CSS pixels at
    [scale] (CSS pixels per image pixel), its centre [x], [y] from the
    stage's centre. "Fit" shows all of it: never cropped, never past the
    stage. *)

type size =
  { w : float
  ; h : float
  }
[@@deriving sexp_of, equal]

type t =
  { stage : size
  ; image : size
  ; scale : float
  ; x : float
  ; y : float
  }
[@@deriving sexp_of, equal]

(** The scale that shows the whole image. A small image (an icon) is scaled
    up to about {!small} pixels on its longer side, at most to the stage. *)
val fit_scale : stage:size -> image:size -> float

(** 256: what a small image is scaled up to at fit. *)
val small : float

(** The bounds: down to fit (or 100% if smaller), up to 8× (or 4× fit). *)
val min_scale : t -> float

val max_scale : t -> float

(** At fit, centred. *)
val create : stage:size -> image:size -> t

(** Kept on a new stage size (a resized window): at fit stays at fit,
    zoomed keeps its scale, clamped. *)
val resize : t -> stage:size -> t

val at_fit : t -> bool

(** Scaled by [factor] about [at] (from the stage's centre; default the
    centre), keeping the point under it still, within the bounds. *)
val zoom : ?at:float * float -> t -> factor:float -> t

(** To [scale] about [at]. *)
val zoom_to : ?at:float * float -> t -> scale:float -> t

val fit : t -> t

(** 100%: one image pixel per CSS pixel. *)
val actual : ?at:float * float -> t -> t

(** Double-click: zoomed in goes back to fit; at fit goes to 100%, or to 2×
    fit for an image that fits at 100% already. *)
val toggle : ?at:float * float -> t -> t

(** Moved by [dx], [dy], clamped. *)
val pan : t -> dx:float -> dy:float -> t

(** Where the image may be: a side smaller than the stage is centred; a
    larger one always covers the stage on that axis, so the image can't be
    lost off screen. *)
val clamp : t -> t

(** The step of the zoom buttons and keys. *)
val step : float

(** [scale] as a whole percentage, "37%". *)
val percent : t -> string

(** The CSS transform for an image laid out at its natural size with its
    centre at the stage's: [translate(...) scale(...)]. *)
val transform : t -> string
