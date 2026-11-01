open! Core

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

let small = 256.
let step = 1.5
let positive v = if Float.(v > 0.) && Float.is_finite v then v else 1.

let fit_scale ~stage ~image =
  let iw = positive image.w
  and ih = positive image.h in
  let contain = Float.min (positive stage.w /. iw) (positive stage.h /. ih) in
  let upscale = Float.max 1. (small /. Float.max iw ih) in
  Float.min contain upscale
;;

let fit_of t = fit_scale ~stage:t.stage ~image:t.image
let min_scale t = Float.min (fit_of t) 1.
let max_scale t = Float.max 8. (fit_of t *. 4.)

let clamp t =
  let scale = Float.clamp_exn t.scale ~min:(min_scale t) ~max:(max_scale t) in
  let axis pos ~image ~stage =
    let room = ((image *. scale) -. stage) /. 2. in
    if Float.(room <= 0.)
    then 0.
    else Float.clamp_exn pos ~min:(-.room) ~max:room
  in
  { t with
    scale
  ; x = axis t.x ~image:t.image.w ~stage:t.stage.w
  ; y = axis t.y ~image:t.image.h ~stage:t.stage.h
  }
;;

let create ~stage ~image =
  { stage; image; scale = fit_scale ~stage ~image; x = 0.; y = 0. }
;;

let at_fit t = Float.(abs (t.scale -. fit_of t) < 1e-3)

let resize t ~stage =
  if at_fit t then create ~stage ~image:t.image else clamp { t with stage }
;;

let zoom_to ?(at = 0., 0.) t ~scale =
  let px, py = at in
  let scale = Float.clamp_exn scale ~min:(min_scale t) ~max:(max_scale t) in
  let k = scale /. t.scale in
  clamp
    { t with scale; x = px -. ((px -. t.x) *. k); y = py -. ((py -. t.y) *. k) }
;;

let zoom ?at t ~factor = zoom_to ?at t ~scale:(t.scale *. factor)
let fit t = { t with scale = fit_of t; x = 0.; y = 0. }
let actual ?at t = zoom_to ?at t ~scale:1.

let toggle ?at t =
  let fit_scale = fit_of t in
  if Float.(t.scale > fit_scale +. 1e-3)
  then fit t
  else if Float.(fit_scale < 0.999)
  then actual ?at t
  else zoom_to ?at t ~scale:(fit_scale *. 2.)
;;

let pan t ~dx ~dy = clamp { t with x = t.x +. dx; y = t.y +. dy }
let percent t = sprintf "%d%%" (Float.iround_nearest_exn (t.scale *. 100.))

let transform t =
  sprintf "translate(%.2fpx, %.2fpx) scale(%.5f)" t.x t.y t.scale
;;
