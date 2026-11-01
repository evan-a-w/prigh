open! Core
open Prigh_web

let size w h : Zoom.size = { w; h }
let desktop = size 1248. 700.

let show (t : Zoom.t) =
  let shown_w = t.image.w *. t.scale
  and shown_h = t.image.h *. t.scale in
  let left = (t.stage.w /. 2.) +. t.x -. (shown_w /. 2.)
  and top = (t.stage.h /. 2.) +. t.y -. (shown_h /. 2.) in
  printf
    "%s  shown %.0fx%.0f at (%.0f, %.0f)  inside: %b  covers: %b\n"
    (Zoom.percent t)
    shown_w
    shown_h
    left
    top
    Float.(
      left >= -0.01
      && top >= -0.01
      && left +. shown_w <= t.stage.w +. 0.01
      && top +. shown_h <= t.stage.h +. 0.01)
    Float.(
      (shown_w <= t.stage.w
       || (left <= 0.01 && left +. shown_w >= t.stage.w -. 0.01))
      && (shown_h <= t.stage.h
          || (top <= 0.01 && top +. shown_h >= t.stage.h -. 0.01)))
;;

let%expect_test "fit: the whole image, never cropped; small ones scaled up" =
  List.iter
    [ "tall", size 600. 2400.
    ; "wide", size 2400. 400.
    ; "tiny", size 24. 24.
    ; "small", size 300. 200.
    ; "phone tall", size 600. 2400.
    ]
    ~f:(fun (name, image) ->
      let stage =
        if String.is_prefix name ~prefix:"phone"
        then size 374. 700.
        else desktop
      in
      printf "%-10s " name;
      show (Zoom.create ~stage ~image));
  [%expect
    {|
    tall       29%  shown 175x700 at (537, 0)  inside: true  covers: true
    wide       52%  shown 1248x208 at (0, 246)  inside: true  covers: true
    tiny       1067%  shown 256x256 at (496, 222)  inside: true  covers: true
    small      100%  shown 300x200 at (474, 250)  inside: true  covers: true
    phone tall 29%  shown 175x700 at (100, 0)  inside: true  covers: true
    |}]
;;

let%expect_test "zoom about a point keeps it still; pan is clamped" =
  let t = Zoom.create ~stage:desktop ~image:(size 600. 2400.) in
  (* The image's point under the pointer, before and after. *)
  let under (t : Zoom.t) (px, py) =
    (px -. t.x) /. t.scale, (py -. t.y) /. t.scale
  in
  let at = 0., -300. in
  let z = Zoom.zoom ~at t ~factor:2. in
  let (ax, ay), (bx, by) = under t at, under z at in
  printf
    "same point: %b\n"
    Float.(abs (ax -. bx) < 1e-6 && abs (ay -. by) < 1e-6);
  show z;
  let a = Zoom.actual t in
  show a;
  (* Far past the top: stops with the image's top at the stage's. *)
  show (Zoom.pan a ~dx:0. ~dy:100_000.);
  show (Zoom.pan a ~dx:0. ~dy:(-100_000.));
  (* x: 600 wide in a 1248 stage stays centred. *)
  show (Zoom.pan a ~dx:500. ~dy:0.);
  [%expect
    {|
    same point: true
    58%  shown 350x1400 at (449, -50)  inside: false  covers: true
    100%  shown 600x2400 at (324, -850)  inside: false  covers: true
    100%  shown 600x2400 at (324, 0)  inside: false  covers: true
    100%  shown 600x2400 at (324, -1700)  inside: false  covers: true
    100%  shown 600x2400 at (324, -850)  inside: false  covers: true
    |}]
;;

let%expect_test "bounds, toggle, resize" =
  let t = Zoom.create ~stage:desktop ~image:(size 600. 2400.) in
  show (Zoom.zoom t ~factor:0.01);
  show (Zoom.zoom t ~factor:1000.);
  let toggled = Zoom.toggle t in
  show toggled;
  show (Zoom.toggle toggled);
  (* An image that fits at 100%: double-click doubles it. *)
  show (Zoom.toggle (Zoom.create ~stage:desktop ~image:(size 300. 200.)));
  (* The tiny one: down to 100%, up to 4x its fit. *)
  let tiny = Zoom.create ~stage:desktop ~image:(size 24. 24.) in
  printf
    "tiny: %s .. %s\n"
    (Zoom.percent { tiny with scale = Zoom.min_scale tiny })
    (Zoom.percent { tiny with scale = Zoom.max_scale tiny });
  (* A smaller window: fit stays fit, zoomed stays zoomed and in bounds. *)
  show (Zoom.resize t ~stage:(size 374. 700.));
  show
    (Zoom.resize
       (Zoom.pan (Zoom.actual t) ~dx:0. ~dy:(-5000.))
       ~stage:(size 374. 700.));
  print_endline (Zoom.transform (Zoom.actual t));
  [%expect
    {|
    29%  shown 175x700 at (537, 0)  inside: true  covers: true
    800%  shown 4800x19200 at (-1776, -9250)  inside: false  covers: true
    100%  shown 600x2400 at (324, -850)  inside: false  covers: true
    29%  shown 175x700 at (537, 0)  inside: true  covers: true
    200%  shown 600x400 at (324, 150)  inside: true  covers: true
    tiny: 100% .. 4267%
    29%  shown 175x700 at (100, 0)  inside: true  covers: true
    100%  shown 600x2400 at (-113, -1700)  inside: false  covers: true
    translate(0.00px, 0.00px) scale(1.00000)
    |}]
;;
