open! Core
open Js_of_ocaml
module App = Prigh_web.App
module Action = App.Action

let schedule = ref (fun (_ : Action.t) -> ())
let get o name = Js.Unsafe.get o (Js.string name)
let meth o name args = Js.Unsafe.meth_call o name args

type stroke =
  { color : string
  ; width : float (** in the image's pixels *)
  ; mutable points : (float * float) list (** newest first *)
  }

let strokes : stroke list ref = ref [] (* newest first *)
let drawing : (stroke * int) option ref = ref None (* and its pointer *)
let by_id id = Dom_html.getElementById_opt id

let canvas () =
  Option.map (by_id "annotate-canvas") ~f:(fun el ->
    (Js.Unsafe.coerce el : Dom_html.canvasElement Js.t))
;;

let image () =
  Option.map (by_id "annotate-image") ~f:(fun el ->
    (Js.Unsafe.coerce el : Dom_html.imageElement Js.t))
;;

let line (ctx : Dom_html.canvasRenderingContext2D Js.t) (s : stroke) =
  ctx##.strokeStyle := Js.string s.color;
  ctx##.lineWidth := Js.float s.width;
  ctx##.lineCap := Js.string "round";
  ctx##.lineJoin := Js.string "round";
  ctx##beginPath;
  (match List.rev s.points with
   | [] -> ()
   | [ (x, y) ] ->
     (* A dot: a stroke that did not move. *)
     ctx##moveTo (Js.float x) (Js.float y);
     ctx##lineTo (Js.float (x +. 0.01)) (Js.float y)
   | (x, y) :: rest ->
     ctx##moveTo (Js.float x) (Js.float y);
     List.iter rest ~f:(fun (x, y) -> ctx##lineTo (Js.float x) (Js.float y)));
  ctx##stroke
;;

let redraw () =
  Option.iter (canvas ()) ~f:(fun c ->
    let ctx = c##getContext Dom_html._2d_ in
    ctx##clearRect
      (Js.float 0.)
      (Js.float 0.)
      (Js.float (Float.of_int c##.width))
      (Js.float (Float.of_int c##.height));
    List.iter (List.rev !strokes) ~f:(line ctx))
;;

let changed () = !schedule (Action.Annotate_strokes (List.length !strokes))

(* Zoom and pan (Prigh_web.Zoom) of the frame, as a CSS transform: the
   canvas's bounding box follows it, so {!point} stays right. One pointer
   draws; two pinch and pan; Ctrl+wheel (a trackpad's pinch) zooms about the
   pointer, the wheel pans; the toolbar's − % + and + − 0. *)
module Zoom = Prigh_web.Zoom

let zoom : Zoom.t option ref = ref None

(* The frame at scale 1 against the image's own pixels: the % shown. *)
let base = ref 1.

type finger =
  { mutable fx : float
  ; mutable fy : float
  }

let fingers : (int, finger) Hashtbl.t = Hashtbl.create (module Int)
let pinch : float option ref = ref None

let stage () =
  Js.Opt.to_option
    (Dom_html.document##querySelector (Js.string ".annotate-stage"))
;;

let frame () =
  Js.Opt.to_option
    (Dom_html.document##querySelector (Js.string ".annotate-frame"))
;;

let render_zoom ?(animate = false) () =
  match !zoom, frame () with
  | Some z, Some f ->
    f##.style##.transition
    := Js.string
         (if animate
          then "transform 0.16s cubic-bezier(0.2, 0.7, 0.2, 1)"
          else "none");
    f##.style##.transform := Js.string (Zoom.transform z);
    Js.Opt.iter
      (Dom_html.document##querySelector (Js.string ".annotate-zoom-pct"))
      (fun el ->
         let pct = Float.iround_nearest_exn (z.scale *. !base *. 100.) in
         el##.textContent := Js.some (Js.string (sprintf "%d%%" pct)));
    let set action disabled =
      Js.Opt.iter
        (Dom_html.document##querySelector
           (Js.string (sprintf "[data-annotate-zoom=%s]" action)))
        (fun el ->
           if disabled
           then el##setAttribute (Js.string "disabled") (Js.string "")
           else el##removeAttribute (Js.string "disabled"))
    in
    set "out" Float.(z.scale <= Zoom.min_scale z +. 1e-3);
    set "in" Float.(z.scale >= Zoom.max_scale z -. 1e-3)
  | _ -> ()
;;

let update_zoom ?animate f =
  Option.iter !zoom ~f:(fun z ->
    zoom := Some (f z);
    render_zoom ?animate ())
;;

(* From the page's coordinates to the stage centre's. *)
let stage_at ~x ~y =
  match stage () with
  | Some s ->
    let r = s##getBoundingClientRect in
    ( x -. Js.to_float r##.left -. (Float.of_int s##.clientWidth /. 2.)
    , y -. Js.to_float r##.top -. (Float.of_int s##.clientHeight /. 2.) )
  | None -> 0., 0.
;;

let zoom_action = function
  | "in" -> update_zoom ~animate:true (fun z -> Zoom.zoom z ~factor:Zoom.step)
  | "out" ->
    update_zoom ~animate:true (fun z -> Zoom.zoom z ~factor:(1. /. Zoom.step))
  | "fit" -> update_zoom ~animate:true Zoom.fit
  | _ -> ()
;;

(* The image is shown as large as the stage allows (a small one scaled up,
   so there is room to draw), the canvas over it. The canvas has the
   image's own resolution, up to twice that where the screen shows it finer,
   so strokes stay crisp without blowing a small image up. *)
let setup () =
  strokes := [];
  drawing := None;
  zoom := None;
  Hashtbl.clear fingers;
  pinch := None;
  Option.iter (frame ()) ~f:(fun f -> f##.style##.transform := Js.string "");
  match canvas (), image () with
  | Some c, Some img ->
    let size () =
      let nw = Float.of_int img##.naturalWidth in
      let nh = Float.of_int img##.naturalHeight in
      let sw, sh =
        match
          Dom_html.document##querySelector (Js.string ".annotate-stage")
          |> Js.Opt.to_option
        with
        | Some stage ->
          ( Float.of_int stage##.clientWidth -. 16.
          , Float.of_int stage##.clientHeight -. 16. )
        | None -> nw, nh
      in
      let fit =
        Float.min (sw /. nw) (sh /. nh) |> Float.min 8. |> Float.max 0.01
      in
      let dw = Float.round_down (nw *. fit)
      and dh = Float.round_down (nh *. fit) in
      img##.style##.width := Js.string (sprintf "%gpx" dw);
      img##.style##.height := Js.string (sprintf "%gpx" dh);
      (* The drawing's resolution: the image's own, finer only as far as
         the screen shows it finer (its pixel ratio, at most 2×), so a
         small image is not sent blown up. *)
      let dpr : float =
        match
          Js.Optdef.to_option (Js.Unsafe.get Dom_html.window "devicePixelRatio")
        with
        | Some r -> Js.to_float r
        | None -> 1.
      in
      let k = Float.max 1. (Float.min fit (Float.min dpr 2.)) in
      c##.width := Int.max 1 (Float.iround_nearest_exn (nw *. k));
      c##.height := Int.max 1 (Float.iround_nearest_exn (nh *. k));
      base := dw /. nw;
      zoom
      := Some (Zoom.create ~stage:{ w = sw; h = sh } ~image:{ w = dw; h = dh });
      render_zoom ();
      redraw ()
    in
    if Js.to_bool img##.complete && img##.naturalWidth > 0
    then size ()
    else
      img##.onload
      := Dom.handler (fun _ ->
           size ();
           Js._true)
  | _ -> ()
;;

let undo () =
  (match !strokes with
   | [] -> ()
   | _ :: rest -> strokes := rest);
  redraw ();
  changed ()
;;

let clear () =
  strokes := [];
  redraw ();
  changed ()
;;

let export () =
  match canvas (), image () with
  | Some c, Some img ->
    let out = Dom_html.createCanvas Dom_html.document in
    out##.width := c##.width;
    out##.height := c##.height;
    let ctx = out##getContext Dom_html._2d_ in
    ctx##drawImage_withSize
      img
      (Js.float 0.)
      (Js.float 0.)
      (Js.float (Float.of_int c##.width))
      (Js.float (Float.of_int c##.height));
    ctx##drawImage_fromCanvas c (Js.float 0.) (Js.float 0.);
    let url = Js.to_string (out##toDataURL_type (Js.string "image/png")) in
    (match String.lsplit2 url ~on:',' with
     | Some (_, data) ->
       !schedule
         (Action.Annotated
            { mime_type = "image/png"
            ; data
            ; bytes = Prigh_protocol.Image.decoded_size data
            })
     | None -> ())
  | _ -> ()
;;

(* Where the pointer is, in the image's pixels. *)
let point (c : Dom_html.canvasElement Js.t) ev =
  let rect = c##getBoundingClientRect in
  let w = Js.to_float rect##.right -. Js.to_float rect##.left in
  let h = Js.to_float rect##.bottom -. Js.to_float rect##.top in
  let sx = if Float.(w > 0.) then Float.of_int c##.width /. w else 1. in
  let sy = if Float.(h > 0.) then Float.of_int c##.height /. h else 1. in
  let cx : float = get ev "clientX" in
  let cy : float = get ev "clientY" in
  ( (cx -. Js.to_float rect##.left) *. sx
  , (cy -. Js.to_float rect##.top) *. sy
  , sx )
;;

let on_canvas ev =
  match canvas () with
  | Some c ->
    let target = get ev "target" in
    if Js.Unsafe.equals target c then Some c else None
  | None -> None
;;

let pointer_id ev : int = get ev "pointerId"
let finger_xy ev : float * float = get ev "clientX", get ev "clientY"

let spread () =
  match Hashtbl.data fingers with
  | [ a; b ] ->
    Some
      ( Float.hypot (a.fx -. b.fx) (a.fy -. b.fy)
      , ((a.fx +. b.fx) /. 2., (a.fy +. b.fy) /. 2.) )
  | _ -> None
;;

let down ev =
  match on_canvas ev with
  | None -> ()
  | Some _ when Hashtbl.length fingers >= 1 ->
    (* A second finger: a pinch, not a stroke; the one begun goes. *)
    ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
    (match !drawing with
     | Some (stroke, _) ->
       strokes := List.filter !strokes ~f:(fun s -> not (phys_equal s stroke));
       drawing := None;
       redraw ()
     | None -> ());
    let fx, fy = finger_xy ev in
    Hashtbl.set fingers ~key:(pointer_id ev) ~data:{ fx; fy };
    pinch := Option.map (spread ()) ~f:fst
  | Some c ->
    ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
    if String.equal (Js.to_string (get ev "pointerType")) "touch"
    then (
      let fx, fy = finger_xy ev in
      Hashtbl.set fingers ~key:(pointer_id ev) ~data:{ fx; fy });
    (try
       ignore
         (meth c "setPointerCapture" [| Js.Unsafe.inject (pointer_id ev) |]
          : Js.Unsafe.any)
     with
     | _ -> ());
    let x, y, scale = point c ev in
    let color =
      Js.Opt.case
        (c##getAttribute (Js.string "data-color"))
        (fun () -> "#ef4444")
        Js.to_string
    in
    let size =
      Js.Opt.case
        (c##getAttribute (Js.string "data-size"))
        (fun () -> 4.)
        (fun s ->
           Float.of_string_opt (Js.to_string s) |> Option.value ~default:4.)
    in
    let stroke = { color; width = size *. scale; points = [ x, y ] } in
    strokes := stroke :: !strokes;
    drawing := Some (stroke, pointer_id ev);
    redraw ()
;;

let move_stroke ev =
  match !drawing, canvas () with
  | Some (stroke, id), Some c when id = pointer_id ev ->
    ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
    let x, y, _ = point c ev in
    (match stroke.points with
     | (px, py) :: _ ->
       let ctx = c##getContext Dom_html._2d_ in
       let segment = { stroke with points = [ x, y; px, py ] } in
       line ctx segment
     | [] -> ());
    stroke.points <- (x, y) :: stroke.points
  | _ -> ()
;;

let move ev =
  match Hashtbl.find fingers (pointer_id ev) with
  | Some f when Hashtbl.length fingers >= 2 ->
    ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
    let before = spread () in
    let fx, fy = finger_xy ev in
    f.fx <- fx;
    f.fy <- fy;
    (match before, spread () with
     | Some (d0, (mx0, my0)), Some (d1, (mx1, my1)) ->
       let d0 = Option.value !pinch ~default:d0 in
       pinch := Some d1;
       update_zoom (fun z ->
         Zoom.pan
           (Zoom.zoom
              ~at:(stage_at ~x:mx1 ~y:my1)
              z
              ~factor:(d1 /. Float.max 1. d0))
           ~dx:(mx1 -. mx0)
           ~dy:(my1 -. my0))
     | _ -> ())
  | Some f ->
    let fx, fy = finger_xy ev in
    f.fx <- fx;
    f.fy <- fy;
    move_stroke ev
  | None -> move_stroke ev
;;

let up ev =
  Hashtbl.remove fingers (pointer_id ev);
  if Hashtbl.length fingers < 2 then pinch := None;
  match !drawing with
  | Some (_, id) when id = pointer_id ev ->
    drawing := None;
    changed ()
  | _ -> ()
;;

let install ~schedule:f =
  schedule := f;
  let listen event handler =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         (Dom.Event.make event)
         (Dom.handler (fun ev ->
            handler (Js.Unsafe.coerce ev);
            Js._true))
         Js._false
       : Dom_html.event_listener_id)
  in
  listen "pointerdown" down;
  listen "pointermove" move;
  listen "pointerup" up;
  listen "pointercancel" up;
  (* Not passive: a wheel over the stage zooms or pans it, not the page. *)
  ignore
    (meth
       Dom_html.document
       "addEventListener"
       [| Js.Unsafe.inject (Js.string "wheel")
        ; Js.Unsafe.inject
            (Js.wrap_callback (fun ev ->
               let target : Dom_html.element Js.t = get ev "target" in
               let in_stage =
                 Js.Opt.test
                   (meth
                      target
                      "closest"
                      [| Js.Unsafe.inject (Js.string ".annotate-stage") |])
               in
               if in_stage && Option.is_some !zoom
               then (
                 ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
                 let dx : float = get ev "deltaX"
                 and dy : float = get ev "deltaY" in
                 let k =
                   match (get ev "deltaMode" : int) with
                   | 1 -> 16.
                   | 2 -> 400.
                   | _ -> 1.
                 in
                 if
                   Js.to_bool (get ev "ctrlKey")
                   || Js.to_bool (get ev "metaKey")
                 then (
                   let x, y = finger_xy ev in
                   update_zoom (fun z ->
                     Zoom.zoom
                       ~at:(stage_at ~x ~y)
                       z
                       ~factor:(Float.exp (-.dy *. k *. 0.01))))
                 else
                   update_zoom (fun z ->
                     Zoom.pan z ~dx:(-.dx *. k) ~dy:(-.dy *. k)))))
        ; Js.Unsafe.inject
            (Js.Unsafe.obj [| "passive", Js.Unsafe.inject Js._false |])
       |]
     : Js.Unsafe.any);
  listen "click" (fun ev ->
    let target : Dom_html.element Js.t = get ev "target" in
    Js.Opt.iter
      (meth
         target
         "closest"
         [| Js.Unsafe.inject (Js.string "[data-annotate-zoom]") |])
      (fun (el : Dom_html.element Js.t) ->
         Js.Opt.iter
           (el##getAttribute (Js.string "data-annotate-zoom"))
           (fun a -> zoom_action (Js.to_string a))));
  (* Ctrl+Z (Cmd+Z) while drawing takes back the last stroke. *)
  listen "keydown" (fun ev ->
    let key = Js.to_string (get ev "key") in
    let ctrl = Js.to_bool (get ev "ctrlKey") || Js.to_bool (get ev "metaKey") in
    if
      ctrl
      && String.equal (String.lowercase key) "z"
      && Option.is_some (canvas ())
    then (
      ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
      undo ())
    else if (not ctrl) && Option.is_some (canvas ())
    then (
      let typing =
        Js.Opt.case
          Dom_html.document##.activeElement
          (fun () -> false)
          (fun el ->
             List.mem
               [ "INPUT"; "TEXTAREA" ]
               (Js.to_string el##.tagName)
               ~equal:String.equal)
      in
      let act a =
        ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
        zoom_action a
      in
      if not typing
      then (
        match key with
        | "+" | "=" -> act "in"
        | "-" | "_" -> act "out"
        | "0" -> act "fit"
        | _ -> ())))
;;
