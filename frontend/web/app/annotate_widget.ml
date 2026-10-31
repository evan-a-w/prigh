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

(* The image is shown as large as the stage allows (a small one scaled up,
   so there is room to draw), the canvas over it. The canvas has the
   image's own resolution, or the shown size when that is larger, so the
   drawing keeps its detail. *)
let setup () =
  strokes := [];
  drawing := None;
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
      c##.width := Int.max img##.naturalWidth (Float.to_int dw);
      c##.height := Int.max img##.naturalHeight (Float.to_int dh);
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

let down ev =
  match on_canvas ev with
  | None -> ()
  | Some c ->
    ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
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

let move ev =
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

let up ev =
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
      undo ()))
;;
