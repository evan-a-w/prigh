open! Core
open Js_of_ocaml
module Action = Prigh_web.App.Action
module Resize = Prigh_web.Resize

module Drag = struct
  type t =
    { handle_el : Dom_html.element Js.t
    ; handle : Resize.Handle.t
    ; target : Dom_html.element Js.t
    ; var : string
    ; grows : int
    ; vertical : bool
    ; pointer : int
    ; start_at : float
    ; start : int
    ; min : int
    ; max : int
    ; mutable value : int
    ; mutable frame : bool
    }
end

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

let int_attribute el name = Option.bind (attribute el name) ~f:Int.of_string_opt

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let set_var (el : Dom_html.element Js.t) var value =
  ignore
    (Js.Unsafe.meth_call
       el##.style
       "setProperty"
       [| Js.Unsafe.inject (Js.string var)
        ; Js.Unsafe.inject (Js.string value)
       |]
     : Js.Unsafe.any)
;;

let remove_var (el : Dom_html.element Js.t) var =
  ignore
    (Js.Unsafe.meth_call
       el##.style
       "removeProperty"
       [| Js.Unsafe.inject (Js.string var) |]
     : Js.Unsafe.any)
;;

let pointer_id (ev : Dom_html.pointerEvent Js.t) =
  (Js.Unsafe.coerce ev)##.pointerId
;;

let coordinate (ev : Dom_html.pointerEvent Js.t) ~vertical =
  Js.to_float (if vertical then ev##.clientX else ev##.clientY)
;;

let body_class (d : Drag.t) =
  Js.string (if d.vertical then "resizing-x" else "resizing-y")
;;

let listen ?(capture = false) target event handler =
  ignore
    (Dom_html.addEventListener
       target
       event
       (Dom.handler (fun ev ->
          handler ev;
          Js._true))
       (Js.bool capture)
     : Dom_html.event_listener_id)
;;

let start (handle_el : Dom_html.element Js.t) (ev : Dom_html.pointerEvent Js.t) =
  let open Option.Let_syntax in
  let%bind handle =
    Option.bind (attribute handle_el "data-resize") ~f:Resize.Handle.of_name
  in
  let%bind target =
    Option.bind (attribute handle_el "data-resize-target") ~f:(fun selector ->
      Js.Opt.to_option (Dom_html.document##querySelector (Js.string selector)))
  in
  let%bind var = attribute handle_el "data-resize-var" in
  let%bind grows = int_attribute handle_el "data-resize-grows" in
  let%bind start = int_attribute handle_el "aria-valuenow" in
  let%bind min = int_attribute handle_el "aria-valuemin" in
  let%map max = int_attribute handle_el "aria-valuemax" in
  let vertical = Resize.Handle.vertical handle in
  { Drag.handle_el
  ; handle
  ; target
  ; var
  ; grows
  ; vertical
  ; pointer = pointer_id ev
  ; start_at = coordinate ev ~vertical
  ; start
  ; min
  ; max
  ; value = start
  ; frame = false
  }
;;

let finish (d : Drag.t) =
  Dom_html.document##.body##.classList##remove (body_class d);
  d.handle_el##.classList##remove (Js.string "dragging");
  (* A click focuses the handle: the sodium ring is for the keyboard's. *)
  d.handle_el##blur;
  (* Once the model has the size, the page's root shows it. *)
  Browser.after_render (fun () -> remove_var d.target d.var)
;;

let install_drag ~schedule =
  let dragging = ref None in
  let doc = Dom_html.document in
  listen
    doc
    Dom_html.Event.pointerdown
    (fun (ev : Dom_html.pointerEvent Js.t) ->
       if ev##.button = 0
       then
         Option.iter
           (Option.bind
              (Js.Opt.to_option ev##.target)
              ~f:(fun el -> closest el "[data-resize]"))
           ~f:(fun handle_el ->
             Option.iter (start handle_el ev) ~f:(fun d ->
               (* No text selection, and no focus ring after a drag. *)
               Dom.preventDefault ev;
               ignore
                 (Js.Unsafe.meth_call
                    handle_el
                    "setPointerCapture"
                    [| Js.Unsafe.inject d.pointer |]
                  : Js.Unsafe.any);
               Dom_html.document##.body##.classList##add (body_class d);
               handle_el##.classList##add (Js.string "dragging");
               dragging := Some d)));
  listen
    doc
    Dom_html.Event.pointermove
    (fun (ev : Dom_html.pointerEvent Js.t) ->
       match !dragging with
       | Some (d : Drag.t) when pointer_id ev = d.pointer ->
         let moved =
           Float.of_int d.grows
           *. (coordinate ev ~vertical:d.vertical -. d.start_at)
         in
         let value =
           Int.max
             d.min
             (Int.min
                d.max
                (d.start + Float.iround_nearest_exn moved))
         in
         if value <> d.value
         then (
           d.value <- value;
           if not d.frame
           then (
             d.frame <- true;
             ignore
               (Dom_html.window##requestAnimationFrame
                  (Js.wrap_callback (fun (_ : Js.number_t) ->
                     d.frame <- false;
                     set_var d.target d.var (sprintf "%dpx" d.value)))
                : Dom_html.animation_frame_request_id)))
       | _ -> ());
  let stop (ev : Dom_html.pointerEvent Js.t) =
    match !dragging with
    | Some (d : Drag.t) when pointer_id ev = d.pointer ->
      dragging := None;
      finish d;
      if d.value <> d.start
      then schedule (Action.Resize (Set (d.handle, d.value)))
    | _ -> ()
  in
  listen doc Dom_html.Event.pointerup stop;
  listen doc (Dom_html.Event.make "pointercancel") stop;
  (* Esc while dragging puts it back. *)
  listen
    ~capture:true
    doc
    Dom_html.Event.keydown
    (fun (ev : Dom_html.keyboardEvent Js.t) ->
       match !dragging with
       | Some d
         when String.equal
                (Js.Optdef.case ev##.key (fun () -> "") Js.to_string)
                "Escape" ->
         dragging := None;
         Dom.preventDefault ev;
         Dom_html.stopPropagation ev;
         remove_var d.target d.var;
         finish d
       | _ -> ())
;;

let viewport () =
  Action.Resize
    (Viewport
       { width = Dom_html.window##.innerWidth
       ; height = Dom_html.window##.innerHeight
       })
;;

let install ~schedule =
  schedule (viewport ());
  install_drag ~schedule
;;
