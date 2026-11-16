open! Core
open Js_of_ocaml

let editor () =
  Dom_html.getElementById_coerce "editor" Dom_html.CoerceTo.textarea
;;

let set_if_changed (prop : Js.js_string Js.t) value ~set =
  let value = Js.string value in
  if not (Js.equals prop value) then set value
;;

(* Measuring from scratch (height auto, then its scrollHeight) lays the page
   out twice; text added at its end can only make it taller, which reading
   [scrollHeight] alone tells. [scrollHeight] is rounded and the text may be
   a fraction taller, which [overflow-y: auto] would show a scrollbar for:
   below its max-height, its overflow is hidden. *)
let measure ~grown (el : Dom_html.textAreaElement Js.t) =
  let style = el##.style in
  if not grown then style##.height := Js.string "auto";
  if (not grown) || el##.scrollHeight > el##.clientHeight
  then
    set_if_changed
      style##.height
      (sprintf "%dpx" el##.scrollHeight)
      ~set:(fun v -> style##.height := v);
  set_if_changed
    style##.overflowY
    (if el##.scrollHeight > el##.clientHeight + 1 then "auto" else "hidden")
    ~set:(fun v -> style##.overflowY := v)
;;

let fit_now () = Option.iter (editor ()) ~f:(measure ~grown:false)

module Watched = struct
  (* The editor whose width is observed (Bonsai may replace it), its last
     width, and whether a fit is due in the next frame. *)
  type t =
    { editor : Dom_html.textAreaElement Js.t
    ; observer : ResizeObserver.resizeObserver Js.t
    ; mutable width : float
    ; mutable due : bool
    }
end

let watched : Watched.t option ref = ref None

(* Fitted in the next frame: resizing it in the observer's callback is a
   loop the browser reports as an error. *)
let on_resize entries _ =
  Option.iter !watched ~f:(fun (w : Watched.t) ->
    Array.iter
      (Js.to_array entries)
      ~f:(fun (entry : ResizeObserver.resizeObserverEntry Js.t) ->
        let width = Js.to_float entry##.contentRect##.width in
        if not (Float.equal width w.width)
        then (
          w.width <- width;
          if not w.due
          then (
            w.due <- true;
            ignore
              (Dom_html.window##requestAnimationFrame
                 (Js.wrap_callback (fun (_ : Js.number_t) ->
                    w.due <- false;
                    fit_now ()))
               : Dom_html.animation_frame_request_id)))))
;;

let watch editor =
  match !watched with
  | Some w when phys_equal w.editor editor -> ()
  | previous ->
    Option.iter previous ~f:(fun (w : Watched.t) -> w.observer##disconnect);
    watched
    := Some
         { editor
         ; observer = ResizeObserver.observe ~node:editor ~f:on_resize ()
         ; width = Float.nan
         ; due = false
         }
;;

let fit ~grown =
  Browser.after_render (fun () ->
    Option.iter (editor ()) ~f:(fun el ->
      if ResizeObserver.is_supported () then watch el;
      measure ~grown el))
;;

let install () =
  let fonts : Dom_html.eventTarget Js.t Js.Optdef.t =
    (Js.Unsafe.coerce Dom_html.document)##.fonts
  in
  Js.Optdef.iter fonts (fun fonts ->
    ignore
      (Dom_html.addEventListener
         fonts
         (Dom.Event.make "loadingdone")
         (Dom_html.handler (fun _ ->
            fit_now ();
            Js._true))
         Js._false
       : Dom_html.event_listener_id))
;;
