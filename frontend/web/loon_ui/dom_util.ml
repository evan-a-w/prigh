open! Import
open Js_of_ocaml

let key (ev : Dom_html.keyboardEvent Js.t) = Dom_html.Keyboard_code.of_event ev
;;

let focus_now selector =
  match Js.Opt.to_option (Dom_html.document##querySelector (Js.string selector)) with
  | None -> ()
  | Some el -> el##focus
;;

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let focus_selector selector =
  Effect.of_sync_fun
    (fun () ->
      if in_browser ()
      then
        ignore
          (Dom_html.window##requestAnimationFrame
             (Js.wrap_callback (fun (_ : Js.number_t) -> focus_now selector))
           : Dom_html.animation_frame_request_id))
    ()
;;

let prevent_default (ev : #Dom_html.event Js.t) =
  Effect.of_thunk (fun () -> ev##preventDefault)
;;

let stop (ev : #Dom_html.event Js.t) =
  Effect.of_thunk (fun () ->
    ev##preventDefault;
    ev##stopPropagation)
;;

let focus_selector_now selector =
  Effect.of_sync_fun (fun () -> if in_browser () then focus_now selector) ()
;;
