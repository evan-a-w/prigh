open! Core
open! Async_kernel
open Js_of_ocaml
module Browser = Prigh_ui_web_app.Browser

let notification_class () : Js.Unsafe.any =
  Js.Unsafe.get Dom_html.window "Notification"
;;

let supported () =
  not
    (Js.to_bool
       (Js.Unsafe.fun_call
          (Js.Unsafe.js_expr "(function (c) { return c === undefined; })")
          [| Js.Unsafe.inject (notification_class ()) |]))
;;

let permission () =
  if supported ()
  then Js.to_string (Js.Unsafe.get (notification_class ()) "permission")
  else "unsupported"
;;

let request_permission () =
  if not (supported ())
  then return "unsupported"
  else (
    let result = Ivar.create () in
    let answer value =
      Ivar.fill_if_empty result (Js.to_string (Js.Unsafe.coerce value))
    in
    let promise =
      Js.Unsafe.meth_call (notification_class ()) "requestPermission" [||]
    in
    ignore
      (Js.Unsafe.meth_call
         promise
         "then"
         [| Js.Unsafe.inject (Js.wrap_callback answer) |]
       : Js.Unsafe.any);
    Ivar.read result)
;;

let key = "prigh-web.notify"

let saved () =
  match Browser.get_item key with
  | Some "off" -> false
  | Some _ | None -> true
;;

let save enabled = Browser.set_item key (if enabled then "on" else "off")

let show (n : Prigh_ui.Notify.t) =
  if supported () && String.equal (permission ()) "granted"
  then (
    let options =
      Js.Unsafe.obj
        [| "body", Js.Unsafe.inject (Js.string n.body)
         ; "tag", Js.Unsafe.inject (Js.string "prigh")
         ; "renotify", Js.Unsafe.inject Js._true
        |]
    in
    let notification =
      Js.Unsafe.new_obj
        (notification_class ())
        [| Js.Unsafe.inject (Js.string n.title); Js.Unsafe.inject options |]
    in
    (* Clicking it brings the tab back. *)
    Js.Unsafe.set
      notification
      "onclick"
      (Js.wrap_callback (fun () ->
         ignore
           (Js.Unsafe.meth_call Dom_html.window "focus" [||] : Js.Unsafe.any);
         ignore (Js.Unsafe.meth_call notification "close" [||] : Js.Unsafe.any))))
;;

let hidden () = Js.to_bool (Js.Unsafe.get Dom_html.document "hidden")

let on_visibility_change f =
  f (hidden ());
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       (Dom_html.Event.make "visibilitychange")
       (Dom.handler (fun _ ->
          f (hidden ());
          Js._true))
       Js._false
     : Dom_html.event_listener_id)
;;

(* The favicon: the workbench's slate tile with a chalk prompt bar; while
   something happened unseen, a sodium dot in its corner. *)
let favicon ~dot =
  let svg =
    sprintf
      {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32"><rect width="32" height="32" rx="7" fill="#23272e"/><rect x="8" y="9" width="3" height="14" rx="1" fill="#f0b44c"/><rect x="14" y="14" width="10" height="3" rx="1.5" fill="#e3e5e1"/>%s</svg>|}
      (if dot
       then
         {|<circle cx="25" cy="7" r="6" fill="#f0b44c" stroke="#1b1e23" stroke-width="2"/>|}
       else "")
  in
  "data:image/svg+xml," ^ Js.to_string (Js.encodeURIComponent (Js.string svg))
;;

let set_title title =
  Dom_html.document##.title := Js.string title;
  let dot = String.is_prefix title ~prefix:"(" in
  let link =
    match Dom_html.getElementById_opt "favicon" with
    | Some link -> link
    | None ->
      let link = Dom_html.createLink Dom_html.document in
      link##.id := Js.string "favicon";
      link##.rel := Js.string "icon";
      Dom.appendChild Dom_html.document##.head link;
      (link :> Dom_html.element Js.t)
  in
  link##setAttribute (Js.string "href") (Js.string (favicon ~dot))
;;
