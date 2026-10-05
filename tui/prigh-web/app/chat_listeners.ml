open! Core
open Js_of_ocaml

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let set_label (button : Dom_html.element Js.t) ~copied =
  if copied
  then button##.classList##add (Js.string "copied")
  else button##.classList##remove (Js.string "copied");
  button##.textContent
  := Js.some (Js.string (if copied then "Copied" else "Copy"))
;;

let copy (ev : Dom_html.mouseEvent Js.t) =
  let open Option.Let_syntax in
  ignore
    (let%bind target = Js.Opt.to_option ev##.target in
     let%bind button = closest target "button.copy" in
     let%bind block = closest button ".copyable" in
     let%map text =
       Js.Opt.to_option (block##querySelector (Js.string ".copy-text"))
       |> Option.bind ~f:(fun el -> Js.Opt.to_option el##.textContent)
     in
     let clipboard = Js.Unsafe.get Dom_html.window##.navigator "clipboard" in
     ignore
       (Js.Unsafe.meth_call clipboard "writeText" [| Js.Unsafe.inject text |]
        : Js.Unsafe.any);
     set_label button ~copied:true;
     ignore
       (Dom_html.setTimeout (fun () -> set_label button ~copied:false) 1500.
        : Dom_html.timeout_id_safe)
     : unit option)
;;

let close_image (ev : Dom_html.keyboardEvent Js.t) =
  if String.equal (Js.Optdef.case ev##.key (fun () -> "") Js.to_string) "Escape"
  then
    Js.Opt.iter
      (Dom_html.document##querySelector (Js.string "details.image[open]"))
      (fun details ->
         details##removeAttribute (Js.string "open");
         Dom_html.stopPropagation ev)
;;

let install () =
  let listen event handler =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         event
         (Dom.handler (fun ev ->
            handler ev;
            Js._true))
         Js._true
       : Dom_html.event_listener_id)
  in
  listen Dom_html.Event.click copy;
  listen Dom_html.Event.keydown close_image
;;
