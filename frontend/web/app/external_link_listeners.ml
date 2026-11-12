open! Core
open Js_of_ocaml
module External_link = Prigh_web.External_link

let bound () =
  let f = Js.Unsafe.get Dom_html.window (Js.string External_link.binding) in
  String.equal (Js.to_string (Js.typeof f)) "function"
;;

(* The HTML link the click is on (an SVG one has no [href] string). *)
let link (ev : Dom_html.mouseEvent Js.t) =
  Option.bind
    (Js.Opt.to_option ev##.target)
    ~f:(fun (el : Dom_html.element Js.t) ->
      let found : Dom_html.element Js.t Js.opt =
        Js.Unsafe.meth_call
          el
          "closest"
          [| Js.Unsafe.inject (Js.string "a[href]") |]
      in
      Js.Opt.to_option found
      |> Option.filter ~f:(fun a -> String.equal (Js.to_string a##.tagName) "A")
      |> Option.map ~f:(fun a ->
        (Js.Unsafe.coerce a : Dom_html.anchorElement Js.t)))
;;

let on_click (ev : Dom_html.mouseEvent Js.t) =
  if
    (not (Js.to_bool (Js.Unsafe.get ev (Js.string "defaultPrevented"))))
    && Desktop_page.in_browser ()
    && bound ()
  then
    Option.iter (link ev) ~f:(fun a ->
      let click : External_link.Click.t =
        { button = ev##.button
        ; ctrl = Js.to_bool ev##.ctrlKey
        ; meta = Js.to_bool ev##.metaKey
        ; shift = Js.to_bool ev##.shiftKey
        }
      in
      match
        External_link.decide
          ~own_origin:
            (Js.to_string
               (Js.Unsafe.get Dom_html.window##.location (Js.string "origin")))
          ~href:(Js.to_string a##.href)
          ~target:(Js.to_string a##.target)
          ~download:(Js.to_bool (a##hasAttribute (Js.string "download")))
          click
      with
      | None -> ()
      | Some open_ ->
        Dom.preventDefault ev;
        ignore
          (Js.Unsafe.meth_call
             Dom_html.window
             External_link.binding
             [| Js.Unsafe.inject (Js.string (External_link.payload open_)) |]
           : Js.Unsafe.any));
  Js._true
;;

let install () =
  (* After the page's own handlers (the bubbling phase, on the window): a
     click one of them took (the image viewer's, the end of a drag) is
     theirs. *)
  List.iter [ "click"; "auxclick" ] ~f:(fun name ->
    ignore
      (Dom_html.addEventListener
         Dom_html.window
         (Dom.Event.make name)
         (Dom.handler on_click)
         Js._false
       : Dom_html.event_listener_id))
;;
