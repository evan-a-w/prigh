open! Core
open Js_of_ocaml
module Session_link_click = Prigh_web.Session_link_click

let anchor (ev : Dom_html.mouseEvent Js.t) =
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

let on_click ~schedule (ev : Dom_html.mouseEvent Js.t) =
  let taken = Js.to_bool (Js.Unsafe.get ev (Js.string "defaultPrevented")) in
  Option.iter (if taken then None else anchor ev) ~f:(fun a ->
    let location = Dom_html.window##.location in
    match
      Session_link_click.decide
        ~own_origin:
          (Js.to_string (Js.Unsafe.get location (Js.string "origin")))
        ~own_path:(Js.to_string location##.pathname)
        ~href:(Js.to_string a##.href)
        ~session:
          (Js.Opt.to_option
             (a##getAttribute
                (Js.string Prigh_web.Session_link_view.id_attribute))
           |> Option.map ~f:Js.to_string)
        { button = ev##.button
        ; ctrl = Js.to_bool ev##.ctrlKey
        ; meta = Js.to_bool ev##.metaKey
        ; shift = Js.to_bool ev##.shiftKey
        }
    with
    | None -> ()
    | Some { id; new_window } ->
      Dom.preventDefault ev;
      schedule (Prigh_web.App.Action.Follow_session_link { id; new_window }));
  Js._true
;;

let install ~schedule =
  List.iter [ "click"; "auxclick" ] ~f:(fun name ->
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         (Dom.Event.make name)
         (Dom.handler (on_click ~schedule))
         Js._true
       : Dom_html.event_listener_id))
;;
