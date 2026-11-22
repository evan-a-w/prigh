open! Core
open Js_of_ocaml
module Action = Prigh_web.App.Action

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let target (ev : #Dom_html.event Js.t) = Js.Opt.to_option ev##.target

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

let listen ?(capture = false) event handler =
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       event
       (Dom.handler (fun ev ->
          handler ev;
          Js._true))
       (Js.bool capture)
     : Dom_html.event_listener_id)
;;

let install ~schedule =
  listen Dom_html.Event.click (fun (ev : Dom_html.mouseEvent Js.t) ->
    match Option.bind (target ev) ~f:(fun el -> closest el "[data-agent]") with
    | None -> ()
    | Some el ->
      Option.iter (attribute el "data-agent") ~f:(fun id ->
        schedule (Action.Open_subagent_chat id)))
;;
