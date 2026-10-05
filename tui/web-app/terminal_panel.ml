open! Core
open Js_of_ocaml
open Bonsai_web

let panel_class = "terminal-panel"

let url ~backend ~user ~as_user ~token ~session =
  let base =
    match String.lsplit2 backend ~on:'?' with
    | Some (base, _) -> base
    | None -> backend
  in
  let base =
    String.chop_suffix base ~suffix:"/" |> Option.value ~default:base
  in
  let base =
    String.chop_suffix base ~suffix:"/ws" |> Option.value ~default:base
  in
  let query =
    List.filter_opt
      [ Option.map user ~f:(fun u -> "user", u)
      ; Option.map as_user ~f:(fun u -> "as_user", u)
      ; Option.map token ~f:(fun t -> "token", t)
      ; Option.map session ~f:(fun s -> "session", s)
      ]
    |> List.map ~f:(fun (k, v) ->
      k ^ "=" ^ Js.to_string (Js.encodeURIComponent (Js.string v)))
  in
  base
  ^ "/terminal"
  ^ if List.is_empty query then "" else "?" ^ String.concat ~sep:"&" query
;;

module Handle = struct
  type t = < focus : unit Js.meth ; dispose : unit Js.meth > Js.t
end

let widget_id : (Handle.t * Dom_html.divElement Js.t) Type_equal.Id.t =
  Type_equal.Id.create ~name:"prigh-terminal" sexp_of_opaque
;;

let widget url =
  Vdom.Node.widget
    ~id:widget_id
    ~init:(fun () ->
      let host = Dom_html.createDiv Dom_html.document in
      host##.className := Js.string "terminal-host";
      let api = Js.Unsafe.get Dom_html.window (Js.string "prighTerminal") in
      let handle : Handle.t =
        Js.Unsafe.meth_call
          api
          "mount"
          [| Js.Unsafe.inject host; Js.Unsafe.inject (Js.string url) |]
      in
      handle, host)
    ~destroy:(fun handle _ -> handle##dispose)
    ()
;;

let view ~url ~on_close =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ panel_class ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "terminal-header" ]
        [ Vdom.Node.span [ Vdom.Node.text "terminal" ]
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.class_ "terminal-close"
              ; Vdom.Attr.title "hide (the shell keeps running for a while)"
              ; Vdom.Attr.on_click (fun _ -> on_close)
              ]
            [ Vdom.Node.text "×" ]
        ]
    ; widget url
    ]
;;

let open_button ~on_click =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.class_ "terminal-open"
      ; Vdom.Attr.title "open a terminal"
      ; Vdom.Attr.on_click (fun _ -> on_click)
      ]
    [ Vdom.Node.text ">_" ]
;;

let contains (element : Dom_html.element Js.t) =
  Js.Opt.test
    ((Js.Unsafe.coerce element)##closest (Js.string ("." ^ panel_class))
     : Dom_html.element Js.t Js.opt)
;;
