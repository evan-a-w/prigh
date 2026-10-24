open! Core
open Js_of_ocaml

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

let contains (element : Dom_html.element Js.t) =
  Js.Opt.test
    ((Js.Unsafe.coerce element)##closest (Js.string ("." ^ panel_class))
     : Dom_html.element Js.t Js.opt)
;;
