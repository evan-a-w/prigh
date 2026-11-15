open! Core
open Js_of_ocaml

let scroll by =
  match Dom_html.getElementById_opt "review-scroll" with
  | None -> false
  | Some scroller ->
    let top = Js.to_float scroller##getBoundingClientRect##.top in
    let starts =
      Dom.list_of_nodeList
        (scroller##querySelectorAll (Js.string "tr.change-start"))
      |> List.map ~f:(fun (el : Dom_html.element Js.t) ->
        Js.to_float el##getBoundingClientRect##.top -. top)
    in
    (* Where a change lands: a few lines of context over it. *)
    let margin = 48. in
    let target =
      if by > 0
      then List.find starts ~f:(fun y -> Float.(y > margin +. 4.))
      else List.rev starts |> List.find ~f:(fun y -> Float.(y < margin -. 4.))
    in
    (match target with
     | Some y ->
       scroller##.scrollTop
       := Js.float (Js.to_float scroller##.scrollTop +. y -. margin)
     | None -> ());
    Option.is_some target
;;
