open! Import
open Js_of_ocaml

let breakpoint = 768
let in_browser () = Js.Optdef.test (Js.Unsafe.global##.window : _ Js.Optdef.t)
let is_mobile () = in_browser () && Dom_html.window##.innerWidth < breakpoint

let query () =
  (Js.Unsafe.coerce Dom_html.window)##matchMedia
    (Js.string (sprintf "(max-width: %dpx)" (breakpoint - 1)))
;;

let component (local_ graph) =
  let open Bonsai.Let_syntax in
  let mobile, set_mobile = Bonsai.state false graph in
  (* The listener added on activation, to remove on deactivation. *)
  let listener = Bonsai.Expert.thunk ~f:(fun () -> ref None) graph in
  let on_activate =
    let%arr set_mobile
    and listener in
    let register () =
      if in_browser () && Option.is_none !listener
      then (
        let mql = query () in
        let f =
          Js.wrap_callback (fun (_ : Js.Unsafe.any) ->
            Effect.Expert.handle_non_dom_event_exn (set_mobile (is_mobile ())))
        in
        (mql##addEventListener (Js.string "change") f : unit);
        listener := Some (mql, f))
    in
    Effect.Many
      [ Effect.of_thunk register
      ; Effect.lazy_ (lazy (set_mobile (is_mobile ())))
      ]
  in
  let on_deactivate =
    let%arr listener in
    Effect.of_thunk (fun () ->
      Option.iter !listener ~f:(fun (mql, f) ->
        (mql##removeEventListener (Js.string "change") f : unit));
      listener := None)
  in
  Bonsai.Edge.lifecycle ~on_activate ~on_deactivate graph;
  mobile
;;
