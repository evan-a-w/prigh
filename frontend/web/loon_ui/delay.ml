open! Import
open Js_of_ocaml

let pending : (string, Dom_html.timeout_id_safe) Hashtbl.t = Hashtbl.create (module String)
let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let drop key =
  Option.iter (Hashtbl.find_and_remove pending key) ~f:Dom_html.clearTimeout
;;

let cancel ~key = Effect.of_thunk (fun () -> drop key)

let schedule ~key ~ms effect =
  if not (in_browser ())
  then effect
  else
    Effect.of_thunk (fun () ->
      drop key;
      Hashtbl.set
        pending
        ~key
        ~data:
          (Dom_html.setTimeout
             (fun () ->
               Hashtbl.remove pending key;
               Effect.Expert.handle_non_dom_event_exn effect)
             ms))
;;
