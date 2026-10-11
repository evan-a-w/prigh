open! Core
open Js_of_ocaml

let chat () = Dom_html.getElementById_opt "chat"

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

(* The height and scroll position before older messages were added. *)
let pending = ref None

let keep_position () =
  Option.iter (chat ()) ~f:(fun el ->
    (* The browser's own scroll anchoring would move it as well. *)
    el##.style##setProperty
      (Js.string "overflow-anchor")
      (Js.string "none")
      Js.undefined
    |> ignore;
    pending := Some (el##.scrollHeight, Js.to_float el##.scrollTop))
;;

let restore () =
  match !pending, chat () with
  | Some (height, top), Some el when el##.scrollHeight <> height ->
    pending := None;
    el##.scrollTop := Js.float (top +. Float.of_int (el##.scrollHeight - height));
    el##.style##removeProperty (Js.string "overflow-anchor") |> ignore
  | _ -> ()
;;

let near_top (el : Dom_html.element Js.t) =
  Float.(Js.to_float el##.scrollTop < 1.5 *. of_int el##.clientHeight)
;;

let more () =
  Js.Opt.test
    (Dom_html.document##querySelector (Js.string "#chat [data-older]"))
;;

let install ~schedule =
  let maybe_load (ev : #Dom_html.event Js.t) =
    match chat () with
    | Some el
      when Js.Opt.case
             ev##.target
             (fun () -> false)
             (fun target ->
                Js.to_bool ((Js.Unsafe.coerce el)##contains target : bool Js.t))
           && near_top el
           && more () -> schedule Prigh_web.App.Action.Load_older
    | _ -> ()
  in
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
  (* Scroll events do not bubble: capturing sees the chat's. A wheel turned
     up at the very top scrolls nothing. *)
  listen (Dom_html.Event.make "scroll") maybe_load;
  listen
    (Dom_html.Event.make "wheel")
    (fun (ev : Dom_html.mouseScrollEvent Js.t) ->
       if Float.(Js.to_float (Js.Unsafe.coerce ev)##.deltaY < 0.)
       then maybe_load ev);
  listen Dom_html.Event.click (fun (ev : Dom_html.mouseEvent Js.t) ->
    Js.Opt.iter ev##.target (fun target ->
      Option.iter
        (closest target (sprintf "[%s]" Prigh_web.Output_view.load_attribute))
        ~f:(fun el ->
          Js.Opt.iter
            (el##getAttribute (Js.string Prigh_web.Output_view.load_attribute))
            (fun key ->
               schedule (Prigh_web.App.Action.Load_output (Js.to_string key))))));
  (* Bonsai patches the DOM in one go, before the frame is painted: moving
     the scroll position then shows no jump. *)
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun _ _ -> if Option.is_some !pending then restore ()))
  in
  let options = MutationObserver.empty_mutation_observer_init () in
  options##.childList := true;
  options##.subtree := true;
  observer##observe (Dom_html.document :> Dom.node Js.t) options
;;
