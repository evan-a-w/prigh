open! Core
open Js_of_ocaml

let storage () = Js.Optdef.to_option Dom_html.window##.localStorage

let get_item key =
  Option.bind (storage ()) ~f:(fun s ->
    Js.Opt.to_option (s##getItem (Js.string key)) |> Option.map ~f:Js.to_string)
;;

let set_item key value =
  Option.iter (storage ()) ~f:(fun s ->
    s##setItem (Js.string key) (Js.string value))
;;

let remove_item key =
  Option.iter (storage ()) ~f:(fun s -> s##removeItem (Js.string key))
;;

let query_param name =
  let search = Js.to_string Dom_html.window##.location##.search in
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  List.find_map (String.split search ~on:'&') ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some (k, v) when String.equal k name ->
      Some (Js.to_string (Js.decodeURIComponent (Js.string v)))
    | _ -> None)
;;

let same_origin_ws_url () =
  let location = Dom_html.window##.location in
  let scheme =
    if String.equal (Js.to_string location##.protocol) "https:"
    then "wss"
    else "ws"
  in
  sprintf "%s://%s/ws" scheme (Js.to_string location##.host)
;;

let copy_to_clipboard text =
  let navigator = Js.Unsafe.get Dom_html.window (Js.string "navigator") in
  let clipboard = Js.Unsafe.get navigator (Js.string "clipboard") in
  if Js.Optdef.test clipboard
  then
    ignore
      (Js.Unsafe.meth_call
         clipboard
         "writeText"
         [| Js.Unsafe.inject (Js.string text) |]
       : unit)
;;

(* The visual viewport is what the user actually sees: unlike the layout
   viewport (window.innerHeight) it shrinks when the on-screen keyboard opens
   and moves when the browser scrolls a focused field into view. *)
module Viewport = struct
  let visual () =
    let vv = Js.Unsafe.get Dom_html.window (Js.string "visualViewport") in
    if Js.Optdef.test vv && Js.Opt.test vv then Some vv else None
  ;;

  let listen target event f =
    ignore
      (Dom_html.addEventListener
         target
         (Dom_html.Event.make event)
         (Dom.handler (fun _ ->
            f ();
            Js._true))
         Js._false
       : Dom_html.event_listener_id)
  ;;

  let on_change f =
    listen Dom_html.window "resize" f;
    Option.iter (visual ()) ~f:(fun vv ->
      let vv : Dom_html.eventTarget Js.t = Js.Unsafe.coerce vv in
      listen vv "resize" f;
      listen vv "scroll" f)
  ;;
end

let waiting_for_render = Queue.create ()
let after_render f = Queue.enqueue waiting_for_render f

(* In a microtask: after the mutation observers' (queued as Bonsai patched
   the DOM), which keep the chat's scroll position, and still before the
   browser paints. *)
let rendered () =
  let run () =
    let callbacks = Queue.to_list waiting_for_render in
    Queue.clear waiting_for_render;
    List.iter callbacks ~f:(fun f -> f ())
  in
  if not (Queue.is_empty waiting_for_render)
  then Js.Unsafe.global##queueMicrotask (Js.wrap_callback run)
;;

let on_viewport_change = Viewport.on_change

let href_with_backend ~pathname ~search ~backend =
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  let search =
    String.split search ~on:'&'
    |> List.filter ~f:(fun pair ->
      let key =
        String.lsplit2 pair ~on:'=' |> Option.value_map ~default:pair ~f:fst
      in
      not (String.equal key "backend" || String.equal key "token"))
    |> String.concat ~sep:"&"
  in
  let suffix = if String.is_empty search then "" else "&" ^ search in
  pathname
  ^ "?backend="
  ^ Js.to_string (Js.encodeURIComponent (Js.string backend))
  ^ suffix
;;

let pairs_without ~search name =
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  String.split search ~on:'&'
  |> List.filter ~f:(fun pair ->
    (not (String.is_empty pair))
    && not
         (String.equal
            (String.lsplit2 pair ~on:'='
             |> Option.value_map ~default:pair ~f:fst)
            name))
;;

let with_query_param ~search name value =
  "?"
  ^ String.concat
      ~sep:"&"
      (pairs_without ~search name
       @ [ name ^ "=" ^ Js.to_string (Js.encodeURIComponent (Js.string value)) ]
      )
;;

let without_query_param ~search name =
  let pairs = pairs_without ~search name in
  if List.is_empty pairs then "" else "?" ^ String.concat ~sep:"&" pairs
;;

let reload_without_query_param name =
  let location = Dom_html.window##.location in
  location##.href
  := Js.string
       (Js.to_string location##.pathname
        ^ without_query_param ~search:(Js.to_string location##.search) name)
;;

let replace_search f =
  let location = Dom_html.window##.location in
  let search = f ~search:(Js.to_string location##.search) in
  if not (String.equal search (Js.to_string location##.search))
  then
    Dom_html.window##.history##replaceState
      Js.null
      (Js.string "")
      (Js.some (Js.string (Js.to_string location##.pathname ^ search)))
;;

let replace_query_param name value =
  replace_search (fun ~search -> with_query_param ~search name value)
;;

let hash () = Js.to_string Dom_html.window##.location##.hash

let clear_hash () =
  let location = Dom_html.window##.location in
  Dom_html.window##.history##replaceState
    Js.null
    (Js.string "")
    (Js.some
       (Js.string
          (Js.to_string location##.pathname ^ Js.to_string location##.search)))
;;

let post ~url ~body ~f =
  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  xhr##.onreadystatechange
  := Js.wrap_callback (fun () ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         f
           (if xhr##.status = 200
            then
              Option.map (Js.Opt.to_option xhr##.responseText) ~f:Js.to_string
            else None)
       | _ -> ());
  xhr##send (Js.some (Js.string body))
;;

let set_app_html html =
  match Dom_html.getElementById_opt "app" with
  | Some el -> el##.innerHTML := Js.string html
  | None -> ()
;;

let input_value id =
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
  | Some input -> Js.to_string input##.value
  | None -> ""
;;
