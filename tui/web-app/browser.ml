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

let open_url url =
  ignore
    (Dom_html.window##open_ (Js.string url) (Js.string "_blank") Js.null
     : Dom_html.window Js.t Js.opt)
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

(* One cell of the monospace grid, measured with a probe in the page's own
   styles. *)
let cell_size () =
  let probe = Dom_html.createPre Dom_html.document in
  probe##.className := Js.string "screen probe";
  let line = Dom_html.createDiv Dom_html.document in
  line##.className := Js.string "line";
  let span = Dom_html.createSpan Dom_html.document in
  span##.textContent := Js.some (Js.string (String.make 100 '0'));
  Dom.appendChild line span;
  Dom.appendChild probe line;
  Dom.appendChild Dom_html.document##.body probe;
  let rect = span##getBoundingClientRect in
  let width = Js.to_float rect##.width /. 100. in
  let height = Js.to_float line##getBoundingClientRect##.height in
  Dom.removeChild Dom_html.document##.body probe;
  Float.max 1. width, Float.max 1. height
;;

let cell_height () = snd (cell_size ())

(* The visual viewport is what the user actually sees: unlike the layout
   viewport (window.innerHeight) it shrinks when the on-screen keyboard opens
   and moves when the browser scrolls a focused field into view. *)
module Viewport = struct
  type t =
    { width : float
    ; height : float
    ; offset_left : float
    ; offset_top : float
    }

  let visual () =
    let vv = Js.Unsafe.get Dom_html.window (Js.string "visualViewport") in
    if Js.Optdef.test vv && Js.Opt.test vv then Some vv else None
  ;;

  let number obj name : float =
    Js.to_float (Js.Unsafe.get obj (Js.string name) : Js.number_t)
  ;;

  let current () =
    match visual () with
    | Some vv ->
      { width = number vv "width"
      ; height = number vv "height"
      ; offset_left = number vv "offsetLeft"
      ; offset_top = number vv "offsetTop"
      }
    | None ->
      { width = Float.of_int Dom_html.window##.innerWidth
      ; height = Float.of_int Dom_html.window##.innerHeight
      ; offset_left = 0.
      ; offset_top = 0.
      }
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

(* Pins [#root] to the visible area so the bottom of the screen (the input) is
   never behind the keyboard, and undoes any scroll the browser applied to bring
   the hidden textarea into view. *)
let fit_root () =
  let v = Viewport.current () in
  Option.iter (Dom_html.getElementById_opt "root") ~f:(fun root ->
    root##.style##.height := Js.string (sprintf "%.0fpx" v.height);
    root##.style##.width := Js.string (sprintf "%.0fpx" v.width);
    (Js.Unsafe.coerce root##.style)##.transform
    := Js.string
         (sprintf "translate(%.0fpx, %.0fpx)" v.offset_left v.offset_top));
  Dom_html.window##scroll (Js.float 0.) (Js.float 0.)
;;

let grid_size () =
  let cell_w, cell_h = cell_size () in
  let v = Viewport.current () in
  ( Int.max 20 (Float.to_int (v.width /. cell_w))
  , Int.max 5 (Float.to_int (v.height /. cell_h)) )
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

let reload_with_backend backend =
  let location = Dom_html.window##.location in
  location##.href
  := Js.string
       (href_with_backend
          ~pathname:(Js.to_string location##.pathname)
          ~search:(Js.to_string location##.search)
          ~backend)
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
