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

let grid_size () =
  let cell_w, cell_h = cell_size () in
  let width = Float.of_int Dom_html.window##.innerWidth in
  let height = Float.of_int Dom_html.window##.innerHeight in
  ( Int.max 20 (Float.to_int (width /. cell_w))
  , Int.max 5 (Float.to_int (height /. cell_h)) )
;;

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
