open! Core
open Js_of_ocaml
module Action = Prigh_web.App.Action
module Image = Prigh_protocol.Image
module Target = Prigh_web.Image_size.Target

let schedule = ref (fun (_ : Action.t) -> ())
let install ~schedule:s = schedule := s

let encode (canvas : Dom_html.canvasElement Js.t) ~mime_type ~quality =
  let url =
    Js.to_string
      (Js.Unsafe.meth_call
         canvas
         "toDataURL"
         [| Js.Unsafe.inject (Js.string mime_type)
          ; Js.Unsafe.inject (Js.number_of_float quality)
         |])
  in
  match String.lsplit2 url ~on:',' with
  | Some (header, data) when String.is_substring header ~substring:mime_type ->
    Some { Image.mime_type; data; bytes = Image.decoded_size data }
  | Some _ | None -> None
;;

let draw (img : Dom_html.imageElement Js.t) ~width ~height =
  let canvas = Dom_html.createCanvas Dom_html.document in
  canvas##.width := width;
  canvas##.height := height;
  let ctx = canvas##getContext Dom_html._2d_ in
  (* JPEG has no transparency: white under it, not black. *)
  ctx##.fillStyle := Js.string "#ffffff";
  ctx##fillRect
    (Js.float 0.)
    (Js.float 0.)
    (Js.float (Float.of_int width))
    (Js.float (Float.of_int height));
  ctx##drawImage_withSize
    img
    (Js.float 0.)
    (Js.float 0.)
    (Js.float (Float.of_int width))
    (Js.float (Float.of_int height));
  canvas
;;

let shrink (original : Image.t) (target : Target.t) img =
  let keep_png =
    String.equal original.mime_type "image/png"
    || String.equal original.mime_type "image/gif"
  in
  let fits (i : Image.t) = i.bytes <= target.max_bytes in
  let rec at ~width ~height tries =
    let canvas = draw img ~width ~height in
    let png =
      if keep_png
      then
        Option.filter (encode canvas ~mime_type:"image/png" ~quality:1.) ~f:fits
      else None
    in
    let found =
      match png with
      | Some _ -> png
      | None ->
        List.find_map [ 0.9; 0.82; 0.72; 0.6 ] ~f:(fun quality ->
          Option.filter (encode canvas ~mime_type:"image/jpeg" ~quality) ~f:fits)
    in
    match found with
    | Some image -> Ok image
    | None when tries > 0 && width > 64 && height > 64 ->
      at ~width:(width * 3 / 4) ~height:(height * 3 / 4) (tries - 1)
    | None -> Error "it is still too large at its smallest"
  in
  at ~width:target.width ~height:target.height 6
;;

let run (original : Image.t) (target : Target.t) =
  let img = Dom_html.createImg Dom_html.document in
  let report = function
    | Ok image -> !schedule (Action.Downsized { original; image })
    | Error reason -> !schedule (Action.Downsize_failed { original; reason })
  in
  img##.onload
  := Dom_html.handler (fun _ ->
       let w = img##.naturalWidth
       and h = img##.naturalHeight in
       (* The header was not read (an unusual format): the page's measure. *)
       let target =
         if w > 0 && h > 0 && target.width = target.height && w <> h
         then (
           let side = Int.max w h in
           let scale =
             Float.min 1. (Float.of_int target.width /. Float.of_int side)
           in
           { target with
             width = Int.max 1 (Float.iround_down_exn (Float.of_int w *. scale))
           ; height =
               Int.max 1 (Float.iround_down_exn (Float.of_int h *. scale))
           })
         else target
       in
       report
         (try shrink original target img with
          | exn -> Error (Exn.to_string exn));
       Js._false);
  img##.onerror
  := Dom_html.handler (fun _ ->
       report (Error "the browser cannot read it");
       Js._false);
  img##.src
  := Js.string (sprintf "data:%s;base64,%s" original.mime_type original.data)
;;
