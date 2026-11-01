open! Core
open! Import

let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
;;

let value =
  let table = Array.create ~len:256 (-1) in
  String.iteri alphabet ~f:(fun i c -> table.(Char.to_int c) <- i);
  table.(Char.to_int '-') <- 62;
  table.(Char.to_int '_') <- 63;
  fun c -> table.(Char.to_int c)
;;

let decode_prefix data ~len =
  let out = Buffer.create len in
  let acc = ref 0
  and bits = ref 0 in
  let n = String.length data in
  let i = ref 0 in
  while !i < n && Buffer.length out < len do
    let v = value data.[!i] in
    if v >= 0
    then (
      acc := (!acc lsl 6) lor v land 0xffffff;
      bits := !bits + 6;
      if !bits >= 8
      then (
        bits := !bits - 8;
        Buffer.add_char out (Char.of_int_exn ((!acc lsr !bits) land 0xff))));
    incr i
  done;
  Buffer.contents out
;;

let has bytes ?(at = 0) prefix =
  String.length bytes >= at + String.length prefix
  && String.equal (String.sub bytes ~pos:at ~len:(String.length prefix)) prefix
;;

(* As the backend's Image.dimensions. *)
let of_header bytes =
  let len = String.length bytes in
  let byte i = Char.to_int bytes.[i] in
  let be16 i = (byte i lsl 8) lor byte (i + 1) in
  let le16 i = byte i lor (byte (i + 1) lsl 8) in
  let be32 i = (be16 i lsl 16) lor be16 (i + 2) in
  let le24 i = le16 i lor (byte (i + 2) lsl 16) in
  let positive (w, h) = if w > 0 && h > 0 then Some (w, h) else None in
  let jpeg () =
    let rec segment i =
      if i + 9 >= len || byte i <> 0xff
      then None
      else (
        let marker = byte (i + 1) in
        if marker = 0xff
        then segment (i + 1)
        else if
          marker = 0xd8 || marker = 0x01 || (marker >= 0xd0 && marker <= 0xd7)
        then segment (i + 2)
        else if
          marker >= 0xc0
          && marker <= 0xcf
          && marker <> 0xc4
          && marker <> 0xc8
          && marker <> 0xcc
        then positive (be16 (i + 7), be16 (i + 5))
        else segment (i + 2 + be16 (i + 2)))
    in
    segment 0
  in
  let webp () =
    if len < 30
    then None
    else (
      match String.sub bytes ~pos:12 ~len:4 with
      | "VP8 " -> positive (le16 26 land 0x3fff, le16 28 land 0x3fff)
      | "VP8L" ->
        let b1 = byte 22
        and b2 = byte 23
        and b3 = byte 24 in
        positive
          ( 1 + (((b1 land 0x3f) lsl 8) lor byte 21)
          , 1
            + (((b3 land 0xf) lsl 10) lor (b2 lsl 2) lor ((b1 land 0xc0) lsr 6))
          )
      | "VP8X" -> positive (1 + le24 24, 1 + le24 27)
      | _ -> None)
  in
  if has bytes "\x89PNG\r\n\x1a\n" && len >= 24
  then positive (be32 16, be32 20)
  else if (has bytes "GIF87a" || has bytes "GIF89a") && len >= 10
  then positive (le16 6, le16 8)
  else if has bytes "\xff\xd8\xff"
  then jpeg ()
  else if has bytes "RIFF" && has bytes ~at:8 "WEBP"
  then webp ()
  else None
;;

(* A JPEG's frame comes after its EXIF, at most 64 KB in. *)
let dimensions (image : Image.t) =
  of_header (decode_prefix image.data ~len:98_304)
;;

let default_limit : Llm.Image_limit.t =
  { max_bytes = 5 * 1024 * 1024 * 3 / 4 * 9 / 10; max_dimension = 8000 }
;;

module Verdict = struct
  type t =
    | Fits
    | Too_large of string
  [@@deriving sexp_of, equal]
end

let mb bytes = sprintf "%.1f MB" (Float.of_int bytes /. 1_048_576.)

let size_hum bytes =
  if bytes < 1024
  then sprintf "%d B" bytes
  else if bytes < 1_048_576
  then sprintf "%.0f KB" (Float.of_int bytes /. 1024.)
  else mb bytes
;;

let check (limit : Llm.Image_limit.t) (image : Image.t) : Verdict.t =
  let dims = dimensions image in
  match dims with
  | Some (w, h) when w > limit.max_dimension || h > limit.max_dimension ->
    Too_large (sprintf "%d×%d px, over %d" w h limit.max_dimension)
  | Some _ | None ->
    if image.bytes > limit.max_bytes
    then Too_large (sprintf "%s, over %s" (mb image.bytes) (mb limit.max_bytes))
    else Fits
;;

module Target = struct
  type t =
    { width : int
    ; height : int
    ; max_bytes : int
    }
  [@@deriving sexp_of, equal]
end

let target (limit : Llm.Image_limit.t) (image : Image.t) =
  match check limit image, dimensions image with
  | Fits, _ -> None
  | Too_large _, None ->
    (* Unknown size: the page measures it; the bytes still bound it. *)
    Some
      { Target.width = limit.max_dimension
      ; height = limit.max_dimension
      ; max_bytes = limit.max_bytes * 9 / 10
      }
  | Too_large _, Some (w, h) ->
    let side = Float.of_int (Int.max w h) in
    let by_pixels = Float.min 1. (Float.of_int limit.max_dimension /. side) in
    let scale = by_pixels in
    let side n = Int.max 1 (Float.iround_down_exn (Float.of_int n *. scale)) in
    Some
      { Target.width = side w
      ; height = side h
      ; max_bytes = limit.max_bytes * 9 / 10
      }
;;

let sniff data =
  let bytes = decode_prefix data ~len:16 in
  if has bytes "\x89PNG\r\n\x1a\n"
  then Some "image/png"
  else if has bytes "\xff\xd8\xff"
  then Some "image/jpeg"
  else if has bytes "GIF87a" || has bytes "GIF89a"
  then Some "image/gif"
  else if has bytes "RIFF" && has bytes ~at:8 "WEBP"
  then Some "image/webp"
  else None
;;

let image_extensions =
  [ "png"
  ; "jpg"
  ; "jpeg"
  ; "jpe"
  ; "jfif"
  ; "gif"
  ; "webp"
  ; "heic"
  ; "heif"
  ; "avif"
  ; "bmp"
  ]
;;

let looks_like_image ~mime_type ~name =
  let mime_type = String.lowercase mime_type in
  (String.is_prefix mime_type ~prefix:"image/"
   && not (String.equal mime_type "image/svg+xml"))
  || ((String.is_empty mime_type
       || String.equal mime_type "application/octet-stream")
      &&
      match String.rsplit2 name ~on:'.' with
      | Some (_, ext) ->
        List.mem image_extensions (String.lowercase ext) ~equal:String.equal
      | None -> false)
;;
