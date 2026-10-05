open! Core

(* Tiny real images made with ImageMagick: [magick -size WxH xc:red ...]. *)

let png_3x2 =
  "iVBORw0KGgoAAAANSUhEUgAAAAMAAAACAQMAAACnuvRZAAAAA1BMVEX/AAAZ4gk3AAAADElEQVQI12NgYGAAAAAEAAEnNCcKAAAAAElFTkSuQmCC"
;;

let gif_5x4 = "R0lGODlhBQAEAPAAAAAA/wAAACH5BAAAAAAALAAAAAAFAAQAAAIEhI+ZBQA7"

let jpeg_7x6 =
  "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/2wBDAQMDAwQDBAgEBAgQCwkLEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBD/wAARCAAGAAcDAREAAhEBAxEB/8QAFAABAAAAAAAAAAAAAAAAAAAACP/EABQQAQAAAAAAAAAAAAAAAAAAAAD/xAAVAQEBAAAAAAAAAAAAAAAAAAAHCP/EABQRAQAAAAAAAAAAAAAAAAAAAAD/2gAMAwEAAhEDEQA/ADkD03v/2Q=="
;;

let webp_lossy_9x8 =
  "UklGRjwAAABXRUJQVlA4IDAAAADQAQCdASoJAAgAAgA0JaACdLoB+AADsAD+8MQL/yC5YXXI1/8gP+QH/ID/+PIAAAA="
;;

let webp_lossless_11x10 = "UklGRhwAAABXRUJQVlA4TA8AAAAvCkACAAcQ/Y/+ByKi/wEA"

let webp_extended_13x12 =
  "UklGRmoAAABXRUJQVlA4WAoAAAAQAAAADAAACwAAQUxQSA4AAAABDzD/ERFiBCL6HwQAAFZQOCA2AAAAkAEAnQEqDQAMAAIANCUAXYYI6DsAAP76bv/+ug/+b///1qrOR3x3kqODPPaf8fff2Kp96wAA"
;;

let bmp_2x2 =
  "Qk2aAAAAAAAAAIoAAAB8AAAAAgAAAAIAAAABABgAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/AAD/AAD/AAAAAAAA/0JHUnOPwvUoUbgeFR6F6wEzMzMTZmZmJmZmZgaZmZkJPQrXAyhcjzIAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAD/AAD/AAAAAP8AAP8AAA=="
;;

let bytes base64 = Base64.decode_exn base64

(* The header of a PNG of any size: enough for [Image.sniff] and
   [Image.dimensions], which is all that decides about downscaling. *)
let png_header ~width ~height =
  let be32 n =
    String.init 4 ~f:(fun i ->
      Char.of_int_exn ((n lsr (8 * (3 - i))) land 0xff))
  in
  "\x89PNG\r\n\x1a\n\x00\x00\x00\x0dIHDR"
  ^ be32 width
  ^ be32 height
  ^ "\x08\x02\x00\x00\x00"
;;

(* Providers pass image data through untouched, so request-shape tests can
   use a placeholder. *)
let placeholder ?(mime_type = "image/png") data =
  { Prigh.Image.mime_type; data }
;;

(* A prompt with an image, an image-only prompt, and a [read] of an image. *)
let conversation ~model =
  let open Prigh in
  [ Message.user ~images:[ placeholder "SHOT" ] "what is this?"
  ; Message.user ~images:[ placeholder ~mime_type:"image/jpeg" "PHOTO" ] ""
  ; Assistant
      { content =
          [ Tool_call
              { id = "call_1"; name = "read"; arguments = {|{"path":"a.png"}|} }
          ]
      ; stop_reason = Tool_use
      ; usage = Usage.zero
      ; model
      }
  ; Tool_result
      { tool_call_id = "call_1"
      ; tool_name = "read"
      ; text = "Read image file [image/png, 3x2]"
      ; is_error = false
      ; images = [ placeholder "READ" ]
      }
  ]
;;
