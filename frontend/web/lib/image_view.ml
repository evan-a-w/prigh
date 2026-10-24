open! Core
open! Import

let src (image : Image.t) =
  sprintf "data:%s;base64,%s" image.mime_type image.data
;;

let thumb (image : Image.t) =
  let src = src image in
  let label = Image.to_string_hum image in
  Node.details
    ~attrs:[ Attr.class_ "image" ]
    [ Node.summary
        ~attrs:[ Attr.title label ]
        [ Node.img
            ~attrs:[ Attr.class_ "thumb"; Attr.src src; Attr.alt label ]
            ()
        ; Node.span
            ~attrs:[ Attr.class_ "lightbox" ]
            [ Node.img ~attrs:[ Attr.src src; Attr.alt label ] () ]
        ]
    ]
;;

let thumbs images =
  match images with
  | [] -> Node.none
  | images ->
    Node.div ~attrs:[ Attr.class_ "images" ] (List.map images ~f:thumb)
;;
