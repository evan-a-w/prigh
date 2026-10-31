open! Core
open! Import

let src (image : Image.t) =
  sprintf "data:%s;base64,%s" image.mime_type image.data
;;

(* The viewer's own button, over its top right corner. Drawing is the
   page's ([data-annotate], app/chat_listeners.ml): it takes the image in
   this viewer. *)
let tools ~target =
  Node.span
    ~attrs:[ Attr.class_ "lightbox-tools" ]
    [ Node.button
        ~attrs:
          [ Attr.type_ "button"
          ; Attr.class_ "btn lightbox-annotate"
          ; Attr.title "Draw on this image"
          ; Attr.create "data-annotate" (Annotate.Target.to_string target)
          ]
        [ Html.icon Pencil; Node.text "Draw" ]
    ]
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
            [ Node.img ~attrs:[ Attr.src src; Attr.alt label ] ()
            ; tools ~target:New
            ]
        ]
    ]
;;

let thumbs images =
  match images with
  | [] -> Node.none
  | images ->
    Node.div
      ~attrs:[ Attr.class_ "images" ]
      (List.map images ~f:(fun image -> thumb image))
;;
