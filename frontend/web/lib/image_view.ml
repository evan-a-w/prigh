open! Core
open! Import

let src (image : Image.t) =
  sprintf "data:%s;base64,%s" image.mime_type image.data
;;

let viewer_attribute = "data-viewer"
let group_attribute = "data-viewer-group"

(* The viewer is the page's (app/viewer_widget.ml): it opens on a click on
   [data-viewer], shows the [img] inside at full size with zoom and pan,
   and steps through the other [data-viewer]s of its [data-viewer-group].
   [data-viewer-draw] offers Draw. *)
let thumb (image : Image.t) =
  let label = Image.to_string_hum image in
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_ "image"
      ; Attr.title label
      ; Attr.create "aria-label" ("View " ^ label)
      ; Attr.create viewer_attribute ""
      ; Attr.create "data-viewer-draw" ""
      ]
    [ Node.img
        ~attrs:[ Attr.class_ "thumb"; Attr.src (src image); Attr.alt label ]
        ()
    ]
;;

let thumbs images =
  match images with
  | [] -> Node.none
  | images ->
    Node.div
      ~attrs:[ Attr.class_ "images"; Attr.create group_attribute "" ]
      (List.map images ~f:thumb)
;;
