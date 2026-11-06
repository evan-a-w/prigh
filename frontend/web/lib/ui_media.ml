open! Core
open! Import

let name path =
  match String.rsplit2 path ~on:'/' with
  | Some (_, name) when not (String.is_empty name) -> name
  | _ -> path
;;

let frame (ctx : Ui_context.t) (image : Ui_spec.Image.t) =
  let alt = Option.value image.alt ~default:(name image.path) in
  match Ui_state.image ctx.state image.path with
  | Some (Url url) ->
    Node.button
      ~attrs:
        [ Attr.type_ "button"
        ; Attr.class_ "ui-image-frame"
        ; Attr.title image.path
        ; Attr.create "aria-label" ("View " ^ alt)
        ; Attr.create Image_view.viewer_attribute ""
        ]
      [ Node.img
          ~attrs:[ Attr.src url; Attr.alt alt; Attr.create "loading" "lazy" ]
          ()
      ]
  | Some (Failed why) ->
    Node.div
      ~attrs:
        [ Attr.class_ "ui-image-frame failed"
        ; Attr.role "img"
        ; Attr.create "aria-label" alt
        ]
      [ Node.span ~attrs:[ Attr.class_ "ui-image-error" ] [ Node.text why ]
      ; Node.span
          ~attrs:[ Attr.class_ "ui-image-path" ]
          [ Node.text image.path ]
      ]
  | Some Loading | None ->
    Node.div
      ~attrs:
        [ Attr.class_ "ui-image-frame loading"
        ; Attr.role "img"
        ; Attr.create "aria-label" ("Loading " ^ alt)
        ; Attr.create "aria-busy" "true"
        ]
      []
;;

let caption (image : Ui_spec.Image.t) =
  match image.caption with
  | Some text ->
    Node.figcaption ~attrs:[ Attr.class_ "ui-caption" ] [ Ui_context.md text ]
  | None -> Node.none
;;

let image ctx (image : Ui_spec.Image.t) ~(width : Ui_spec.Image_width.t) =
  let width =
    match width with
    | Small -> "small"
    | Medium -> "medium"
    | Full -> "full"
  in
  Node.figure
    ~attrs:[ Attr.classes [ "ui-image"; "ui-w-" ^ width ] ]
    [ frame ctx image; caption image ]
;;

let gallery ctx ~title images =
  Node.div
    ~attrs:[ Attr.class_ "ui-gallery-wrap" ]
    [ (match title with
       | Some t -> Node.div ~attrs:[ Attr.class_ "ui-title" ] [ Node.text t ]
       | None -> Node.none)
    ; Node.div
        ~attrs:
          [ Attr.class_ "ui-gallery"
          ; Attr.create Image_view.group_attribute ""
          ]
        (List.map images ~f:(fun (i : Ui_spec.Image.t) ->
           Node.figure
             ~attrs:[ Attr.class_ "ui-image" ]
             [ frame ctx i; caption i ]))
    ]
;;
