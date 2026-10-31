open! Core
open! Import
open Html
module Action = App.Action

let swatches (a : Annotate.t) ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "annotate-swatches"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Colour"
      ]
    (List.map Annotate.colors ~f:(fun (name, color) ->
       let on = String.equal color a.color in
       Node.button
         ~attrs:
           [ classes [ "annotate-swatch" ] [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string on)
           ; Attr.title name
           ; Attr.create "aria-label" name
           ; Attr.css_var ~name:"swatch" color
           ; Attr.on_click (fun _ -> inject (Action.Annotate_color color))
           ]
         []))
;;

let widths (a : Annotate.t) ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "annotate-sizes"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Pen width"
      ]
    (List.map Annotate.sizes ~f:(fun (name, size) ->
       let on = size = a.size in
       Node.button
         ~attrs:
           [ classes [ "annotate-size" ] [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string on)
           ; Attr.title name
           ; Attr.create "aria-label" name
           ; Attr.css_var ~name:"pen" (sprintf "%dpx" size)
           ; Attr.css_var ~name:"swatch" a.color
           ; Attr.on_click (fun _ -> inject (Action.Annotate_size size))
           ]
         [ Node.span ~attrs:[ Attr.class_ "annotate-dot" ] [] ]))
;;

let view (a : Annotate.t) ~inject =
  let nothing = a.strokes = 0 in
  Modal.view
    ~cls:"annotate-dialog"
    ~title:"Draw on the image"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ span
          ~cls:"annotate-hint"
          (match a.target with
           | Pending _ -> "Saving replaces the attached image."
           | New -> "Saving attaches the drawing to your next message.")
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary annotate-save"
          ~disabled:nothing
          ~on_click:(inject Action.Annotate_save)
          [ Node.text
              (match a.target with
               | Pending _ -> "Save"
               | New -> "Attach")
          ]
      ]
    [ div
        ~cls:"annotate-toolbar"
        [ swatches a ~inject
        ; Node.span ~attrs:[ Attr.class_ "annotate-sep" ] []
        ; widths a ~inject
        ; Node.span ~attrs:[ Attr.class_ "annotate-spacer" ] []
        ; button
            ~cls:"ghost small annotate-undo"
            ~title:"Undo the last stroke (Ctrl+Z)"
            ~disabled:nothing
            ~on_click:(inject Action.Annotate_undo)
            [ icon Undo; Node.text "Undo" ]
        ; button
            ~cls:"ghost small annotate-clear"
            ~title:"Rub out every stroke"
            ~disabled:nothing
            ~on_click:(inject Action.Annotate_clear)
            [ icon Eraser; Node.text "Clear" ]
        ]
    ; div
        ~cls:"annotate-stage"
        [ Node.div
            ~attrs:[ Attr.class_ "annotate-frame" ]
            [ Node.img
                ~attrs:
                  [ Attr.id "annotate-image"
                  ; Attr.src (Image_view.src a.image)
                  ; Attr.alt (Image.to_string_hum a.image)
                  ; Attr.create "draggable" "false"
                  ]
                ()
            ; Node.canvas
                ~attrs:
                  [ Attr.id "annotate-canvas"
                  ; Attr.class_ "annotate-canvas"
                  ; Attr.create "data-color" a.color
                  ; Attr.create "data-size" (Int.to_string a.size)
                  ; Attr.create "aria-label" "Drawing surface: drag to draw"
                  ]
                []
            ]
        ]
    ]
;;
