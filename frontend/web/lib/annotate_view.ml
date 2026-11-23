open! Core
open! Import
open Html
module Action = App.Action

let swatches (a : Annotate.t) ~inject =
  Node.div
    ~attrs:
      [ Attr.class_ "annotate-swatches flex items-center gap-1.5"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Colour"
      ]
    (List.map Annotate.colors ~f:(fun (name, color) ->
       let on = String.equal color a.color in
       Node.button
         ~attrs:
           [ classes
               [ "annotate-swatch size-6 cursor-pointer rounded-full border-2 \
                  border-transparent bg-(--swatch) p-0 \
                  shadow-[inset_0_0_0_1px_#0000003d] [&.on]:border-foreground \
                  [&.on]:shadow-[0_0_0_2px_var(--popover),inset_0_0_0_1px_#0000003d]"
               ]
               [ "on", on ]
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
      [ Attr.class_ "annotate-sizes flex items-center gap-1.5"
      ; Attr.role "radiogroup"
      ; Attr.create "aria-label" "Pen width"
      ]
    (List.map Annotate.sizes ~f:(fun (name, size) ->
       let on = size = a.size in
       Node.button
         ~attrs:
           [ classes
               [ "annotate-size grid size-[30px] cursor-pointer \
                  place-items-center rounded-md border border-transparent p-0 \
                  hover:bg-accent [&.on]:border-border [&.on]:bg-accent"
               ]
               [ "on", on ]
           ; Attr.type_ "button"
           ; Attr.role "radio"
           ; Attr.create "aria-checked" (Bool.to_string on)
           ; Attr.title name
           ; Attr.create "aria-label" name
           ; Attr.css_var ~name:"pen" (sprintf "%dpx" size)
           ; Attr.css_var ~name:"swatch" a.color
           ; Attr.on_click (fun _ -> inject (Action.Annotate_size size))
           ]
         [ Node.span
             ~attrs:
               [ Attr.class_
                   "annotate-dot size-(--pen) max-h-[18px] max-w-[18px] \
                    rounded-full bg-(--swatch)"
               ]
             []
         ]))
;;

(* Zoom, done by the page (app/annotate_widget.ml: [data-annotate-zoom]),
   which also writes the percentage. *)
let zoom_controls =
  let zoom ?(cls = "") action ~label body =
    Node.button
      ~attrs:
        [ Attr.type_ "button"
        ; Attr.class_
            ("annotate-zoom-btn inline-flex h-[26px] min-w-7 cursor-pointer \
              items-center justify-center rounded-md px-1.5 font-mono \
              text-[11.5px] font-semibold text-foreground \
              hover:enabled:bg-accent disabled:cursor-default \
              disabled:text-muted-foreground/50 focus-visible:outline-2 \
              focus-visible:outline-ring [&_.icon]:size-[15px] "
             ^ cls)
        ; Attr.title label
        ; Attr.create "aria-label" label
        ; Attr.create "data-annotate-zoom" action
        ]
      body
  in
  Node.span
    ~attrs:
      [ Attr.class_
          "annotate-zoom inline-flex items-center rounded-lg bg-muted p-0.5 \
           ring-1 ring-border"
      ; Attr.role "group"
      ; Attr.create "aria-label" "Zoom"
      ]
    [ zoom "out" ~label:"Zoom out (−)" [ icon Zoom_out ]
    ; zoom
        ~cls:"annotate-zoom-pct min-w-[50px] tabular-nums"
        "fit"
        ~label:"Fit (0); Ctrl+scroll or pinch to zoom, scroll to pan"
        [ Node.text "100%" ]
    ; zoom "in" ~label:"Zoom in (+)" [ icon Zoom_in ]
    ]
;;

let view (a : Annotate.t) ~inject =
  let nothing = a.strokes = 0 in
  Modal.view
    ~cls:
      "annotate-dialog h-[min(860px,100%)] max-h-none \
       w-[min(1100px,calc(100vw-32px))] max-w-none [&_.modal-body]:flex \
       [&_.modal-body]:flex-1 [&_.modal-body]:flex-col [&_.modal-body]:gap-2.5 \
       [&_.modal-body]:overflow-hidden max-[760px]:h-dvh max-[760px]:w-screen \
       max-[760px]:rounded-none narrow:h-(--vvh,100dvh) narrow:rounded-none \
       narrow:border-0 narrow:pt-(--safe-t) narrow:pb-(--safe-b)"
    ~title:"Draw on the image"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ span
          ~cls:
            "annotate-hint flex-1 text-xs text-muted-foreground \
             max-[760px]:hidden"
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
        ~cls:
          "annotate-toolbar flex flex-wrap items-center gap-2.5 \
           [&_.icon]:size-3.5"
        [ swatches a ~inject
        ; Node.span
            ~attrs:[ Attr.class_ "annotate-sep h-[22px] w-px bg-border" ]
            []
        ; widths a ~inject
        ; Node.span ~attrs:[ Attr.class_ "annotate-spacer flex-1" ] []
        ; zoom_controls
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
        ~cls:
          "annotate-stage grid min-h-0 flex-1 place-items-center \
           overflow-hidden rounded-md bg-muted \
           bg-[repeating-conic-gradient(var(--checker)_0_25%,transparent_0_50%)] \
           bg-size-[16px_16px]"
        [ Node.div
            ~attrs:
              [ Attr.class_
                  "annotate-frame relative inline-block max-h-full max-w-full \
                   origin-center leading-none will-change-transform \
                   motion-reduce:transition-none! [&_img]:block \
                   [&_img]:max-h-full [&_img]:max-w-full \
                   [&_img]:object-contain [&_img]:select-none"
              ]
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
                  ; Attr.class_
                      "annotate-canvas absolute inset-0 size-full \
                       cursor-crosshair touch-none"
                  ; Attr.create "data-color" a.color
                  ; Attr.create "data-size" (Int.to_string a.size)
                  ; Attr.create "aria-label" "Drawing surface: drag to draw"
                  ]
                []
            ]
        ]
    ]
;;
