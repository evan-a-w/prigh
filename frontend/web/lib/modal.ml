open! Core
open! Import
open Html

let view ?(cls = "") ?(id = "dialog") ?(footer = []) ?back ~title ~on_close body =
  Node.div
    ~attrs:
      [ Attr.class_ "loon modal-backdrop"
      ; Attr.create "data-slot" "dialog-overlay"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then on_close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ (String.strip ("modal " ^ cls))
          ; Attr.id id
          ; Attr.create "data-slot" "dialog-content"
          ; Attr.role "dialog"
          ; Attr.create "aria-modal" "true"
          ; Attr.create "aria-label" title
          ; Attr.tabindex (-1)
          ]
        [ Node.header
            ~attrs:
              [ Attr.class_ "modal-head"; Attr.create "data-slot" "dialog-header" ]
            ((match back with
              | Some (label, on_back) ->
                [ button
                    ~cls:"ghost modal-back"
                    ~title:label
                    ~on_click:on_back
                    [ icon Back; span ~cls:"modal-back-label" label ]
                ]
              | None -> [])
             @ [ Node.h2
                   ~attrs:[ Attr.create "data-slot" "dialog-title" ]
                   [ Node.text title ]
            ; button
                ~cls:"icon ghost"
                ~title:"Close (Esc)"
                ~label:"Close"
                ~on_click:on_close
                [ icon Close ]
            ])
        ; div ~cls:"modal-body" body
        ; (match footer with
           | [] -> Node.none
           | footer ->
             Node.footer
               ~attrs:
                 [ Attr.class_ "modal-buttons"
                 ; Attr.create "data-slot" "dialog-footer"
                 ]
               footer)
        ]
    ]
;;
