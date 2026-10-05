open! Core
open! Import
open Html

let view ?(cls = "") ?(id = "dialog") ?(footer = []) ~title ~on_close body =
  Node.div
    ~attrs:
      [ Attr.class_ "modal-backdrop"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then on_close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ (String.strip ("modal " ^ cls))
          ; Attr.id id
          ; Attr.role "dialog"
          ; Attr.create "aria-modal" "true"
          ; Attr.create "aria-label" title
          ; Attr.tabindex (-1)
          ]
        [ Node.header
            ~attrs:[ Attr.class_ "modal-head" ]
            [ Node.h2 [ Node.text title ]
            ; button
                ~cls:"icon ghost"
                ~title:"Close (Esc)"
                ~on_click:on_close
                [ icon Close ]
            ]
        ; div ~cls:"modal-body" body
        ; (match footer with
           | [] -> Node.none
           | footer -> Node.footer ~attrs:[ Attr.class_ "modal-buttons" ] footer)
        ]
    ]
;;
