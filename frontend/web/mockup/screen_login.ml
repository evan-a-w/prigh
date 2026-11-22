open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children

let field ~id ~label ?hint input =
  Field.root
    [ div
        "flex items-center"
        [ Field.label ~for_:id [ Node.text label ]
        ; (match hint with
           | None -> Node.none
           | Some h -> div "ml-auto" [ h ])
        ]
    ; input
    ]
;;

let component ~tip:_ (local_ _graph) =
  Bonsai.return
    (div
       "flex min-h-svh flex-col items-center justify-center gap-6 bg-background px-4 \
        py-[max(2rem,env(safe-area-inset-top))] font-sans text-foreground antialiased"
       [ div
           "flex items-center gap-2 text-lg font-semibold tracking-tight"
           [ div
               "flex size-7 items-center justify-center rounded-lg bg-primary \
                text-primary-foreground [&_svg]:size-4"
               [ Icon.view Terminal ]
           ; Node.text "prigh"
           ]
       ; Card.root
           ~class_:"w-full max-w-sm"
           [ Card.header
               [ Card.title [ Node.text "Sign in" ]
               ; Card.description [ Node.text "to the prigh server on desktop.local" ]
               ]
           ; Card.content
               [ Field.group
                   [ field
                       ~id:"server"
                       ~label:"Server"
                       (Input.view
                          ~id:"server"
                          ~type_:Url
                          ~class_:"font-mono text-base md:text-[13px]"
                          ~value:"https://desktop.local:7790"
                          ())
                   ; field
                       ~id:"user"
                       ~label:"User"
                       (Input.view
                          ~id:"user"
                          ~class_:"text-base md:text-sm"
                          ~value:"lloyd"
                          ())
                   ; field
                       ~id:"password"
                       ~label:"Password"
                       ~hint:
                         (Node.a
                            ~attrs:
                              [ Attr.href "#"
                              ; Attr.class_
                                  "text-xs text-muted-foreground underline-offset-4 \
                                   hover:underline"
                              ]
                            [ Node.text "Use a token" ])
                       (Input.view
                          ~id:"password"
                          ~type_:Password
                          ~class_:"text-base md:text-sm"
                          ())
                   ; Button.view ~class_:"w-full max-md:h-11" [ Node.text "Sign in" ]
                   ]
               ]
           ]
       ; Node.p
           ~attrs:[ Attr.class_ "max-w-sm text-center text-xs text-muted-foreground" ]
           [ Node.text "On the server: "
           ; Node.code
               ~attrs:[ Attr.class_ "font-mono text-foreground/80" ]
               [ Node.text "prigh user add NAME" ]
           ; Node.text " makes an account."
           ]
       ])
;;
