open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children

let groups =
  [ ( "General"
    , [ "Control centre", [ "Ctrl"; "K" ]
      ; "Toggle the sidebar", [ "Ctrl"; "B" ]
      ; "Cycle verbosity", [ "Ctrl"; "O" ]
      ; "Terminal", [ "Ctrl"; "`" ]
      ; "Review changes", [ "Ctrl"; "Shift"; "R" ]
      ; "Help", [ "?" ]
      ] )
  ; ( "Composer"
    , [ "Send", [ "Enter" ]
      ; "New line", [ "Shift"; "Enter" ]
      ; "Queue while running", [ "Alt"; "Enter" ]
      ; "Model", [ "Ctrl"; "M" ]
      ; "Thinking level", [ "Ctrl"; "T" ]
      ; "Stop the turn", [ "Esc" ]
      ] )
  ; ( "Sessions"
    , [ "New session", [ "Ctrl"; "Shift"; "O" ]
      ; "Previous / next session", [ "Alt"; "↑ ↓" ]
      ; "Fork", [ "Ctrl"; "Shift"; "F" ]
      ; "Rewind", [ "Esc"; "Esc" ]
      ] )
  ]
;;

let component ~tip:_ (local_ graph) =
  let body =
    Bonsai.return
      (div
         "flex flex-col gap-8"
         [ Input_group.root
             ~class_:"h-9"
             [ Input_group.addon [ Icon.view Search ]
             ; Input_group.input ~placeholder:"Search shortcuts" ()
             ]
         ; div
             "flex flex-col gap-8"
             (List.map groups ~f:(fun (title, keys) ->
                div
                  "flex flex-col gap-2.5"
                  [ Node.h2
                      ~attrs:[ Attr.class_ "px-1 text-sm font-medium" ]
                      [ Node.text title ]
                  ; div
                      "overflow-hidden rounded-xl border bg-card"
                      [ Table.root
                          [ Table.body
                              (List.map keys ~f:(fun (action, ks) ->
                                 Table.row
                                   ~class_:"hover:bg-accent/40"
                                   [ Table.cell ~class_:"px-4 py-2.5" [ Node.text action ]
                                   ; Table.cell
                                       ~class_:"px-4 py-2.5 text-right"
                                       [ Kbd.group
                                           ~class_:"justify-end"
                                           (List.map ks ~f:(fun k ->
                                              Kbd.view [ Node.text k ]))
                                       ]
                                   ]))
                          ]
                      ]
                  ]))
         ])
  in
  Settings_page.component
    ~section:Keys
    ~description:"Every binding, as /help lists them. Click one to change it."
    ~actions:[ Button.view ~variant:Outline ~size:Sm [ Node.text "Reset all" ] ]
    ~body
    graph
;;
