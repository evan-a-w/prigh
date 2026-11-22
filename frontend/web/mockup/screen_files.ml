open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let entries =
  [ `Dir, "app", "", "2h"; `Dir, "bin", "", "1d"; `Dir, "e2e", "", "3h"; `Dir, "lib", "", "12m"
  ; `Dir, "loon_ui", "", "4h"; `Dir, "mockup", "", "now"; `Dir, "test", "", "1d"
  ; `File, "AGENTS.md", "3.1 KB", "2d"; `File, "dune", "412 B", "4h"; `File, "dune-project", "86 B", "9d"
  ; `File, "screenshot.png", "184 KB", "now"
  ]
;;

let panel ~tip =
  Panel.view
    ~title:[ span "font-medium" [ Node.text "Files" ]; span "text-xs text-muted-foreground" [ Node.text "desktop" ] ]
    ~actions:
      [ Panel.icon_button tip ~id:"files-search" "Search files" (Icon.view Search)
      ; Panel.icon_button tip ~id:"files-folder" "New folder" (Icons.view Folder_plus)
      ; Panel.icon_button tip ~id:"files-upload" "Upload" (Icon.view Upload)
      ; Panel.icon_button tip ~id:"files-close" "Close" (Icon.view X)
      ]
    [ div "px-3 pt-2.5 pb-1"
        [ Breadcrumb.root [ Breadcrumb.list ~class_:"text-xs"
            (List.concat_mapi [ "~"; "projects"; "prigh"; "frontend" ] ~f:(fun i s ->
               (if i = 0 then [] else [ Breadcrumb.separator () ]) @ [ Breadcrumb.item [ Breadcrumb.link ~href:"#" [ Node.text s ] ] ])
             @ [ Breadcrumb.separator (); Breadcrumb.item [ Breadcrumb.page [ Node.text "web" ] ] ]) ] ]
    ; div "mx-3 my-2 flex flex-col gap-1.5 rounded-lg border bg-card px-3 py-2"
        [ div "flex items-center gap-2 text-xs"
            [ Icon.view ~class_:"size-3.5 text-muted-foreground" Cloud_upload
            ; span "truncate" [ Node.text "recording-2026-11-22.mp4" ]
            ; span "ml-auto tabular-nums text-muted-foreground" [ Node.text "64% · 38 of 59 MB" ]
            ; Button.view ~variant:Ghost ~size:Icon_xs ~class_:"size-5 text-muted-foreground" [ Icon.view X ]
            ]
        ; Progress.view ~class_:"h-1" ~value:64. ()
        ]
    ; div "min-h-0 flex-1 overflow-y-auto px-1.5"
        [ Table.root ~class_:"text-xs"
            [ Table.header [ Table.row [ Table.head ~class_:"h-8" [ Node.text "Name" ]; Table.head ~class_:"h-8 text-right" [ Node.text "Size" ]; Table.head ~class_:"h-8 text-right" [ Node.text "Modified" ] ] ]
            ; Table.body
                (List.map entries ~f:(fun (kind, name, size, age) ->
                   Table.row ~selected:(String.equal name "mockup") ~class_:"cursor-pointer"
                     [ Table.cell ~class_:"py-1.5"
                         [ span "flex items-center gap-2"
                             [ (match kind with `Dir -> Icon.view ~class_:"size-4 text-sky-400/80" Folder | `File -> Icon.view ~class_:"size-4 text-muted-foreground" File)
                             ; Node.text name ] ]
                     ; Table.cell ~class_:"py-1.5 text-right tabular-nums text-muted-foreground" [ Node.text size ]
                     ; Table.cell ~class_:"py-1.5 text-right text-muted-foreground" [ Node.text age ]
                     ]))
            ]
        ]
    ]
;;

let component ~tip (local_ graph) =
  Shell.component ~right:(Bonsai.map tip ~f:(fun tip -> panel ~tip)) ~right_size:34. ~right_title:"Files" ~tip ~main:(Screen_chat.component ~tip graph) graph
;;
