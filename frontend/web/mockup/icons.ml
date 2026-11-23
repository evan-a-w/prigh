(* Lucide icons (lucide-react 0.474.0, ISC) that loon_ui's Icon lacks. Generated. *)
open! Core
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

type t =
  | Paperclip
  | Mic
  | Square_pen
  | Folder_plus
  | Panel_right
  | Panel_bottom
  | Git_pull_request
  | Wrench
  | File_diff
  | History
  | Cpu
  | At_sign
  | Hammer
  | File_search
  | Eye
  | Git_compare
  | Arrow_up_right
  | Circle_dot
  | Pencil_line
  | List_todo
  | Square_terminal
  | Bot_message_square
  | Chevrons_down_up

let class_name = function
  | Paperclip -> "lucide-paperclip"
  | Mic -> "lucide-mic"
  | Square_pen -> "lucide-square-pen"
  | Folder_plus -> "lucide-folder-plus"
  | Panel_right -> "lucide-panel-right"
  | Panel_bottom -> "lucide-panel-bottom"
  | Git_pull_request -> "lucide-git-pull-request"
  | Wrench -> "lucide-wrench"
  | File_diff -> "lucide-file-diff"
  | History -> "lucide-history"
  | Cpu -> "lucide-cpu"
  | At_sign -> "lucide-at-sign"
  | Hammer -> "lucide-hammer"
  | File_search -> "lucide-file-search"
  | Eye -> "lucide-eye"
  | Git_compare -> "lucide-git-compare"
  | Arrow_up_right -> "lucide-arrow-up-right"
  | Circle_dot -> "lucide-circle-dot"
  | Pencil_line -> "lucide-pencil-line"
  | List_todo -> "lucide-list-todo"
  | Square_terminal -> "lucide-square-terminal"
  | Bot_message_square -> "lucide-bot-message-square"
  | Chevrons_down_up -> "lucide-chevrons-down-up"
;;

let shapes = function
  | Paperclip ->
    [ "path", [ "d", "M13.234 20.252 21 12.3" ]
    ; ( "path"
      , [ ( "d"
          , "m16 6-8.414 8.586a2 2 0 0 0 0 2.828 2 2 0 0 0 2.828 \
             0l8.414-8.586a4 4 0 0 0 0-5.656 4 4 0 0 0-5.656 0l-8.415 8.585a6 \
             6 0 1 0 8.486 8.486" )
        ] )
    ]
  | Mic ->
    [ "path", [ "d", "M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z" ]
    ; "path", [ "d", "M19 10v2a7 7 0 0 1-14 0v-2" ]
    ; "line", [ "x1", "12"; "x2", "12"; "y1", "19"; "y2", "22" ]
    ]
  | Square_pen ->
    [ ( "path"
      , [ "d", "M12 3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7" ] )
    ; ( "path"
      , [ ( "d"
          , "M18.375 2.625a1 1 0 0 1 3 3l-9.013 9.014a2 2 0 0 \
             1-.853.505l-2.873.84a.5.5 0 0 1-.62-.62l.84-2.873a2 2 0 0 1 \
             .506-.852z" )
        ] )
    ]
  | Folder_plus ->
    [ "path", [ "d", "M12 10v6" ]
    ; "path", [ "d", "M9 13h6" ]
    ; ( "path"
      , [ ( "d"
          , "M20 20a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.9a2 2 0 0 1-1.69-.9L9.6 \
             3.9A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z" )
        ] )
    ]
  | Panel_right ->
    [ "rect", [ "width", "18"; "height", "18"; "x", "3"; "y", "3"; "rx", "2" ]
    ; "path", [ "d", "M15 3v18" ]
    ]
  | Panel_bottom ->
    [ "rect", [ "width", "18"; "height", "18"; "x", "3"; "y", "3"; "rx", "2" ]
    ; "path", [ "d", "M3 15h18" ]
    ]
  | Git_pull_request ->
    [ "circle", [ "cx", "18"; "cy", "18"; "r", "3" ]
    ; "circle", [ "cx", "6"; "cy", "6"; "r", "3" ]
    ; "path", [ "d", "M13 6h3a2 2 0 0 1 2 2v7" ]
    ; "line", [ "x1", "6"; "x2", "6"; "y1", "9"; "y2", "21" ]
    ]
  | Wrench ->
    [ ( "path"
      , [ ( "d"
          , "M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 \
             0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 \
             7.94-7.94l-3.76 3.76z" )
        ] )
    ]
  | File_diff ->
    [ ( "path"
      , [ "d", "M15 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7Z" ] )
    ; "path", [ "d", "M9 10h6" ]
    ; "path", [ "d", "M12 13V7" ]
    ; "path", [ "d", "M9 17h6" ]
    ]
  | History ->
    [ "path", [ "d", "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8" ]
    ; "path", [ "d", "M3 3v5h5" ]
    ; "path", [ "d", "M12 7v5l4 2" ]
    ]
  | Cpu ->
    [ "rect", [ "width", "16"; "height", "16"; "x", "4"; "y", "4"; "rx", "2" ]
    ; "rect", [ "width", "6"; "height", "6"; "x", "9"; "y", "9"; "rx", "1" ]
    ; "path", [ "d", "M15 2v2" ]
    ; "path", [ "d", "M15 20v2" ]
    ; "path", [ "d", "M2 15h2" ]
    ; "path", [ "d", "M2 9h2" ]
    ; "path", [ "d", "M20 15h2" ]
    ; "path", [ "d", "M20 9h2" ]
    ; "path", [ "d", "M9 2v2" ]
    ; "path", [ "d", "M9 20v2" ]
    ]
  | At_sign ->
    [ "circle", [ "cx", "12"; "cy", "12"; "r", "4" ]
    ; "path", [ "d", "M16 8v5a3 3 0 0 0 6 0v-1a10 10 0 1 0-4 8" ]
    ]
  | Hammer ->
    [ "path", [ "d", "m15 12-8.373 8.373a1 1 0 1 1-3-3L12 9" ]
    ; "path", [ "d", "m18 15 4-4" ]
    ; ( "path"
      , [ ( "d"
          , "m21.5 11.5-1.914-1.914A2 2 0 0 1 19 8.172V7l-2.26-2.26a6 6 0 0 \
             0-4.202-1.756L9 2.96l.92.82A6.18 6.18 0 0 1 12 8.4V10l2 2h1.172a2 \
             2 0 0 1 1.414.586L18.5 14.5" )
        ] )
    ]
  | File_search ->
    [ "path", [ "d", "M14 2v4a2 2 0 0 0 2 2h4" ]
    ; ( "path"
      , [ ( "d"
          , "M4.268 21a2 2 0 0 0 1.727 1H18a2 2 0 0 0 2-2V7l-5-5H6a2 2 0 0 0-2 \
             2v3" )
        ] )
    ; "path", [ "d", "m9 18-1.5-1.5" ]
    ; "circle", [ "cx", "5"; "cy", "14"; "r", "3" ]
    ]
  | Eye ->
    [ ( "path"
      , [ ( "d"
          , "M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 \
             1 0 .696 10.75 10.75 0 0 1-19.876 0" )
        ] )
    ; "circle", [ "cx", "12"; "cy", "12"; "r", "3" ]
    ]
  | Git_compare ->
    [ "circle", [ "cx", "18"; "cy", "18"; "r", "3" ]
    ; "circle", [ "cx", "6"; "cy", "6"; "r", "3" ]
    ; "path", [ "d", "M13 6h3a2 2 0 0 1 2 2v7" ]
    ; "path", [ "d", "M11 18H8a2 2 0 0 1-2-2V9" ]
    ]
  | Arrow_up_right ->
    [ "path", [ "d", "M7 7h10v10" ]; "path", [ "d", "M7 17 17 7" ] ]
  | Circle_dot ->
    [ "circle", [ "cx", "12"; "cy", "12"; "r", "10" ]
    ; "circle", [ "cx", "12"; "cy", "12"; "r", "1" ]
    ]
  | Pencil_line ->
    [ "path", [ "d", "M12 20h9" ]
    ; ( "path"
      , [ ( "d"
          , "M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 \
             1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 \
             .506-.854z" )
        ] )
    ; "path", [ "d", "m15 5 3 3" ]
    ]
  | List_todo ->
    [ "rect", [ "x", "3"; "y", "5"; "width", "6"; "height", "6"; "rx", "1" ]
    ; "path", [ "d", "m3 17 2 2 4-4" ]
    ; "path", [ "d", "M13 6h8" ]
    ; "path", [ "d", "M13 12h8" ]
    ; "path", [ "d", "M13 18h8" ]
    ]
  | Square_terminal ->
    [ "path", [ "d", "m7 11 2-2-2-2" ]
    ; "path", [ "d", "M11 13h4" ]
    ; ( "rect"
      , [ "width", "18"
        ; "height", "18"
        ; "x", "3"
        ; "y", "3"
        ; "rx", "2"
        ; "ry", "2"
        ] )
    ]
  | Bot_message_square ->
    [ "path", [ "d", "M12 6V2H8" ]
    ; ( "path"
      , [ "d", "m8 18-4 4V8a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2Z" ] )
    ; "path", [ "d", "M2 12h2" ]
    ; "path", [ "d", "M9 11v2" ]
    ; "path", [ "d", "M15 11v2" ]
    ; "path", [ "d", "M20 12h2" ]
    ]
  | Chevrons_down_up ->
    [ "path", [ "d", "m7 20 5-5 5 5" ]; "path", [ "d", "m7 4 5 5 5-5" ] ]
;;

let view ?(attrs = []) ?class_ t =
  Node.create_svg
    "svg"
    ~attrs:
      ([ Attr.create "xmlns" "http://www.w3.org/2000/svg"
       ; Attr.create "width" "24"
       ; Attr.create "height" "24"
       ; Attr.create "viewBox" "0 0 24 24"
       ; Attr.create "fill" "none"
       ; Attr.create "stroke" "currentColor"
       ; Attr.create "stroke-width" "2"
       ; Attr.create "stroke-linecap" "round"
       ; Attr.create "stroke-linejoin" "round"
       ; Attr.classes ([ "lucide"; class_name t ] @ Option.to_list class_)
       ]
       @ attrs)
    (List.map (shapes t) ~f:(fun (tag, a) ->
       Node.create_svg
         tag
         ~attrs:(List.map a ~f:(fun (k, v) -> Attr.create k v))
         []))
;;
