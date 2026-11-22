open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

module Kind = struct
  type t =
    | Commands
    | Files
end

let commands =
  [ "/review", "Review the branch's changes"
  ; "/rewind", "Go back to an earlier message"
  ; "/rename", "Rename this session"
  ; "/resume", "Resume a settled session"
  ; "/reload", "Reload skills and instructions"
  ]
;;

let files =
  [ "composer.ml", "frontend/web/mockup/"
  ; "composer_view.ml", "frontend/web/lib/"
  ; "composer.mli", "frontend/web/mockup/"
  ; "composer_test.ml", "frontend/web/test/"
  ]
;;

let menu (kind : Kind.t) ~state ~set_state =
  let query, heading, rows =
    match kind with
    | Commands ->
      ( "/re"
      , "Commands"
      , List.map commands ~f:(fun (name, description) ->
          Command.item
            ~value:name
            [ span "font-mono" (Overlay_parts.highlight ~query:"/re" name)
            ; span "truncate text-muted-foreground" [ Node.text description ]
            ]) )
    | Files ->
      ( "comp"
      , "Files"
      , List.map files ~f:(fun (name, dir) ->
          Command.item
            ~value:(dir ^ name)
            [ span "" (Overlay_parts.highlight ~query:"comp" name)
            ; span "truncate font-mono text-xs text-muted-foreground" [ Node.text dir ]
            ]) )
  in
  Node.div
    ~attrs:
      [ Attr.class_
          "absolute inset-x-4 bottom-full z-20 mb-1 overflow-hidden rounded-xl border \
           bg-popover text-popover-foreground shadow-lg"
      ]
    [ Command.view
        ~id:"completion"
        ~class_:"bg-transparent [&_[data-slot=command-item]]:w-full"
        ~should_filter:false
        ~state
        ~on_change:set_state
        [ Command.list
            ~class_:"max-h-64"
            [ Command.group ~heading rows ]
        ; Command.static
            (Overlay_parts.footer_hints
               [ [ "↑"; "↓" ], "choose"; [ "Tab" ], "complete"; [ "esc" ], "dismiss" ])
        ]
    ]
    |> fun m -> m, query
;;

let component kind ~tip (local_ graph) =
  let state, set_state = Bonsai.state Command.State.empty graph in
  let composer =
    let%arr state and set_state in
    let above, query = menu kind ~state ~set_state in
    let draft =
      match kind with
      | Commands -> query
      | Files -> "Tidy up @" ^ query
    in
    fun tip -> Composer.view ~draft ~above ~tip ()
  in
  Shell.component ~tip ~main:(Screen_chat.component ~composer ~tip graph) graph
;;
