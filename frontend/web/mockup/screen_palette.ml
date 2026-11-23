open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let sessions : (Fake.Project.t * string * string) list =
  [ Fake.Project.prigh, "Model picker on phones", "2d"
  ; Fake.Project.prigh, "Rebuild prigh-web on loon", "now"
  ; Fake.Project.loon, "Port shadcn's Resizable", "12m"
  ; Fake.Project.co, "Monday standup notes", "5d"
  ; Fake.Project.prigh, "Flaky tmux-test on posix backend", "1h"
  ]
;;

let commands : (Icon.t * string * string * string list) list =
  [ Sparkles, "Switch model", "/model", [ "Ctrl"; "M" ]
  ; Shield, "Change mode", "/mode", [ "Shift"; "Tab" ]
  ; Brain, "Thinking level", "/thinking", [ "Ctrl"; "T" ]
  ; Moon, "Toggle dark mode", "", [ "Ctrl"; "Shift"; "L" ]
  ; Square_terminal, "New terminal", "/terminal", [ "Ctrl"; "`" ]
  ; Git_branch, "Fork session", "/fork", []
  ]
;;

let settings : (Icon.t * string * string) list =
  [ Sparkles, "Models & providers", "Default model, API keys"
  ; Shield, "Default mode", "Confirm risky tools"
  ; Server, "Hosts", "desktop, laptop, build box"
  ; Paintbrush, "Appearance", "Theme, accent, density"
  ]
;;

let item_row ~query ~icon ~label ?detail right =
  [ icon
  ; span "truncate" (Overlay_parts.highlight ~query label)
  ; (match detail with
     | None -> Node.none
     | Some d -> span "truncate text-xs text-muted-foreground" [ Node.text d ])
  ; Command.shortcut ~class_:"flex shrink-0 items-center gap-2 tracking-normal" right
  ]
;;

let parts ~query =
  [ Command.input ~placeholder:"Search sessions, commands and settings…" ~auto_focus:true ()
  ; Command.list
      ~class_:"max-h-[min(420px,60svh)]"
      [ Command.empty [ Node.text "Nothing matches. Try a session title or /command." ]
      ; Command.group
          ~heading:"Sessions"
          (List.map sessions ~f:(fun (p, title, age) ->
             Command.item
               ~value:title
               ~keywords:[ p.name ]
               [ span "truncate" (Overlay_parts.highlight ~query title)
               ; span "truncate text-xs text-muted-foreground" [ Node.text p.name ]
               ; Command.shortcut ~class_:"tabular-nums tracking-normal" [ Node.text age ]
               ]))
      ; Command.separator ()
      ; Command.group
          ~heading:"Commands"
          (List.map commands ~f:(fun (icon, label, slash, keys) ->
             Command.item
               ~value:label
               ~keywords:[ slash ]
               (item_row
                  ~query
                  ~icon:(Icon.view icon)
                  ~label
                  ?detail:(if String.is_empty slash then None else Some slash)
                  [ (match keys with
                     | [] -> Node.none
                     | keys -> Kbd.group (List.map keys ~f:(fun k -> Kbd.view [ Node.text k ])))
                  ])))
      ; Command.separator ()
      ; Command.group
          ~heading:"Settings"
          (List.map settings ~f:(fun (icon, label, detail) ->
             Command.item
               ~value:label
               ~keywords:[ detail ]
               (item_row
                  ~query
                  ~icon:(Icon.view icon)
                  ~label
                  ~detail
                  [ Icon.view ~class_:"text-muted-foreground" Chevron_right ])))
      ]
  ; Command.static
      (Overlay_parts.footer_hints
         [ [ "↑"; "↓" ], "navigate"; [ "↵" ], "open"; [ "esc" ], "close" ])
  ]
;;

let component ~tip (local_ graph) =
  let state, set_state =
    Bonsai.state { Command.State.search = "mod"; value = None } graph
  in
  let open_, set_open = Bonsai.state true graph in
  let overlay =
    let%arr state and set_state and open_ and set_open in
    Command.dialog
      ~id:"palette"
      ~title:"Control centre"
      ~class_:"top-[14%] sm:max-w-[620px] [&_[data-slot=command-item]]:gap-2.5 [&_[data-slot=command-item]]:w-full"
      ~open_
      ~on_open_change:set_open
      ~state
      ~on_change:set_state
      (parts ~query:state.search)
  in
  Shell.component ~tip ~overlay ~main:(Screen_chat.component ~tip graph) graph
;;
