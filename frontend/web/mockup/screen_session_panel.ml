open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr
module M = Dropdown_menu

let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children
let t s = Node.text s

let sub label icon items =
  M.sub (M.sub_trigger [ icon; t label ]) (M.sub_content ~class_:"min-w-48" items)
;;

let items =
  [ M.label
      ~class_:"flex items-center gap-2 py-1.5 font-normal text-muted-foreground"
      [ Icon.view ~class_:"size-4" Monitor
      ; t Fake.User.host
      ; span "ml-auto truncate font-mono text-[11px]" [ t "~/projects/prigh" ]
      ]
  ; sub
      "Open in"
      (Icon.view Folder)
      [ M.item [ Icon.view Folder; t "Files" ]
      ; M.item [ Icon.view Code; t "VS Code" ]
      ; M.item [ Icon.view Terminal; t "Terminal" ]
      ]
  ; M.item [ Icon.view Plus; t "Add project script" ]
  ; M.separator ()
  ; sub
      Fake.User.branch
      (Icon.view Git_branch)
      [ M.item [ t "main" ]
      ; M.item [ Icon.view Check; t Fake.User.branch ]
      ; M.separator ()
      ; M.item [ Icon.view Plus; t "New branch…" ]
      ]
  ; sub
      "Commit"
      (Icon.view Git_commit_vertical)
      [ M.item [ t "Commit…" ]
      ; M.item [ t "Commit and push" ]
      ; M.item [ Icons.view Git_pull_request; t "Open pull request" ]
      ]
  ; M.item
      [ Icons.view File_diff
      ; t "Changes"
      ; M.shortcut
          ~class_:"font-mono tracking-normal"
          [ span "text-emerald-400" [ t "+12" ]; t " "; span "text-red-400" [ t "−3" ] ]
      ]
  ; sub
      "Previous agents"
      (Icon.view Bot)
      [ M.item [ t "Composer chips as a toggle group"; M.shortcut [ t "2h" ] ]
      ; M.item [ t "Review: chips on 320px"; M.shortcut [ t "2h" ] ]
      ]
  ; M.separator ()
  ; M.item [ Icon.view Git_branch; t "Fork"; M.shortcut [ t "/fork" ] ]
  ; M.item [ Icons.view History; t "Rewind…"; M.shortcut [ t "/rewind" ] ]
  ; M.item [ Icons.view Pencil_line; t "Rename"; M.shortcut [ t "F2" ] ]
  ; M.item [ Icon.view Download; t "Export" ]
  ; M.separator ()
  ; M.item ~variant:Destructive [ Icon.view Trash_2; t "Delete session" ]
  ]
;;

let component ~tip (local_ graph) =
  let state, set_state = Bonsai.state (M.State.Open Menu.State.initial) graph in
  let more =
    let%arr state and set_state in
    fun button ->
      M.view ~id:"session-menu" ~state ~on_change:set_state (fun m ->
        [ M.trigger m button; M.content m ~align:End ~side_offset:6. ~class_:"w-64" items ])
  in
  Shell.component ~tip ~more ~main:(Screen_chat.component ~tip graph) graph
;;
