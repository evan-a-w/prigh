open! Core
open! Import
open Html
module Action = App.Action

let item_icon : Context_menu.Item.t -> Icon.t = function
  | Open -> Arrow_down
  | Open_tab -> External
  | Rename -> Pencil
  | Fork -> Fork
  | New_worktree -> Branch_plus
  | Switch_here -> Locate
  | New_session_here -> Plus
  | Review -> Review
  | Files -> Folder
  | Terminal -> Terminal
  | Create_pr -> Pull_request
  | Copy_path | Copy_id -> Copy
  | Archive -> Archive
  | Unarchive -> Unarchive
  | Delete | Remove -> Trash
;;

(* What the menu is about, above its items. *)
let heading (m : App.Model.t) (menu : Context_menu.t) =
  match
    List.find m.sessions ~f:(fun s ->
      String.equal s.path (Context_menu.session menu))
  with
  | None -> Node.none
  | Some s ->
    (match menu.target with
     | Session _ ->
       div
         ~cls:"context-menu-head"
         [ span ~cls:"context-menu-title" (Session_list.title s)
         ; (match Prigh_ui.Session_worktrees.place s with
            | Some place -> span ~cls:"context-menu-sub" place
            | None -> span ~cls:"context-menu-sub" (Sidebar_view.home_relative s.cwd))
         ]
     | Worktree { path; _ } ->
       let label =
         List.find s.worktrees ~f:(fun w -> String.equal w.path path)
         |> Option.value_map ~default:(Filename.basename path) ~f:Session_worktree.label
       in
       div
         ~cls:"context-menu-head"
         [ span ~cls:"context-menu-title" ("⎇ " ^ label)
         ; span ~cls:"context-menu-sub" ("in " ^ Session_list.title s)
         ])
;;

let view (m : App.Model.t) (menu : Context_menu.t) picker ~inject =
  let close = inject Action.Close_dialog in
  let items = Picker.visible picker in
  let row i (item : Picker.Item.t) =
    let kind = Context_menu.Item.of_id item.id in
    let danger = Option.exists kind ~f:Context_menu.Item.danger in
    Node.div
      ~attrs:
        [ classes
            [ "menu-item" ]
            [ "selected", i = Picker.selected picker; "danger", danger ]
        ; Attr.role "menuitem"
        ; Attr.create "data-item" item.id
        ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
        ]
      [ Option.value_map kind ~default:Node.none ~f:(fun k -> icon (item_icon k))
      ; span ~cls:"menu-label" item.label
      ; (if String.is_empty item.detail
         then Node.none
         else Node.kbd ~attrs:[ Attr.class_ "menu-command" ] [ Node.text item.detail ])
      ]
  in
  (* Separated: going there, changing it, looking at it, copying, and the
     destructive ones. *)
  let group : Context_menu.Item.t -> int = function
    | Open | Open_tab | Switch_here | New_session_here -> 0
    | Rename | Fork | New_worktree | Create_pr -> 1
    | Review | Files | Terminal -> 2
    | Copy_path | Copy_id -> 3
    | Archive | Unarchive | Delete | Remove -> 4
  in
  let sections =
    List.mapi items ~f:(fun i item -> i, item)
    |> List.group ~break:(fun (_, a) (_, b) ->
      let g (item : Picker.Item.t) =
        Option.value_map (Context_menu.Item.of_id item.id) ~default:5 ~f:group
      in
      g a <> g b)
  in
  Node.div
    ~attrs:
      [ Attr.class_ "menu-backdrop"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then close
          else Effect.Ignore)
      ; Attr.on_contextmenu (fun ev ->
          Js_of_ocaml.Dom.preventDefault ev;
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ "context-menu menu-surface"
          ; Attr.id "dialog"
          ; Attr.role "menu"
          ; Attr.create "aria-label" (Picker.title picker)
          ; Attr.tabindex (-1)
          ; (match menu.anchor with
             | Point { x; y } ->
               Attr.many
                 [ Attr.create "data-x" (Int.to_string x)
                 ; Attr.create "data-y" (Int.to_string y)
                 ]
             | Element id -> Attr.create "data-anchor" id)
          ]
        (heading m menu
         :: List.map sections ~f:(fun section ->
           div ~cls:"menu-section" (List.map section ~f:(fun (i, item) -> row i item))))
    ]
;;
