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
  | Switch_back -> Back
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
  | Copy_message -> Copy
  | Trim_before | Trim_after -> Scissors
  | Approve_files -> Check
  | Unapprove_files -> Undo
  | Stage_files -> Plus
  | Unstage_files -> Dash
  | Commit_files -> Branch
  | Select_files -> Square
  | Unselect_files -> Cross
  | Copy_paths -> Copy
;;

(* What the menu is about, above its items. *)
let heading (m : App.Model.t) (menu : Context_menu.t) =
  match
    ( menu.target
    , List.find m.sessions ~f:(fun s ->
        [%equal: string option] (Some s.path) (Context_menu.session menu)) )
  with
  | Review_files paths, _ ->
    div
      ~cls:"context-menu-head"
      [ span
          ~cls:"context-menu-title"
          (match paths with
           | [ path ] -> Filename.basename path
           | _ -> sprintf "%d selected files" (List.length paths))
      ; span
          ~cls:"context-menu-sub"
          (match paths with
           | [ path ] -> Filename.dirname path
           | _ -> String.concat ~sep:", " (List.map paths ~f:Filename.basename))
      ]
  | Message n, _ ->
    (match App.Model.message_menu m n with
     | None -> Node.none
     | Some msg ->
       div
         ~cls:"context-menu-head"
         [ span ~cls:"context-menu-title" (Context_menu.Message.title msg)
         ; (match
              String.split_lines msg.text
              |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
            with
            | Some line ->
              span ~cls:"context-menu-sub preview" (String.strip line)
            | None -> Node.none)
         ])
  | _, None -> Node.none
  | (Session _ | Worktree _), Some s ->
    (match menu.target with
     | Session _ | Message _ | Review_files _ ->
       div
         ~cls:"context-menu-head"
         [ span ~cls:"context-menu-title" (Session_list.title s)
         ; Sidebar_view.place
             ~cls:"context-menu-sub"
             ?host_home:(Sidebar_view.session_host_home m s)
             s
             ~cwd:s.cwd
         ]
     | Worktree { path; _ } ->
       let label =
         List.find s.worktrees ~f:(fun w -> String.equal w.path path)
         |> Option.value_map
              ~default:(Filename.basename path)
              ~f:Session_worktree.label
       in
       div
         ~cls:"context-menu-head"
         [ Node.span
             ~attrs:[ Attr.class_ "context-menu-title" ]
             [ icon ~cls:"place-icon" Branch; Node.text label ]
         ; span ~cls:"context-menu-sub" ("in " ^ Session_list.title s)
         ])
;;

let view (m : App.Model.t) (menu : Context_menu.t) picker ~inject =
  let close = inject Action.Close_dialog in
  let items = Picker.visible picker in
  let row i (item : Picker.Item.t) =
    let kind = Context_menu.Item.of_id item.id in
    let danger = Option.exists kind ~f:Context_menu.Item.danger in
    (* Dimmed: disabled, the detail says why. *)
    let disabled = item.dimmed in
    let selected = i = Picker.selected picker in
    Node.div
      ~attrs:
        [ classes
            [ "menu-item" ]
            [ "selected", selected; "danger", danger; "disabled", disabled ]
        ; Attr.role "menuitem"
        ; Attr.create "data-item" item.id
        ; (if disabled then Attr.create "aria-disabled" "true" else Attr.empty)
        ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
        ; Attr.on_mouseenter (fun _ ->
            if selected
            then Effect.Ignore
            else inject (Action.Picker_move (i - Picker.selected picker)))
        ]
      [ Option.value_map kind ~default:Node.none ~f:(fun k ->
          icon (item_icon k))
      ; (if disabled && not (String.is_empty item.detail)
         then
           Node.span
             ~attrs:[ Attr.class_ "menu-text" ]
             [ span ~cls:"menu-label" item.label
             ; span ~cls:"menu-hint" item.detail
             ]
         else span ~cls:"menu-label" item.label)
      ; (if String.is_empty item.detail || disabled
         then Node.none
         else
           Node.kbd
             ~attrs:[ Attr.class_ "menu-command" ]
             [ Node.text item.detail ])
      ]
  in
  (* Separated: going there, changing it, looking at it, copying, and the
     destructive ones. *)
  let group : Context_menu.Item.t -> int = function
    | Switch_back | Open | Open_tab | Switch_here | New_session_here -> 0
    | Rename | Fork | New_worktree | Create_pr | Trim_before | Trim_after -> 1
    | Review | Files | Terminal -> 2
    | Copy_path | Copy_id | Copy_message -> 3
    | Archive | Unarchive | Delete | Remove -> 4
    | Approve_files | Unapprove_files -> 0
    | Stage_files | Unstage_files | Commit_files -> 1
    | Select_files | Unselect_files -> 2
    | Copy_paths -> 3
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
      [ Attr.class_ "menu-backdrop context-menu-backdrop"
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
           div
             ~cls:"menu-section"
             (List.map section ~f:(fun (i, item) -> row i item))))
    ]
;;
