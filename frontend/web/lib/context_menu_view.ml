open! Core
open! Import
open Html
module Action = App.Action

let item_icon : Context_menu.Item.t -> Icon.t = function
  | Open -> Arrow_down
  | Open_window -> External
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
  | Pin -> Pin
  | Unpin -> Pin_off
  | Delete | Remove -> Trash
  | Copy_message -> Copy
  | Select_text -> Lines
  | Trim_before | Trim_after -> Scissors
  | Approve_files -> Check
  | Unapprove_files -> Undo
  | Stage_files -> Plus
  | Unstage_files -> Dash
  | Commit_files -> Branch
  | Select_files -> Square
  | Unselect_files -> Cross
  | Copy_paths -> Copy
  | Show_archived -> Archive
  | Hide_archived -> Archive
  | Delete_archived -> Trash
;;

(* Where the page put it (data-x, data-y or data-anchor; hidden until it is
   placed); on a narrow window, along the bottom. *)
let surface_cls =
  "context-menu fixed min-w-[248px] max-w-[min(360px,calc(100vw-16px))] \
   max-h-[calc(100vh-16px)] [&:not(.placed)]:opacity-0 \
   [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 \
   motion-reduce:animate-none max-[760px]:top-auto! max-[760px]:right-2 \
   max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none \
   max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px]"
;;

(* What the menu is about, above its items. *)
let head_cls =
  "context-menu-head -mx-1 mb-1 flex flex-col gap-px border-b border-border \
   px-2.5 pt-1 pb-1.5 [&_.session-place]:flex-none"
;;

let title_cls =
  "context-menu-title flex items-center gap-1.5 truncate text-[13px] \
   font-medium text-foreground"
;;

let sub_cls =
  "context-menu-sub truncate font-mono text-xs text-muted-foreground"
;;

let heading (m : App.Model.t) (menu : Context_menu.t) =
  match
    ( menu.target
    , List.find m.sessions ~f:(fun s ->
        [%equal: string option] (Some s.path) (Context_menu.session menu)) )
  with
  | Review_files paths, _ ->
    div
      ~cls:head_cls
      [ span
          ~cls:title_cls
          (match paths with
           | [ path ] -> Filename.basename path
           | _ -> sprintf "%d selected files" (List.length paths))
      ; span
          ~cls:sub_cls
          (match paths with
           | [ path ] -> Filename.dirname path
           | _ -> String.concat ~sep:", " (List.map paths ~f:Filename.basename))
      ]
  | Message n, _ ->
    (match App.Model.message_menu m n with
     | None -> Node.none
     | Some msg ->
       div
         ~cls:head_cls
         [ span ~cls:title_cls (Context_menu.Message.title msg)
         ; (match
              String.split_lines msg.text
              |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
            with
            | Some line ->
              span ~cls:(sub_cls ^ " preview font-sans") (String.strip line)
            | None -> Node.none)
         ])
  | Archive, _ ->
    let count = List.count m.sessions ~f:(fun s -> s.archived) in
    div
      ~cls:head_cls
      [ span ~cls:title_cls "Archived sessions"
      ; span
          ~cls:sub_cls
          (match count with
           | 0 -> "none"
           | 1 -> "1 session"
           | n -> sprintf "%d sessions" n)
      ]
  | _, None -> Node.none
  | (Session _ | Worktree _), Some s ->
    (match menu.target with
     | Session _ | Message _ | Review_files _ | Archive ->
       div
         ~cls:head_cls
         [ span ~cls:title_cls (Session_list.title s)
         ; Sidebar_view.place
             ~cls:sub_cls
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
         ~cls:head_cls
         [ Node.span
             ~attrs:[ Attr.class_ title_cls ]
             [ icon ~cls:"place-icon size-3.5 text-primary" Branch
             ; Node.text label
             ]
         ; span ~cls:sub_cls ("in " ^ Session_list.title s)
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
    Menu_view.item
      ~selected
      ~danger
      ~disabled
      ?icon:(Option.map kind ~f:item_icon)
      ?hint:(if disabled then Some item.detail else None)
      ?shortcut:
        (if String.is_empty item.detail || disabled
         then None
         else Some item.detail)
      ~attrs:
        [ Attr.create "data-item" item.id
        ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
        ; Attr.on_mouseenter (fun _ ->
            if selected
            then Effect.Ignore
            else inject (Action.Picker_move (i - Picker.selected picker)))
        ]
      item.label
  in
  (* Separated: going there, changing it, looking at it, copying, and the
     destructive ones. *)
  let group : Context_menu.Item.t -> int = function
    | Switch_back | Open | Open_window | Switch_here | New_session_here -> 0
    | Rename | Fork | New_worktree | Create_pr | Trim_before | Trim_after -> 1
    | Review | Files | Terminal -> 2
    | Copy_path | Copy_id | Copy_message | Select_text -> 3
    | Pin | Unpin | Archive | Unarchive | Delete | Remove -> 4
    | Approve_files | Unapprove_files -> 0
    | Stage_files | Unstage_files | Commit_files -> 1
    | Select_files | Unselect_files -> 2
    | Copy_paths -> 3
    | Show_archived | Hide_archived -> 0
    | Delete_archived -> 4
  in
  let sections =
    List.mapi items ~f:(fun i item -> i, item)
    |> List.group ~break:(fun (_, a) (_, b) ->
      let g (item : Picker.Item.t) =
        Option.value_map (Context_menu.Item.of_id item.id) ~default:5 ~f:group
      in
      g a <> g b)
  in
  Menu_view.backdrop
    ~cls:"context-menu-backdrop"
    ~on_close:close
    [ Menu_view.surface
        ~cls:surface_cls
        ~label:(Picker.title picker)
        ~attrs:
          [ Attr.id "dialog"
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
           Menu_view.section (List.map section ~f:(fun (i, item) -> row i item)))
        )
    ]
;;
