open! Core
open! Import
open Html
module Action = App.Action

let hero (m : App.Model.t) =
  let temporary = App.temporary_shown m in
  Node.div
    ~attrs:[ classes [ "hero" ] [ "temporary", temporary ] ]
    [ Node.h2
        [ Node.text
            (if temporary then "Temporary session" else "What are we building?")
        ; Node.span
            ~attrs:
              [ Attr.class_ "hero-caret"; Attr.create "aria-hidden" "true" ]
            []
        ]
    ; (if temporary
       then
         Node.p
           ~attrs:[ Attr.class_ "hero-sub" ]
           [ Node.text "Not saved and not listed. Deleted when you leave." ]
       else Node.none)
    ]
;;

let chip (m : App.Model.t) ~inject =
  let on = App.temporary_shown m in
  Node.button
    ~attrs:
      [ classes [ "btn"; "temporary-chip" ] [ "on", on ]
      ; Attr.type_ "button"
      ; Attr.role "switch"
      ; Attr.create "aria-checked" (Bool.to_string on)
      ; Attr.title
          (if on
           then "Temporary: not saved, deleted when you leave (Ctrl+Shift+O)"
           else "Make it temporary: not saved or listed (Ctrl+Shift+O)")
      ; Attr.on_click (fun _ -> inject (Action.Set_temporary (not on)))
      ]
    [ icon Ghost; Node.text "Temporary" ]
;;

let tray (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    let folder =
      Sidebar_view.home_relative
        ?host_home:(Sidebar_view.state_host_home m)
        state.cwd
    in
    div
      ~cls:"tray"
      [ button
          ~cls:"ghost tray-where"
          ~title:"Start in another folder (/cd)"
          ~label:(sprintf "Folder: %s. Change it" folder)
          ~on_click:(inject (Action.Run "/cd"))
          [ icon Folder
          ; span ~cls:"tray-folder" folder
          ; span ~cls:"tray-folder-short" (Filename.basename folder)
          ]
      ; (match state.git_branch with
         | Some branch ->
           button
             ~cls:"ghost tray-where"
             ~title:"Work in another worktree (/worktree)"
             ~label:(sprintf "Branch: %s. Change it" branch)
             ~on_click:(inject (Action.Run "/worktree"))
             [ icon Branch; span ~cls:"tray-branch" branch; icon Chevron ]
         | None -> Node.none)
      ; chip m ~inject
      ]
;;

let pill (m : App.Model.t) ~inject =
  Node.button
    ~attrs:
      [ classes [ "btn"; "temporary-pill" ] [ "open", Option.is_some m.dialog ]
      ; Attr.type_ "button"
      ; Attr.create "aria-haspopup" "dialog"
      ; Attr.title "Not saved: deleted when you leave (/temporary)"
      ; Attr.on_click (fun _ -> inject Action.Temporary_menu)
      ]
    [ icon Ghost
    ; span
        ~cls:"pill-label"
        (if m.narrow then "Temporary session" else "Temporary")
    ; icon Chevron
    ]
;;

let menu ~inject =
  Modal.view
    ~cls:"temporary-menu"
    ~title:"Temporary session"
    ~on_close:(inject Action.Close_dialog)
    [ Node.p
        [ Node.text
            "This session isn't saved. It's deleted, with its subagents, when \
             you leave it."
        ]
    ; Node.div
        ~attrs:[ Attr.class_ "temporary-actions" ]
        [ button
            ~cls:"temporary-keep"
            ~on_click:(inject Action.Keep_session)
            [ icon Keep; Node.text "Keep this session" ]
        ; button
            ~cls:"temporary-delete"
            ~on_click:(inject Action.Delete_temporary)
            [ icon Trash; Node.text "Delete it now" ]
        ]
    ]
;;

let leave (m : App.Model.t) ~inject =
  let working =
    match m.state with
    | None -> []
    | Some s ->
      let count what n =
        match n with
        | 0 -> None
        | 1 -> Some (sprintf "a %s" what)
        | n -> Some (sprintf "%d %ss" n what)
      in
      List.filter_opt
        [ Option.some_if s.running "its turn"
        ; count "subagent" (List.count s.subagents ~f:(fun a -> a.running))
        ; count "job" (List.count s.jobs ~f:(fun j -> j.running))
        ]
  in
  let them =
    match working with
    | [] | [ "its turn" ] -> "stops it"
    | _ -> "stops them"
  in
  Modal.view
    ~cls:"leave-temporary"
    ~title:"Delete this temporary session?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button
          ~cls:"ghost leave-keep"
          ~on_click:(inject Action.Leave_keep)
          [ Node.text "Keep it instead" ]
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Stay" ]
      ; button
          ~cls:"danger"
          ~on_click:(inject Action.Leave_delete)
          [ Node.text "Delete and leave" ]
      ]
    [ Node.p
        [ Node.textf
            "It's still working%s. Leaving deletes the transcript and %s."
            (match working with
             | [] -> ""
             | working -> sprintf " (%s)" (String.concat ~sep:", " working))
            them
        ]
    ]
;;
