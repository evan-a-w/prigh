open! Core
open! Import
open Html
module Action = App.Action

let session_title (m : App.Model.t) (state : State.t) =
  match state.session_name, state.session_description with
  | Some name, _ when not (String.is_empty (String.strip name)) -> name
  | _, Some description -> description
  | _ ->
    (match
       List.find m.sessions ~f:(fun s -> String.equal s.id state.session_id)
     with
     | Some s when Option.is_some s.description || Option.is_some s.first_prompt
       -> Session_list.title s
     | _ ->
       (* Before the session list knows it: the first prompt, at once. *)
       List.find_map (Chat.entries m.chat) ~f:(function
         | User u ->
           (match String.strip (Injected_note.typed u) with
            | "" -> None
            | text -> Some (List.hd_exn (String.split_lines text)))
         | _ -> None)
       |> Option.value ~default:"New session")
;;

(* On a phone: one button for the rest, which opens a sheet from the bottom
   with a labelled row each: what applies, the jump list, the filter, the
   panels and the settings (no row of bare icons to guess at). *)
let sheet_row ?(on = false) ?id ?detail ?badge ~cls ~name ~glyph ~on_click () =
  Node.button
    ~attrs:
      [ classes ("sheet-row" :: String.split cls ~on:' ') [ "on", on ]
      ; Option.value_map id ~default:Attr.empty ~f:Attr.id
      ; Attr.type_ "button"
      ; (if on then Attr.create "aria-pressed" "true" else Attr.empty)
      ; Attr.on_click (fun _ -> on_click)
      ]
    [ icon glyph
    ; Node.span
        ~attrs:[ Attr.class_ "sheet-row-text" ]
        [ span ~cls:"sheet-row-name" name
        ; (match detail with
           | Some d -> span ~cls:"sheet-row-detail" d
           | None -> Node.none)
        ]
    ; (match badge with
       | Some b -> span ~cls:"badge" b
       | None -> Node.none)
    ]
;;

let sheet (m : App.Model.t) ~inject =
  let go a = Effect.Many [ inject Action.Toggle_status_tray; inject a ] in
  let hidden = List.length (Transcript_filter.hidden m.transcript_filter) in
  Node.div
    ~attrs:
      [ Attr.class_ "chrome-menu menu-surface sheet"
      ; Attr.id "chrome-menu"
      ; Attr.role "group"
      ; Attr.create "aria-label" "Panels and settings"
      ]
    ([ div ~cls:"sheet-grip" []
     ; Chip_popover_view.session_sections m ~inject
     ; Status_view.notes m ~inject
     ; sheet_row
         ~cls:"jump-row"
         ~name:"Jump to a message"
         ~glyph:Toc
         ~on_click:(go Action.Open_jump_list)
         ()
     ; sheet_row
         ~cls:"filter-row"
         ~id:Filter_menu.anchor
         ~name:"Filter the transcript"
         ?detail:
           (if hidden > 0
            then Some ("Hiding " ^ Filter_view.hidden_names m.transcript_filter)
            else None)
         ~on:(hidden > 0)
         ~glyph:Filter
         ~on_click:(go Action.Open_filter)
         ()
     ; sheet_row
         ~cls:"files-sheet-row files-toggle"
         ~name:"Files"
         ~on:m.files.open_
         ~glyph:Folder
         ~on_click:(go (Action.Files Toggle))
         ()
     ; sheet_row
         ~cls:"terminal-row terminal-toggle"
         ~name:"Terminal"
         ~on:m.terminal.open_
         ~glyph:Terminal
         ~on_click:(go Action.Toggle_terminal)
         ()
     ; sheet_row
         ~cls:"review-row review-toggle"
         ~name:"Review changes"
         ~on:m.review.open_
         ~glyph:Review
         ~on_click:(go (Action.Review Toggle))
         ()
     ]
     @ (if Session_card_view.something m
        then
          [ sheet_row
              ~cls:"session-card-row session-card-toggle"
              ~name:"Subagents and worktrees"
              ~on:m.session_card.sheet
              ~glyph:Panel_right
              ~on_click:(go Action.Toggle_session_card)
              ()
          ]
        else [])
     @ [ sheet_row
           ~cls:"settings-row"
           ~name:"Settings"
           ~glyph:Gear
           ~on_click:(go Action.Open_settings)
           ()
       ]
     @
     if m.desktop
     then
       [ sheet_row
           ~cls:"quit-row"
           ~name:"Quit prigh"
           ~glyph:Logout
           ~on_click:(go Action.Quit_desktop)
           ()
       ]
     else [])
;;

let more (m : App.Model.t) ~inject =
  Node.div
    ~attrs:[ classes [ "chrome-more" ] [ "open", m.status_tray ] ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "btn icon ghost round chrome-button more-toggle"
          ; Attr.type_ "button"
          ; Attr.title "Panels and settings"
          ; Attr.create "aria-label" "Panels and settings"
          ; Attr.create "aria-controls" "chrome-menu"
          ; Attr.create "aria-expanded" (Bool.to_string m.status_tray)
          ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
          ]
        [ icon More ]
    ; (if m.status_tray
       then
         Node.fragment
           [ Node.div
               ~attrs:
                 [ Attr.class_ "chrome-backdrop"
                 ; Attr.on_click (fun _ -> inject Action.Toggle_status_tray)
                 ]
               []
           ; sheet m ~inject
           ]
       else Node.none)
    ]
;;

(* A toggle of the top bar, lit while what it shows is open: a panel says
   it is expanded, the card that it is pressed. *)
let toggle ~cls ~title ~label ~on ~on_click ?(panel = true) glyph =
  Loon.Button.view
    ~variant:Ghost
    ~size:Icon_sm
    ~class_:
      (cls
       ^
       if on
       then " open bg-accent text-foreground"
       else " text-muted-foreground hover:text-foreground")
    ~attrs:
      [ Attr.title title
      ; Attr.create "aria-label" label
      ; Attr.create
          (if panel then "aria-expanded" else "aria-pressed")
          (Bool.to_string on)
      ]
    ~on_click
    [ icon glyph ]
;;

(* The top bar's toggles: the transcript's filter, the files, the terminal,
   the review and the session card. *)
let toggles (m : App.Model.t) ~inject =
  div
    ~cls:"panel-toggles flex items-center gap-0.5"
    [ Filter_view.button m ~inject
    ; toggle
        ~cls:"files-toggle"
        ~title:"Files (Ctrl+Shift+F)"
        ~label:"Files"
        ~on:m.files.open_
        ~on_click:(inject (Action.Files Toggle))
        Folder
    ; toggle
        ~cls:"terminal-toggle"
        ~title:"Terminal (Ctrl+`)"
        ~label:"Terminal"
        ~on:m.terminal.open_
        ~on_click:(inject Action.Toggle_terminal)
        Panel_bottom
    ; toggle
        ~cls:"review-toggle"
        ~title:"Review changes (Alt+R)"
        ~label:"Review changes"
        ~on:m.review.open_
        ~on_click:(inject (Action.Review Toggle))
        Review
    ; (if Session_card_view.something m
       then
         toggle
           ~cls:"session-card-toggle"
           ~title:"Subagents and worktrees"
           ~label:"Subagents and worktrees"
           ~panel:false
           ~on:(if m.narrow then m.session_card.sheet else m.session_card_open)
           ~on_click:(inject Action.Toggle_session_card)
           Panel_right
       else Node.none)
    ]
;;

(* The subagent whose chat is on show, after its ancestors. *)
let lineage (m : App.Model.t) =
  Option.value_map m.subagent_chat ~default:[] ~f:(Agents.lineage m.agents)
;;

(* Where the session is and what it is: its project, then its name; in a
   subagent's chat, then the subagents from the top-level one down, each
   before the last a way back to its chat. *)
let breadcrumb (m : App.Model.t) ~inject (state : State.t) =
  let module B = Loon.Breadcrumb in
  let project =
    match
      List.find m.sessions ~f:(fun s -> String.equal s.id state.session_id)
    with
    | Some s ->
      Some
        (Session_project.name
           ?host_home:(Tilde.active_host_home state)
           s
           ~cwd:state.cwd)
    | None -> None
  in
  let separator =
    B.separator
      ~class_:"text-muted-foreground/60"
      ~children:[ Node.text "/" ]
      ()
  in
  let link ~cls ~title action text =
    Node.button
      ~attrs:
        [ Attr.type_ "button"
        ; Attr.class_
            (B.link_classes ~class_:(cls ^ " truncate cursor-pointer") ())
        ; Attr.title title
        ; Attr.on_click (fun _ -> inject action)
        ]
      [ Node.text text ]
  in
  let lineage = lineage m in
  let session =
    match lineage with
    | [] ->
      B.page
        ~class_:"chrome-title-text truncate"
        [ Node.text (session_title m state) ]
    | _ :: _ ->
      link
        ~cls:"chrome-title-text"
        ~title:"Back to the session's chat"
        Action.Close_subagent_chat
        (session_title m state)
  in
  let agents =
    List.concat_mapi lineage ~f:(fun i (a : Agents.Agent.t) ->
      let last = i = List.length lineage - 1 in
      [ separator
      ; B.item
          ~class_:(if last then "min-w-0" else "min-w-0 max-w-[12rem]")
          [ (if last
             then
               B.page
                 ~class_:"chrome-subagent truncate"
                 ~attrs:[ Attr.title (String.strip a.task) ]
                 [ Node.text (Agents.Agent.title a) ]
             else
               link
                 ~cls:"chrome-subagent"
                 ~title:(String.strip a.task)
                 (Action.Open_subagent_chat a.id)
                 (Agents.Agent.title a))
          ]
      ])
  in
  B.root
    ~class_:"session-crumbs min-w-0"
    [ B.list
        ~class_:"flex-nowrap text-sm"
        ((match project with
          | Some project ->
            [ B.item ~class_:"chrome-project shrink-0" [ Node.text project ]
            ; separator
            ]
          | None -> [])
         @ [ B.item
               ~class_:
                 (if List.is_empty lineage
                  then "min-w-0"
                  else "min-w-0 max-w-[14rem] shrink")
               [ session ]
           ]
         @ agents)
    ]
;;

(* The top of the chat: on a wide page a bar, the sidebar's toggle (when it
   is not docked) and where the session is at the left, and what applies and
   the panels' toggles at the right; on a phone a strip, ☰, the session's
   name and a ⋯ opening a sheet. *)
let chrome (m : App.Model.t) ~inject =
  let docked = App.Model.docked m in
  let temporary = App.temporary m && not (App.fresh m) in
  if m.narrow
  then (
    let shown = List.last (lineage m) in
    let title =
      match m.state, shown with
      | Some _, Some a ->
        Node.h1
          ~attrs:[ Attr.class_ "chrome-title chrome-subagent" ]
          [ Node.text (Agents.Agent.title a) ]
      | Some _, None when temporary -> Temporary_view.pill m ~inject
      | Some state, None ->
        Node.h1
          ~attrs:[ Attr.class_ "chrome-title" ]
          [ Node.text (session_title m state) ]
      | None, _ -> Node.none
    in
    div
      ~cls:"chrome-anchor"
      [ Node.div
          ~attrs:[ Attr.class_ "chat-chrome strip" ]
          [ div
              ~cls:"chrome-left"
              [ (match shown with
                 | Some _ ->
                   button
                     ~cls:"icon ghost round chrome-button subagent-back"
                     ~title:"Back to its parent's chat (Esc)"
                     ~label:"Back"
                     ~on_click:(inject Action.Subagent_back)
                     [ icon Back ]
                 | None when docked -> Node.none
                 | None ->
                   button
                     ~cls:"icon ghost round chrome-button toggle-sidebar"
                     ~title:"Sessions (Ctrl+B)"
                     ~label:"Sessions"
                     ~on_click:(inject Action.Toggle_sidebar)
                     [ icon Menu ])
              ]
          ; div ~cls:"chrome-center" [ title ]
          ; div ~cls:"chrome-right" [ more m ~inject ]
          ]
      ])
  else
    div
      ~cls:"chrome-anchor loon"
      [ Node.div
          ~attrs:
            [ classes
                [ "chat-chrome"
                ; "flex h-12 items-center gap-2 px-3 text-foreground"
                ]
                [ "docked", docked ]
            ]
          [ (if docked
             then Node.none
             else
               Loon.Button.view
                 ~variant:Ghost
                 ~size:Icon_sm
                 ~class_:
                   "chrome-button toggle-sidebar text-muted-foreground \
                    hover:text-foreground"
                 ~attrs:
                   [ Attr.title "Sessions (Ctrl+B)"
                   ; Attr.create "aria-label" "Sessions"
                   ]
                 ~on_click:(inject Action.Toggle_sidebar)
                 [ icon Sidebar ])
          ; div
              ~cls:"chrome-center flex min-w-0 items-center"
              [ (match m.state with
                 | Some _ when temporary -> Temporary_view.pill m ~inject
                 | Some state -> breadcrumb m ~inject state
                 | None -> Node.none)
              ]
          ; div
              ~cls:"chrome-tools ml-auto flex items-center gap-2"
              [ Status_view.notes m ~inject
              ; toggles m ~inject
              ; (if docked then Node.none else Sidebar_view.settings ~inject)
              ]
          ]
      ]
;;
