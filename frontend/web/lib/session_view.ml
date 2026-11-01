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

let panel_toggles (m : App.Model.t) ~inject =
  (* A panel opened from a phone's menu: the menu gets out of its way (the
     filter's menu hangs from its button, so that one keeps it). *)
  let opening a =
    if m.narrow && m.status_tray
    then Effect.Many [ inject Action.Toggle_status_tray; inject a ]
    else inject a
  in
  div
    ~cls:"panel-toggles"
    [ Filter_view.button m ~inject
    ; Files_view.toggle m ~inject:opening
    ; Terminal_view.toggle m ~inject:opening
    ; Agents_view.toggle m ~inject:opening
    ]
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
  let a = m.agents in
  let running = Agents.running_agents a + Agents.running_jobs a in
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
    ]
    @ (if (not a.open_) && List.is_empty a.agents && List.is_empty a.jobs
       then []
       else
         [ sheet_row
           ~cls:"agents-row"
           ~name:"Subagents and jobs"
           ?detail:
             (if running > 0 then Some (sprintf "%d running" running) else None)
           ?badge:(if running > 0 then Some (Int.to_string running) else None)
           ~on:a.open_
           ~glyph:Bot
           ~on_click:(go Action.Toggle_subagents)
           ()
         ])
    @ [ sheet_row
          ~cls:"settings-row"
          ~name:"Settings"
          ~glyph:Gear
          ~on_click:(go Action.Open_settings)
          ()
      ])
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

(* Over the top of the chat, with no bar of its own: the sidebar's toggle
   (when it is not docked) at the left, and what applies and the panels'
   toggles at the right (on a phone, a strip with the session's name and a
   sheet). *)
let chrome (m : App.Model.t) ~inject =
  let docked = App.Model.docked m in
  let toggle =
    if docked
    then Node.none
    else
      button
        ~cls:"icon ghost round chrome-button toggle-sidebar"
        ~title:"Sessions (Ctrl+B)"
        ~label:"Sessions"
        ~on_click:(inject Action.Toggle_sidebar)
        [ icon Menu ]
  in
  (* A phone's strip has the session's name between its buttons. *)
  let title =
    match m.state with
    | Some state when m.narrow ->
      Node.h1
        ~attrs:[ Attr.class_ "chrome-title" ]
        [ Node.text (session_title m state) ]
    | _ -> Node.none
  in
  let tools =
    if m.narrow
    then more m ~inject
    else
      div
        ~cls:"chrome-tools"
        [ Status_view.notes m ~inject
        ; panel_toggles m ~inject
        ; (if docked then Node.none else Sidebar_view.settings ~inject)
        ]
  in
  div
    ~cls:"chrome-anchor"
    [ Node.div
        ~attrs:
          [ classes
              [ "chat-chrome" ]
              [ "docked", docked; "strip", m.narrow ]
          ]
        [ div ~cls:"chrome-left" [ toggle ]
        ; div ~cls:"chrome-center" [ title ]
        ; div ~cls:"chrome-right" [ tools ]
        ]
    ]
;;
