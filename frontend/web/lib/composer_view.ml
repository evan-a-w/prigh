open! Core
open! Import
open Html
module Action = App.Action

let popup (m : App.Model.t) ~inject =
  match App.Model.popup m with
  | None -> Node.none
  | Some c ->
    let title =
      match Completion.source c with
      | Command -> "Commands"
      | Argument Model -> "Models"
      | Argument Models -> "Fallback models, in order"
      | Argument Thinking -> "Thinking"
      | Argument (Login | Logout) -> "Providers"
      | Argument Directory -> "Directories"
      | Argument Backend_directory -> "Directories on the backend"
      | Argument (Path | File) | Path -> "Files"
      | Argument Verbosity -> "Verbosity"
      | Argument Adapter -> "Switch this session to"
      | Argument Confirm -> "Mode, also for new sessions"
      | Argument Mode -> "Modes"
      | Argument Plan -> "Plan mode"
      | Argument Chat -> "Chat mode"
      | Argument Session -> "Sessions"
      | Argument Host -> "Hosts"
      | Argument User -> "Users"
      | Argument Skill -> "Skills"
      | Argument Snippet | Snippet -> "Snippets"
      | Argument Budget -> "Budget"
      | Argument Goal -> "Goal"
      | Argument Remind -> "Reminders"
      | Argument Compaction -> "Compaction"
      | Argument Generate_model -> "Drafting model"
      | Argument Approval_model -> "Approval model"
      | Argument Naming -> "Naming"
      | Argument Worktree -> "Worktrees"
      | Argument Review -> "Review"
      | Argument Resume -> "Automatic resume"
      | Argument Notify -> "Notifications"
      | Argument Memory -> "Memory"
      | Argument Instructions -> "Custom instructions"
      | Argument Settings -> "Settings sections"
      | Argument Pr -> "Pull requests"
      | Argument Theme -> "Themes"
      | Argument Accent -> "Accent colours"
      | Argument Corners -> "Corners"
      | Argument (Font | Code_font) -> "Fonts"
      | Argument Trim -> "Trim"
      | Argument Filter -> "Show only"
      | Argument Delete -> "Delete"
    in
    div
      ~cls:"popup"
      [ div
          ~cls:"popup-title"
          [ Node.text title
          ; span ~cls:"popup-keys" "↑↓ Tab Enter Esc   Ctrl+Enter as typed"
          ]
      ; div
          ~cls:"popup-items"
          ~attrs:
            [ Attr.role "listbox"
            ; Attr.id "completion-list"
            ; Attr.create "aria-label" title
            ]
          (List.mapi
             (List.take (Completion.items c) 50)
             ~f:(fun i (item : Picker.Item.t) ->
               Node.div
                 ~attrs:
                   [ classes
                       [ "popup-item" ]
                       [ "selected", i = Completion.selected c
                       ; "dimmed", item.dimmed
                       ; "marked", item.marked
                       ]
                   ; Attr.role "option"
                   ; Attr.id (sprintf "completion-option-%d" i)
                   ; Attr.create
                       "aria-selected"
                       (Bool.to_string (i = Completion.selected c))
                   ; Attr.on_mousedown (fun ev ->
                       Js_of_ocaml.Dom.preventDefault ev;
                       inject (Action.Complete_choose i))
                   ]
                 [ span ~cls:"popup-label" item.label
                 ; (if item.marked
                    then Html.mark ~cls:"picked" ~label:"current" Check
                    else Node.none)
                 ; (if String.is_empty item.detail
                    then Node.none
                    else span ~cls:"popup-detail" item.detail)
                 ]))
      ]
;;

(* Each opens full size in the drawing dialog (a lightbox inside the
   composer would be held to it: its container query contains fixed
   positions), where saving a drawing replaces it; the cross takes it off
   the message. *)

(* Long pastes: a chip each, opening a dialog with the text. Never their
   whole text in the page here. *)
let pasted (m : App.Model.t) ~inject =
  match m.pasted with
  | [] -> Node.none
  | pasted ->
    div
      ~cls:"attach-chips pasted-chips"
      ~attrs:[ Attr.create "aria-label" "Pasted texts" ]
      (List.mapi pasted ~f:(fun i text ->
         let name = Prigh_ui.Pasted_text.name (i + 1) in
         let summary =
           Prigh_ui.Pasted_text.summary
             ~lines:(Prigh_ui.Pasted_text.count_lines text)
             ~bytes:(String.length text)
         in
         Node.div
           ~attrs:[ Attr.class_ "attach-chip pasted-chip" ]
           [ button
               ~cls:"ghost pasted-chip-open"
               ~title:
                 (sprintf
                    "%s: %s. Open it to read it, or to insert it as text"
                    name
                    (Prigh_ui.Pasted_text.preview text))
               ~on_click:(inject (Action.View_pasted i))
               [ span ~cls:"attach-chip-name" name
               ; span ~cls:"attach-chip-detail" summary
               ]
           ; button
               ~cls:"icon ghost attach-chip-remove"
               ~title:(sprintf "Remove %s" name)
               ~label:(sprintf "Remove %s" name)
               ~on_click:(inject (Action.Remove_pasted i))
               [ icon Close ]
           ]))
;;

let images (m : App.Model.t) ~inject =
  match m.images with
  | [] -> Node.none
  | list ->
    let verdicts = App.Model.image_verdicts m in
    div
      ~cls:"pending-images"
      (List.mapi (List.zip_exn list verdicts) ~f:(fun i (image, verdict) ->
         let downsizing = List.mem m.downsizing image ~equal:phys_equal in
         let too_large =
           match verdict with
           | Fits -> None
           | Too_large why -> Some why
         in
         Node.div
           ~attrs:
             [ classes
                 [ "pending-image" ]
                 [ "too-large", Option.is_some too_large
                 ; "downsizing", downsizing
                 ]
             ]
           ([ Node.button
                ~attrs:
                  [ Attr.type_ "button"
                  ; Attr.class_ "pending-thumb"
                  ; Attr.title "View it, or draw on it"
                  ; Attr.create
                      "aria-label"
                      ("View or draw on " ^ Image.to_string_hum image)
                  ; Attr.on_click (fun _ ->
                      inject (Action.Annotate { image; target = Pending i }))
                  ]
                [ Node.img
                    ~attrs:
                      [ Attr.class_ "thumb"
                      ; Attr.src (Image_view.src image)
                      ; Attr.alt (Image.to_string_hum image)
                      ]
                    ()
                ; icon ~cls:"draw-hint" Pencil
                ]
            ; button
                ~cls:"remove-image"
                ~title:"Remove image"
                ~label:"Remove image"
                ~on_click:(inject (Action.Remove_image i))
                [ icon Close ]
            ]
            @
            match too_large with
            | None -> []
            | Some why ->
              [ Node.button
                  ~attrs:
                    [ Attr.type_ "button"
                    ; Attr.class_ "downsize-image"
                    ; Attr.title
                        (sprintf
                           "Too large for the model (%s). Downsize it here, or \
                            send it as it is and prigh tries to send it \
                            smaller"
                           why)
                    ; Attr.create
                        "aria-label"
                        (sprintf
                           "Downsize the image: too large for the model, %s"
                           why)
                    ; (if downsizing then Attr.disabled else Attr.empty)
                    ; Attr.on_click (fun _ -> inject (Action.Downsize_image i))
                    ]
                  [ (if downsizing
                     then Html.lamp ()
                     else icon ~cls:"downsize-icon" Warning)
                  ; Node.span
                      ~attrs:[ Attr.class_ "downsize-text" ]
                      [ span ~cls:"downsize-why" "Too large"
                      ; span
                          ~cls:"downsize-label"
                          (if downsizing then "Downsizing" else "Downsize")
                      ]
                  ]
              ])))
;;

let pending_chip = Session_chips_view.pending_chip
let menu_attrs = Session_chips_view.menu_attrs

(* The provider's mark and the model's short name ("6.2 Sol"); the tooltip
   and the menu have the reasoning level. *)
let model_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.model with
  | Some key ->
    let name =
      List.find m.models ~f:(fun model -> String.equal model.key key)
      |> Option.value_map ~default:key ~f:(fun model -> model.name)
    in
    pending_chip ~cls:"model-chip" (Model_menu.short_name name)
  | None ->
    let thinking = Option.value m.pending.thinking ~default:state.thinking in
    let level = Reasoning.chip_label state.model ~current:thinking in
    (* model@login when the provider has several logins; the title always
       names it. *)
    let login = Prigh_ui.Provider_logins.beside_model m.auth state in
    let at login = Option.value_map login ~default:"" ~f:(fun l -> "@" ^ l) in
    button
      ~cls:"chip-button model-chip"
      ~title:
        (sprintf
           "%s%s%s: models, reasoning and access (Ctrl+L)"
           (Session_engine.model_label state.engine ~model:state.model.name)
           (at (Prigh_ui.Provider_logins.login_name m.auth state))
           (Option.value_map level ~default:"" ~f:(fun l -> ", reasoning " ^ l)))
      ~label:
        (sprintf
           "Model: %s%s%s"
           (Session_engine.model_label state.engine ~model:state.model.name)
           (at login)
           (Option.value_map level ~default:"" ~f:(fun l -> ", " ^ l)))
      ~attrs:(menu_attrs m Model)
      ~on_click:(inject (Action.Open_chip Model))
      ([ Provider_mark.view
           (Provider_mark.of_model
              ~provider:state.model.provider
              ~name:state.model.name)
       ; (if Session_engine.is_harness state.engine
          then span ~cls:"chip-label" (Model_menu.short_name state.model.name)
          else
            (* On the narrowest phones the model keeps the room: the engine
               is in the chip's title and label. *)
            Node.span
              ~attrs:[ Attr.class_ "chip-label" ]
              [ Node.span
                  ~attrs:[ Attr.class_ "chip-engine max-[359px]:hidden" ]
                  [ Node.text (Session_engine.who state.engine ^ " · ") ]
              ; Node.text (Model_menu.short_name state.model.name)
              ])
       ]
       @ Option.value_map login ~default:[] ~f:(fun l ->
         [ span
             ~cls:"chip-login max-w-[9ch] truncate text-muted-foreground"
             ("@" ^ l)
         ]))
;;

(* Where tools run, always named, also while that host is disconnected (its
   server struck through, "offline" beside it); no host at all is lit. A
   chat uses none. *)
let host_chip (m : App.Model.t) (state : State.t) ~inject =
  let cls, title, aria, content =
    match Active_host.of_state state with
    | _ when state.chat_mode ->
      ( " chatting"
      , sprintf
          "Chat mode uses no tool host (%s to work on one)"
          Session_mode.leave_chat
      , Some "No tool host (chat mode)"
      , [ icon Message; span ~cls:"chip-label" "no host" ] )
    | Connected h ->
      ( ""
      , sprintf "Tools run on %s, in %s (/host switches)" h.name h.cwd
      , Some h.name
      , [ icon Server; span ~cls:"chip-label" h.name ] )
    | Disconnected { name; _ } ->
      ( " offline"
      , Active_host.reconnect_hint ~name
      , Some (sprintf "%s, disconnected" name)
      , [ icon Server_off
        ; span ~cls:"chip-label" name
        ; span ~cls:"chip-state" "offline"
        ] )
    | Unset when String.equal state.active_host Host.backend_id ->
      ( ""
      , "Tools run on the backend (/host switches)"
      , Some "backend"
      , [ icon Server; span ~cls:"chip-label" "backend" ] )
    | Unset ->
      ( " warm"
      , "No host runs the tools: /host picks one"
      , Some "No tool host"
      , [ icon Server; span ~cls:"chip-label" "no host" ] )
  in
  button
    ~cls:("chip-button host-chip" ^ cls)
    ~title
    ?label:aria
    ~attrs:(menu_attrs m Host)
    ~on_click:(inject (Action.Open_chip Host))
    (content @ [ icon ~cls:"chevron" Chevron ])
;;

(* Where tools run and the model, at the right of the row under the text,
   each opening its menu over itself; the model's has the reasoning level
   and the mode too. *)
let chips (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    Node.div
      ~attrs:
        [ classes
            [ "composer-controls" ]
            [ "menu-open", Session_chips_view.menu_open m [ Model; Host ] ]
        ; Attr.role "group"
        ; Attr.create "aria-label" "Host and model"
        ]
      [ Session_chips_view.anchor m Host (host_chip m state ~inject) ~inject
      ; Session_chips_view.anchor m Model (model_chip m state ~inject) ~inject
      ]
;;

(* The paperclip: a menu of what can go with the message. *)
let plus (m : App.Model.t) ~inject =
  Node.div
    ~attrs:
      [ classes
          [ "composer-plus" ]
          [ "menu-open", Session_chips_view.menu_open m [ Attach ] ]
      ]
    [ Session_chips_view.anchor
        m
        Attach
        (button
           ~cls:"icon ghost round plus attach"
           ~title:"Add files or images (or paste, or drop them here)"
           ~label:"Add files or images"
           ~attrs:
             [ Attr.create "aria-haspopup" "menu"
             ; Attr.create
                 "aria-expanded"
                 (Bool.to_string (Session_chips_view.menu_open m [ Attach ]))
             ]
           ~on_click:(inject (Action.Open_chip Attach))
           [ icon Paperclip ])
        ~inject
    ]
;;

(* The subagent whose chat is on show: the composer writes to it. *)
let subagent (m : App.Model.t) =
  Option.bind m.subagent_chat ~f:(Agents.find_agent m.agents)
;;

let subagent_running (a : Agents.Agent.t) =
  match a.status with
  | Running -> true
  | Complete | Failed -> false
;;

let connected (m : App.Model.t) =
  match m.connection, m.state with
  | Connected, Some _ -> Option.is_none m.session_gone
  | Reconnecting _, _ | _, None -> false
;;

(* One line at any width (CSS cuts it with an ellipsis, never wraps it). *)
let planning (m : App.Model.t) =
  Option.value_map m.state ~default:false ~f:(fun (s : State.t) ->
    s.plan_mode && not s.chat_mode)
;;

let chatting (m : App.Model.t) =
  Option.value_map m.state ~default:false ~f:(fun (s : State.t) -> s.chat_mode)
;;

let placeholder (m : App.Model.t) =
  (* A phone, or the chat beside a side panel. *)
  let cramped = m.narrow || m.files.open_ in
  let text s = s in
  if Option.is_some m.session_gone
  then text "Session gone: /new starts one"
  else if Option.is_some (Engine_banner.reason m)
  then
    text
      (if cramped
       then "Can't run here"
       else "This session can't run here: see above")
  else if not (connected m)
  then
    text
      (if cramped
       then "Offline: send once back"
       else "Offline: write now, send once it is back")
  else (
    match subagent m with
    | Some a when Option.is_some (Recipient.refusal m.agents a.id) ->
      text "A subagent's own subagent: write to its parent"
    | Some a when subagent_running a && cramped ->
      text (sprintf "Steer %s…" (Agents.Agent.title a))
    | Some a when subagent_running a ->
      text
        (sprintf
           "Steer %s (Enter) or queue a follow-up (Alt+Enter)…"
           (Agents.Agent.title a))
    | Some a -> text (sprintf "Message %s…" (Agents.Agent.title a))
    | None when App.Model.running m && cramped -> text "Steer the agent…"
    | None when App.Model.running m ->
      text "Steer (Enter) or queue a follow-up (Alt+Enter)…"
    | None when chatting m && cramped -> text "Chat: no files or commands…"
    | None when chatting m ->
      text "Chat, without a tool host: no files or commands. Ask anything…"
    | None when planning m && cramped ->
      text "Plan mode: /plan off to implement"
    | None when planning m ->
      text
        (sprintf
           "Plan mode: discuss the plan, or leave it to implement (%s)…"
           Session_mode.leave)
    | None ->
      if cramped
      then text "Ask prigh…  / commands"
      else text "Ask prigh anything…  / commands, @ files")
;;

let view (m : App.Model.t) ~inject =
  let sub = subagent m in
  let running =
    match sub with
    | Some a -> subagent_running a
    | None -> App.Model.running m
  in
  let stop =
    match sub with
    | Some a -> Action.Cancel_subagent a.id
    | None -> Action.Abort
  in
  let stop_title =
    if Option.is_some sub then "Stop the subagent" else "Stop (Esc)"
  in
  let has_input =
    not
      (String.is_empty (String.strip m.draft)
       && List.is_empty m.images
       && List.is_empty m.pasted
       && List.is_empty m.files.attachments)
  in
  let sending = m.pending.sending > 0 in
  (* Running with nothing to steer with, the disc stops the agent (or the
     subagent whose chat this is). *)
  let stops = running && not has_input in
  let connected = connected m in
  let placeholder = placeholder m in
  (* A chip's menu leaves only the chips live. *)
  let rest =
    match m.dialog with
    | Some (Popover _) -> inert
    | _ -> Fn.id
  in
  Node.footer
    ~attrs:
      [ classes
          [ "composer" ]
          [ "offline", not connected
          ; "sending", sending
          ; "busy", Status_view.busy m
          ]
      ]
    [ Computer_banner.view m ~inject
    ; Engine_banner.view m ~inject
    ; No_host_banner.view m ~inject
    ; rest (Queue_view.view m ~inject)
    ; Node.div
        ~attrs:
          [ classes
              [ "composer-stack" ]
              [ "completing", Option.is_some (App.Model.popup m) ]
          ; Attr.create "data-popover-box" ""
          ]
        [ rest (popup m ~inject)
        ; Node.div
            ~attrs:
              [ Attr.class_ "composer-box"
              ; Attr.create "data-slot" "input-group"
              ; Attr.role "group"
              ]
            [ rest (images m ~inject)
            ; rest (pasted m ~inject)
            ; rest (Files_view.chips m ~inject)
            ; rest (Code_editor_view.chip m ~inject)
            ; rest
              @@ div
                   ~cls:"editor-wrap"
                   [ Node.textarea
                       ~attrs:
                         [ Attr.class_ "editor"
                         ; Attr.id "editor"
                         ; Attr.create "data-slot" "input-group-control"
                         ; Attr.placeholder placeholder
                         ; Attr.create "aria-label" "Message"
                         ; Attr.create "dir" "auto"
                         ; (match App.Model.popup m with
                            | None -> Attr.empty
                            | Some c ->
                              Attr.many
                                [ Attr.role "combobox"
                                ; Attr.create "aria-controls" "completion-list"
                                ; Attr.create "aria-expanded" "true"
                                ; Attr.create "aria-autocomplete" "list"
                                ; Attr.create
                                    "aria-activedescendant"
                                    (sprintf
                                       "completion-option-%d"
                                       (Completion.selected c))
                                ])
                         ; Attr.value m.draft
                         ; Attr.rows 1
                         ; Attr.create "autocomplete" "off"
                         ; Attr.create "enterkeyhint" "send"
                         ; Attr.on_input (fun ev text ->
                             let cursor =
                               Option.value
                                 (caret ev)
                                 ~default:(String.length text)
                             in
                             inject (Action.Edit { text; cursor }))
                         ]
                       []
                   ]
            ; Node.div
                ~attrs:
                  [ Attr.class_ "composer-row"
                  ; Attr.create "data-slot" "input-group-addon"
                  ; Attr.create "data-align" "block-end"
                  ]
                [ plus m ~inject
                ; rest (Context_ring_view.view m ~inject)
                ; (* Stop while there is something to steer with: at the
                     left, so that nothing at the right moves as a turn
                     starts and ends. *)
                  (if running && not stops
                   then
                     rest
                       (button
                          ~cls:"icon ghost round stop"
                          ~title:stop_title
                          ~label:"Stop"
                          ~on_click:(inject stop)
                          [ icon Stop ])
                   else Node.none)
                ; chips m ~inject
                ; rest
                  @@ div
                       ~cls:"composer-buttons"
                       [ (if sending
                          then
                            button
                              ~cls:"primary round send busy"
                              ~title:"Sending…"
                              ~label:"Sending"
                              ~disabled:true
                              ~attrs:[ Attr.create "aria-busy" "true" ]
                              ~on_click:Effect.Ignore
                              [ Html.lamp () ]
                          else if stops
                          then
                            (* Nothing to steer with: the one button stops. *)
                            button
                              ~cls:"primary round stop"
                              ~title:stop_title
                              ~label:"Stop"
                              ~on_click:(inject stop)
                              [ icon Stop ]
                          else
                            button
                              ~cls:"primary round send"
                              ~title:
                                (if Option.is_some (Engine_banner.reason m)
                                 then "This session can't run here: see above"
                                 else if not connected
                                 then "Not connected to the backend"
                                 else if running
                                 then "Steer (Enter)"
                                 else "Send (Enter)")
                              ~label:(if running then "Steer" else "Send")
                              ~disabled:
                                ((not has_input)
                                 || (not connected)
                                 || Option.is_some (Engine_banner.reason m))
                              ~on_click:(inject Action.Send)
                              [ icon Send ])
                       ]
                ]
            ]
        ]
    ; (if App.fresh m
       then rest (Temporary_view.tray m ~inject)
       else
         Option.value_map
           (Composer_where.of_model m)
           ~default:Node.none
           ~f:(fun where -> rest (Composer_where.view where ~inject)))
    ]
;;
