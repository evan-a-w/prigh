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
      | Argument Path | Path -> "Files"
      | Argument Verbosity -> "Verbosity"
      | Argument Confirm -> "Tool confirmation"
      | Argument Mode -> "Modes"
      | Argument Plan -> "Plan mode"
      | Argument Session -> "Sessions"
      | Argument Host -> "Hosts"
      | Argument User -> "Users"
      | Argument Skill -> "Skills"
      | Argument Snippet | Snippet -> "Snippets"
      | Argument Budget -> "Budget"
      | Argument Goal -> "Goal"
      | Argument Remind -> "Reminders"
      | Argument Compaction -> "Compaction"
      | Argument Naming -> "Naming"
      | Argument Dictation -> "Dictation"
      | Argument Worktree -> "Worktrees"
      | Argument Review -> "Review"
      | Argument Resume -> "Automatic resume"
      | Argument Notify -> "Notifications"
      | Argument Tabs -> "Session tabs"
      | Argument Settings -> "Settings sections"
      | Argument Pr -> "Pull requests"
      | Argument Theme -> "Themes"
      | Argument Accent -> "Accent colours"
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
let images (m : App.Model.t) ~inject =
  match m.images with
  | [] -> Node.none
  | list ->
    div
      ~cls:"pending-images"
      (List.mapi list ~f:(fun i image ->
         div
           ~cls:"pending-image"
           [ Node.button
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
           ]))
;;

let pending_chip = Session_chips_view.pending_chip
let menu_attrs = Session_chips_view.menu_attrs

let model_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.model with
  | Some key ->
    let name =
      List.find m.models ~f:(fun model -> String.equal model.key key)
      |> Option.value_map ~default:key ~f:(fun model -> model.name)
    in
    pending_chip ~cls:"model-chip" name
  | None ->
    button
      ~cls:"chip-button model-chip"
      ~title:(sprintf "%s: switch model (Ctrl+L)" state.model.name)
      ~attrs:(menu_attrs m Model)
      ~on_click:(inject (Action.Open_chip Model))
      [ span ~cls:"chip-label" state.model.name; icon ~cls:"chevron" Chevron ]
;;

(* Where tools run, always named, also while that host is disconnected (its
   server struck through, "offline" beside it); no host at all is lit. *)
let host_chip (m : App.Model.t) (state : State.t) ~inject =
  let cls, title, aria, content =
    match Active_host.of_state state with
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
      , None
      , [ icon Server; span ~cls:"chip-label" "No host" ] )
  in
  button
    ~cls:("chip-button host-chip" ^ cls)
    ~title
    ?label:aria
    ~attrs:(menu_attrs m Host)
    ~on_click:(inject (Action.Open_chip Host))
    (content @ [ icon ~cls:"chevron" Chevron ])
;;

(* What the message goes to, in the row under the text beside the +, each
   chip opening its menu over itself (the command's choices). The thinking
   level and the mode are the chat header's ({!Session_chips_view}). *)
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
        ; Attr.create "aria-label" "Model and host"
        ]
      [ Session_chips_view.anchor m Model (model_chip m state ~inject) ~inject
      ; Session_chips_view.anchor m Host (host_chip m state ~inject) ~inject
      ]
;;

(* The +: a menu of what can go with the message. *)
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
           ~cls:"icon ghost round plus"
           ~title:"Add files or images (or paste, or drop them here)"
           ~label:"Add files or images"
           ~attrs:
             [ Attr.create "aria-haspopup" "menu"
             ; Attr.create
                 "aria-expanded"
                 (Bool.to_string (Session_chips_view.menu_open m [ Attach ]))
             ]
           ~on_click:(inject (Action.Open_chip Attach))
           [ icon Plus ])
        ~inject
    ]
;;

(* While dictating: how long, how loud, and a way out. *)
let dictation (m : App.Model.t) ~inject =
  let status ~cls children =
    Node.div
      ~attrs:
        [ Attr.class_ ("dictation " ^ cls)
        ; Attr.role "status"
        ; Attr.create "aria-live" "polite"
        ]
      (children
       @ [ button
             ~cls:"ghost dictation-cancel"
             ~title:"Discard the recording (Esc)"
             ~label:"Discard the recording"
             ~on_click:(inject Action.Dictation_cancel)
             [ icon Close ]
         ])
  in
  match m.dictation with
  | Idle -> Node.none
  | Starting _ ->
    status
      ~cls:"starting"
      [ Html.lamp (); span ~cls:"dictation-label" "Listening…" ]
  | Recording { engine; elapsed_ms; level } ->
    status
      ~cls:"recording"
      [ Node.span
          ~attrs:
            [ Attr.class_ "dictation-dot"
            ; Attr.css_var ~name:"level" (sprintf "%.2f" level)
            ]
          []
      ; span ~cls:"dictation-time" (Dictation.elapsed elapsed_ms)
      ; span
          ~cls:"dictation-label"
          (match engine with
           | Server -> "Recording"
           | Browser -> "Listening (browser)")
      ]
  | Transcribing ->
    status
      ~cls:"transcribing"
      [ Html.lamp (); span ~cls:"dictation-label" "Transcribing…" ]
;;

let mic (m : App.Model.t) ~inject =
  let active = Dictation.active m.dictation in
  button
    ~cls:(if active then "icon round mic on" else "icon ghost round mic")
    ~title:
      (match m.dictation with
       | Idle -> "Dictate (speak, then click again to stop)"
       | Starting _ | Recording _ -> "Stop and put the words in"
       | Transcribing -> "Transcribing…")
    ~label:(if active then "Stop dictating" else "Dictate")
    ~disabled:(Dictation.equal m.dictation Transcribing)
    ~attrs:[ Attr.create "aria-pressed" (Bool.to_string active) ]
    ~on_click:(inject Action.Dictate)
    [ icon Mic ]
;;

let steered (m : App.Model.t) =
  match m.recipient with
  | Main -> None
  | Agent id -> Some id
;;

(* Whom the composer writes to, while it is a subagent; the cross (or Esc)
   goes back to the main agent. *)
let steer_chip (m : App.Model.t) id ~inject =
  let task =
    Option.value_map (Agents.find_agent m.agents id) ~default:"" ~f:(fun a ->
      Chat_html.first_line a.task)
  in
  div
    ~cls:"steer-chip"
    ~attrs:
      [ Attr.title
          (sprintf
             "Your messages go to subagent %s: Enter reaches it at its next \
              step, Alt+Enter when it ends its turn"
             id)
      ]
    [ icon ~cls:"steer-chip-icon" Target
    ; Node.span
        ~attrs:[ Attr.class_ "steer-chip-label" ]
        [ Node.text "Steering "; span ~cls:"steer-chip-id" id ]
    ; (if String.is_empty task
       then Node.none
       else span ~cls:"steer-chip-task" task)
    ; button
        ~cls:"icon ghost steer-chip-close"
        ~title:"Write to the main agent again (Esc)"
        ~label:"Stop steering"
        ~on_click:(inject (Action.Set_recipient Main))
        [ icon Close ]
    ]
;;

let connected (m : App.Model.t) =
  match m.connection, m.state with
  | Connected, Some _ -> Option.is_none m.session_gone
  | Reconnecting _, _ | _, None -> false
;;

(* One line at any width (CSS cuts it with an ellipsis, never wraps it). *)
let planning (m : App.Model.t) =
  Option.value_map m.state ~default:false ~f:(fun (s : State.t) -> s.plan_mode)
;;

let placeholder (m : App.Model.t) =
  (* A phone, or the chat beside a side panel. *)
  let cramped = m.narrow || m.agents.open_ || m.files.open_ in
  if Option.is_some m.session_gone
  then "Session gone: /new starts one"
  else if not (connected m)
  then
    if cramped
    then "Offline: send once back"
    else "Offline: write now, send once it is back"
  else (
    match steered m with
    | Some id when cramped -> sprintf "Steer %s (Alt+Enter: after its turn)…" id
    | Some id ->
      sprintf
        "Steer %s (Enter) or queue a follow-up for when it ends its turn \
         (Alt+Enter)…"
        id
    | None when App.Model.running m && cramped -> "Steer the agent…"
    | None when App.Model.running m ->
      "Steer (Enter) or queue a follow-up (Alt+Enter)…"
    | None when planning m && cramped -> "Plan mode: /plan off to implement"
    | None when planning m ->
      sprintf
        "Plan mode: discuss the plan, or leave it to implement (%s)…"
        Session_mode.leave
    | None when cramped -> "Ask prigh…  / commands"
    | None -> "Ask prigh anything…  / commands, @ files")
;;

let view (m : App.Model.t) ~inject =
  let running = App.Model.running m in
  let has_input =
    not
      (String.is_empty (String.strip m.draft)
       && List.is_empty m.images
       && List.is_empty m.files.attachments)
  in
  let sending = m.pending.sending > 0 in
  (* Running with nothing to steer with, the disc stops the agent (a
     subagent being steered keeps its steer). *)
  let stops = running && (not has_input) && Option.is_none (steered m) in
  let connected = connected m in
  (* A chip's menu leaves only the chips live. *)
  let rest =
    match m.dialog with
    | Some (Popover _) -> inert
    | _ -> Fn.id
  in
  Node.footer
    ~attrs:
      [ classes [ "composer" ] [ "offline", not connected; "sending", sending ]
      ]
    [ rest (Queue_view.view m ~inject)
    ; Node.div
        ~attrs:
          [ classes
              [ "composer-stack" ]
              [ "completing", Option.is_some (App.Model.popup m) ]
          ]
        [ rest (popup m ~inject)
        ; div
            ~cls:"composer-box"
            [ (match m.recipient with
               | Main -> Node.none
               | Agent id -> rest (steer_chip m id ~inject))
            ; rest (images m ~inject)
            ; rest (Files_view.chips m ~inject)
            ; rest
              @@ Node.textarea
                   ~attrs:
                     [ Attr.class_ "editor"
                     ; Attr.id "editor"
                     ; Attr.placeholder (placeholder m)
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
                           Option.value (caret ev) ~default:(String.length text)
                         in
                         inject (Action.Edit { text; cursor }))
                     ]
                   []
            ; div
                ~cls:"composer-row"
                [ plus m ~inject
                ; (* While dictating, the recording takes the chips' place. *)
                  (match m.dictation with
                   | Idle -> chips m ~inject
                   | Starting _ | Recording _ | Transcribing ->
                     rest (dictation m ~inject))
                ; rest
                  @@ div
                       ~cls:"composer-buttons"
                       [ mic m ~inject
                       ; (if running && not stops
                          then
                            button
                              ~cls:"icon ghost round stop"
                              ~title:"Stop (Esc)"
                              ~label:"Stop"
                              ~on_click:(inject Action.Abort)
                              [ icon Stop ]
                          else Node.none)
                       ; (if sending
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
                              ~title:"Stop (Esc)"
                              ~label:"Stop"
                              ~on_click:(inject Action.Abort)
                              [ icon Stop ]
                          else
                            button
                              ~cls:"primary round send"
                              ~title:
                                (match steered m with
                                 | _ when not connected ->
                                   "Not connected to the backend"
                                 | Some id -> sprintf "Steer %s (Enter)" id
                                 | None when running -> "Steer (Enter)"
                                 | None -> "Send (Enter)")
                              ~label:
                                (if running || Option.is_some (steered m)
                                 then "Steer"
                                 else "Send")
                              ~disabled:((not has_input) || not connected)
                              ~on_click:(inject Action.Send)
                              [ icon Send ])
                       ]
                ]
            ]
        ]
    ]
;;
