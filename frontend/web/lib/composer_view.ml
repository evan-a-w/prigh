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
      | Argument Worktree -> "Worktrees"
      | Argument Review -> "Review"
      | Argument Resume -> "Automatic resume"
      | Argument Notify -> "Notifications"
      | Argument Pr -> "Pull requests"
      | Argument Theme -> "Themes"
      | Argument Accent -> "Accent colours"
      | Argument Trim -> "Trim"
      | Argument Filter -> "Show only"
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

let images (m : App.Model.t) ~inject =
  match m.images with
  | [] -> Node.none
  | list ->
    div
      ~cls:"pending-images"
      (List.mapi list ~f:(fun i image ->
         div
           ~cls:"pending-image"
           [ Node.img
               ~attrs:
                 [ Attr.src (Chat_view.image_src image)
                 ; Attr.alt (Image.to_string_hum image)
                 ; Attr.title (Image.to_string_hum image)
                 ]
               ()
           ; button
               ~cls:"remove-image"
               ~title:"Remove image"
               ~label:"Remove image"
               ~on_click:(inject (Action.Remove_image i))
               [ icon Close ]
           ]))
;;

(* A choice the backend has still to confirm. *)
let pending_chip ?(attrs = []) ?title ~cls label =
  button
    ~cls:("chip-button awaiting " ^ cls)
    ~title:(Option.value title ~default:(sprintf "Switching to %s…" label))
    ~disabled:true
    ~attrs:(Attr.create "aria-busy" "true" :: attrs)
    ~on_click:Effect.Ignore
    [ Html.lamp (); span ~cls:"chip-label" label ]
;;

(* What the chip's button says about its menu, open or not. *)
let menu_attrs (m : App.Model.t) (chip : Chip.t) =
  let open_ =
    match m.dialog with
    | Some (Popover { chip = shown; _ }) -> Chip.equal shown chip
    | _ -> false
  in
  [ Attr.create "aria-haspopup" "dialog"
  ; Attr.create "aria-expanded" (Bool.to_string open_)
  ; (if open_ then Attr.create "aria-controls" "chip-popover" else Attr.empty)
  ]
;;

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

(* A narrow composer shows the level alone, after its icon. *)
let thinking_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.thinking with
  | Some level ->
    pending_chip
      ~cls:"thinking-chip"
      ~title:(sprintf "Switching to thinking %s…" level)
      ("Thinking " ^ level)
  | None when state.model.supports_thinking ->
    button
      ~cls:"chip-button thinking-chip"
      ~title:
        (sprintf
           "Thinking %s: how much the model reasons before it answers (Alt+T \
            cycles, /thinking)"
           state.thinking)
      ~attrs:(menu_attrs m Thinking)
      ~on_click:(inject (Action.Open_chip Thinking))
      [ icon Brain
      ; span ~cls:"chip-key" "Thinking"
      ; span ~cls:"chip-label" state.thinking
      ; icon ~cls:"chevron" Chevron
      ]
  | None ->
    button
      ~cls:"chip-button thinking-chip unsupported"
      ~title:(sprintf "%s has no thinking levels" state.model.name)
      ~on_click:(inject (Action.Open_chip Thinking))
      [ icon Brain; span ~cls:"chip-label" "No thinking" ]
;;

(* What asks first: a narrow composer shows its icon and [label] (ask,
   auto-edit, auto, plan). *)
let mode_chip (m : App.Model.t) (state : State.t) ~inject =
  let short mode = [ Attr.create "data-short" (Session_mode.label mode) ] in
  match m.pending.mode with
  | Some mode ->
    pending_chip
      ~attrs:(short mode)
      ~cls:"mode-chip"
      ~title:(sprintf "Switching to %s…" (Session_mode.label mode))
      (Session_mode.title mode)
  | None ->
    let mode = Session_mode.of_state state in
    button
      ~cls:("chip-button mode-chip" ^ if mode.plan then " plan-mode" else "")
      ~title:
        (sprintf
           "%s (Alt+M cycles, /mode picks)"
           (String.capitalize (Session_mode.describe mode)))
      ~attrs:(short mode @ menu_attrs m Mode)
      ~on_click:(inject (Action.Open_chip Mode))
      [ icon Shield
      ; span ~cls:"chip-label" (Session_mode.title mode)
      ; icon ~cls:"chevron" Chevron
      ]
;;

(* The model, the thinking level and the approval mode, along the bottom of
   the box, each opening its menu over itself (the command's choices). *)
let controls (m : App.Model.t) ~inject =
  match m.state with
  | None -> div ~cls:"composer-controls" []
  | Some state ->
    Node.div
      ~attrs:
        [ classes
            [ "composer-controls" ]
            [ ( "menu-open"
              , match m.dialog with
                | Some (Popover _) -> true
                | _ -> false )
            ]
        ; Attr.role "group"
        ; Attr.create "aria-label" "Model, thinking and mode"
        ]
      (List.map
         [ Chip.Model, model_chip; Thinking, thinking_chip; Mode, mode_chip ]
         ~f:(fun (chip, view) ->
           let menu =
             match m.dialog with
             | Some (Popover { chip = shown; picker })
               when Chip.equal shown chip ->
               Some (Chip_popover_view.view m ~chip picker ~inject)
             | _ -> None
           in
           div
             ~attrs:
               [ classes
                   [ "chip-anchor"
                   ; (match chip with
                      | Model -> "model"
                      | Thinking -> "thinking"
                      | Mode -> "mode")
                   ]
                   [ "open", Option.is_some menu ]
               ]
             [ view m state ~inject; Option.value menu ~default:Node.none ]))
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
    [ rest (popup m ~inject)
    ; rest (Queue_view.view m ~inject)
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
            ~cls:"composer-bar"
            [ controls m ~inject
            ; rest
              @@ div
                   ~cls:"composer-buttons"
                   [ Files_view.attach_button m ~inject
                   ; (if running
                      then
                        button
                          ~cls:"stop"
                          ~title:"Stop (Esc)"
                          ~label:"Stop"
                          ~on_click:(inject Action.Abort)
                          [ icon Stop ]
                      else Node.none)
                   ; (if sending
                      then
                        button
                          ~cls:"primary send busy"
                          ~title:"Sending…"
                          ~label:"Sending"
                          ~disabled:true
                          ~attrs:[ Attr.create "aria-busy" "true" ]
                          ~on_click:Effect.Ignore
                          [ Html.lamp () ]
                      else
                        button
                          ~cls:"primary send"
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
;;
