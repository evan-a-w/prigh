open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action

(* While the chat is scrolled up: back to the end (and following again).
   Inside the chat, stuck to its bottom edge, so the wheel over it still
   scrolls the chat. *)
let jump_to_bottom (m : Model.t) ~inject =
  if not m.scrolled_up
  then Node.none
  else
    Node.div
      ~attrs:[ Attr.class_ "jump-anchor" ]
      [ Node.button
          ~attrs:
            [ Attr.class_ "jump-to-bottom"
            ; Attr.title "Jump to the latest"
            ; Attr.on_click (fun _ -> inject Action.Jump_to_bottom)
            ]
          [ icon Arrow_down ]
      ]
;;

(* Over the top of the chat: the transcript does not move. *)
let banner (m : Model.t) ~inject =
  let over banner = div ~cls:"banner-anchor" [ banner ] in
  match m.connection, m.session_gone with
  | Connected, None -> Node.none
  | Connected, Some _ ->
    over
    @@ div
         ~cls:"banner gone"
         ~attrs:[ Attr.role "alert" ]
         [ Node.text
             "This session is no longer on the backend (it restarted before \
              saving it): what it showed stays here, read-only."
         ; button
             ~cls:"small"
             ~title:"/new"
             ~on_click:(inject Action.New_session)
             [ Node.text "New session" ]
         ; button
             ~cls:"small"
             ~title:"Ctrl+K"
             ~on_click:(inject Action.Open_sessions)
             [ Node.text "Sessions" ]
         ]
  | Reconnecting _, _ when App.Drops.gave_up m.drops ->
    over
    @@ div
         ~cls:"banner gave-up"
         ~attrs:[ Attr.role "alert" ]
         [ Node.text
             "The backend keeps dropping the connection while loading this \
              session: open another session, or reload."
         ; button
             ~cls:"small"
             ~title:"/retry-backend-connection"
             ~on_click:(inject Action.Retry_connection)
             [ Node.text "Retry now" ]
         ; button
             ~cls:"small"
             ~title:"Reload the page without this session"
             ~on_click:(inject (Action.Reload_page { keep_session = false }))
             [ Node.text "Another session" ]
         ; button
             ~cls:"small"
             ~title:"Reload the page"
             ~on_click:(inject (Action.Reload_page { keep_session = true }))
             [ Node.text "Reload" ]
         ]
  | Reconnecting { attempt; _ }, _ ->
    over
    @@ div
         ~cls:"banner"
         ~attrs:[ Attr.role "status" ]
         [ Html.lamp ()
         ; Node.text
             (if attempt = 0
              then "Connection lost: reconnecting…"
              else
                sprintf
                  "Connection lost: reconnecting (attempt %d, next in %.1fs)…"
                  (attempt + 1)
                  (Float.of_int (App.Connection.delay_ms ~attempt) /. 1000.))
         ; button
             ~cls:"small"
             ~title:"/retry-backend-connection"
             ~on_click:(inject Action.Retry_connection)
             [ Node.text "Retry now" ]
         ]
;;

(* Over the bottom of the chat, above the composer: not over the transcript's
   top, the side panels or a dialog. *)
let toasts (m : Model.t) ~inject =
  div
    ~cls:"toast-anchor"
    [ div
        ~cls:"toasts"
        ~attrs:[ Attr.create "aria-live" "polite" ]
        (List.map m.toasts ~f:(fun (t : App.Toast.t) ->
           let shown = Toast.short t.text in
           Node.div
             ~attrs:
               [ classes [ "toast" ] [ "error", t.error ]
               ; Attr.role (if t.error then "alert" else "status")
               ; Attr.title
                   (if String.equal shown t.text
                    then "Dismiss"
                    else t.text ^ " (click to dismiss)")
               ; Attr.on_click (fun _ -> inject (Action.Dismiss_toast t.id))
               ]
             [ Node.span ~attrs:[ Attr.create "dir" "auto" ] [ Node.text shown ]
             ; (if t.repeats > 0
                then span ~cls:"toast-repeats" (sprintf "×%d" (t.repeats + 1))
                else Node.none)
             ; Option.value_map t.undo ~default:Node.none ~f:(fun h ->
                 Html.button
                   ~cls:"ghost small toast-undo"
                   ~title:
                     "Bring it back into the transcript and the model's \
                      context (/delete undo)"
                   ~on_click:
                     (Effect.Many
                        [ Effect.Stop_propagation
                        ; inject (Action.Restore_hidden h)
                        ])
                   [ Node.text "Undo" ])
             ; (if t.error
                then
                  Html.button
                    ~cls:"icon ghost toast-close"
                    ~title:"Dismiss"
                    ~label:"Dismiss"
                    ~on_click:(inject (Action.Dismiss_toast t.id))
                    [ icon Close ]
                else Node.none)
             ]))
    ]
;;

let empty (m : Model.t) =
  let hint key text = Node.li [ Node.kbd [ Node.text key ]; Node.text text ] in
  div
    ~cls:"empty"
    [ Node.h2 [ Node.text "What are we building?" ]
    ; (match m.state with
       | Some state ->
         div
           ~cls:"empty-where"
           [ Node.text
               (Sidebar_view.home_relative
                  ?host_home:(Sidebar_view.state_host_home m)
                  state.cwd)
           ]
       | None -> Node.none)
    ; Node.ul
        ~attrs:[ Attr.class_ "empty-hints" ]
        [ hint "/" " commands"
        ; hint "@" " mention a file"
        ; hint "!" " run a command"
        ; hint "Ctrl+L" " switch model"
        ; hint "Ctrl+K" " find a session"
        ]
    ]
;;

(* Until the backend has answered: what we wait for, over the shape of a
   transcript. *)
let placeholder text =
  Node.div
    ~attrs:[ Attr.class_ "chat-loading"; Attr.role "status" ]
    [ div ~cls:"loading-label" [ Html.lamp (); Node.text text ]
    ; div
        ~cls:"skeleton"
        (List.map
           [ "user"; "line"; "line short"; "line"; "user"; "line" ]
           ~f:(fun cls -> div ~cls:("bone " ^ cls) []))
    ]
;;

let loading (m : Model.t) =
  match m.state, m.connection, m.pending with
  | None, Connected, _ -> Some "Connecting to the backend…"
  | None, Reconnecting _, _ -> Some "Reconnecting…"
  | Some _, _, { session = Some Starting; _ } -> Some "Starting a session…"
  | Some _, _, { session = Some (Switching _); _ }
  | Some _, _, { messages = true; _ } -> Some "Loading the session…"
  | Some _, _, { session = None; messages = false; _ } -> None
;;

(* Prompts sent that the transcript does not show yet. *)
let echoes (m : Model.t) =
  match m.pending.prompts with
  | [] -> Node.none
  | prompts ->
    div
      ~cls:"entries echoes"
      (List.map prompts ~f:(fun (p : In_flight.Prompt.t) ->
         div
           ~cls:"msg user sending"
           [ Image_view.thumbs p.images
           ; (if String.is_empty (String.strip p.text)
              then Node.none
              else
                div
                  ~cls:"bubble"
                  ~attrs:[ Attr.create "dir" "auto" ]
                  [ Node.text p.text ])
           ; span ~cls:"time" "Sending…"
           ]))
;;

(* What is loaded of the transcript, after how much is not. *)
let transcript (m : Model.t) ~inject =
  let index = Model.message_index m in
  let dividers =
    Option.value_map m.state ~default:[] ~f:(fun s ->
      Hidden.trims_after s.hidden)
    |> List.filter_map ~f:(fun (hidden : Hidden.t) ->
      Option.bind hidden.divider ~f:(fun divider ->
        Option.map (Message_index.after index ~divider) ~f:(fun after ->
          { Chat_view.Divider.after
          ; hidden
          ; shown = Trimmed.range m.trimmed hidden.entry
          ; restoring = Set.mem m.trimmed.restoring_ranges hidden.entry
          })))
  in
  Node.fragment
    (Filter_view.note m ~inject
     @ [ Paging.view m.paging ~load:(inject Action.Load_older) ]
     @ (match Option.bind m.state ~f:(fun s -> s.trim) with
        | Some _ -> [ Trim_view.view m ~inject ]
        | None -> [])
     @ [ Chat_view.view
           ~fold:(Model.fold m)
           ?menu:
             (match m.dialog with
              | Some
                  (Picker { kind = Menu { target = Message entry; _ }; picker })
                ->
                Some
                  { Chat_view.Menu.entry
                  ; highlighted =
                      Option.bind (Picker.selected_item picker) ~f:(fun item ->
                        if item.dimmed
                        then None
                        else Context_menu.Item.of_id item.id)
                  }
              | _ -> None)
           ~index
           ~deleting:m.trimmed.deleting
           ~dividers
           (Model.message_time m)
           m.chat
       ])
;;

let view (m : Model.t) ~inject ~terminal =
  let behind =
    if Option.is_some m.dialog || Model.waiting m || Files.asking m.files
    then inert
    else Fn.id
  in
  (* A chip's menu is in the composer, which keeps its chips live (the
     composer makes the rest of itself inert). *)
  let behind_composer =
    match m.dialog with
    | Some (Popover _) when not (Model.waiting m || Files.asking m.files) ->
      Fn.id
    | _ -> behind
  in
  (* On a phone the agents panel is a sheet over the page: it has the
     keyboard while open. *)
  let sheet = m.narrow && (m.agents.open_ || m.files.open_) in
  (* So is the drawer, over the page. *)
  let drawer = Model.drawer m in
  let layered behind =
    let under_sheet = if sheet then Fn.compose inert behind else behind in
    if drawer then Fn.compose inert under_sheet else under_sheet
  in
  let under_sheet = if sheet then Fn.compose inert behind else behind in
  let under_drawer = layered behind in
  (* The panels beside the review are under its drawer; a phone's sheets
     are over its drawer. *)
  let panel =
    if Model.focused m && drawer then Fn.compose inert behind else behind
  in
  let chat =
    match loading m, Chat.entries m.chat, m.pending.prompts with
    | Some text, _, _ -> placeholder text
    | None, [], [] -> empty m
    | None, [], _ -> echoes m
    | None, _, [] -> transcript m ~inject
    | None, _, _ -> Node.fragment [ transcript m ~inject; echoes m ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "app" ]
          [ "sidebar-open", Model.sidebar_shown m
          ; "sidebar-drawer", drawer
          ; "narrow", m.narrow
          ; "agents-open", m.agents.open_
          ; "terminal-open", m.terminal.open_
          ; ( "offline"
            , match m.connection with
              | Reconnecting _ -> true
              | Connected -> false )
          ; "files-open", m.files.open_
          ; "reviewing", m.review.open_
          ]
      ; Resize_view.vars m
      ]
    [ (* A closed drawer is only out of sight. *)
      (if (m.narrow || Model.focused m) && not (Model.sidebar_shown m)
       then inert
       else under_sheet)
        (Sidebar_view.view
           m
           ~inject
           ~session:
             (if Model.docked m then Session_view.card m ~inject else Node.none))
    ; (if Model.docked m
       then Resize_view.handle m ~inject Sidebar
       else Node.none)
    ; (if drawer
       then
         Node.div
           ~attrs:
             [ Attr.class_ "scrim"
             ; Attr.on_click (fun _ -> inject Action.Toggle_sidebar)
             ]
           []
       else Node.none)
    ; (* The review page, the chat and the terminal: a grid that the review
         rearranges (review.css), each keeping its place in the tree, so that
         the terminal's widget stays mounted. *)
      div
        ~cls:"desk"
        [ (* Its pickers are on the page, which keeps them live (the page
             makes the rest of itself inert). *)
          (match m.dialog with
           | Some (Review_pick _)
             when not (Model.waiting m || Files.asking m.files) -> layered Fn.id
           | _ -> under_drawer)
            (Review_page_view.view m ~inject)
        ; (if m.review.open_
           then Resize_view.handle m ~inject Review_chat
           else Node.none)
        ; Node.main
            ~attrs:[ Attr.class_ "main" ]
            (List.map
               ~f:under_drawer
               [ (if Model.docked m
                  then Node.none
                  else Session_view.bar m ~inject)
               ; banner m ~inject
               ; div
                   ~cls:"chat-frame"
                   [ (if Model.docked m
                      then Session_view.tools m ~inject
                      else Node.none)
                   ; Node.div
                       ~attrs:
                         [ Attr.classes
                             [ "chat"
                             ; "verbosity-"
                               ^ Prigh_ui.Verbosity.name m.verbosity
                             ]
                         ; Attr.id "chat"
                         ; Attr.role "region"
                         ; Attr.create "aria-label" "Transcript"
                         ; Attr.tabindex 0
                         ; (if Option.is_some (loading m)
                            then Attr.create "aria-busy" "true"
                            else Attr.empty)
                         ]
                       [ chat; jump_to_bottom m ~inject ]
                   ; (match loading m with
                      | Some _ -> Node.none
                      | None -> Jump_view.view m ~inject)
                   ]
               ]
             @ [ toasts m ~inject ]
             @ [ under_drawer (Btw_view.view m ~inject)
               ; layered behind_composer (Composer_view.view m ~inject)
               ])
        ; under_drawer (Terminal_view.view m ~inject ~widget:terminal)
        ]
    ; panel (Files_view.view m ~inject)
    ; panel (Agents_view.view m ~inject)
    ; Dialog_view.view m ~inject
    ; Files_view.dialog m ~inject
    ; Files_view.pickers
    ]
;;
