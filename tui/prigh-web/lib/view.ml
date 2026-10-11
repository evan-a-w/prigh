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

let banner (m : Model.t) ~inject =
  match m.connection with
  | Connected -> Node.none
  | Reconnecting { attempt; _ } ->
    div
      ~cls:"banner"
      ~attrs:[ Attr.role "status" ]
      [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
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

let toasts (m : Model.t) ~inject =
  div
    ~cls:"toasts"
    ~attrs:[ Attr.create "aria-live" "polite" ]
    (List.map m.toasts ~f:(fun (t : App.Toast.t) ->
       Node.div
         ~attrs:
           [ classes [ "toast" ] [ "error", t.error ]
           ; Attr.role (if t.error then "alert" else "status")
           ; Attr.title "Dismiss"
           ; Attr.on_click (fun _ -> inject (Action.Dismiss_toast t.id))
           ]
         [ Node.span [ Node.text t.text ]
         ; (if t.error then icon ~cls:"toast-close" Close else Node.none)
         ]))
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
           [ Node.text (Sidebar_view.home_relative state.cwd) ]
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
    [ div
        ~cls:"loading-label"
        [ Node.span ~attrs:[ Attr.class_ "spinner" ] []; Node.text text ]
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
      (List.map prompts ~f:(fun (p : Pending.Prompt.t) ->
         div
           ~cls:"msg user sending"
           [ Image_view.thumbs p.images
           ; (if String.is_empty (String.strip p.text)
              then Node.none
              else div ~cls:"bubble" [ Node.text p.text ])
           ; span ~cls:"time" "Sending…"
           ]))
;;

(* Under a dialog the page is inert: Tab and clicks cannot reach it, so the
   dialog owns the keyboard until it closes. *)
let rec inert (node : Node.t) : Node.t =
  match node with
  | Element e ->
    Element
      (Node.Element.map_attrs e ~f:(fun a ->
         Attr.many [ a; Attr.create "inert" "" ]))
  | Lazy { key; t } -> Lazy { key; t = lazy (inert (Lazy.force t)) }
  | Fragment nodes -> Fragment (List.map nodes ~f:inert)
  | (None | Text _ | Widget _) as node -> node
;;

let view (m : Model.t) ~inject ~terminal =
  let behind =
    if Option.is_some m.dialog || not (List.is_empty m.confirms)
    then inert
    else Fn.id
  in
  (* On a phone the agents panel is a sheet over the page: it has the
     keyboard while open. *)
  let under_sheet =
    if m.narrow && m.agents.open_ then Fn.compose inert behind else behind
  in
  let chat =
    match loading m, Chat.entries m.chat, m.pending.prompts with
    | Some text, _, _ -> placeholder text
    | None, [], [] -> empty m
    | None, [], _ -> echoes m
    | None, _, [] ->
      Chat_view.view ~fold:(Model.fold m) (Model.message_time m) m.chat
    | None, _, _ ->
      Node.fragment
        [ Chat_view.view ~fold:(Model.fold m) (Model.message_time m) m.chat
        ; echoes m
        ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "app" ]
          [ "sidebar-open", m.sidebar_open
          ; "narrow", m.narrow
          ; "agents-open", m.agents.open_
          ; "terminal-open", m.terminal.open_
          ]
      ]
    [ under_sheet (Sidebar_view.view m ~inject)
    ; (if m.narrow && m.sidebar_open
       then
         Node.div
           ~attrs:
             [ Attr.class_ "scrim"
             ; Attr.on_click (fun _ -> inject Action.Toggle_sidebar)
             ]
           []
       else Node.none)
    ; under_sheet
      @@ Node.main
           ~attrs:[ Attr.class_ "main" ]
           [ Topbar_view.view m ~inject
           ; banner m ~inject
           ; Node.div
               ~attrs:
                 [ Attr.classes
                     [ "chat"
                     ; "verbosity-" ^ Prigh_ui.Verbosity.name m.verbosity
                     ]
                 ; Attr.id "chat"
                 ]
               [ chat; jump_to_bottom m ~inject ]
           ; Btw_view.view m ~inject
           ; Composer_view.view m ~inject
           ; Terminal_view.view m ~inject ~widget:terminal
           ]
    ; behind (Agents_view.view m ~inject)
    ; Dialog_view.view m ~inject
    ; toasts m ~inject
    ]
;;
