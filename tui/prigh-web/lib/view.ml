open! Core
open! Import
module Model = App.Model
module Action = App.Action

let div ?(cls = "") children =
  Node.div
    ~attrs:(if String.is_empty cls then [] else [ Attr.class_ cls ])
    children
;;

let span ?(cls = "") text =
  Node.span
    ~attrs:(if String.is_empty cls then [] else [ Attr.class_ cls ])
    [ Node.text text ]
;;

let button ?(cls = "") ?title ?(disabled = false) ~on_click label =
  Node.button
    ~attrs:
      ([ Attr.class_ ("btn " ^ cls); Attr.on_click (fun _ -> on_click) ]
       @ Option.value_map title ~default:[] ~f:(fun t -> [ Attr.title t ])
       @ if disabled then [ Attr.disabled ] else [])
    [ Node.text label ]
;;

let session_title (s : Session_summary.t) =
  match s.name with
  | Some name -> name
  | None -> Session_summary.blurb s
;;

let sidebar (m : Model.t) ~inject =
  let current = Option.map m.state ~f:(fun s -> s.session_id) in
  Node.aside
    ~attrs:[ Attr.class_ "sidebar" ]
    [ div ~cls:"brand" [ span ~cls:"logo" "prigh"; span ~cls:"brand-sub" "web" ]
    ; button
        ~cls:"new-session"
        ~on_click:(inject Action.New_session)
        "+ New session"
    ; div
        ~cls:"sessions"
        (List.map m.sessions ~f:(fun s ->
           let selected = Option.equal String.equal current (Some s.id) in
           Node.div
             ~attrs:
               [ Attr.classes
                   ([ "session" ] @ if selected then [ "selected" ] else [])
               ; Attr.on_click (fun _ -> inject (Action.Switch_session s.path))
               ; Attr.title s.path
               ]
             [ div ~cls:"session-title" [ Node.text (session_title s) ]
             ; div
                 ~cls:"session-meta"
                 [ (if s.running
                    then span ~cls:"dot running" "●"
                    else if s.live
                    then span ~cls:"dot live" "●"
                    else Node.none)
                 ; span (sprintf "%d messages" s.message_count)
                 ]
             ]))
    ]
;;

let select ~cls ~title ~options ~selected ~on_change =
  Node.select
    ~attrs:
      [ Attr.class_ cls
      ; Attr.title title
      ; Attr.on_change (fun _ value -> on_change value)
      ]
    (List.map options ~f:(fun (value, label) ->
       Node.option
         ~attrs:
           ([ Attr.value value ]
            @ if String.equal value selected then [ Attr.selected ] else [])
         [ Node.text label ]))
;;

let topbar (m : Model.t) ~inject =
  match m.state with
  | None -> Node.header ~attrs:[ Attr.class_ "topbar" ] [ span "connecting…" ]
  | Some state ->
    let title =
      Option.value
        state.session_name
        ~default:(Option.value state.session_description ~default:"New session")
    in
    let models =
      let known =
        List.map m.models ~f:(fun (model : Llm.t) ->
          model.key, model.name ^ " · " ^ model.provider)
      in
      if List.Assoc.mem known ~equal:String.equal state.model.key
      then known
      else (state.model.key, state.model.name) :: known
    in
    let context =
      if state.model.context_window > 0
      then
        sprintf
          "%.0f%% ctx"
          (100.
           *. Float.of_int state.context_tokens
           /. Float.of_int state.model.context_window)
      else ""
    in
    Node.header
      ~attrs:[ Attr.class_ "topbar" ]
      [ button
          ~cls:"icon toggle-sidebar"
          ~title:"Sessions"
          ~on_click:(inject Action.Toggle_sidebar)
          "☰"
      ; div
          ~cls:"title"
          [ div ~cls:"session-name" [ Node.text title ]
          ; div
              ~cls:"session-cwd"
              [ Node.text
                  (state.cwd
                   ^ Option.value_map state.git_branch ~default:"" ~f:(fun b ->
                     " · " ^ b))
              ]
          ]
      ; div
          ~cls:"controls"
          [ select
              ~cls:"model-select"
              ~title:"Model"
              ~options:models
              ~selected:state.model.key
              ~on_change:(fun key -> inject (Action.Set_model key))
          ; (if state.model.supports_thinking
             then
               select
                 ~cls:"thinking-select"
                 ~title:"Thinking"
                 ~options:
                   (List.map App.thinking_levels ~f:(fun l ->
                      l, "thinking: " ^ l))
                 ~selected:state.thinking
                 ~on_change:(fun level -> inject (Action.Set_thinking level))
             else Node.none)
          ; span ~cls:"chip" context
          ; span ~cls:"chip" (sprintf "$%.4f" state.cost_usd)
          ]
      ]
;;

let composer (m : Model.t) ~inject =
  let running = Model.running m in
  let steer, follow_up = m.queue in
  Node.footer
    ~attrs:[ Attr.class_ "composer" ]
    [ (if steer + follow_up > 0
       then div ~cls:"queued" [ Node.textf "%d queued" (steer + follow_up) ]
       else Node.none)
    ; (match m.images with
       | [] -> Node.none
       | list ->
         div
           ~cls:"pending-images"
           (List.mapi list ~f:(fun i image ->
              Node.button
                ~attrs:
                  [ Attr.class_ "pending-image"
                  ; Attr.title "Remove image"
                  ; Attr.on_click (fun _ -> inject (Action.Remove_image i))
                  ]
                [ Node.img
                    ~attrs:
                      [ Attr.src (Chat_view.image_src image)
                      ; Attr.alt (Image.to_string_hum image)
                      ]
                    ()
                ])))
    ; div
        ~cls:"composer-row"
        [ Node.textarea
            ~attrs:
              [ Attr.class_ "editor"
              ; Attr.id "editor"
              ; Attr.placeholder
                  (if running
                   then
                     "Steer the agent (Enter), or queue a follow-up \
                      (Alt+Enter)…"
                   else
                     "Ask prigh anything… (Enter to send, Shift+Enter for a \
                      newline)")
              ; Attr.value m.draft
              ; Attr.rows 3
              ; Attr.on_input (fun _ text -> inject (Action.Set_draft text))
              ; Attr.on_keydown (fun ev ->
                  let str v =
                    Js_of_ocaml.Js.Optdef.case
                      v
                      (fun () -> "")
                      Js_of_ocaml.Js.to_string
                  in
                  let key : Keys.t =
                    { key = str ev##.key
                    ; shift = Js_of_ocaml.Js.to_bool ev##.shiftKey
                    ; alt = Js_of_ocaml.Js.to_bool ev##.altKey
                    ; ctrl = Js_of_ocaml.Js.to_bool ev##.ctrlKey
                    ; meta = Js_of_ocaml.Js.to_bool ev##.metaKey
                    }
                  in
                  match Keys.editor_action key ~running with
                  | Some action ->
                    ev##preventDefault;
                    inject action
                  | None -> Effect.Ignore)
              ]
            []
        ; div
            ~cls:"composer-buttons"
            [ (if running
               then button ~cls:"stop" ~on_click:(inject Action.Abort) "Stop"
               else Node.none)
            ; button
                ~cls:"primary send"
                ~on_click:(inject Action.Send)
                (if running then "Steer" else "Send")
            ]
        ]
    ]
;;

let confirm_dialog (m : Model.t) ~inject =
  match m.confirms with
  | [] -> Node.none
  | c :: _ ->
    div
      ~cls:"modal-backdrop"
      [ div
          ~cls:"modal"
          [ Node.h3 [ Node.textf "Allow %s?" c.name ]
          ; Node.pre
              ~attrs:[ Attr.class_ "confirm-summary" ]
              [ Node.text c.summary ]
          ; div
              ~cls:"modal-buttons"
              [ button
                  ~cls:"deny"
                  ~on_click:
                    (inject
                       (Action.Respond_confirm
                          { call_id = c.call_id; allow = false }))
                  "Deny"
              ; button
                  ~cls:"primary allow"
                  ~on_click:
                    (inject
                       (Action.Respond_confirm
                          { call_id = c.call_id; allow = true }))
                  "Allow"
              ]
          ]
      ]
;;

let toasts (m : Model.t) ~inject =
  div
    ~cls:"toasts"
    (List.map m.toasts ~f:(fun (t : App.Toast.t) ->
       Node.div
         ~attrs:
           [ Attr.classes ([ "toast" ] @ if t.error then [ "error" ] else [])
           ; Attr.on_click (fun _ -> inject (Action.Dismiss_toast t.id))
           ]
         [ Node.text t.text ]))
;;

let view (m : Model.t) ~inject =
  let banner =
    match m.connection with
    | Connected -> Node.none
    | Reconnecting _ ->
      div ~cls:"banner" [ Node.text "Connection lost — reconnecting…" ]
  in
  let chat =
    match Chat.entries m.chat with
    | [] ->
      div
        ~cls:"empty"
        [ Node.h2 [ Node.text "What are we building?" ]
        ; Node.p
            [ Node.text
                "Ask a question, paste a screenshot, or point prigh at a file \
                 with @path."
            ]
        ]
    | _ -> Chat_view.view m.chat
  in
  Node.div
    ~attrs:
      [ Attr.classes
          ([ "app" ] @ if m.sidebar_open then [ "sidebar-open" ] else [])
      ]
    [ sidebar m ~inject
    ; Node.main
        ~attrs:[ Attr.class_ "main" ]
        [ topbar m ~inject
        ; banner
        ; Node.div ~attrs:[ Attr.class_ "chat"; Attr.id "chat" ] [ chat ]
        ; composer m ~inject
        ]
    ; confirm_dialog m ~inject
    ; toasts m ~inject
    ]
;;
