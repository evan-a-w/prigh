open! Core
open! Import
open Html
module Action = App.Action

let home_relative cwd =
  match String.chop_prefix cwd ~prefix:"/home/" with
  | Some rest ->
    (match String.lsplit2 rest ~on:'/' with
     | Some (_, path) -> "~/" ^ path
     | None -> "~")
  | None -> cwd
;;

let age (m : App.Model.t) (s : Session_summary.t) =
  match
    m.now, Rel_time.parse (Option.value s.updated_at ~default:s.created_at)
  with
  | Some now, Some time -> Rel_time.ago ~now time
  | _ -> ""
;;

let session (m : App.Model.t) ~inject ~current (s : Session_summary.t) =
  let selected = Option.equal String.equal current (Some s.id) in
  let running = if selected then App.Model.running m else s.running in
  Node.div
    ~attrs:
      [ classes [ "session" ] [ "selected", selected; "running", running ]
      ; Attr.on_click (fun _ -> inject (Action.Switch_session s.path))
      ; Attr.title s.path
      ]
    [ div
        ~cls:"session-top"
        [ (if running
           then span ~cls:"dot running" ~attrs:[ Attr.title "running" ] ""
           else if s.live
           then
             span ~cls:"dot live" ~attrs:[ Attr.title "open in the backend" ] ""
           else Node.none)
        ; span ~cls:"session-title" (Session_list.title s)
        ; span ~cls:"session-age" (age m s)
        ]
    ; div
        ~cls:"session-meta"
        [ span ~cls:"session-cwd" (home_relative s.cwd)
        ; span
            ~cls:"session-count"
            (sprintf
               "%d msg%s"
               s.message_count
               (if s.message_count = 1 then "" else "s"))
        ; Node.button
            ~attrs:
              [ Attr.class_ "btn icon ghost delete"
              ; Attr.type_ "button"
              ; Attr.title "Delete session"
              ; Attr.on_click (fun ev ->
                  Js_of_ocaml.Dom_html.stopPropagation ev;
                  inject (Action.Ask_delete s.path))
              ]
            [ icon Trash ]
        ]
    ]
;;

let user_footer (m : App.Model.t) ~inject =
  let help =
    button
      ~cls:"icon ghost"
      ~title:"Commands and keys (/help)"
      ~on_click:(inject Action.Open_help)
      [ icon Help ]
  in
  let sign_out =
    button
      ~cls:"icon ghost"
      ~title:"Sign out"
      ~on_click:(inject Action.Sign_out)
      [ icon Logout ]
  in
  match m.hello with
  | Some ({ user = Some user; _ } as hello) ->
    div
      ~cls:"sidebar-footer"
      [ span ~cls:"avatar" (String.prefix (String.uppercase user) 1)
      ; div
          ~cls:"user"
          [ span ~cls:"user-name" user
          ; (match Hello_reply.acting_as hello with
             | Some ns -> span ~cls:"user-acting" ("acting as " ^ ns)
             | None -> Node.none)
          ]
      ; help
      ; sign_out
      ]
  | _ ->
    div
      ~cls:"sidebar-footer"
      [ button
          ~cls:"ghost help-button"
          ~title:"Commands and keys (/help)"
          ~on_click:(inject Action.Open_help)
          [ icon Help; Node.text "Commands & keys" ]
      ; (if m.saved_login then sign_out else Node.none)
      ]
;;

let view (m : App.Model.t) ~inject =
  let current = Option.map m.state ~f:(fun s -> s.session_id) in
  let sessions = Session_list.filter m.sessions ~query:m.session_query in
  Node.aside
    ~attrs:[ Attr.class_ "sidebar"; Attr.id "sidebar" ]
    [ div
        ~cls:"sidebar-head"
        [ div
            ~cls:"brand"
            [ span ~cls:"logo" "prigh"; span ~cls:"brand-sub" "web" ]
        ; button
            ~cls:"icon ghost"
            ~title:"Hide sidebar (Ctrl+B)"
            ~on_click:(inject Action.Toggle_sidebar)
            [ icon Sidebar ]
        ]
    ; button
        ~cls:"new-session"
        ~title:"Start a new session (/new)"
        ~on_click:(inject Action.New_session)
        [ icon Plus; Node.text "New session" ]
    ; div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "session-search"
              ; Attr.type_ "search"
              ; Attr.placeholder "Search sessions  (Ctrl+K)"
              ; Attr.value m.session_query
              ; Attr.create "autocomplete" "off"
              ; Attr.on_input (fun _ text ->
                  inject (Action.Set_session_query text))
              ]
            ()
        ]
    ; div
        ~cls:"sessions"
        (match sessions, m.sessions with
         | [], [] ->
           [ div ~cls:"sessions-empty" [ Node.text "No saved sessions yet." ] ]
         | [], _ ->
           [ div
               ~cls:"sessions-empty"
               [ Node.textf "Nothing matches “%s”." m.session_query ]
           ]
         | sessions, _ -> List.map sessions ~f:(session m ~inject ~current))
    ; user_footer m ~inject
    ]
;;
