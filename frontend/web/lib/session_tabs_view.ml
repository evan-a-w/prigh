open! Core
open! Import
open Html
module Action = App.Action
module Js = Js_of_ocaml.Js

let dom_id id = "tab-" ^ id

let status_mark (status : Session_tabs.Status.t) =
  let label = Session_tabs.Status.label status in
  match status with
  | Idle -> Node.none
  | Running -> lamp ~title:label ()
  | Waiting -> mark ~cls:"warn" ~label Warning
  | Failed -> mark ~cls:"failed" ~label Cross
  | Finished -> mark ~cls:"done" ~label Check
;;

let title (m : App.Model.t) ~active (s : Session_summary.t option) =
  match m.state, s with
  | _, Some s -> Session_list.title s
  | Some state, None when active -> Session_view.session_title m state
  | _, None -> "New session"
;;

(* Firefox starts a drag only with data. *)
let drag_start ~inject id (ev : Js_of_ocaml.Dom_html.dragEvent Js.t) =
  Js.Opt.iter ev##.dataTransfer (fun data ->
    data##setData (Js.string "text/plain") (Js.string id);
    Js.Unsafe.set data "effectAllowed" (Js.string "move"));
  inject (Action.Drag_tab (Some id))
;;

let tab (m : App.Model.t) ~inject ~current (id, s) =
  let active = Option.exists current ~f:(String.equal id) in
  let status = Session_tabs.status m.tabs s ~current:active in
  let title = title m ~active s in
  let dragging = Option.exists m.tabs.dragging ~f:(String.equal id) in
  Node.div
    ~attrs:
      [ classes
          [ "session-tab" ]
          [ "active", active
          ; "dragging", dragging
          ; "running", Session_tabs.Status.equal status Running
          ; "waiting", Session_tabs.Status.equal status Waiting
          ; "failed", Session_tabs.Status.equal status Failed
          ; "finished", Session_tabs.Status.equal status Finished
          ]
      ; Attr.id (dom_id id)
      ; Attr.draggable true
      ; Attr.on_dragstart (drag_start ~inject id)
      ; Attr.on_dragend (fun _ -> inject (Action.Drag_tab None))
      ; Attr.on_drop (fun _ -> inject (Action.Drop_tab (Some id)))
      ; Attr.on_auxclick (fun ev ->
          if ev##.button = 1
          then inject (Action.Close_tab (Some id))
          else Effect.Ignore)
      ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "session-tab-open"
          ; Attr.type_ "button"
          ; Attr.role "tab"
          ; Attr.create "aria-selected" (Bool.to_string active)
          ; Attr.title
              (match status with
               | Idle -> title
               | _ -> sprintf "%s — %s" title (Session_tabs.Status.label status))
          ; Attr.on_click (fun _ ->
              match s with
              | Some s when not active -> inject (Action.Switch_session s.path)
              | Some _ | None -> Effect.Ignore)
          ]
        [ status_mark status
        ; Node.span
            ~attrs:[ Attr.class_ "session-tab-title"; Attr.create "dir" "auto" ]
            [ Node.text title ]
        ]
    ; button
        ~cls:"icon ghost session-tab-close"
        ~title:(if active then "Close tab (Alt+W)" else "Close tab")
        ~label:("Close " ^ title)
        ~on_click:(inject (Action.Close_tab (Some id)))
        [ icon Close ]
    ]
;;

let view (m : App.Model.t) ~inject =
  if (not m.tabs.enabled) || m.narrow
  then None
  else (
    let current = Option.map m.state ~f:(fun s -> s.session_id) in
    Option.return
    @@ Node.nav
         ~attrs:
           [ classes
               [ "session-tabs" ]
               [ "dragging", Option.is_some m.tabs.dragging ]
           ; Attr.create "aria-label" "Session tabs"
           ]
         [ div
             ~attrs:[ Attr.role "tablist" ]
             ~cls:"session-tabs-list"
             (List.map
                (Session_tabs.shown m.tabs m.sessions ~current)
                ~f:(tab m ~inject ~current))
         ; Node.div
             ~attrs:
               [ Attr.class_ "session-tabs-end"
               ; Attr.on_drop (fun _ -> inject (Action.Drop_tab None))
               ]
             [ button
                 ~cls:"icon ghost session-tabs-new"
                 ~title:"New session (/new)"
                 ~label:"New session"
                 ~on_click:(inject Action.New_session)
                 [ icon Plus ]
             ]
         ])
;;
