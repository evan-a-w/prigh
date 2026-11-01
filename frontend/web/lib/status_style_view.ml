open! Core
open! Import
open Html

let style (m : App.Model.t) : Status_style.t = m.status_style

let sidebar_docked (m : App.Model.t) =
  match style m with
  | Sidebar -> App.Model.docked m
  | Island | Ring | Whisper | Border | Hub -> false
;;

let chips_in_sidebar = sidebar_docked

let composer_classes m =
  [ "status-style-" ^ Status_style.name (style m), true; "busy", Status_view.busy m ]
;;

let percent_text percent = sprintf "%.0f%%" percent

(* The context's figures, as a button opening the details ([/session]). *)
let context ?cls (state : State.t) ~inject children =
  Status_view.context_button ?cls state ~inject children
;;

let percent_of state = Status_view.context_percent state

(* {2 E: the island} *)

let island (m : App.Model.t) ~inject =
  let body =
    match m.state with
    | None -> [ Status_view.activity m ]
    | Some state ->
      [ Status_view.gauge
          ~cls:"below"
          m
          state
          [ Status_view.activity m
          ; (match percent_of state with
             | Some p ->
               context
                 state
                 ~inject
                 [ Status_view.ring ~cls:"mini" p []
                 ; span ~cls:"percent" (percent_text p)
                 ]
             | None -> Node.none)
          ; Status_view.cost state ~inject
          ]
      ; Session_chips_view.view ~cls:"island-chips" m ~inject
      ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "status-island" ]
          [ "busy", Status_view.busy m
          ; "menu-open", Session_chips_view.menu_open m [ Thinking; Mode ]
          ]
      ; Attr.create "data-popover-box" ""
      ; Attr.role "group"
      ; Attr.create "aria-label" "Session status"
      ]
    body
;;

(* {2 A: the ring round send} *)

let around_send (m : App.Model.t) ~inject send =
  match style m, m.state with
  | Ring, Some state ->
    let p = Option.value (percent_of state) ~default:0. in
    Status_view.gauge
      ~cls:"above send-gauge"
      m
      state
      [ Status_view.ring ~cls:"send-ring" p [ send ]
      ; (* For the keyboard and screen readers: the ring itself is drawn. *)
        context ~cls:"sr-only" state ~inject []
      ; div ~cls:"sr-only" [ Status_view.activity m ]
      ]
  | _ -> send
;;

let row_chips (m : App.Model.t) ~inject =
  match style m with
  | Ring -> Session_chips_view.view ~cls:"row-chips" ~quiet_ask:true m ~inject
  | Border -> Session_chips_view.view ~cls:"row-chips" m ~inject
  | Island | Whisper | Hub | Sidebar -> Node.none
;;

(* {2 C: the border meter; the hub's activity} *)

let in_composer (m : App.Model.t) ~inject =
  match style m, m.state with
  | Border, Some state ->
    let p = Option.value (percent_of state) ~default:0. in
    Node.fragment
      [ Node.span
          ~attrs:
            [ Attr.class_
                (String.strip ("border-meter " ^ Status_view.context_level p))
            ; Attr.create "aria-hidden" "true"
            ]
          [ Node.span
              ~attrs:
                [ Attr.class_ "border-fill"
                ; Attr.style
                    (Css_gen.width
                       (`Percent
                           (Percent.of_percentage
                              (Float.clamp_exn p ~min:0. ~max:100.))))
                ]
              []
          ]
      ; Status_view.gauge
          ~cls:"above border-tag"
          m
          state
          [ Status_view.activity m
          ; context state ~inject [ span ~cls:"percent" (percent_text p) ]
          ; Status_view.cost state ~inject
          ]
      ]
  | Hub, _ -> div ~cls:"sr-only" [ Status_view.activity m ]
  | _ -> Node.none
;;

(* {2 B: the whisper} *)

let whisper (m : App.Model.t) ~inject =
  let sep = span ~cls:"whisper-sep" "·" in
  let body =
    match m.state with
    | None -> [ Status_view.activity m ]
    | Some state ->
      [ Status_view.activity m
      ; sep
      ; Status_view.gauge
          ~cls:"above"
          m
          state
          [ (match percent_of state with
             | Some p ->
               context
                 state
                 ~inject
                 [ Status_view.meter p
                 ; span ~cls:"percent" (percent_text p)
                 ; span
                     ~cls:"whisper-of"
                     (" of " ^ Status_view.tokens state.model.context_window)
                 ]
             | None -> context state ~inject [ Node.text "context" ])
          ]
      ; sep
      ; Status_view.cost state ~inject
      ; sep
      ; Session_chips_view.view ~cls:"whisper-chips" m ~inject
      ; (match state.git_branch with
         | Some branch ->
           span
             ~cls:"whisper-branch"
             ~attrs:[ Attr.title ("On branch " ^ branch) ]
             branch
         | None -> Node.none)
      ]
  in
  Node.div
    ~attrs:
      [ classes
          [ "status-whisper" ]
          [ "busy", Status_view.busy m
          ; "menu-open", Session_chips_view.menu_open m [ Thinking; Mode ]
          ]
      ; Attr.create "data-popover-box" ""
      ; Attr.role "group"
      ; Attr.create "aria-label" "Session status"
      ]
    body
;;

let under_composer (m : App.Model.t) ~inject =
  match style m with
  | Whisper -> whisper m ~inject
  | Sidebar when not (sidebar_docked m) -> whisper m ~inject
  | Island | Ring | Border | Hub | Sidebar -> Node.none
;;

(* {2 D: the hub} *)

let model_badge (m : App.Model.t) =
  match style m, m.state with
  | Hub, Some state ->
    let mode =
      Option.value m.pending.mode ~default:(Session_mode.of_state state)
    in
    Node.span
      ~attrs:[ Attr.class_ "hub-badge"; Attr.create "aria-hidden" "true" ]
      [ (match percent_of state with
         | Some p ->
           Node.fragment
             [ Status_view.ring ~cls:"mini" p []
             ; span ~cls:"percent" (percent_text p)
             ]
         | None -> Node.none)
      ; (if Session_mode.equal mode { approval = Ask; plan = false }
         then Node.none
         else
           Node.span
             ~attrs:
               [ classes
                   [ "hub-mode" ]
                   [ "loud", mode.plan || (match mode.approval with
                        | Auto -> true
                        | _ -> false) ]
               ; Attr.title (Session_mode.title mode)
               ]
             [ icon Shield ])
      ]
  | _ -> Node.none
;;

(* {2 F: the sidebar's card} *)

let sidebar_card (m : App.Model.t) ~inject =
  if not (sidebar_docked m)
  then Node.none
  else (
    match m.state with
    | None -> Node.none
    | Some state ->
      Node.section
        ~attrs:
          [ classes
              [ "status-card" ]
              [ "busy", Status_view.busy m
              ; "menu-open", Session_chips_view.menu_open m [ Thinking; Mode ]
              ]
          ; Attr.create "data-popover-box" ""
          ; Attr.create "aria-label" "Session status"
          ]
        [ div
            ~cls:"status-card-head"
            [ Status_view.activity m; Status_view.cost state ~inject ]
        ; Status_view.gauge
            ~cls:"above"
            m
            state
            [ context
                state
                ~inject
                (match percent_of state with
                 | Some p ->
                   [ Status_view.meter p; span ~cls:"percent" (percent_text p) ]
                 | None -> [ Node.text "Context" ])
            ]
        ; Session_chips_view.view ~cls:"card-chips" m ~inject
        ])
;;
