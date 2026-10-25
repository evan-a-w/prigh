open! Core
open! Import
open Html
module Action = App.Action

(* A setting: its name, what it does in a line, and its control. *)
let row ?(cls = "") ?note ~name ~about controls =
  div
    ~cls:(String.strip ("setting " ^ cls))
    [ div
        ~cls:"setting-text"
        [ span ~cls:"setting-name" name
        ; span ~cls:"setting-about" about
        ; (match note with
           | Some (level, text) -> span ~cls:("setting-note " ^ level) text
           | None -> Node.none)
        ]
    ; div ~cls:"setting-control" controls
    ]
;;

let section title rows =
  Node.section
    ~attrs:[ Attr.class_ "settings-section" ]
    (Node.h3 [ Node.text title ] :: rows)
;;

let switch ?(disabled = false) ~label ~title ~on ~on_click () =
  Node.button
    ~attrs:
      [ classes [ "switch" ] [ "on", on ]
      ; Attr.type_ "button"
      ; Attr.role "switch"
      ; Attr.create "aria-checked" (Bool.to_string on)
      ; Attr.create "aria-label" label
      ; Attr.title title
      ; (if disabled then Attr.disabled else Attr.on_click (fun _ -> on_click))
      ]
    [ Node.span ~attrs:[ Attr.class_ "switch-knob" ] [] ]
;;

let notifications (m : App.Model.t) ~inject =
  let n = m.notifications in
  let run command = inject (Action.Run command) in
  let note =
    match n.enabled, n.permission with
    | false, _ | true, Granted -> None
    | true, Default ->
      Some ("warm", "The browser has not been asked yet: Allow asks it.")
    | true, Denied ->
      Some
        ( "warm"
        , "The browser blocks them: allow notifications in its site settings \
           (the icon left of the address), then turn them off and on." )
    | true, Unsupported ->
      Some
        ( "muted"
        , "This page cannot show them: they need https or localhost. The tab's \
           title still counts what happened." )
  in
  row
    ~cls:"notify"
    ?note
    ~name:"Notifications"
    ~about:
      "Tell you when a turn ends or something waits for you while this tab is \
       hidden."
    [ (match n.enabled, n.permission with
       | true, Default ->
         button
           ~cls:"small primary"
           ~title:"Ask the browser to allow notifications (/notify on)"
           ~on_click:(run "/notify on")
           [ Node.text "Allow" ]
       | _ -> Node.none)
    ; switch
        ~label:"Notifications"
        ~title:
          (if n.enabled
           then "Turn notifications off (/notify off)"
           else "Turn notifications on (/notify on)")
        ~on:n.enabled
        ~on_click:(run (if n.enabled then "/notify off" else "/notify on"))
        ()
    ]
;;

let verbosity (m : App.Model.t) ~inject =
  row
    ~cls:"verbosity"
    ~name:"Transcript detail"
    ~about:
      "How much of tool calls and thinking the transcript shows (Ctrl+O \
       cycles)."
    [ Node.div
        ~attrs:
          [ Attr.class_ "segmented"
          ; Attr.role "group"
          ; Attr.create "aria-label" "Transcript detail"
          ]
        (List.map Prigh_ui.Verbosity.all ~f:(fun v ->
           let name = Prigh_ui.Verbosity.name v in
           let current = Prigh_ui.Verbosity.equal v m.verbosity in
           Node.button
             ~attrs:
               [ classes [ "segment" ] [ "on", current ]
               ; Attr.type_ "button"
               ; Attr.create "aria-pressed" (Bool.to_string current)
               ; Attr.title ("/verbosity " ^ name)
               ; Attr.on_click (fun _ ->
                   inject (Action.Run ("/verbosity " ^ name)))
               ]
             [ Node.text (String.capitalize name) ]))
    ]
;;

let confirm (m : App.Model.t) ~inject =
  let on =
    Option.map m.config ~f:(fun c ->
      not (Approval_mode.equal c.approval_mode Auto))
  in
  row
    ~cls:"confirm"
    ~name:"Tool confirmation"
    ~about:
      "Ask before bash, edits and MCP tools run, in this session and new ones."
    ?note:
      (match on with
       | None -> Some ("muted", "Waiting for the backend's settings…")
       | Some _ -> None)
    [ switch
        ~disabled:(Option.is_none on)
        ~label:"Tool confirmation"
        ~title:
          (if Option.value on ~default:false
           then "Run tools without asking (/confirm off)"
           else "Ask before tools run (/confirm on)")
        ~on:(Option.value on ~default:false)
        ~on_click:
          (inject
             (Action.Run
                (if Option.value on ~default:false
                 then "/confirm off"
                 else "/confirm on")))
        ()
    ]
;;

let providers (m : App.Model.t) ~inject =
  let logged_in =
    List.filter_map m.auth ~f:(fun (s : Auth_status.t) ->
      Option.map s.configured ~f:(fun _ -> s.name))
  in
  row
    ~cls:"providers"
    ~name:"Model providers"
    ~about:
      (match logged_in with
       | [] -> "Not logged in to any: log in to one to use its models."
       | names -> "Logged in to " ^ String.concat ~sep:", " names ^ ".")
    [ button
        ~cls:"small"
        ~title:"/auth (/login, /logout)"
        ~on_click:(inject (Action.Run "/auth"))
        [ Node.text "Manage providers" ]
    ]
;;

let account (m : App.Model.t) ~inject =
  match Account_view.who m with
  | None ->
    [ row
        ~name:"No sign-in"
        ~about:
          "This backend lets anyone in: there is no account to switch or sign \
           out of."
        []
    ]
  | Some { name; acting_as; host } ->
    [ div
        ~cls:"setting account-setting"
        [ span ~cls:"avatar large" (Account_view.initial name)
        ; div
            ~cls:"setting-text"
            [ span ~cls:"setting-name" ("Signed in as " ^ name)
            ; (match acting_as with
               | Some ns ->
                 span
                   ~cls:"setting-note warm"
                   (sprintf
                      "Acting as %s: you see and work in their sessions."
                      ns)
               | None -> Node.none)
            ; (match host with
               | Some host -> span ~cls:"setting-about host" host
               | None -> Node.none)
            ]
        ; div
            ~cls:"setting-control"
            [ button
                ~cls:"small"
                ~title:"Switch, add or act as another account"
                ~on_click:(inject Action.Open_accounts)
                [ Node.text "Switch account" ]
            ; button
                ~cls:"small sign-out"
                ~title:"Forget this account in this browser (/signout)"
                ~on_click:(inject Action.Sign_out)
                [ Node.text "Sign out" ]
            ]
        ]
    ]
;;

let view (m : App.Model.t) ~inject =
  Modal.view
    ~cls:"settings-dialog"
    ~title:"Settings"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button
          ~cls:"ghost shortcuts"
          ~title:"/hotkeys"
          ~on_click:(inject (Action.Run "/hotkeys"))
          [ Node.text "Keyboard shortcuts" ]
      ; button
          ~cls:"primary"
          ~on_click:(inject Action.Close_dialog)
          [ Node.text "Done" ]
      ]
    [ section "In this browser" [ notifications m ~inject; verbosity m ~inject ]
    ; section "On the backend" [ confirm m ~inject; providers m ~inject ]
    ; section "Account" (account m ~inject)
    ]
;;
