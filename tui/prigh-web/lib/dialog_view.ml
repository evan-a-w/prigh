open! Core
open! Import
open Html
module Action = App.Action

let keys_hint text = span ~cls:"keys-hint" text

let picker_title : Dialog.Picker_kind.t -> string = function
  | Models -> "Models"
  | Thinking -> "Thinking"
  | Login -> "Providers"
  | Logout -> "Providers"
;;

let picker ~kind (p : Picker.t) ~inject =
  let visible = Picker.visible p in
  Modal.view
    ~cls:"picker-dialog"
    ~title:(Picker.title p)
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_hint "↑↓ move · Enter choose · Esc close" ]
    [ div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "picker-input"
              ; Attr.type_ "text"
              ; Attr.placeholder
                  ("Filter " ^ String.lowercase (picker_title kind) ^ "…")
              ; Attr.value (Picker.query p)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Picker_query text))
              ]
            ()
        ]
    ; (match visible with
       | [] ->
         div
           ~cls:"picker-empty"
           [ Node.textf
               "Nothing matches “%s”: Backspace widens the search."
               (Picker.query p)
           ]
       | visible ->
         div
           ~cls:"picker-items"
           ~attrs:[ Attr.role "listbox" ]
           (List.mapi visible ~f:(fun i (item : Picker.Item.t) ->
              Node.div
                ~attrs:
                  [ classes
                      [ "picker-item" ]
                      [ "selected", i = Picker.selected p
                      ; "marked", item.marked
                      ; "dimmed", item.dimmed
                      ]
                  ; Attr.role "option"
                  ; Attr.on_click (fun _ ->
                      inject (Action.Picker_choose item.id))
                  ]
                [ span ~cls:"picker-label" item.label
                ; (if String.is_empty item.detail
                   then Node.none
                   else span ~cls:"picker-detail" item.detail)
                ; (if item.marked
                   then span ~cls:"picker-check" "✓"
                   else Node.none)
                ])))
    ]
;;

let help ~inject =
  let table rows =
    Node.table
      ~attrs:[ Attr.class_ "help-table" ]
      [ Node.tbody
          (List.map rows ~f:(fun (key, what) ->
             Node.tr
               [ Node.td [ Node.kbd [ Node.text key ] ]
               ; Node.td [ Node.text what ]
               ]))
      ]
  in
  Modal.view
    ~cls:"help-dialog"
    ~title:"Commands and keys"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button
          ~cls:"primary"
          ~on_click:(inject Action.Close_dialog)
          [ Node.text "Done" ]
      ]
    [ Node.h3 [ Node.text "Keys" ]
    ; table Keys.help
    ; Node.h3 [ Node.text "Commands" ]
    ; table
        (List.map Slash.all ~f:(fun (s : Slash.Spec.t) ->
           String.strip ("/" ^ s.name ^ " " ^ s.args), s.help))
    ]
;;

let text_input ?(kind = "text") ?(placeholder = "") ~value ~inject () =
  Node.input
    ~attrs:
      [ Attr.id "dialog-input"
      ; Attr.class_ "text-input"
      ; Attr.type_ kind
      ; Attr.placeholder placeholder
      ; Attr.value value
      ; Attr.create "autocomplete" "off"
      ; Attr.create "spellcheck" "false"
      ; Attr.on_input (fun _ text -> inject (Action.Dialog_input text))
      ]
    ()
;;

let cancel_accept ?(danger = false) ~inject label =
  [ button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
  ; button
      ~cls:(if danger then "danger" else "primary")
      ~on_click:(inject Action.Dialog_accept)
      [ Node.text label ]
  ]
;;

let rename name ~inject =
  Modal.view
    ~title:"Rename session"
    ~on_close:(inject Action.Close_dialog)
    ~footer:(cancel_accept ~inject "Rename")
    [ text_input ~placeholder:"Session name" ~value:name ~inject () ]
;;

let delete ~title ~inject =
  Modal.view
    ~title:"Delete session?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:(cancel_accept ~danger:true ~inject "Delete")
    [ Node.p [ Node.textf "“%s” will be deleted. This can't be undone." title ]
    ]
;;

let login (m : App.Model.t) (flow : Login_flow.t) ~inject =
  let title =
    match
      List.find m.auth ~f:(fun s -> String.equal s.provider flow.provider)
    with
    | Some s -> "Log in to " ^ s.name
    | None when String.is_empty flow.provider -> "Log in"
    | None -> "Log in to " ^ flow.provider
  in
  let url =
    match flow.url with
    | None -> Node.none
    | Some (url, instructions) ->
      div
        ~cls:"login-url"
        [ (if String.is_empty instructions
           then Node.none
           else Node.p [ Node.text instructions ])
        ; Node.a
            ~attrs:
              [ Attr.href url
              ; Attr.target "_blank"
              ; Attr.create "rel" "noopener noreferrer"
              ; Attr.class_ "btn primary link-button"
              ]
            [ Node.text "Open the login page"; icon External ]
        ; div ~cls:"login-url-text" [ Node.text url ]
        ]
  in
  let progress =
    match flow.progress with
    | [] -> Node.none
    | lines ->
      Node.ul
        ~attrs:[ Attr.class_ "login-progress" ]
        (List.map lines ~f:(fun l -> Node.li [ Node.text l ]))
  in
  let prompt =
    match flow.prompt with
    | None -> Node.none
    | Some (_, prompt) ->
      div
        ~cls:"login-prompt"
        [ Node.label [ Node.text (Auth_event.Prompt.message prompt) ]
        ; (match prompt with
           | Secret _ ->
             text_input ~kind:"password" ~value:flow.input ~inject ()
           | Manual_code { placeholder; _ } ->
             text_input ~placeholder ~value:flow.input ~inject ()
           | Select { options; _ } ->
             div
               ~cls:"picker-items"
               ~attrs:[ Attr.role "listbox" ]
               (List.mapi options ~f:(fun i (_, label) ->
                  Node.div
                    ~attrs:
                      [ classes
                          [ "picker-item" ]
                          [ "selected", i = flow.selected ]
                      ; Attr.role "option"
                      ; Attr.on_click (fun _ -> inject (Action.Login_choose i))
                      ]
                    [ span ~cls:"picker-label" label ])))
        ]
  in
  let status, footer =
    match flow.failed, flow.prompt with
    | Some error, _ ->
      ( div
          ~cls:"login-failed"
          [ Node.textf "Login failed: %s. /login tries again." error ]
      , [ button
            ~cls:"primary"
            ~on_click:(inject Action.Dialog_accept)
            [ Node.text "Close" ]
        ] )
    | None, Some _ -> Node.none, cancel_accept ~inject "Continue"
    | None, None ->
      ( div
          ~cls:"login-waiting"
          [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
          ; Node.text "Waiting for the provider…"
          ]
      , [ button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ] ]
      )
  in
  Modal.view
    ~cls:"login-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer
    [ url; progress; prompt; status ]
;;

let auth (statuses : Auth_status.t list) ~inject =
  Modal.view
    ~cls:"auth-dialog"
    ~title:"Providers"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "/login and /logout pick a provider"
      ; button
          ~cls:"primary"
          ~on_click:(inject Action.Close_dialog)
          [ Node.text "Done" ]
      ]
    [ (match statuses with
       | [] ->
         div ~cls:"picker-empty" [ Node.text "The backend knows no providers." ]
       | statuses ->
         div
           ~cls:"auth-list"
           (List.map statuses ~f:(fun s ->
              div
                ~cls:"auth-row"
                [ div
                    ~cls:"auth-name"
                    [ span ~cls:"picker-label" s.name
                    ; span
                        ~cls:"picker-detail"
                        (match s.configured with
                         | Some c ->
                           sprintf "logged in with %s (%s)" c.method_ c.source
                         | None -> "not logged in")
                    ]
                ; (match s.configured with
                   | Some _ ->
                     button
                       ~cls:"small"
                       ~on_click:(inject (Action.Logout s.provider))
                       [ Node.text "Log out" ]
                   | None ->
                     button
                       ~cls:"small primary"
                       ~on_click:(inject (Action.Start_login s.provider))
                       [ Node.text "Log in" ])
                ])))
    ]
;;

let agents (m : App.Model.t) ~inject =
  let subagents, jobs =
    match m.state with
    | Some s -> s.subagents, s.jobs
    | None -> [], []
  in
  let row ~cls ~title ~status ~running ~on_stop ~stop_label =
    div
      ~cls:("bg-row " ^ cls)
      [ div
          ~cls:"bg-text"
          [ span ~cls:"bg-title" title
          ; span
              ~cls:(if running then "bg-status running" else "bg-status")
              status
          ]
      ; (if running
         then button ~cls:"small" ~on_click:on_stop [ Node.text stop_label ]
         else Node.none)
      ]
  in
  Modal.view
    ~title:"Background work"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button
          ~cls:"primary"
          ~on_click:(inject Action.Close_dialog)
          [ Node.text "Done" ]
      ]
    (match subagents, jobs with
     | [], [] ->
       [ div
           ~cls:"picker-empty"
           [ Node.text "No subagents or jobs in this session." ]
       ]
     | _ ->
       List.map subagents ~f:(fun (a : State.Subagent.t) ->
         row
           ~cls:"subagent-row"
           ~title:a.task
           ~status:(if a.running then "running" else "done, reporting")
           ~running:a.running
           ~on_stop:(inject (Action.Cancel_subagent a.id))
           ~stop_label:"Cancel")
       @ List.map jobs ~f:(fun (j : State.Job.t) ->
         row
           ~cls:"job-row"
           ~title:j.command
           ~status:
             (if j.running
              then "running"
              else Option.value j.exit ~default:"finished")
           ~running:j.running
           ~on_stop:(inject (Action.Kill_job j.id))
           ~stop_label:"Kill"))
;;

let dialog (m : App.Model.t) ~inject =
  match m.dialog with
  | None -> Node.none
  | Some (Picker { kind; picker = p }) -> picker ~kind p ~inject
  | Some Help -> help ~inject
  | Some (Rename name) -> rename name ~inject
  | Some (Delete { title; _ }) -> delete ~title ~inject
  | Some (Login flow) -> login m flow ~inject
  | Some (Auth statuses) -> auth statuses ~inject
  | Some Agents -> agents m ~inject
;;

let confirm (m : App.Model.t) ~inject =
  match m.confirms with
  | [] -> Node.none
  | c :: rest ->
    let respond allow =
      inject (Action.Respond_confirm { call_id = c.call_id; allow })
    in
    Modal.view
      ~id:"confirm"
      ~cls:"confirm-dialog"
      ~title:(sprintf "Allow %s?" c.name)
      ~on_close:(respond false)
      ~footer:
        [ (match rest with
           | [] -> Node.none
           | rest -> keys_hint (sprintf "%d more waiting" (List.length rest)))
        ; button
            ~cls:"deny"
            ~title:"Esc"
            ~on_click:(respond false)
            [ Node.text "Deny" ]
        ; button
            ~cls:"primary allow"
            ~title:"Enter"
            ~on_click:(respond true)
            [ Node.text "Allow" ]
        ]
      [ Node.pre
          ~attrs:[ Attr.class_ "confirm-summary" ]
          [ Node.text c.summary ]
      ]
;;

let view m ~inject = Node.fragment [ dialog m ~inject; confirm m ~inject ]
