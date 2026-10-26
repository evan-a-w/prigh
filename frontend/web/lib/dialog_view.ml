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
  | Verbosity -> "Levels"
  | Confirm_tools -> "Settings"
  | Modes -> "Modes"
  | Fork _ | Rewind _ -> "Messages"
  | Tree -> "Messages"
  | Hosts -> "Hosts"
  | Users -> "Users"
  | Accounts -> "Accounts"
  | Skills -> "Skills"
  | Snippets -> "Snippets"
  | Reminders -> "Reminders"
  | Mcp _ -> "Servers"
  | Jump -> "Messages"
  | Worktrees _ -> "Worktrees"
  | Themes -> "Themes"
  | Accents -> "Colours"
;;

(* What is wrong with the MCP configuration, under its servers. *)
let mcp_problems : Dialog.Picker_kind.t -> Node.t = function
  | Mcp { problems = _ :: _ as problems; _ } ->
    div
      ~cls:"mcp-problems"
      [ Node.h3 [ Node.text "Configuration problems" ]
      ; Node.ul (List.map problems ~f:(fun p -> Node.li [ Node.text p ]))
      ]
  | _ -> Node.none
;;

let picker ~kind (p : Picker.t) ~inject =
  let visible = Picker.visible p in
  Modal.view
    ~cls:
      (match kind with
       | Dialog.Picker_kind.Tree -> "picker-dialog tree-dialog"
       | Mcp _ -> "picker-dialog mcp-dialog"
       | _ -> "picker-dialog")
    ~title:(Picker.title p)
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint
          (match kind with
           | Reminders ->
             "↑↓ to move, Enter to show, Delete to remove, Esc to close"
           | Worktrees _ ->
             "Enter works there, Alt+Enter in a new session, Alt+R reviews, \
              Delete removes, Esc closes"
           | _ -> "↑↓ to move, Enter to choose, Esc to close")
      ]
    [ div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "picker-input"
              ; Attr.type_ "text"
              ; Attr.create
                  "aria-label"
                  ("Filter " ^ String.lowercase (picker_title kind))
              ; Attr.role "combobox"
              ; Attr.create "aria-controls" "picker-list"
              ; Attr.create "aria-expanded" "true"
              ; Attr.create "aria-autocomplete" "list"
              ; (if List.is_empty visible
                 then Attr.empty
                 else
                   Attr.create
                     "aria-activedescendant"
                     (sprintf "picker-option-%d" (Picker.selected p)))
              ; Attr.placeholder
                  ("Filter " ^ String.lowercase (picker_title kind) ^ "…")
              ; Attr.value (Picker.query p)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Picker_query text))
              ]
            ()
        ]
    ; (match visible, kind with
       | [], Mcp { servers = []; _ } ->
         div ~cls:"picker-empty" [ Node.text Mcp_servers.none ]
       | [], _ ->
         div
           ~cls:"picker-empty"
           [ Node.textf
               "Nothing matches “%s”: Backspace widens the search."
               (Picker.query p)
           ]
       | visible, _ ->
         div
           ~cls:"picker-items"
           ~attrs:
             [ Attr.role "listbox"
             ; Attr.id "picker-list"
             ; Attr.create "aria-label" (Picker.title p)
             ]
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
                  ; Attr.id (sprintf "picker-option-%d" i)
                  ; Attr.create
                      "aria-selected"
                      (Bool.to_string (i = Picker.selected p))
                  ; (if item.marked
                     then Attr.create "aria-checked" "true"
                     else Attr.empty)
                  ; Attr.on_click (fun _ ->
                      inject (Action.Picker_choose item.id))
                  ]
                ((match kind with
                  | Accents ->
                    Option.to_list
                      (Option.map
                         (Theme.Accent.of_string item.id)
                         ~f:Theme_view.swatch)
                  | Themes ->
                    Option.to_list
                      (Option.map
                         (Theme.Name.of_string item.id)
                         ~f:Theme_view.sample)
                  | _ -> [])
                 @ [ span ~cls:"picker-label" item.label
                   ; (if String.is_empty item.detail
                      then Node.none
                      else span ~cls:"picker-detail" item.detail)
                   ; (if item.marked
                      then Html.mark ~cls:"picker-check" Check
                      else Node.none)
                   ]
                 @
                 match kind with
                 | Reminders ->
                   [ button
                       ~cls:"small picker-remove"
                       ~title:"Delete"
                       ~label:("Remove reminder " ^ item.id)
                       ~on_click:
                         (Effect.Many
                            [ Effect.Stop_propagation
                            ; inject (Action.Remove_reminder item.id)
                            ])
                       [ Node.text "Remove" ]
                   ]
                 | _ -> []))))
    ; mcp_problems kind
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
           Slash.Spec.usage s, s.help))
    ; Node.h3 [ Node.text "Left to the browser" ]
    ; table Keys.browser
    ]
;;

let text_input ?(kind = "text") ?(placeholder = "") ~label ~value ~inject () =
  Node.input
    ~attrs:
      [ Attr.id "dialog-input"
      ; Attr.create "aria-label" label
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
    [ text_input
        ~placeholder:"Session name"
        ~label:"Session name"
        ~value:name
        ~inject
        ()
    ]
;;

(* Why the backend will keep a session, if it will. *)
let kept = App.Model.kept_from_deletion

let delete (m : App.Model.t) (target : Dialog.Delete_target.t) ~inject =
  let sessions =
    match target with
    | Sessions paths ->
      List.filter m.sessions ~f:(fun s ->
        List.mem paths s.path ~equal:String.equal)
    | Archived -> List.filter m.sessions ~f:(fun s -> s.archived)
  in
  let deleted = List.filter sessions ~f:(fun s -> Option.is_none (kept m s)) in
  let count = List.length deleted in
  let size =
    Session_summary.size_string (Session_summary.total_bytes deleted)
  in
  let title =
    match target, sessions with
    | _, _ when count = 0 && List.length sessions = 1 ->
      "Can't delete this session"
    | _, _ when count = 0 -> "Can't delete these sessions"
    | Archived, _ -> sprintf "Delete all %d archived sessions?" count
    | Sessions _, [ _ ] -> "Delete session?"
    | Sessions _, _ -> sprintf "Delete %d sessions?" count
  in
  let shown = 6 in
  let row (s : Session_summary.t) =
    Node.li
      ~attrs:[ classes [ "delete-row" ] [ "kept", Option.is_some (kept m s) ] ]
      [ span ~cls:"delete-title" (Session_list.title s)
      ; span ~cls:"delete-size" (Session_summary.size_string s.bytes)
      ]
  in
  let warnings =
    List.filter_map sessions ~f:(fun s ->
      Option.map (kept m s) ~f:(fun why ->
        Node.li [ Node.textf "“%s” %s" (Session_list.title s) why ]))
  in
  Modal.view
    ~cls:"delete-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      (if count = 0
       then
         [ button
             ~cls:"primary"
             ~on_click:(inject Action.Close_dialog)
             [ Node.text "Close" ]
         ]
       else
         cancel_accept
           ~danger:true
           ~inject
           (if count = 1 then "Delete" else sprintf "Delete %d" count))
    [ (if count = 0
       then Node.none
       else
         Node.p
           [ Node.textf
               "%s, with %s subagent logs: %s on disk. This can't be undone."
               (if count = 1
                then "It will be deleted"
                else sprintf "They will be deleted")
               (if count = 1 then "its" else "their")
               size
           ])
    ; Node.ul
        ~attrs:[ Attr.class_ "delete-list" ]
        (List.map (List.take sessions shown) ~f:row
         @
         if count > shown
         then
           [ Node.li
               ~attrs:[ Attr.class_ "delete-more" ]
               [ Node.textf "and %d more" (count - shown) ]
           ]
         else [])
    ; (match warnings with
       | [] -> Node.none
       | warnings -> Node.ul ~attrs:[ Attr.class_ "delete-warnings" ] warnings)
    ]
;;

let login (m : App.Model.t) (flow : Login_flow.t) ~inject =
  let title =
    match
      ( flow.purpose
      , List.find m.auth ~f:(fun s -> String.equal s.provider flow.provider) )
    with
    | _ when Option.is_some flow.forge ->
      "Sign in to "
      ^ Option.value_map
          (Forges.find m.forges flow.provider)
          ~default:flow.provider
          ~f:Forges.name
    | Logout, Some s -> "Log out of " ^ s.name
    | Logout, None -> "Log out of " ^ flow.provider
    | Login, Some { custom = Some _; name; _ } -> "Edit " ^ name
    | Login, Some s -> "Log in to " ^ s.name
    | Login, None when String.is_empty flow.provider -> "Log in"
    | Login, None when String.equal flow.provider "custom" ->
      "Add a custom provider"
    | Login, None -> "Log in to " ^ flow.provider
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
        ; (match Login_flow.device_code flow with
           | None -> Node.none
           | Some code ->
             div
               ~cls:"device-code copyable"
               [ Node.code
                   ~attrs:
                     [ Attr.class_ "copy-text"
                     ; Attr.create "aria-label" ("Your code: " ^ code)
                     ]
                   [ Node.text code ]
               ; Node.button
                   ~attrs:
                     [ Attr.class_ "btn small copy"
                     ; Attr.type_ "button"
                     ; Attr.title "Copy the code"
                     ]
                   [ Node.text "Copy" ]
               ])
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
        [ div
            ~cls:"login-message"
            (List.map
               (String.split_lines (Auth_event.Prompt.message prompt))
               ~f:(fun line -> Node.p [ Node.text line ]))
        ; (match prompt with
           | Secret _ ->
             text_input
               ~kind:"password"
               ~label:"Secret"
               ~value:flow.input
               ~inject
               ()
           | Manual_code { placeholder; _ } | Text { placeholder; _ } ->
             text_input
               ~placeholder
               ~label:
                 (List.hd
                    (String.split_lines (Auth_event.Prompt.message prompt))
                  |> Option.value ~default:"Answer")
               ~value:flow.input
               ~inject
               ()
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
          [ (match flow.purpose, flow.forge with
             | Login, Some again ->
               Node.textf
                 "Couldn't sign in to %s: %s. /login %s tries again."
                 flow.provider
                 error
                 again
             | Login, None ->
               Node.textf "Login failed: %s. /login tries again." error
             | Logout, _ ->
               Node.textf "Logout failed: %s. /logout tries again." error)
          ]
      , [ button
            ~cls:"primary"
            ~on_click:(inject Action.Dialog_accept)
            [ Node.text "Close" ]
        ] )
    | None, Some _ -> Node.none, cancel_accept ~inject "Continue"
    | None, None ->
      ( div
          ~cls:"login-waiting"
          [ Html.lamp ()
          ; Node.text
              (match flow.forge with
               | Some _ -> "Waiting for " ^ flow.provider ^ "…"
               | None -> "Waiting for the provider…")
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

let forge_host ~input ~error ~inject =
  Modal.view
    ~title:"Sign in to a Forgejo host"
    ~on_close:(inject Action.Close_dialog)
    ~footer:(cancel_accept ~inject "Continue")
    [ Node.p
        [ Node.text
            "A Forgejo or Gitea instance, by its host name: codeberg.org is \
             listed already."
        ]
    ; text_input
        ~placeholder:"git.example.com"
        ~label:"Host"
        ~value:input
        ~inject
        ()
    ; (match error with
       | Some e ->
         div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
       | None -> Node.none)
    ]
;;

let auth (m : App.Model.t) (statuses : Auth_status.t list) ~inject =
  Modal.view
    ~cls:"auth-dialog"
    ~title:"Providers"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "/login and /logout pick a provider"
      ; button
          ~on_click:(inject (Action.Start_login "custom"))
          [ Node.text "Add a custom provider" ]
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
                        (match s.custom, s.configured with
                         | Some c, configured ->
                           sprintf
                             "custom, %s, %s, %s"
                             c.base_url
                             c.api_label
                             (match configured with
                              | Some { source = "no key"; _ } | None -> "no key"
                              | Some k -> "key: " ^ k.source)
                         | None, Some c ->
                           sprintf "logged in with %s (%s)" c.method_ c.source
                         | None, None -> "not logged in")
                    ]
                ; (match s.custom, s.configured with
                   | Some _, _ ->
                     div
                       ~cls:"auth-buttons"
                       [ button
                           ~cls:"small"
                           ~on_click:(inject (Action.Start_login s.provider))
                           [ Node.text "Edit" ]
                       ; button
                           ~cls:"small"
                           ~on_click:(inject (Action.Logout s.provider))
                           [ Node.text "Remove" ]
                       ]
                   | None, Some _ ->
                     button
                       ~cls:"small"
                       ~on_click:(inject (Action.Logout s.provider))
                       [ Node.text "Log out" ]
                   | None, None ->
                     button
                       ~cls:"small primary"
                       ~on_click:(inject (Action.Start_login s.provider))
                       [ Node.text "Log in" ])
                ])))
    ; div
        ~cls:"auth-git-hosts"
        (Node.h3 [ Node.text "Git hosting" ]
         :: Settings_view.git_hosts m ~inject)
    ]
;;

let dialog (m : App.Model.t) ~inject =
  match m.dialog with
  | None -> Node.none
  | Some (Picker { kind = Accounts; picker = p }) ->
    Account_view.menu m p ~inject
  | Some (Picker { kind; picker = p }) -> picker ~kind p ~inject
  | Some (Popover _) -> Chip_popover_view.backdrop ~inject
  | Some Help -> help ~inject
  | Some Settings -> Settings_view.view m ~inject
  | Some (Rename name) -> rename name ~inject
  | Some (Delete target) -> delete m target ~inject
  | Some (Login flow) -> login m flow ~inject
  | Some (Auth statuses) -> auth m statuses ~inject
  | Some (Forge_host { input; error }) -> forge_host ~input ~error ~inject
  | Some (Pulls l) -> Pull_requests_view.list l ~inject
  | Some (Pull p) -> Pull_requests_view.page m p ~inject
  | Some (Pr_create f) -> Pull_requests_view.create f ~inject
  | Some (Pr_checked_out { number; checkout }) ->
    Pull_requests_view.checked_out ~number checkout ~inject
  | Some dialog ->
    Option.value (Command_dialog_view.view m dialog ~inject) ~default:Node.none
;;

let confirm (m : App.Model.t) ~inject =
  match m.confirms with
  | [] -> Node.none
  | c :: rest ->
    let answer answer =
      inject
        (Action.Answer_confirm
           { call_id = c.call_id; agent_id = c.agent_id; answer })
    in
    let plan = Confirm_prompt.is_plan c in
    let waiting =
      match rest with
      | [] -> []
      | rest -> [ keys_hint (sprintf "%d more waiting" (List.length rest)) ]
    in
    let body, footer, on_close =
      match m.confirm_reason with
      | None ->
        ( [ Node.p
              ~attrs:[ Attr.class_ "confirm-question" ]
              [ Node.text (Confirm_prompt.title c) ]
          ; (if plan
             then div ~cls:"confirm-plan" [ Markdown_view.render c.summary ]
             else Node.none)
          ; div
              ~cls:"confirm-answers"
              (List.map (Confirm_prompt.answers c) ~f:(fun (a, key, label) ->
                 let cls =
                   match a with
                   | Allow -> "primary allow"
                   | Always -> "always"
                   | Deny -> "deny"
                   | Deny_with_reason -> "reason"
                 in
                 button
                   ~cls:("confirm-answer " ^ cls)
                   ~title:
                     (match a with
                      | Allow -> key ^ " or Enter"
                      | Deny -> key ^ " or Esc"
                      | Always | Deny_with_reason -> key)
                   ~label:(String.capitalize label)
                   ~attrs:[ Attr.create "aria-keyshortcuts" key ]
                   ~on_click:(answer a)
                   [ Node.kbd [ Node.text key ]
                   ; Node.text (String.capitalize label)
                   ]))
          ]
        , waiting
        , answer Deny )
      | Some reason ->
        let back = inject Action.Cancel_confirm_reason in
        ( [ Node.p
              ~attrs:[ Attr.class_ "confirm-question" ]
              [ Node.text (Confirm_prompt.title c) ]
          ; Node.label
              ~attrs:
                [ Attr.class_ "confirm-reason-label"
                ; Attr.for_ "confirm-reason"
                ]
              [ Node.text (Confirm_prompt.reason_question c) ]
          ; Node.input
              ~attrs:
                [ Attr.id "confirm-reason"
                ; Attr.class_ "text-input"
                ; Attr.type_ "text"
                ; Attr.value reason
                ; Attr.create "autocomplete" "off"
                ; Attr.on_input (fun _ text ->
                    inject (Action.Confirm_reason_input text))
                ]
              ()
          ]
        , waiting
          @ [ button ~title:"Esc" ~on_click:back [ Node.text "Back" ]
            ; button
                ~cls:"danger"
                ~title:"Enter"
                ~on_click:(inject Action.Send_confirm_reason)
                [ Node.text (if plan then "Keep planning" else "Deny") ]
            ]
        , back )
    in
    Modal.view
      ~id:"confirm"
      ~cls:(if plan then "confirm-dialog plan-dialog" else "confirm-dialog")
      ~title:(if plan then "Plan" else "Confirm")
      ~on_close
      ~footer
      body
;;

let view m ~inject =
  Node.fragment
    [ dialog m ~inject
    ; (if List.is_empty m.confirms
       then
         Question_view.view
           m.questions
           ~utc_offset:m.utc_offset
           ~inject:(fun a -> inject (Action.Question a))
       else Node.none)
    ; confirm m ~inject
    ]
;;
