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

let group ?title rows =
  Node.section
    ~attrs:[ Attr.class_ "settings-group" ]
    (match title with
     | Some title -> Node.h4 [ Node.text title ] :: rows
     | None -> rows)
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

let tabs (m : App.Model.t) ~inject =
  let on = m.tabs.enabled in
  row
    ~cls:"tabs"
    ~name:"Tabs"
    ~about:
      "Show the open sessions as tabs over the chat (not on a phone). Saved \
       for your account."
    [ switch
        ~label:"Tabs"
        ~title:
          (if on
           then "Turn session tabs off (/tabs off)"
           else "Turn session tabs on (/tabs on)")
        ~on
        ~on_click:(inject (Action.Run (if on then "/tabs off" else "/tabs on")))
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

module Compaction_command = Prigh_ui.Compaction_command

let compact_at (m : App.Model.t) (config : Config.t option) ~inject =
  let off = Option.exists config ~f:(fun c -> c.compact_at_tokens <= 0) in
  let error = Compact_at.error m.compact_at in
  let value =
    match Compact_at.draft m.compact_at, config with
    | Some draft, _ -> draft
    | None, Some c when c.compact_at_tokens > 0 ->
      Compaction_command.tokens c.compact_at_tokens
    | None, _ -> ""
  in
  let disabled = Option.is_none config in
  row
    ~cls:"compact-at"
    ~name:"Compact at"
    ~about:
      "Also summarise once the context passes this many tokens (default 200k), \
       not only at 80% of the model's window."
    ?note:
      (match error, config with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some ("muted", "Waiting for the backend's settings…")
       | None, Some _ when off ->
         Some
           ("muted", "Off: only at 80% of the window. Type a number to set one.")
       | None, Some _ -> None)
    [ Node.div
        ~attrs:
          [ Attr.class_ "segmented"
          ; Attr.role "group"
          ; Attr.create "aria-label" "Compact at"
          ]
        [ Node.label
            ~attrs:
              [ classes [ "segment"; "compact-at-field" ] [ "on", not off ] ]
            [ Node.input
                ~attrs:
                  [ Attr.id "compact-at"
                  ; Attr.class_ "compact-at-input"
                  ; Attr.type_ "text"
                  ; Attr.value value
                  ; Attr.placeholder (if off then "150k" else "200k")
                  ; Attr.title "/compaction at TOKENS, e.g. 150k or 200000"
                  ; Attr.create "aria-label" "Compact at, in tokens"
                  ; Attr.create
                      "aria-invalid"
                      (Bool.to_string (Option.is_some error))
                  ; Attr.create "autocomplete" "off"
                  ; Attr.create "spellcheck" "false"
                  ; (if disabled then Attr.disabled else Attr.empty)
                  ; Attr.on_input (fun _ text ->
                      inject (Action.Compact_at_input text))
                  ; Attr.on_change (fun _ _ -> inject Action.Compact_at_commit)
                  ]
                ()
            ; span ~cls:"compact-at-unit" "tokens"
            ]
        ; Node.button
            ~attrs:
              [ classes [ "segment" ] [ "on", off ]
              ; Attr.type_ "button"
              ; Attr.create "aria-pressed" (Bool.to_string off)
              ; Attr.title
                  "Only at 80% of the model's window (/compaction at off)"
              ; (if disabled
                 then Attr.disabled
                 else
                   Attr.on_click (fun _ ->
                     inject (Action.Run "/compaction at off")))
              ]
            [ Node.text "Off" ]
        ]
    ]
;;

let summary_model (m : App.Model.t) (config : Config.t option) ~inject =
  let current =
    match Option.bind config ~f:(fun c -> c.compaction_model) with
    | None -> "The conversation's model"
    | Some key ->
      (match
         List.find m.models ~f:(fun (model : Llm.t) ->
           String.equal model.key key)
       with
       | Some model -> model.name
       | None -> key)
  in
  row
    ~cls:"summary-model"
    ~name:"Summary model"
    ~about:
      "The model that writes the summaries: a cheaper one costs less, but \
       cannot reuse the conversation's prompt cache."
    [ button
        ~cls:"small summary-model-choice"
        ~title:"Choose the model (/compaction model)"
        ~label:("Summary model: " ^ current)
        ~disabled:(Option.is_none config)
        ~on_click:(inject (Action.Run "/compaction model"))
        [ Node.text current ]
    ]
;;

let mask (config : Config.t option) ~inject =
  let on = Option.exists config ~f:(fun c -> c.mask_observations) in
  row
    ~cls:"mask"
    ~name:"Mask old tool outputs"
    ~about:
      "Replace tool outputs older than the last 10 turns with placeholders \
       first, which often saves summarising."
    [ switch
        ~disabled:(Option.is_none config)
        ~label:"Mask old tool outputs"
        ~title:
          (if on
           then "Keep old tool outputs (/compaction mask off)"
           else "Mask old tool outputs (/compaction mask on)")
        ~on
        ~on_click:
          (inject
             (Action.Run
                (if on then "/compaction mask off" else "/compaction mask on")))
        ()
    ]
;;

let compaction (m : App.Model.t) ~inject =
  [ compact_at m m.config ~inject
  ; summary_model m m.config ~inject
  ; mask m.config ~inject
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

let git_host (m : App.Model.t) (a : Forge_account.t) ~inject =
  let run command = inject (Action.Run command) in
  let login = "/login " ^ Forge_account.login_arg a in
  let note =
    match a.oauth_note with
    | Some note when a.oauth || Forge_account.signed_in a ->
      Some ("muted", String.capitalize note)
    | Some note -> Some ("warm", String.capitalize note)
    | None -> None
  in
  div
    ~cls:"setting git-host"
    [ div
        ~cls:"setting-text"
        [ div
            ~cls:"git-host-name"
            [ span ~cls:"setting-name" a.label
            ; span ~cls:"setting-about host" a.host
            ]
        ; span ~cls:"setting-about" (String.capitalize (Forges.describe a) ^ ".")
        ; (match note with
           | Some (level, text) -> span ~cls:("setting-note " ^ level) text
           | None -> Node.none)
        ]
    ; div
        ~cls:"setting-control"
        (if Set.mem m.forges.signing_out a.host
         then
           [ button
               ~cls:"small"
               ~disabled:true
               ~attrs:[ Attr.create "aria-busy" "true" ]
               ~on_click:Effect.Ignore
               [ Html.lamp (); Node.text "Disconnecting…" ]
           ]
         else if Forge_account.signed_in a
         then
           [ button
               ~cls:"small"
               ~title:("/logout " ^ a.host)
               ~label:("Disconnect " ^ Forges.name a)
               ~on_click:(run ("/logout " ^ a.host))
               [ Node.text "Disconnect" ]
           ]
         else if a.oauth
         then
           [ button
               ~cls:"small ghost"
               ~title:(login ^ " token")
               ~label:("Connect " ^ Forges.name a ^ " with a token")
               ~on_click:(run (login ^ " token"))
               [ Node.text "Use a token" ]
           ; button
               ~cls:"small primary"
               ~title:login
               ~label:("Connect " ^ Forges.name a)
               ~on_click:(run login)
               [ Node.text "Connect" ]
           ]
         else
           [ button
               ~cls:"small primary"
               ~title:(login ^ " token")
               ~label:("Connect " ^ Forges.name a ^ " with a token")
               ~on_click:(run (login ^ " token"))
               [ Node.text "Connect with a token" ]
           ])
    ]
;;

let git_hosts (m : App.Model.t) ~inject =
  let another =
    row
      ~cls:"git-host-add"
      ~name:"Another Forgejo host"
      ~about:"A self-hosted Forgejo or Gitea, by its host name."
      [ button
          ~cls:"small"
          ~title:"/login forgejo HOST"
          ~on_click:(inject (Action.Run "/login forgejo"))
          [ Node.text "Add host…" ]
      ]
  in
  match m.forges.accounts with
  | Loading ->
    [ row
        ~name:"Git hosts"
        ~about:"GitHub, Codeberg and the Forgejo hosts you sign in to."
        ~note:("muted", "Waiting for the backend…")
        []
    ]
  | Failed e ->
    [ row
        ~name:"Git hosts"
        ~about:"This backend can't sign in to git hosts."
        ~note:("warm", String.capitalize e)
        []
    ]
  | Loaded accounts ->
    List.map accounts ~f:(fun a -> git_host m a ~inject) @ [ another ]
;;

module Nix_gc_command = Prigh_ui.Nix_gc_command

(* The days of profile generations kept: the schedule's, or two weeks. *)
let keep_days (n : Nix_gc.t) =
  match n.status with
  | Loaded { schedule = Some { keep_days = Some days; _ }; _ } when days > 0 ->
    days
  | _ -> 14
;;

let nix_store (m : App.Model.t) ~inject =
  let run command = inject (Action.Run command) in
  let n = m.nix_gc in
  let about =
    "The packages agents fetch with Nix, on the shared /nix volume."
  in
  let refresh label =
    button
      ~cls:"small ghost"
      ~title:"Read the store again (/nix-gc)"
      ~on_click:(run "/nix-gc")
      [ Node.text label ]
  in
  let store =
    match n.status with
    | Unknown | Loading ->
      row
        ~cls:"nix-store"
        ~name:"Disk usage"
        ~about
        ~note:("muted", "Reading the store…")
        []
    | Failed e ->
      row
        ~cls:"nix-store"
        ~name:"Disk usage"
        ~about
        ~note:("warm", String.capitalize e)
        [ refresh "Retry" ]
    | Loaded s ->
      div
        ~cls:"setting nix-store"
        [ div
            ~cls:"setting-text"
            ([ span ~cls:"setting-name" "Disk usage"
             ; span
                 ~cls:"setting-about"
                 (String.capitalize
                    (sprintf
                       "%s; %s."
                       (Nix_gc_command.size s)
                       (Nix_gc_command.space s)))
             ]
             @ List.map
                 (Nix_gc_command.details ~utc_offset:m.utc_offset s)
                 ~f:(fun line ->
                   span ~cls:"setting-note muted" (String.capitalize line)))
        ; div ~cls:"setting-control" [ refresh "Refresh" ]
        ]
  in
  let days = keep_days n in
  let collect =
    row
      ~cls:"nix-gc"
      ~name:"Collect garbage"
      ~about:
        "Delete the store paths no profile or GC root uses. Deleting users' \
         old profile generations first frees more, but they can no longer roll \
         back to them."
      ?note:
        (match n.collecting, n.outcome with
         | Some p, _ ->
           Some
             ( "muted"
             , String.concat
                 ~sep:" · "
                 (("Collecting: " ^ Nix_gc_command.deleted_so_far p)
                  :: Option.to_list p.message) )
         | None, Some (Ok report) ->
           Some ("muted", String.capitalize (Nix_gc_command.report report))
         | None, Some (Error e) -> Some ("warm", String.capitalize e)
         | None, None -> None)
      (match n.collecting with
       | Some _ ->
         [ button
             ~cls:"small"
             ~disabled:true
             ~attrs:[ Attr.create "aria-busy" "true" ]
             ~on_click:Effect.Ignore
             [ Html.lamp (); Node.text "Collecting…" ]
         ]
       | None ->
         let deleting = sprintf "/nix-gc run %d" days in
         [ button
             ~cls:"small ghost"
             ~title:deleting
             ~on_click:(run deleting)
             [ Node.text (sprintf "Also delete generations over %d days…" days)
             ]
         ; button
             ~cls:"small primary"
             ~title:"/nix-gc run"
             ~on_click:(run "/nix-gc run")
             [ Node.text "Collect" ]
         ])
  in
  [ store; collect ]
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

module Section = Settings_section

(* A section's settings, in groups: a new setting is a row in its list. *)
let content (m : App.Model.t) (section : Section.t) ~inject =
  match section with
  | Appearance ->
    [ group (Theme_view.rows m ~inject @ [ tabs m ~inject; verbosity m ~inject ])
    ]
  | Behaviour -> [ group [ confirm m ~inject; notifications m ~inject ] ]
  | Models ->
    [ group ~title:"Providers" [ providers m ~inject ]
    ; group ~title:"Compaction" (compaction m ~inject)
    ]
  | Git_hosting -> [ group (git_hosts m ~inject) ]
  | Nix_store -> [ group (nix_store m ~inject) ]
  | Account -> [ group (account m ~inject) ]
;;

let section_icon : Section.t -> Icon.t = function
  | Appearance -> Palette
  | Behaviour -> Shield
  | Models -> Brain
  | Git_hosting -> Repo
  | Nix_store -> Package
  | Account -> User
;;

(* The sections down the side (a strip over the page on a phone), as tabs:
   the arrows move between them. The account's tab, last, is the user. *)
let nav (m : App.Model.t) ~current ~inject =
  let tab (section : Section.t) =
    let selected = Section.equal section current in
    let face =
      match section, Account_view.who m with
      | Account, Some who ->
        [ span ~cls:"avatar" (Account_view.initial who.name)
        ; span ~cls:"settings-tab-name" who.name
        ]
      | _ ->
        [ icon (section_icon section)
        ; span ~cls:"settings-tab-name" (Section.name section)
        ]
    in
    Node.button
      ~attrs:
        [ classes
            [ "settings-tab"; "settings-tab-" ^ Section.arg section ]
            [ "on", selected ]
        ; Attr.id (Section.tab_id section)
        ; Attr.type_ "button"
        ; Attr.role "tab"
        ; Attr.create "data-settings-tab" (Section.arg section)
        ; Attr.create "aria-selected" (Bool.to_string selected)
        ; Attr.create "aria-controls" "settings-panel"
        ; Attr.create "aria-label" (Section.name section)
        ; Attr.title ("/settings " ^ Section.arg section)
        ; Attr.tabindex (if selected then 0 else -1)
        ; Attr.on_click (fun _ -> inject (Action.Settings_section section))
        ]
      face
  in
  Node.div
    ~attrs:
      [ Attr.class_ "settings-nav"
      ; Attr.role "tablist"
      ; Attr.create "aria-label" "Settings sections"
      ; Attr.create
          "aria-orientation"
          (if m.narrow then "horizontal" else "vertical")
      ]
    (List.map (Section.available ~superuser:(App.superuser m)) ~f:tab)
;;

let view (m : App.Model.t) ~inject =
  let current =
    Section.visible m.settings_section ~superuser:(App.superuser m)
  in
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
    [ nav m ~current ~inject
    ; Node.div
        ~key:(Section.arg current)
        ~attrs:
          [ classes
              [ "settings-panel"; "settings-panel-" ^ Section.arg current ]
              []
          ; Attr.id "settings-panel"
          ; Attr.role "tabpanel"
          ; Attr.create "aria-labelledby" (Section.tab_id current)
          ]
        (Node.header
           ~attrs:[ Attr.class_ "settings-panel-head" ]
           [ Node.h3 [ Node.text (Section.name current) ]
           ; Node.p [ Node.text (Section.about current) ]
           ]
         :: content m current ~inject)
    ]
;;
