open! Core
open! Import
open Html
module Action = App.Action
open Settings_row
module Mcp_command = Prigh_ui.Mcp_command

let host_name (m : App.Model.t) =
  match m.state with
  | None -> "this host"
  | Some state ->
    (match Prigh_ui.Active_host.of_state state with
     | Connected h -> h.name
     | Disconnected { name; _ } -> name ^ " (offline)"
     | Unset -> "the backend")
;;

let home (m : App.Model.t) =
  match m.state with
  | None -> None
  | Some state ->
    (match Prigh_ui.Active_host.of_state state with
     | Connected h -> h.home
     | Disconnected _ | Unset -> None)
;;

let plural n word = if n = 1 then sprintf "1 %s" word else sprintf "%d %ss" n word

let tools_list (tools : Mcp_server.Tool.t list) =
  Node.ul
    ~attrs:[ Attr.class_ "mcp-tools mcp-settings-tools" ]
    (List.map tools ~f:(fun (tool : Mcp_server.Tool.t) ->
       Node.li
         ~attrs:[ Attr.class_ "mcp-tool" ]
         [ span ~cls:"mcp-tool-name" tool.name
         ; (if String.is_empty tool.description
            then Node.none
            else span ~cls:"mcp-tool-description" tool.description)
         ]))
;;

let tools_button ~shown ~name ~count ~inject =
  button
    ~cls:"mcp-tools-toggle"
    ~attrs:[ Attr.create "aria-expanded" (Bool.to_string shown) ]
    ~title:(if shown then "Hide its tools" else "Show its tools")
    ~on_click:(inject (Action.Mcp_show_tools name))
    [ Node.text (plural count "tool") ]
;;

(* What the server is doing on the session's host, in a line, and its tools
   when shown. *)
let health_note (m : App.Model.t) (s : Mcp_settings.Server.t) ~inject =
  let host = host_name m in
  let note level parts = div ~cls:("setting-note mcp-health " ^ level) parts in
  match Mcp_page.health m.mcp s with
  | Off -> note "muted" [ Node.text "Off: not started on any host" ], []
  | Checking -> note "muted" [ Node.textf "Checking %s…" host ], []
  | Not_listed ->
    ( note
        "muted"
        [ Node.textf
            "Not running on %s: its tool host may be older than the backend"
            host
        ]
    , [] )
  | Failed e ->
    ( note "warm" [ Node.textf "Failed on %s: %s" host (String.capitalize e) ]
    , [] )
  | Overridden source ->
    ( note
        "warm"
        [ Node.textf
            "Overridden by %s on %s: that one runs instead. Rename one of them \
             to use both."
            (Mcp_command.source_label ~home:(home m) source)
            host
        ]
    , [] )
  | Ready tools ->
    let shown = Set.mem m.mcp.expanded s.name in
    ( note
        "good"
        [ Node.textf "Ready on %s · " host
        ; tools_button ~shown ~name:s.name ~count:(List.length tools) ~inject
        ]
    , if shown then [ tools_list tools ] else [] )
;;

let pair_rows (d : Mcp_page.Draft.t) ~inject =
  let what, placeholder_key, placeholder_value =
    match d.kind with
    | Stdio -> "Environment variable", "API_KEY", "value"
    | Http -> "Header", "Authorization", "Bearer …"
  in
  let field f = inject (Action.Mcp_field f) in
  List.mapi d.pairs ~f:(fun i (p : Mcp_page.Pair.t) ->
    div
      ~cls:"mcp-pair"
      [ Node.input
          ~attrs:
            [ Attr.class_ "text-input mcp-pair-key"
            ; Attr.type_ "text"
            ; Attr.create "aria-label" (sprintf "%s %d: name" what (i + 1))
            ; Attr.placeholder placeholder_key
            ; Attr.value p.key
            ; Attr.create "autocomplete" "off"
            ; Attr.create "autocapitalize" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.on_input (fun _ text -> field (Pair_key (i, text)))
            ]
          ()
      ; Node.input
          ~attrs:
            [ Attr.class_ "text-input mcp-pair-value"
            ; Attr.type_ (if p.shown then "text" else "password")
            ; Attr.create "aria-label" (sprintf "%s %d: value" what (i + 1))
            ; Attr.placeholder placeholder_value
            ; Attr.value p.value
            ; Attr.create "autocomplete" "off"
            ; Attr.create "autocapitalize" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.on_input (fun _ text -> field (Pair_value (i, text)))
            ]
          ()
      ; button
          ~cls:"small ghost mcp-pair-show"
          ~attrs:[ Attr.create "aria-pressed" (Bool.to_string p.shown) ]
          ~title:(if p.shown then "Hide the value" else "Show the value")
          ~on_click:(field (Pair_shown i))
          [ Node.text (if p.shown then "Hide" else "Show") ]
      ; button
          ~cls:"icon ghost mcp-pair-remove"
          ~label:(sprintf "Remove %s %d" (String.lowercase what) (i + 1))
          ~title:"Remove"
          ~on_click:(field (Pair_remove i))
          [ icon Close ]
      ])
;;

let labelled ~label ~for_ ?hint control =
  div
    ~cls:"mcp-field"
    [ Node.label
        ~attrs:[ Attr.class_ "mcp-field-label"; Attr.for_ for_ ]
        [ Node.text label ]
    ; control
    ; (match hint with
       | Some hint -> span ~cls:"mcp-field-hint" hint
       | None -> Node.none)
    ]
;;

let text_input ~id ~value ?placeholder ?(disabled = false) ~on_input () =
  Node.input
    ~attrs:
      [ Attr.id id
      ; Attr.class_ "text-input"
      ; Attr.type_ "text"
      ; Attr.value value
      ; Option.value_map placeholder ~default:Attr.empty ~f:Attr.placeholder
      ; Attr.create "autocomplete" "off"
      ; Attr.create "autocapitalize" "off"
      ; Attr.create "spellcheck" "false"
      ; (if disabled then Attr.disabled else Attr.empty)
      ; Attr.on_input (fun _ text -> on_input text)
      ]
    ()
;;

let draft_form (d : Mcp_page.Draft.t) ~inject =
  let field f = inject (Action.Mcp_field f) in
  let kind_choice =
    segmented
      ~disabled:d.saving
      ~label:"How prigh reaches it"
      [ ( "Command"
        , "A program the tool host runs (stdio)"
        , Mcp_page.Kind.equal d.kind Stdio
        , field (Kind Stdio) )
      ; ( "URL"
        , "A server over HTTP"
        , Mcp_page.Kind.equal d.kind Http
        , field (Kind Http) )
      ]
  in
  let where =
    match d.kind with
    | Stdio ->
      [ labelled
          ~label:"Command"
          ~for_:"mcp-command"
          (text_input
             ~id:"mcp-command"
             ~value:d.command
             ~placeholder:"npx"
             ~disabled:d.saving
             ~on_input:(fun t -> field (Command t))
             ())
      ; labelled
          ~label:"Arguments"
          ~for_:"mcp-args"
          ~hint:"One per line, as they are: no quoting needed."
          (Node.textarea
             ~attrs:
               [ Attr.id "mcp-args"
               ; Attr.class_ "text-input mcp-args"
               ; Attr.rows 3
               ; Attr.placeholder "-y\n@modelcontextprotocol/server-filesystem"
               ; Attr.value d.args
               ; Attr.create "autocapitalize" "off"
               ; Attr.create "spellcheck" "false"
               ; (if d.saving then Attr.disabled else Attr.empty)
               ; Attr.on_input (fun _ t -> field (Args t))
               ]
             [])
      ]
    | Http ->
      [ labelled
          ~label:"URL"
          ~for_:"mcp-url"
          (text_input
             ~id:"mcp-url"
             ~value:d.url
             ~placeholder:"https://example.com/mcp"
             ~disabled:d.saving
             ~on_input:(fun t -> field (Url t))
             ())
      ]
  in
  let pairs_title, add_label =
    match d.kind with
    | Stdio -> "Environment", "Add variable"
    | Http -> "Headers", "Add header"
  in
  div
    ~cls:"setting mcp-draft"
    ~attrs:
      [ Attr.create
          "aria-label"
          (match d.editing with
           | Some name -> "Edit " ^ name
           | None -> "New MCP server")
      ]
    ([ span
         ~cls:"setting-name"
         (match d.editing with
          | Some name -> "Edit " ^ name
          | None -> "New server")
     ; labelled
         ~label:"Name"
         ~for_:"mcp-name"
         ~hint:"Its tools are called mcp__NAME__TOOL."
         (text_input
            ~id:"mcp-name"
            ~value:d.name
            ~placeholder:"docs"
            ~disabled:d.saving
            ~on_input:(fun t -> field (Name t))
            ())
     ; kind_choice
     ]
     @ where
     @ [ div
           ~cls:"mcp-pairs"
           ([ span ~cls:"mcp-field-label" pairs_title ]
            @ pair_rows d ~inject
            @ [ button
                  ~cls:"small ghost mcp-pair-add"
                  ~disabled:d.saving
                  ~on_click:(field Pair_add)
                  [ icon Plus; Node.text add_label ]
              ; span
                  ~cls:"mcp-field-hint"
                  "Values are kept on the backend and hidden here until shown; \
                   ${VAR} takes VAR from the tool host's environment."
              ])
       ; (match d.error with
          | Some e -> span ~cls:"setting-note warm mcp-error" (String.capitalize e)
          | None -> Node.none)
       ; div
           ~cls:"mcp-buttons"
           [ span ~cls:"mcp-keys" "Ctrl+Enter saves, Esc cancels"
           ; button
               ~cls:"small ghost"
               ~title:"Esc"
               ~on_click:(inject Action.Mcp_cancel)
               [ Node.text "Cancel" ]
           ; button
               ~cls:"small primary mcp-save"
               ~title:"Ctrl+Enter"
               ~disabled:d.saving
               ~attrs:
                 (if d.saving then [ Attr.create "aria-busy" "true" ] else [])
               ~on_click:(inject Action.Mcp_save)
               [ Node.text
                   (match d.saving, d.editing with
                    | true, _ -> "Saving…"
                    | false, Some _ -> "Save"
                    | false, None -> "Add server")
               ]
           ]
       ])
;;

let paste_form (p : Mcp_page.Paste.t) ~inject =
  div
    ~cls:"setting mcp-paste"
    [ div
        ~cls:"setting-text"
        [ span ~cls:"setting-name" "Paste JSON"
        ; span
            ~cls:"setting-about"
            "An mcpServers object, as in Claude Code's or Cursor's settings, \
             with or without the mcpServers around it. Every server in it is \
             added, or none is."
        ]
    ; Node.textarea
        ~attrs:
          [ Attr.id "mcp-paste-text"
          ; Attr.class_ "text-input mcp-paste-input"
          ; Attr.create "aria-label" "mcpServers JSON"
          ; Attr.rows 6
          ; Attr.placeholder
              {|{"docs": {"type": "http", "url": "https://example.com/mcp"}}|}
          ; Attr.value p.text
          ; Attr.create "autocapitalize" "off"
          ; Attr.create "spellcheck" "false"
          ; (if p.saving then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ t -> inject (Action.Mcp_paste_text t))
          ]
        []
    ; (match p.error with
       | Some e -> span ~cls:"setting-note warm mcp-error" (String.capitalize e)
       | None -> Node.none)
    ; div
        ~cls:"mcp-buttons"
        [ span ~cls:"mcp-keys" "Ctrl+Enter adds, Esc cancels"
        ; button
            ~cls:"small ghost"
            ~title:"Esc"
            ~on_click:(inject Action.Mcp_cancel)
            [ Node.text "Cancel" ]
        ; button
            ~cls:"small primary mcp-paste-save"
            ~title:"Ctrl+Enter"
            ~disabled:p.saving
            ~attrs:(if p.saving then [ Attr.create "aria-busy" "true" ] else [])
            ~on_click:(inject Action.Mcp_paste_save)
            [ Node.text (if p.saving then "Adding…" else "Add servers") ]
        ]
    ]
;;

let server_row (m : App.Model.t) (s : Mcp_settings.Server.t) ~inject =
  match m.mcp.draft with
  | Some ({ editing = Some name; _ } as d) when String.equal name s.name ->
    draft_form d ~inject
  | _ ->
    let removing = Option.equal String.equal m.mcp.removing (Some s.name) in
    let busy = Set.mem m.mcp.busy s.name in
    let health, tools = health_note m s ~inject in
    div
      ~cls:
        (String.concat
           ~sep:" "
           ([ "setting"; "mcp-server" ]
            @ (if s.enabled then [] else [ "off" ])
            @ if removing then [ "removing" ] else []))
      ~attrs:[ Attr.create "data-server" s.name ]
      ([ div
           ~cls:"setting-text"
           [ span ~cls:"setting-name" s.name
           ; span
               ~cls:"setting-about mcp-summary"
               (Mcp_settings.Transport.summary s.transport)
           ; (if removing
              then
                span
                  ~cls:"setting-note warm"
                  "Remove it from your settings? Its tools go from every \
                   session."
              else health)
           ]
       ; div
           ~cls:"setting-control"
           (if removing
            then
              [ button
                  ~cls:"small ghost"
                  ~title:"Keep it (Esc)"
                  ~on_click:(inject Action.Mcp_remove_cancel)
                  [ Node.text "Keep" ]
              ; button
                  ~cls:"small danger"
                  ~attrs:[ Attr.id "mcp-remove-yes" ]
                  ~on_click:(inject Action.Mcp_remove_confirm)
                  [ Node.text "Remove" ]
              ]
            else
              [ switch
                  ~disabled:busy
                  ~label:("Use " ^ s.name)
                  ~title:
                    (if s.enabled
                     then sprintf "Stop it on every host (/mcp disable %s)" s.name
                     else sprintf "Start it on every host (/mcp enable %s)" s.name)
                  ~on:s.enabled
                  ~on_click:(inject (Action.Mcp_toggle s.name))
                  ()
              ; button
                  ~cls:"icon ghost mcp-edit"
                  ~label:("Edit " ^ s.name)
                  ~title:"Edit"
                  ~on_click:(inject (Action.Mcp_edit (Some s.name)))
                  [ icon Pencil ]
              ; button
                  ~cls:"icon ghost mcp-remove"
                  ~label:("Remove " ^ s.name)
                  ~title:(sprintf "Remove (/mcp remove %s)" s.name)
                  ~on_click:(inject (Action.Mcp_remove s.name))
                  [ icon Trash ]
              ])
       ]
       @ tools)
;;

let host_row (m : App.Model.t) (s : Mcp_server.t) ~inject =
  let source = Mcp_command.source_label ~home:(home m) s.source in
  let busy = Set.mem m.mcp.busy s.name in
  let shown = Set.mem m.mcp.expanded ("host:" ^ s.name) in
  let note, control =
    match s.status with
    | Ready ->
      ( div
          ~cls:"setting-note good mcp-health"
          [ Node.text "Ready · "
          ; button
              ~cls:"mcp-tools-toggle"
              ~attrs:[ Attr.create "aria-expanded" (Bool.to_string shown) ]
              ~title:(if shown then "Hide its tools" else "Show its tools")
              ~on_click:(inject (Action.Mcp_show_tools ("host:" ^ s.name)))
              [ Node.text (plural (List.length s.tools) "tool") ]
          ]
      , [] )
    | Failed ->
      ( span
          ~cls:"setting-note warm mcp-health"
          ("Failed: "
           ^ String.capitalize (Option.value s.error ~default:"it did not start"))
      , [ button
            ~cls:"small"
            ~disabled:busy
            ~title:"/mcp reconnect"
            ~on_click:(inject Action.Mcp_reconnect)
            [ icon Restart; Node.text "Retry" ]
        ] )
    | Needs_approval ->
      ( span
          ~cls:"setting-note warm mcp-health"
          "The project asks to run it: approve it to start it"
      , [ button
            ~cls:"small primary"
            ~disabled:busy
            ~attrs:(if busy then [ Attr.create "aria-busy" "true" ] else [])
            ~title:"Run it in this project's sessions"
            ~on_click:
              (inject (Action.Mcp_approve { source = s.source; server = s.name }))
            [ Node.text (if busy then "Approving…" else "Approve") ]
        ] )
  in
  div
    ~cls:"setting mcp-server mcp-host-server"
    ~attrs:[ Attr.create "data-server" s.name ]
    ([ div
         ~cls:"setting-text"
         [ span ~cls:"setting-name" s.name
         ; span ~cls:"setting-about mcp-source" source
         ; note
         ]
     ; div ~cls:"setting-control" control
     ]
     @ if shown then [ tools_list s.tools ] else [])
;;

let settings_servers (m : App.Model.t) ~inject =
  let p = m.mcp in
  let adding =
    match p.draft with
    | Some ({ editing = None; _ } as d) -> [ draft_form d ~inject ]
    | _ -> []
  in
  let paste =
    match p.paste with
    | Some paste -> [ paste_form paste ~inject ]
    | None -> []
  in
  let actions =
    if Option.is_some p.paste || not (List.is_empty adding)
    then []
    else
      [ div
          ~cls:"setting mcp-actions"
          [ button
              ~cls:"small primary mcp-add"
              ~title:"/mcp add NAME URL|COMMAND [ARGS...]"
              ~on_click:(inject (Action.Mcp_edit None))
              [ icon Plus; Node.text "Add server" ]
          ; button
              ~cls:"small ghost mcp-paste-open"
              ~title:"Claude Code's mcpServers JSON (/mcp add {...})"
              ~on_click:(inject Action.Mcp_paste)
              [ icon Clipboard; Node.text "Paste JSON" ]
          ]
      ]
  in
  let error =
    match p.error with
    | Some e ->
      [ div
          ~cls:"setting mcp-failed"
          [ span ~cls:"setting-note warm" (String.capitalize e) ]
      ]
    | None -> []
  in
  match p.status with
  | Unknown | Loading when List.is_empty p.servers ->
    [ row
        ~name:"Your servers"
        ~about:"Kept on the backend, for your account."
        ~note:("muted", "Reading your settings…")
        []
    ]
  | Failed e ->
    [ row
        ~name:"Your servers"
        ~about:"Kept on the backend, for your account."
        ~note:
          ( "warm"
          , sprintf "Could not read them: %s. Open the section again to retry." e
          )
        []
    ]
  | Unknown | Loading | Loaded ->
    let servers =
      match p.servers with
      | [] when List.is_empty adding && Option.is_none p.paste ->
        [ row
            ~cls:"mcp-empty"
            ~name:"No servers yet"
            ~about:
              "Add one by the command that starts it or by its URL, or paste \
               the mcpServers JSON from another tool's settings. It runs on \
               every host your sessions use."
            []
        ]
      | servers -> List.map servers ~f:(fun s -> server_row m s ~inject)
    in
    error @ servers @ adding @ paste @ actions
;;

let host_servers (m : App.Model.t) ~inject =
  let p = m.mcp in
  match p.host, p.host_error with
  | None, Some e ->
    [ row
        ~name:"Couldn't ask the host"
        ~about:"Its servers show here once it answers."
        ~note:("warm", String.capitalize e)
        []
    ]
  | None, None ->
    [ row ~name:"Asking the host…" ~about:"" ~note:("muted", "Waiting…") [] ]
  | Some l, _ ->
    let problems =
      List.map l.problems ~f:(fun problem ->
        div
          ~cls:"setting mcp-problem"
          [ span ~cls:"setting-note warm" (String.capitalize problem) ])
    in
    (match Mcp_page.host_servers p with
     | [] ->
       problems
       @ [ row
             ~cls:"mcp-host-empty"
             ~name:"None from files"
             ~about:
               "A host's ~/.prigh/mcp.json and a project's .mcp.json add \
                servers on that host only; a server there with the name of one \
                of yours replaces it."
             []
         ]
     | servers -> problems @ List.map servers ~f:(fun s -> host_row m s ~inject))
;;

let groups (m : App.Model.t) ~inject =
  let n = List.length m.mcp.servers in
  [ group
      ~title:"Your servers"
      ?note:(if n = 0 then None else Some (plural n "server"))
      (settings_servers m ~inject)
  ; group
      ~title:("On " ^ host_name m)
      ~note:"From its files: change them there"
      (host_servers m ~inject)
  ]
;;
