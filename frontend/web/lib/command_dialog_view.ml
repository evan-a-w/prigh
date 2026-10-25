open! Core
open! Import
open Html
module Action = App.Action

let keys_hint text = span ~cls:"keys-hint" text

let done_button ~inject =
  button
    ~cls:"primary"
    ~on_click:(inject Action.Close_dialog)
    [ Node.text "Done" ]
;;

let key_table rows =
  Node.table
    ~attrs:[ Attr.class_ "help-table" ]
    [ Node.tbody
        (List.map rows ~f:(fun (key, what) ->
           Node.tr
             [ Node.td [ Node.kbd [ Node.text key ] ]
             ; Node.td [ Node.text what ]
             ]))
    ]
;;

let hotkeys ~inject =
  Modal.view
    ~cls:"help-dialog"
    ~title:"Keyboard shortcuts"
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_hint "/help also lists the commands"; done_button ~inject ]
    [ key_table Keys.help
    ; Node.h3 [ Node.text "Left to the browser" ]
    ; key_table Keys.browser
    ]
;;

let scoped_models (picker : Picker.t) checked ~inject =
  let visible = Picker.visible picker in
  Modal.view
    ~cls:"picker-dialog scoped-dialog"
    ~title:"Scoped models"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint
          (sprintf "%d checked. Tab toggles, Enter saves" (Set.length checked))
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text "Save" ]
      ]
    [ Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            "Ctrl+P and Alt+P cycle through the checked models. With none \
             checked they cycle through the logged-in ones."
        ]
    ; div
        ~cls:"search"
        [ icon Search
        ; Node.input
            ~attrs:
              [ Attr.id "picker-input"
              ; Attr.type_ "text"
              ; Attr.placeholder "Filter models…"
              ; Attr.create "aria-label" "Filter models"
              ; Attr.role "combobox"
              ; Attr.create "aria-controls" "picker-list"
              ; Attr.create "aria-expanded" "true"
              ; Attr.create "aria-autocomplete" "list"
              ; (if List.is_empty visible
                 then Attr.empty
                 else
                   Attr.create
                     "aria-activedescendant"
                     (sprintf "picker-option-%d" (Picker.selected picker)))
              ; Attr.value (Picker.query picker)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Picker_query text))
              ]
            ()
        ]
    ; div
        ~cls:"picker-items"
        ~attrs:
          [ Attr.role "listbox"
          ; Attr.id "picker-list"
          ; Attr.create "aria-label" "Models"
          ; Attr.create "aria-multiselectable" "true"
          ]
        (List.mapi visible ~f:(fun i (item : Picker.Item.t) ->
           let on = Set.mem checked item.id in
           Node.div
             ~attrs:
               [ classes
                   [ "picker-item"; "check-item" ]
                   [ "selected", i = Picker.selected picker
                   ; "checked", on
                   ; "dimmed", item.dimmed
                   ]
               ; Attr.role "option"
               ; Attr.id (sprintf "picker-option-%d" i)
               ; Attr.create "aria-selected" (Bool.to_string on)
               ; Attr.on_click (fun _ -> inject (Action.Toggle_scoped item.id))
               ]
             [ Html.check on
             ; span ~cls:"picker-label" item.label
             ; span ~cls:"picker-detail" item.detail
             ]))
    ]
;;

let prompt (m : App.Model.t) (p : Prompt.t) ~inject =
  let title, label, placeholder, accept =
    match p.action with
    | Cd ->
      ( "Change directory"
      , "The session's working directory"
      , "/path/to/project"
      , "Change" )
    | Mkdir ->
      ( "Create directory"
      , "Created with any missing parents, then made the working directory"
      , "/path/to/new/project"
      , "Create and switch" )
    | Host_cwd { name; _ } ->
      ( "Run tools on " ^ name
      , "Working directory there"
      , "/path/on/that/host"
      , "Switch" )
    | Export ->
      ( "Export transcript"
      , "Path on the backend: markdown, or .jsonl for a session file"
      , "empty: ~/.prigh/sessions/exports/"
      , "Export" )
    | Import ->
      ( "Import session"
      , "A session's .jsonl file on the backend"
      , "path/to/session.jsonl"
      , "Import" )
  in
  let host_note =
    match p.action, m.state with
    | (Cd | Mkdir), Some state
      when not (String.equal state.active_host Host.backend_id) ->
      Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.textf
            "On %s, where tools run (/host changes it)."
            (List.find state.hosts ~f:(fun h ->
               String.equal h.id state.active_host)
             |> Option.value_map ~default:state.active_host ~f:(fun h -> h.name)
            )
        ]
    | _ -> Node.none
  in
  Modal.view
    ~cls:"prompt-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "Tab completes, ↑↓ choose"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:p.busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if p.busy then "Working…" else accept) ]
      ]
    [ Node.label
        ~attrs:[ Attr.class_ "field" ]
        [ span ~cls:"field-label" label
        ; Node.input
            ~attrs:
              [ Attr.id "dialog-input"
              ; Attr.class_ "text-input"
              ; Attr.type_ "text"
              ; Attr.placeholder placeholder
              ; Attr.value p.input
              ; Attr.create "autocomplete" "off"
              ; Attr.create "autocapitalize" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Dialog_input text))
              ]
            ()
        ]
    ; host_note
    ; (match p.error with
       | Some e ->
         div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
       | None -> Node.none)
    ; (match p.suggestions with
       | [] -> Node.none
       | suggestions ->
         div
           ~cls:"suggestions"
           ~attrs:[ Attr.role "listbox" ]
           (List.mapi (List.take suggestions 12) ~f:(fun i path ->
              Node.div
                ~attrs:
                  [ classes
                      [ "picker-item"; "suggestion" ]
                      [ "selected", Option.equal Int.equal p.selected (Some i) ]
                  ; Attr.role "option"
                  ; Attr.on_mousedown (fun ev ->
                      Js_of_ocaml.Dom.preventDefault ev;
                      inject (Action.Choose_suggestion i))
                  ]
                [ icon Folder; span ~cls:"picker-label" path ])))
    ]
;;

let active_host_name (m : App.Model.t) =
  Option.bind m.state ~f:(fun state ->
    List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host))
  |> Option.map ~f:(fun h -> h.name)
;;

let create_dir (m : App.Model.t) ~target ~path ~busy ~error ~inject =
  let where =
    match (target : Dir_change.Target.t) with
    | Host { name; _ } -> Some name
    | Cwd -> active_host_name m
  in
  Modal.view
    ~cls:"create-dir-dialog"
    ~title:"Create folder?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "Enter creates, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if busy then "Creating…" else "Create folder and switch")
          ]
      ]
    [ Node.p
        [ Node.text
            (match where with
             | Some host -> sprintf "This folder doesn't exist on %s:" host
             | None -> "This folder doesn't exist:")
        ]
    ; Node.p ~attrs:[ Attr.class_ "create-dir-path" ] [ Node.text path ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            (match target with
             | Cwd -> "Missing parent folders are created too."
             | Host { name; _ } ->
               sprintf
                 "Missing parent folders are created too, then tools run on %s \
                  in it."
                 name)
        ]
    ; (match error with
       | Some e ->
         div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
       | None -> Node.none)
    ]
;;

let dialog_error = function
  | Some e ->
    div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
  | None -> Node.none
;;

let new_worktree (d : New_worktree.t) ~inject =
  let field (f : New_worktree.Field.t) ~label ~value ~placeholder ~note =
    Node.label
      ~attrs:[ Attr.class_ "field" ]
      [ span ~cls:"field-label" label
      ; Node.input
          ~attrs:
            [ Attr.id (New_worktree.Field.id f)
            ; Attr.class_ "text-input"
            ; Attr.type_ "text"
            ; Attr.placeholder placeholder
            ; Attr.value value
            ; Attr.create "autocomplete" "off"
            ; Attr.create "autocapitalize" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.on_input (fun _ text ->
                inject (Action.New_worktree_input { field = f; text }))
            ]
          ()
      ; span ~cls:"field-note" note
      ]
  in
  let head = Option.value d.head ~default:"HEAD" in
  let fresh = Option.is_some d.repo in
  Modal.view
    ~cls:"prompt-dialog worktree-dialog"
    ~title:
      (match d.repo with
       | Some repo -> "New worktree of " ^ Filename.basename repo
       | None -> "New worktree")
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "Enter creates, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:d.busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text
              (match d.busy, fresh with
               | true, _ -> "Creating…"
               | false, true -> "Create and start a session"
               | false, false -> "Create and switch")
          ]
      ]
    [ field
        Branch
        ~label:"Branch"
        ~value:d.branch
        ~placeholder:"feature/name"
        ~note:"A new branch, or an existing one to check out"
    ; field
        Base
        ~label:"Base"
        ~value:d.base
        ~placeholder:head
        ~note:
          (if fresh
           then sprintf "Where a new branch starts (empty: %s)" head
           else
             sprintf
               "Where a new branch starts (empty: %s, where this session is)"
               head)
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            (match d.project_dir, fresh with
             | Some dir, false ->
               sprintf
                 "Created in %s, then this session works there."
                 (Sidebar_view.home_relative dir)
             | None, false -> "This session works there once it is created."
             | Some dir, true ->
               sprintf
                 "Created in %s, then a new session starts there."
                 (Sidebar_view.home_relative dir)
             | None, true -> "A new session starts there once it is created.")
        ]
    ; dialog_error d.error
    ]
;;

let remove_worktree (w : Worktree.t) ~force ~busy ~error ~inject =
  let dirty = Worktree.dirty w in
  (* Also once the backend refused: it may know of changes or a lock that
     the listing did not show. *)
  let forcing = dirty || w.locked || Option.is_some error in
  let discard =
    button
      ~cls:"danger"
      ~disabled:busy
      ~title:
        (sprintf
           "%s (/worktree rm -f %s)"
           (if dirty
            then "Lose the uncommitted changes"
            else "Also when locked or with changes")
           (Prigh_ui.Worktree_command.target w))
      ~on_click:(inject (Action.Remove_worktree { force = true }))
      [ Node.text
          (if busy && force then "Removing…" else Worktrees.force_label w)
      ]
  in
  Modal.view
    ~cls:"remove-worktree-dialog"
    ~title:
      (if dirty then "Remove worktree with changes?" else "Remove worktree?")
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      ([ keys_hint
           (if force
            then "Enter removes it anyway, Esc cancels"
            else "Enter removes, Esc cancels")
       ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
       ]
       @
       if force
       then [ discard ]
       else
         (if forcing then [ discard ] else [])
         @ [ button
               ~cls:(if forcing then "" else "danger")
               ~disabled:busy
               ~on_click:(inject (Action.Remove_worktree { force = false }))
               [ Node.text (if busy then "Removing…" else "Remove") ]
           ])
    [ Node.p [ Node.text (Prigh_ui.Worktree_command.confirm_remove w ~force) ]
    ; dialog_error error
    ]
;;

let rewind_confirm ~text ~inject =
  Modal.view
    ~title:"Rewind here?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text "Rewind" ]
      ]
    [ Node.p [ Node.text "The conversation goes back to:" ]
    ; Node.blockquote
        ~attrs:[ Attr.class_ "rewind-quote" ]
        [ Node.text (String.prefix text 400) ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            "Later messages are kept as a branch: /tree returns to them."
        ]
    ]
;;

let tokens n =
  if n >= 1_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else if n >= 1000
  then sprintf "%.1fk" (Float.of_int n /. 1000.)
  else Int.to_string n
;;

let duration seconds =
  let s = Float.iround_down_exn seconds in
  if s < 60
  then sprintf "%ds" s
  else if s < 3600
  then sprintf "%dm %02ds" (s / 60) (s % 60)
  else sprintf "%dh %02dm" (s / 3600) (s % 3600 / 60)
;;

let rows list =
  Node.dl
    ~attrs:[ Attr.class_ "info-list" ]
    (List.concat_map list ~f:(fun (k, v) ->
       [ Node.dt [ Node.text k ]; Node.dd [ Node.text v ] ]))
;;

let session (m : App.Model.t) (s : Session_stats.t) ~inject =
  let info =
    match m.state with
    | None -> []
    | Some state ->
      [ ( "Name"
        , Option.value
            state.session_name
            ~default:
              (Option.value state.session_description ~default:"(unnamed)") )
      ; "Id", state.session_id
      ; "File", state.session_path
      ; "Directory", state.cwd
      ; "Model", sprintf "%s (%s)" state.model.name state.model.key
      ; "Thinking", state.thinking
      ]
  in
  let tools =
    match s.tool_calls with
    | [] -> "none"
    | calls ->
      String.concat
        ~sep:", "
        (List.map calls ~f:(fun (name, n) -> sprintf "%s %d" name n))
  in
  Modal.view
    ~cls:"session-dialog"
    ~title:"Session"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button ~on_click:(inject (Action.Run "/name")) [ Node.text "Rename" ]
      ; button ~on_click:(inject (Action.Run "/export")) [ Node.text "Export" ]
      ; button ~on_click:(inject (Action.Run "/tree")) [ Node.text "Tree" ]
      ; done_button ~inject
      ]
    [ rows info
    ; div
        ~cls:"stats-grid"
        (List.map
           [ "Messages", Int.to_string s.message_count
           ; "Turns", Int.to_string s.turns
           ; "Cost", sprintf "$%.4f" s.cost_usd
           ; "Context", sprintf "%.1f%%" s.context_percent
           ; "Input", tokens s.usage.input
           ; "Output", tokens s.usage.output
           ; "Cache read", tokens s.usage.cache_read
           ; "Duration", duration s.duration_seconds
           ]
           ~f:(fun (k, v) ->
             div
               ~cls:"stat"
               [ span ~cls:"stat-value" v; span ~cls:"stat-label" k ]))
    ; rows
        [ "Tools", tools
        ; "Model changes", Int.to_string s.model_changes
        ; "Compactions", Int.to_string s.compactions
        ]
    ]
;;

let text ~title ~text ~inject =
  Modal.view
    ~cls:"text-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ done_button ~inject ]
    [ Node.pre ~attrs:[ Attr.class_ "text-block" ] [ Node.text text ] ]
;;

let mcp_tools (server : Mcp_server.t) ~inject =
  Modal.view
    ~cls:"mcp-tools-dialog"
    ~title:
      (sprintf
         "%s: %s"
         server.name
         (Chat_html.plural (List.length server.tools) "tool"))
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_hint "/mcp lists the servers"; done_button ~inject ]
    [ Node.p ~attrs:[ Attr.class_ "dialog-note" ] [ Node.text server.source ]
    ; (match server.tools with
       | [] ->
         div ~cls:"picker-empty" [ Node.text "This server offers no tools." ]
       | tools ->
         div
           ~cls:"mcp-tools"
           (List.map tools ~f:(fun (tool : Mcp_server.Tool.t) ->
              div
                ~cls:"mcp-tool"
                [ span ~cls:"mcp-tool-name" tool.name
                ; (if String.is_empty tool.description
                   then Node.none
                   else span ~cls:"mcp-tool-description" tool.description)
                ])))
    ]
;;

(* Shown at once while the backend lists what the dialog shows. *)
let loading ~title ~inject =
  Modal.view
    ~cls:"loading-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_hint "Esc closes" ]
    [ div
        ~cls:"loading-row"
        ~attrs:[ Attr.role "status" ]
        [ Html.lamp (); Node.text "Loading…" ]
    ]
;;

let view (m : App.Model.t) (dialog : Dialog.t) ~inject =
  match dialog with
  | Hotkeys -> Some (hotkeys ~inject)
  | Scoped_models { picker; checked } ->
    Some (scoped_models picker checked ~inject)
  | Prompt p -> Some (prompt m p ~inject)
  | Rewind_confirm { text = t; _ } -> Some (rewind_confirm ~text:t ~inject)
  | Create_dir { target; path; busy; error } ->
    Some (create_dir m ~target ~path ~busy ~error ~inject)
  | Session stats -> Some (session m stats ~inject)
  | Text { title; text = t } -> Some (text ~title ~text:t ~inject)
  | Mcp_tools server -> Some (mcp_tools server ~inject)
  | Loading title -> Some (loading ~title ~inject)
  | Diff diff -> Some (Git_diff_view.view ~fold:(App.Model.fold m) diff ~inject)
  | New_worktree d -> Some (new_worktree d ~inject)
  | Remove_worktree { worktree; force; busy; error; repo = _ } ->
    Some (remove_worktree worktree ~force ~busy ~error ~inject)
  | Picker _ | Help | Settings | Rename _ | Delete _ | Login _ | Auth _ -> None
;;
