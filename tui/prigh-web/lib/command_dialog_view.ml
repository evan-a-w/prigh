open! Core
open! Import
open Html
module Action = App.Action

let keys_hint text = span ~cls:"keys-hint" text

let done_button ~inject =
  button ~cls:"primary" ~on_click:(inject Action.Close_dialog) [ Node.text "Done" ]
;;

let key_table rows =
  Node.table
    ~attrs:[ Attr.class_ "help-table" ]
    [ Node.tbody
        (List.map rows ~f:(fun (key, what) ->
           Node.tr
             [ Node.td [ Node.kbd [ Node.text key ] ]; Node.td [ Node.text what ] ]))
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
          (sprintf
             "%d checked · Tab toggles · Enter saves"
             (Set.length checked))
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
              ; Attr.value (Picker.query picker)
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; Attr.on_input (fun _ text -> inject (Action.Picker_query text))
              ]
            ()
        ]
    ; div
        ~cls:"picker-items"
        ~attrs:[ Attr.role "listbox"; Attr.create "aria-multiselectable" "true" ]
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
               ; Attr.create "aria-selected" (Bool.to_string on)
               ; Attr.on_click (fun _ -> inject (Action.Toggle_scoped item.id))
               ]
             [ span ~cls:"checkbox" (if on then "✓" else "")
             ; span ~cls:"picker-label" item.label
             ; span ~cls:"picker-detail" item.detail
             ]))
    ]
;;

let prompt (m : App.Model.t) (p : Prompt.t) ~inject =
  let title, label, placeholder, accept =
    match p.action with
    | Cd -> "Change directory", "The session's working directory", "/path/to/project", "Change"
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
    | Cd, Some state when not (String.equal state.active_host Host.backend_id) ->
      Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.textf
            "On %s, where tools run (/host changes it)."
            (List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host)
             |> Option.value_map ~default:state.active_host ~f:(fun h -> h.name))
        ]
    | _ -> Node.none
  in
  Modal.view
    ~cls:"prompt-dialog"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_hint "Tab completes · ↑↓ choose"
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
       | Some e -> div ~cls:"dialog-error" ~attrs:[ Attr.role "alert" ] [ Node.text e ]
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
        [ Node.text "Later messages are kept as a branch: /tree returns to them." ]
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
            ~default:(Option.value state.session_description ~default:"(unnamed)") )
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
             div ~cls:"stat" [ span ~cls:"stat-value" v; span ~cls:"stat-label" k ]))
    ; rows
        [ "Tools", tools
        ; "Model changes", Int.to_string s.model_changes
        ; "Compactions", Int.to_string s.compactions
        ]
    ]
;;

let jobs ({ jobs; selected; output } : Dialog.Jobs.t) ~inject =
  Modal.view
    ~cls:"jobs-dialog"
    ~title:"Background jobs"
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_hint "↑↓ move · Enter output · Delete kill"; done_button ~inject ]
    [ (match jobs with
       | [] -> div ~cls:"picker-empty" [ Node.text "No jobs: !&command starts one." ]
       | jobs ->
         div
           ~cls:"picker-items"
           ~attrs:[ Attr.role "listbox" ]
           (List.mapi jobs ~f:(fun i (j : Job_info.t) ->
              Node.div
                ~attrs:
                  [ classes [ "job-item" ] [ "selected", i = selected; "running", j.running ]
                  ; Attr.role "option"
                  ]
                [ div
                    ~cls:"job-main"
                    [ span ~cls:"job-command" j.command
                    ; span
                        ~cls:"job-meta"
                        (String.concat
                           ~sep:" · "
                           ([ j.id
                            ; Option.value j.exit ~default:"running"
                            ; duration j.elapsed
                            ]
                            @ Option.to_list j.last_line))
                    ]
                ; div
                    ~cls:"job-buttons"
                    [ button
                        ~cls:"small"
                        ~on_click:(inject (Action.Show_job_output j.id))
                        [ Node.text "Output" ]
                    ; (if j.running
                       then
                         button
                           ~cls:"small danger"
                           ~on_click:(inject (Action.Kill_job j.id))
                           [ Node.text "Kill" ]
                       else Node.none)
                    ]
                ])))
    ; (match output with
       | None -> Node.none
       | Some (id, text) ->
         div
           ~cls:"job-output"
           [ span ~cls:"job-output-title" (sprintf "Output of %s (last 200 lines)" id)
           ; Node.pre
               [ Node.text (if String.is_empty text then "(no output yet)" else text) ]
           ])
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

let view (m : App.Model.t) (dialog : Dialog.t) ~inject =
  match dialog with
  | Hotkeys -> Some (hotkeys ~inject)
  | Scoped_models { picker; checked } -> Some (scoped_models picker checked ~inject)
  | Prompt p -> Some (prompt m p ~inject)
  | Rewind_confirm { text = t; _ } -> Some (rewind_confirm ~text:t ~inject)
  | Session stats -> Some (session m stats ~inject)
  | Jobs d -> Some (jobs d ~inject)
  | Text { title; text = t } -> Some (text ~title ~text:t ~inject)
  | Picker _ | Help | Rename _ | Delete _ | Login _ | Auth _ | Agents -> None
;;
