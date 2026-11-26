open! Core
open! Import
open Html
module Action = App.Action

let keys_hint text = span ~cls:"keys-hint" text

(* Only for a keyboard: hidden on a phone. *)
let keys_only text = span ~cls:"keys-hint keyboard" text

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
    ~cls:"help-dialog w-[min(720px,100%)]"
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
            (match Active_host.of_state state with
             | Connected h -> h.name
             | Disconnected { name; _ } -> name ^ " (offline)"
             | Unset -> state.active_host)
        ]
    | _ -> Node.none
  in
  Modal.view
    ~cls:
      "prompt-dialog [&_.text-input]:w-full [&_.text-input]:font-mono \
       [&_.text-input]:text-[13px]"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Tab completes, ↑↓ choose"
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
           ~cls:
             "suggestions mt-2 flex max-h-[260px] flex-col gap-px \
              overflow-y-auto [&_.icon]:size-3.5 \
              [&_.icon]:text-muted-foreground [&_.picker-label]:truncate \
              [&_.picker-label]:font-mono [&_.picker-label]:text-[12.5px]"
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
    match Active_host.of_state state with
    | Connected h -> Some h.name
    | Disconnected { name; _ } -> Some name
    | Unset -> None)
;;

let create_dir (m : App.Model.t) ~target ~path ~busy ~error ~inject =
  let where =
    match (target : Dir_change.Target.t) with
    | Host { name; _ } -> Some name
    | Cwd -> active_host_name m
  in
  Modal.view
    ~cls:"create-dir-dialog [&_p]:mb-2"
    ~title:"Create folder?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Enter creates, Esc cancels"
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
    ; Node.p
        ~attrs:
          [ Attr.class_
              "create-dir-path rounded-md bg-muted px-3 py-2 font-mono \
               text-[13px] break-words text-foreground"
          ]
        [ Node.text path ]
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

let new_worktree (m : App.Model.t) (d : New_worktree.t) ~inject =
  let field
        ?(corner = Node.none)
        (f : New_worktree.Field.t)
        ~label
        ~value
        ~placeholder
        ~note
    =
    Node.label
      ~attrs:[ Attr.class_ "field" ]
      [ div ~cls:"field-head" [ span ~cls:"field-label" label; corner ]
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
      ; span ~cls:"field-note text-[11.5px] text-muted-foreground" note
      ]
  in
  let head = Option.value d.head ~default:"HEAD" in
  Modal.view
    ~cls:
      "prompt-dialog [&_.text-input]:w-full [&_.text-input]:font-mono \
       [&_.text-input]:text-[13px] worktree-dialog [&_.field+.field]:mt-3 \
       [&_.dialog-note]:mt-3 [&_.dialog-note]:mb-0"
    ~title:"New worktree"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Enter creates, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:d.busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if d.busy then "Creating…" else "Create and switch") ]
      ]
    [ field
        Branch
        ~corner:
          (Generate_view.button
             ~label:"Generate name"
             m.generate
             Worktree_branch
             ~inject)
        ~label:"Branch"
        ~value:d.branch
        ~placeholder:"feature/name, or the task to name it from"
        ~note:
          "A new branch, or an existing one to check out (✦ names one from the \
           task typed here)"
    ; field
        Base
        ~label:"Base"
        ~value:d.base
        ~placeholder:head
        ~note:
          (sprintf
             "Where a new branch starts (empty: %s, where this session is)"
             head)
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            (match d.project_dir with
             | Some dir ->
               sprintf
                 "Created in %s, then this session works there."
                 (Sidebar_view.home_relative
                    ?host_home:(Sidebar_view.state_host_home m)
                    dir)
             | None -> "This session works there once it is created.")
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
    ~cls:"remove-worktree-dialog [&_p]:mb-2 [&_p]:break-words"
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
        ~attrs:
          [ Attr.class_
              "rewind-quote mb-2.5 rounded-r-md border-l-2 border-primary \
               bg-muted px-3 py-2 whitespace-pre-wrap"
          ]
        [ Node.text (String.prefix text 400) ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            "Later messages are kept as a branch: /tree returns to them."
        ]
    ]
;;

let trim_confirm (m : App.Model.t) (point : Trim.Point.t) ~busy ~error ~inject =
  let text = Jump.headline point.text in
  Modal.view
    ~cls:"trim-confirm"
    ~title:"Trim the context here?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Enter trims, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if busy then "Trimming…" else "Trim") ]
      ]
    [ Node.p
        [ Node.textf
            "Trim %d message%s (%s) before this %s?"
            point.messages
            (if point.messages = 1 then "" else "s")
            (Trim.tokens_label point.tokens)
            (if point.reply then "reply" else "message")
        ]
    ; Node.blockquote
        ~attrs:
          [ Attr.class_
              "rewind-quote mb-2.5 rounded-r-md border-l-2 border-primary \
               bg-muted px-3 py-2 whitespace-pre-wrap"
          ]
        [ Node.text
            (if String.is_empty text
             then "(an image)"
             else String.prefix text 400)
        ; Option.value_map point.at ~default:Node.none ~f:(fun at ->
            span
              ~cls:
                "quote-time mt-1 block font-mono text-[11.5px] \
                 text-muted-foreground"
              (Message_time.short (App.Model.message_time m) at))
        ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            "They leave the model's context; the transcript keeps them folded, \
             and Undo brings them back."
        ]
    ; dialog_error error
    ]
;;

let import_login_confirm (i : Adapter_login.Import.t) ~busy ~error ~inject =
  let cli = Session_engine.adapter_label i.adapter in
  Modal.view
    ~cls:"import-login-confirm"
    ~title:(sprintf "Move %s's login from %s into prigh?" cli i.host)
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Enter moves it, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"danger"
          ~disabled:busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if busy then "Moving…" else "Move it") ]
      ]
    ([ Node.p [ Node.text i.warning ]
     ; Node.p
         ~attrs:[ Attr.class_ "dialog-note" ]
         [ Node.textf
             "prigh keeps it as %S, refreshing it for every tool host that \
              runs %s on it."
             i.label
             cli
         ]
     ]
     @ (match i.adapter with
        | "claude" ->
          [ Node.p
              ~attrs:[ Attr.class_ "dialog-note" ]
              [ Node.text
                  "Anthropic's terms keep Claude Pro/Max logins to Claude's \
                   own apps: running it through prigh may breach them."
              ]
          ]
        | _ -> [])
     @ [ dialog_error error ])
;;

let trim_after_confirm
      (m : App.Model.t)
      (point : Message_point.t)
      ~busy
      ~error
      ~inject
  =
  let text = Jump.headline point.text in
  let freed =
    Option.value
      point.trim_after
      ~default:{ Trim.Freed.messages = 0; tokens = 0 }
  in
  Modal.view
    ~cls:"trim-confirm"
    ~title:"Trim the context after this?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ keys_only "Enter trims, Esc cancels"
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"primary"
          ~disabled:busy
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text (if busy then "Trimming…" else "Trim") ]
      ]
    [ Node.p
        [ Node.textf
            "Trim %d message%s (%s) after this message?"
            freed.messages
            (if freed.messages = 1 then "" else "s")
            (Trim.tokens_label freed.tokens)
        ]
    ; Node.blockquote
        ~attrs:
          [ Attr.class_
              "rewind-quote mb-2.5 rounded-r-md border-l-2 border-primary \
               bg-muted px-3 py-2 whitespace-pre-wrap"
          ]
        [ Node.text
            (if String.is_empty text
             then "(an image)"
             else String.prefix text 400)
        ; Option.value_map point.at ~default:Node.none ~f:(fun at ->
            span
              ~cls:
                "quote-time mt-1 block font-mono text-[11.5px] \
                 text-muted-foreground"
              (Message_time.short (App.Model.message_time m) at))
        ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text
            "They leave the model's context; the transcript keeps them folded \
             where they were, what you send next goes after them, and Undo \
             brings them back."
        ]
    ; dialog_error error
    ]
;;

let nix_gc_confirm generations ~inject =
  let what =
    Option.value
      (Prigh_ui.Nix_gc_command.describe_generations (Some generations))
      ~default:""
  in
  Modal.view
    ~cls:"nix-gc-confirm"
    ~title:"Delete old profile generations?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button ~on_click:(inject Action.Close_dialog) [ Node.text "Cancel" ]
      ; button
          ~cls:"danger"
          ~on_click:(inject Action.Dialog_accept)
          [ Node.text "Delete and collect" ]
      ]
    [ Node.p
        [ Node.text
            (sprintf
               "Collecting Nix garbage, first %s of every user. They can no \
                longer roll back to them; current ones are kept."
               what)
        ]
    ; Node.p
        ~attrs:[ Attr.class_ "dialog-note" ]
        [ Node.text "Collect in Settings keeps the generations." ]
    ]
;;

let tokens n =
  if Float.(n >= 1e6)
  then sprintf "%.1fM" (n /. 1e6)
  else if Float.(n >= 1000.)
  then sprintf "%.1fk" (n /. 1000.)
  else sprintf "%.0f" n
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
    ~attrs:
      [ Attr.class_
          "info-list mb-3 grid grid-cols-[max-content_1fr] gap-x-4 gap-y-1 \
           text-xs [&_dt]:text-muted-foreground [&_dd]:m-0 [&_dd]:min-w-0 \
           [&_dd]:break-words"
      ]
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
    ~cls:"session-dialog w-[min(600px,100%)]"
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
        ~cls:"stats-grid mb-3 grid grid-cols-4 gap-2 narrow:grid-cols-2"
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
               ~cls:"stat flex flex-col rounded-md bg-muted px-2.5 py-2"
               [ span
                   ~cls:
                     "stat-value font-mono text-[15px] font-semibold \
                      tabular-nums"
                   v
               ; span ~cls:"stat-label text-[11.5px] text-muted-foreground" k
               ]))
    ; rows
        [ "Tools", tools
        ; "Model changes", Int.to_string s.model_changes
        ; "Compactions", Int.to_string s.compactions
        ; "Maskings", Int.to_string s.masks
        ; ( "Approvals"
          , sprintf "%d judged · $%.4f" s.approvals_judged s.approvals_cost_usd
          )
        ]
    ]
;;

let text ~title ~text ~inject =
  Modal.view
    ~cls:"text-dialog w-[min(720px,100%)]"
    ~title
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ done_button ~inject ]
    [ Node.pre
        ~attrs:
          [ Attr.class_
              "text-block mt-1.5 max-h-[40vh] overflow-auto rounded-md border \
               border-border bg-muted px-3 py-2.5 font-mono text-xs \
               leading-normal break-words whitespace-pre-wrap \
               in-[.text-dialog]:max-h-none in-[.text-dialog]:overflow-visible"
          ]
        [ Node.text text ]
    ]
;;

(* [/permissions]: the remembered calls come from the state, so one forgotten
   goes at once (with the backend's next state). *)
let permissions (m : App.Model.t) ~rules ~hint ~inject =
  let calls =
    Option.value_map m.state ~default:[] ~f:(fun (s : State.t) ->
      s.allowed_calls)
  in
  Modal.view
    ~cls:
      "text-dialog w-[min(720px,100%)] permissions-dialog [&_.modal-body]:flex \
       [&_.modal-body]:flex-col [&_.modal-body]:gap-2.5 [&_.dialog-note]:m-0"
    ~title:"Permissions"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      ((match calls with
        | [] -> []
        | _ ->
          [ button
              ~cls:"danger forget-all"
              ~title:
                "Smart mode judges every one of these calls again, and other \
                 hosts ask until you name them"
              ~on_click:(inject (Action.Forget_allowed_call None))
              [ Node.text "Forget all" ]
          ])
       @ [ done_button ~inject ])
    [ Node.pre
        ~attrs:
          [ Attr.class_
              "text-block mt-1.5 max-h-[40vh] overflow-auto rounded-md border \
               border-border bg-muted px-3 py-2.5 font-mono text-xs \
               leading-normal break-words whitespace-pre-wrap \
               in-[.text-dialog]:max-h-none in-[.text-dialog]:overflow-visible"
          ]
        [ Node.text rules ]
    ; (match calls with
       | [] -> Node.none
       | calls ->
         Node.div
           ~attrs:
             [ Attr.class_
                 "allowed-calls [&_ul]:mt-1.5 [&_ul]:flex [&_ul]:list-none \
                  [&_ul]:flex-col [&_ul]:gap-1 [&_ul]:p-0"
             ]
           [ Node.p
               ~attrs:[ Attr.class_ "dialog-note" ]
               [ Node.text
                   "Exact calls allowed this session: smart mode does not \
                    judge them again until you forget them. Forgetting one you \
                    let run on another host takes that host back: its calls \
                    ask again. Forget all takes back every other host until \
                    you name it again."
               ]
           ; Node.ul
               (List.map calls ~f:(fun line ->
                  Node.li
                    ~attrs:
                      [ Attr.class_
                          "allowed-call flex items-center gap-2.5 rounded-md \
                           border border-border bg-muted py-1.5 pr-1.5 pl-3 \
                           [&_.forget]:shrink-0 narrow:[&_.forget]:min-h-11"
                      ]
                    [ span
                        ~cls:
                          "allowed-call-text min-w-0 flex-auto font-mono \
                           text-xs leading-normal break-words"
                        line
                    ; button
                        ~cls:"small forget"
                        ~title:"Forget it: smart mode judges it again"
                        ~label:("Forget " ^ line)
                        ~on_click:
                          (inject (Action.Forget_allowed_call (Some line)))
                        [ Node.text "Forget" ]
                    ]))
           ])
    ; Node.div
        (List.map (String.split_lines hint) ~f:(fun line ->
           Node.p ~attrs:[ Attr.class_ "dialog-note" ] [ Node.text line ]))
    ]
;;

(* A long paste of the composer's, before it is sent: read it, put it in the
   box as text after all, or drop it. *)
let pasted (m : App.Model.t) i ~inject =
  match List.nth m.pasted i with
  | None -> None
  | Some t ->
    Some
      (Modal.view
         ~cls:"text-dialog w-[min(720px,100%)] pasted-dialog"
         ~title:
           (sprintf
              "%s · %s"
              (Prigh_ui.Pasted_text.name (i + 1))
              (Prigh_ui.Pasted_text.summary
                 ~lines:(Prigh_ui.Pasted_text.count_lines t)
                 ~bytes:(String.length t)))
         ~on_close:(inject Action.Close_dialog)
         ~footer:
           [ keys_only "Sent whole with the message, not in the box"
           ; button
               ~cls:"danger"
               ~on_click:(inject (Action.Remove_pasted i))
               [ Node.text "Remove" ]
           ; button
               ~title:"Put it in the message box as text"
               ~on_click:(inject (Action.Insert_pasted i))
               [ Node.text "Insert as text" ]
           ; done_button ~inject
           ]
         [ Node.pre
             ~attrs:
               [ Attr.class_
                   "text-block mt-1.5 max-h-[40vh] overflow-auto rounded-md \
                    border border-border bg-muted px-3 py-2.5 font-mono \
                    text-xs leading-normal break-words whitespace-pre-wrap \
                    in-[.text-dialog]:max-h-none \
                    in-[.text-dialog]:overflow-visible"
               ]
             [ Node.text t ]
         ])
;;

let mcp_sign_in ~server ~url ~error ~inject =
  Modal.view
    ~cls:"mcp-sign-in-dialog"
    ~title:("Sign in to " ^ server)
    ~on_close:(inject Action.Close_dialog)
    ~footer:[ keys_only "Esc closes" ]
    [ (match url, error with
       | _, Some e ->
         Node.p ~attrs:[ Attr.class_ "dialog-error" ] [ Node.text e ]
       | None, None ->
         div
           ~cls:"loading-row"
           ~attrs:[ Attr.role "status" ]
           [ Html.lamp (); Node.text "Starting the sign-in…" ]
       | Some url, None ->
         div
           ~cls:"mcp-sign-in"
           [ Node.p
               ~attrs:[ Attr.class_ "dialog-note" ]
               [ Node.text
                   "Sign in on the server's page, in a new tab. The server \
                    connects once you have, and this closes."
               ]
           ; Node.a
               ~attrs:
                 [ Attr.href url
                 ; Attr.target "_blank"
                 ; Attr.create "rel" "noopener noreferrer"
                 ; Attr.class_ "btn primary link-button mcp-sign-in-open"
                 ]
               [ Node.text "Open the sign-in page"; icon External ]
           ; div
               ~cls:
                 "login-url-text font-mono text-[11.5px] break-all \
                  text-muted-foreground"
               [ Node.text url ]
           ])
    ]
;;

let mcp_tools (server : Mcp_server.t) ~inject =
  Modal.view
    ~cls:
      "mcp-tools-dialog w-[min(720px,100%)] [&_.dialog-note]:font-mono \
       [&_.dialog-note]:text-[11.5px] [&_.dialog-note]:break-words"
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
    ~footer:[ keys_only "Esc closes" ]
    [ div
        ~cls:"loading-row"
        ~attrs:[ Attr.role "status" ]
        [ Html.lamp (); Node.text "Loading…" ]
    ]
;;

let view (m : App.Model.t) (dialog : Dialog.t) ~inject =
  match dialog with
  | Hotkeys -> Some (hotkeys ~inject)
  | Filter _ -> None
  | Scoped_models { picker; checked } ->
    Some (scoped_models picker checked ~inject)
  | Prompt p -> Some (prompt m p ~inject)
  | Rewind_confirm { text = t; _ } -> Some (rewind_confirm ~text:t ~inject)
  | Trim_confirm { point; busy; error } ->
    Some (trim_confirm m point ~busy ~error ~inject)
  | Trim_after_confirm { point; busy; error } ->
    Some (trim_after_confirm m point ~busy ~error ~inject)
  | Nix_gc_confirm generations -> Some (nix_gc_confirm generations ~inject)
  | Import_login_confirm { import; busy; error } ->
    Some (import_login_confirm import ~busy ~error ~inject)
  | Annotate _ -> None
  | Temporary_menu -> Some (Temporary_view.menu ~inject)
  | Leave_temporary -> Some (Temporary_view.leave m ~inject)
  | Control _ -> None
  | Create_dir { target; path; busy; error } ->
    Some (create_dir m ~target ~path ~busy ~error ~inject)
  | Session stats -> Some (session m stats ~inject)
  | Text { title; text = t } -> Some (text ~title ~text:t ~inject)
  | Permissions { rules; hint } -> Some (permissions m ~rules ~hint ~inject)
  | Pasted i -> pasted m i ~inject
  | Mcp_tools server -> Some (mcp_tools server ~inject)
  | Mcp_sign_in { server; url; error; source = _ } ->
    Some (mcp_sign_in ~server ~url ~error ~inject)
  | Loading title -> Some (loading ~title ~inject)
  | Diff diff -> Some (Git_diff_view.view ~fold:(App.Model.fold m) diff ~inject)
  | Picker _
  | Popover _
  | Review_pick _
  | Help
  | Settings
  | Rename _
  | Delete _
  | Login _
  | Auth _
  | Forge_host _
  | Login_label _
  | Pulls _
  | Pull _
  | Pr_create _
  | Pr_checked_out _ -> None
  | New_worktree d -> Some (new_worktree m d ~inject)
  | Remove_worktree { worktree; force; busy; error; repo = _ } ->
    Some (remove_worktree worktree ~force ~busy ~error ~inject)
;;
