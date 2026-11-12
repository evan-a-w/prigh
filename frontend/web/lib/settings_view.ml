open! Core
open! Import
open Html
module Action = App.Action
open Settings_row

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
    ~scope:Browser
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
    ~scope:Browser
    ~name:"Transcript detail"
    ~keys:"Ctrl+O"
    ~about:"How much of tool calls and thinking the transcript shows."
    [ segmented
        ~label:"Transcript detail"
        (List.map Prigh_ui.Verbosity.all ~f:(fun v ->
           let name = Prigh_ui.Verbosity.name v in
           ( String.capitalize name
           , "/verbosity " ^ name
           , Prigh_ui.Verbosity.equal v m.verbosity
           , inject (Action.Run ("/verbosity " ^ name)) )))
    ]
;;

let waiting = "muted", "Waiting for the backend's settings…"

let model_name (m : App.Model.t) key =
  match
    List.find m.models ~f:(fun (model : Llm.t) -> String.equal model.key key)
  with
  | Some model -> model.name
  | None -> key
;;

(* What new sessions ask before they act; the backend applies it to this
   session too. Plan is a session's, not a default: the composer's chip. *)
let default_mode (m : App.Model.t) ~inject =
  let current = Option.map m.config ~f:(fun c -> c.approval_mode) in
  row
    ~cls:"default-mode"
    ~name:"Mode"
    ~about:
      "What asks before it runs: everything (Ask), all but file edits \
       (Auto-edit) or nothing (Auto). This session changes too."
    ?note:(Option.value_map current ~default:(Some waiting) ~f:(fun _ -> None))
    [ segmented
        ~disabled:(Option.is_none current)
        ~label:"Default mode"
        (List.map Approval_mode.all ~f:(fun mode ->
           let arg = Approval_mode.to_string mode in
           ( (match mode with
              | Ask -> "Ask"
              | Auto_edit -> "Auto-edit"
              | Auto -> "Auto")
           , "/confirm " ^ arg
           , Option.equal Approval_mode.equal current (Some mode)
           , inject (Action.Run ("/confirm " ^ arg)) )))
    ]
;;

let default_model (m : App.Model.t) ~inject =
  let current =
    match m.config with
    | None -> None
    | Some { default_model = None; _ } -> Some "the backend's default"
    | Some { default_model = Some key; default_thinking; _ } ->
      Some
        (match default_thinking with
         | Some level when not (String.equal level "off") ->
           sprintf "%s · thinking %s" (model_name m key) level
         | _ -> model_name m key)
  in
  let this =
    Option.map m.state ~f:(fun s ->
      if s.model.supports_thinking && not (String.equal s.thinking "off")
      then sprintf "%s · thinking %s" s.model.name s.thinking
      else s.model.name)
  in
  row
    ~cls:"default-model"
    ~name:"Model"
    ~about:
      (match current with
       | Some current -> "New sessions start with " ^ current ^ "."
       | None -> "The model and thinking level new sessions start with.")
    ?note:(Option.value_map current ~default:(Some waiting) ~f:(fun _ -> None))
    [ button
        ~cls:"small"
        ~title:
          (match this with
           | Some this ->
             sprintf "Start new sessions with %s (/change_default)" this
           | None -> "/change_default")
        ~disabled:(Option.is_none m.config || Option.is_none m.state)
        ~on_click:(inject (Action.Run "/change_default"))
        [ Node.text "Use this session’s" ]
    ]
;;

let default_dir (m : App.Model.t) ~inject =
  let current = Option.bind m.config ~f:(fun c -> c.default_cwd) in
  row
    ~cls:"default-dir"
    ~name:"Starting directory"
    ~about:
      (match current with
       | Some dir -> "New sessions start in " ^ dir ^ "."
       | None ->
         "New sessions start in the tool host's home. /default-dir PATH sets \
          one.")
    (match current with
     | Some _ ->
       [ button
           ~cls:"small"
           ~title:"Start in the home directory again (/default-dir off)"
           ~on_click:(inject (Action.Run "/default-dir off"))
           [ Node.text "Reset" ]
       ]
     | None -> [])
;;

let fallback (m : App.Model.t) ~inject =
  let models =
    Option.value_map m.config ~default:[] ~f:(fun c -> c.fallback_models)
  in
  row
    ~cls:"fallback"
    ~name:"Fallback models"
    ~about:
      (match models with
       | [] ->
         "When a model's usage runs out, the session stops. /fallback MODEL… \
          names models to take over, in order."
       | models ->
         "When a model's usage runs out: "
         ^ String.concat ~sep:", then " (List.map models ~f:(model_name m))
         ^ ".")
    (match models with
     | [] -> []
     | _ ->
       [ button
           ~cls:"small"
           ~title:"No fallback (/fallback off)"
           ~on_click:(inject (Action.Run "/fallback off"))
           [ Node.text "Clear" ]
       ])
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
    | Some key -> model_name m key
  in
  row
    ~cls:"summary-model"
    ~name:"Summaries"
    ~about:
      "Writes the summary when a conversation is compacted: a cheaper model \
       costs less, but cannot reuse the conversation's prompt cache."
    [ select
        ~cls:"summary-model-choice"
        ~title:"Choose the model (/compaction model)"
        ~label:("Summary model: " ^ current)
        ~disabled:(Option.is_none config)
        ~on_click:(inject (Action.Run "/compaction model"))
        current
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

let naming_model (m : App.Model.t) ~inject =
  let current =
    match Option.bind m.config ~f:(fun c -> c.naming_model) with
    | None -> "Default"
    | Some key -> model_name m key
  in
  row
    ~cls:"naming-model"
    ~name:"Naming"
    ~about:
      "Titles sessions and names subagents: a small, fast model is plenty. \
       Default: the generate model, else the session's."
    [ select
        ~cls:"naming-model-choice"
        ~title:"Choose the model (/naming model)"
        ~label:("Naming model: " ^ current)
        ~disabled:(Option.is_none m.config)
        ~on_click:(inject (Action.Run "/naming model"))
        current
    ]
;;

let dictation_model (m : App.Model.t) ~inject =
  let current =
    Option.bind m.config ~f:(fun c -> c.dictation_model)
    |> Option.value ~default:Config.default_dictation_model
  in
  let groq =
    List.exists m.auth ~f:(fun s ->
      String.equal s.provider "groq" && Option.is_some s.custom)
  in
  let note =
    if String.is_prefix current ~prefix:"groq/"
    then None
    else if groq
    then Some ("muted", "Free: groq/whisper-large-v3-turbo, in the list")
    else
      Some
        ( "muted"
        , "Free: Groq's whisper-large-v3-turbo. Add Groq with /login custom \
           (README: Free dictation with Groq)" )
  in
  row
    ?note
    ~cls:"dictation-model"
    ~name:"Dictation"
    ~about:
      "Turns what you say into text (the composer's microphone): an OpenAI or \
       OpenAI-compatible transcription model. Without one, the browser's own \
       speech recognition, where it has one."
    [ select
        ~cls:"dictation-model-choice"
        ~title:"Choose the model (/dictation model)"
        ~label:("Dictation model: " ^ current)
        ~disabled:(Option.is_none m.config)
        ~on_click:(inject (Action.Run "/dictation model"))
        current
    ]
;;

(* A number typed in, with its unit, and Off: like "Compact at". *)
let number_field
      (m : App.Model.t)
      (which : Sessions_fields.Which.t)
      ~id
      ~label
      ~unit
      ~placeholder
      ~title
      ~off_title
      ~disabled
      ~current
      ~inject
  =
  let off = Option.exists current ~f:(fun n -> Float.(n <= 0.)) in
  let error = Sessions_fields.error m.sessions_fields which in
  let value =
    match Sessions_fields.draft m.sessions_fields which, current with
    | Some draft, _ -> draft
    | None, Some n when Float.(n > 0.) -> sprintf "%g" n
    | None, _ -> ""
  in
  Node.div
    ~attrs:
      [ Attr.class_ "segmented"
      ; Attr.role "group"
      ; Attr.create "aria-label" label
      ]
    [ Node.label
        ~attrs:[ classes [ "segment"; "number-field" ] [ "on", not off ] ]
        [ Node.input
            ~attrs:
              [ Attr.id id
              ; Attr.class_ "number-field-input"
              ; Attr.type_ "text"
              ; Attr.create "inputmode" "decimal"
              ; Attr.value value
              ; Attr.placeholder placeholder
              ; Attr.title title
              ; Attr.create "aria-label" (label ^ ", in " ^ unit)
              ; Attr.create
                  "aria-invalid"
                  (Bool.to_string (Option.is_some error))
              ; Attr.create "autocomplete" "off"
              ; Attr.create "spellcheck" "false"
              ; (if disabled then Attr.disabled else Attr.empty)
              ; Attr.on_input (fun _ text ->
                  inject (App.Action.Sessions_field_input (which, text)))
              ; Attr.on_change (fun _ _ ->
                  inject (App.Action.Sessions_field_commit which))
              ]
            ()
        ; span ~cls:"number-field-unit" unit
        ]
    ; Node.button
        ~attrs:
          [ classes [ "segment" ] [ "on", off ]
          ; Attr.type_ "button"
          ; Attr.create "aria-pressed" (Bool.to_string off)
          ; Attr.title off_title
          ; (if disabled
             then Attr.disabled
             else
               Attr.on_click (fun _ ->
                 Effect.Many
                   [ inject (App.Action.Sessions_field_input (which, "off"))
                   ; inject (App.Action.Sessions_field_commit which)
                   ]))
          ]
        [ Node.text "Off" ]
    ]
;;

(* A display preference, the user's on every browser: the TUI's list has no
   folds. *)
let worktree_hours (m : App.Model.t) ~inject =
  let hours = Worktree_folds.hours m.worktree_folds in
  let which = Sessions_fields.Which.Worktree_hours in
  row
    ~cls:"worktree-hours"
    ~scope:Account
    ~name:"Show worktrees"
    ~about:
      (sprintf
         "Show worktrees for sessions used in the last %s; older ones fold \
          them behind their count. Your own fold or unfold of a session wins."
         (if hours = 0
          then "N hours (0 or Off: none)"
          else Worktree_folds.hours_string hours))
    ?note:
      (match Sessions_fields.error m.sessions_fields which with
       | Some e -> Some ("warm", e)
       | None when hours = 0 ->
         Some ("muted", "Off: every session's worktrees start folded.")
       | None -> None)
    [ number_field
        m
        which
        ~id:"worktree-hours"
        ~label:"Show worktrees for sessions used in the last"
        ~unit:"hours"
        ~placeholder:(Int.to_string Worktree_folds.default_hours)
        ~title:
          (sprintf
             "Hours, 0 to %d (default %d)"
             Worktree_folds.max_hours
             Worktree_folds.default_hours)
        ~off_title:"Fold every session's worktrees until opened"
        ~disabled:false
        ~current:(Some (Float.of_int hours))
        ~inject
    ]
;;

let auto_archive (m : App.Model.t) ~inject =
  let which = Sessions_fields.Which.Archive_days in
  let days = Option.map m.config ~f:(fun c -> c.archive_after_days) in
  row
    ~cls:"auto-archive"
    ~name:"Archive after"
    ~about:
      (match days with
       | Some d when Float.(d > 0.) ->
         sprintf
           "Sessions without activity for %s move to Archived (pinned and \
            running ones never do)."
           (Prigh_ui.Auto_archive_command.days d)
       | _ ->
         "Sessions without activity this many days move to Archived (pinned \
          and running ones never do).")
    ?note:
      (match Sessions_fields.error m.sessions_fields which, days with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some waiting
       | None, Some d when Float.(d <= 0.) ->
         Some ("muted", "Off: sessions are archived only by hand.")
       | None, Some _ -> None)
    [ number_field
        m
        which
        ~id:"auto-archive"
        ~label:"Archive sessions after"
        ~unit:"days"
        ~placeholder:"7"
        ~title:
          (sprintf
             "/auto-archive DAYS: 0 to %g days"
             Prigh_ui.Auto_archive_command.max_days)
        ~off_title:"Archive only by hand (/auto-archive off)"
        ~disabled:(Option.is_none m.config)
        ~current:days
        ~inject
    ]
;;

let compaction (m : App.Model.t) ~inject =
  [ compact_at m m.config ~inject; mask m.config ~inject ]
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

(* The model's memory: whether it has it, and the notes, each to edit or
   delete. *)
let memory_switch (m : App.Model.t) ~inject =
  let n = m.memory in
  let on = n.memory.enabled in
  row
    ~cls:"memory-enabled"
    ~name:"Use memory"
    ~about:
      "The model saves a note only when you ask it to remember something, and \
       looks one up when you refer back to it. Off, it is not given the memory \
       tool, and your notes stay here."
    ?note:
      (match n.status with
       | Unknown | Loading -> Some ("muted", "Reading your memory…")
       | Loaded | Failed _ -> None)
    [ switch
        ~disabled:(not (Memory_notes.Status.equal n.status Loaded))
        ~label:"Use memory"
        ~title:
          (if on
           then "Stop giving the model your memory (/memory off)"
           else "Give the model your memory (/memory on)")
        ~on
        ~on_click:
          (inject (Action.Run (if on then "/memory off" else "/memory on")))
        ()
    ]
;;

let memory_draft (d : Memory_notes.Draft.t) ~inject =
  div
    ~cls:"setting memory-draft"
    [ Node.input
        ~attrs:
          [ Attr.id "memory-title"
          ; Attr.class_ "text-input memory-title-input"
          ; Attr.type_ "text"
          ; Attr.create "aria-label" "Title"
          ; Attr.placeholder "Title (optional): a few words"
          ; Attr.value d.title
          ; Attr.create "maxlength" "80"
          ; Attr.create "autocomplete" "off"
          ; (if d.saving then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text -> inject (Action.Memory_title text))
          ]
        ()
    ; Node.textarea
        ~attrs:
          [ Attr.id "memory-text"
          ; Attr.class_ "text-input memory-text-input"
          ; Attr.create "aria-label" "Note"
          ; Attr.placeholder "What the model should remember"
          ; Attr.value d.text
          ; Attr.rows 3
          ; Attr.create "maxlength" "2000"
          ; (if d.saving then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text -> inject (Action.Memory_text text))
          ]
        []
    ; (match d.error with
       | Some e -> span ~cls:"setting-note warm" e
       | None -> Node.none)
    ; div
        ~cls:"memory-draft-buttons"
        [ span ~cls:"memory-draft-keys" "Ctrl+Enter saves, Esc cancels"
        ; button
            ~cls:"small ghost"
            ~title:"Esc"
            ~on_click:(inject Action.Memory_cancel)
            [ Node.text "Cancel" ]
        ; button
            ~cls:"small primary"
            ~title:"Ctrl+Enter (Enter in the title)"
            ~disabled:d.saving
            ~attrs:(if d.saving then [ Attr.create "aria-busy" "true" ] else [])
            ~on_click:(inject Action.Memory_save)
            [ Node.text (if d.saving then "Saving…" else "Save") ]
        ]
    ]
;;

(* What the user wrote about themselves: one text, saved as a whole. *)
let about_you (m : App.Model.t) ~inject =
  let a = m.about_you in
  let text = About_you.text a in
  let size = String.length (String.strip text) in
  let max = a.info.max in
  let dirty = About_you.dirty a in
  let ready = About_you.Status.equal a.status Loaded in
  let note cls text = span ~cls:("setting-note " ^ cls) text in
  let status =
    match a.status, a.error with
    | _, Some e -> note "warm about-you-error" e
    | Failed e, None ->
      note
        "warm about-you-error"
        (sprintf "Could not read it: %s. Open the section again to retry." e)
    | (Unknown | Loading), None -> note "muted" "Reading what you wrote…"
    | Loaded, None when a.elsewhere ->
      div
        ~cls:"about-you-elsewhere"
        [ note
            "warm"
            "It was changed somewhere else while you were typing. Saving \
             replaces that with this."
        ; button
            ~cls:"small"
            ~title:"Drop what is typed here for the text as it is saved now"
            ~on_click:(inject Action.About_you_revert)
            [ Node.text "Load that one" ]
        ]
    | Loaded, None when a.saved ->
      note
        "good about-you-saved"
        (if String.is_empty a.info.text
         then "Removed. Open sessions are told with your next message in them."
         else
           "Saved. New sessions start with it; open ones get it with your next \
            message in them.")
    | Loaded, None -> Node.none
  in
  div
    ~cls:"setting about-you"
    [ div
        ~cls:"setting-text"
        [ span ~cls:"setting-name" "Describe yourself"
        ; span
            ~cls:"setting-about"
            "Who you are, what you work on, how you like answers. This text is \
             given to the model in every session: new ones start with it, and \
             open ones get it with your next message. Memory is for notes the \
             model looks up only when it needs them."
        ]
    ; Node.textarea
        ~attrs:
          [ Attr.id "about-you-text"
          ; Attr.class_ "text-input about-you-input"
          ; Attr.create "aria-label" "About you"
          ; Attr.placeholder
              "I'm a backend engineer working mostly in OCaml. Keep answers \
               short, and ask before adding a dependency."
          ; Attr.value text
          ; Attr.rows 8
          ; (if max > 0
             then Attr.create "maxlength" (Int.to_string max)
             else Attr.empty)
          ; (if a.saving || not ready then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text -> inject (Action.About_you_text text))
          ]
        []
    ; status
    ; div
        ~cls:"about-you-buttons"
        [ (if max > 0
           then
             span
               ~cls:
                 (if size * 10 >= max * 9
                  then "about-you-count near"
                  else "about-you-count")
               (sprintf "%d of %d bytes" size max)
           else Node.none)
        ; span ~cls:"about-you-keys" "Ctrl+Enter saves"
        ; (if dirty
           then
             button
               ~cls:"small ghost"
               ~title:"Back to the text as it is saved"
               ~disabled:a.saving
               ~on_click:(inject Action.About_you_revert)
               [ Node.text "Revert" ]
           else Node.none)
        ; button
            ~cls:"small primary"
            ~title:"Ctrl+Enter"
            ~disabled:(a.saving || not dirty)
            ~attrs:(if a.saving then [ Attr.create "aria-busy" "true" ] else [])
            ~on_click:(inject Action.About_you_save)
            [ Node.text (if a.saving then "Saving…" else "Save") ]
        ]
    ]
;;

let memory_note (n : Memory_notes.t) (note : Memory.Note.t) ~inject =
  match n.draft with
  | Some ({ id = Some id; _ } as d) when String.equal id note.id ->
    memory_draft d ~inject
  | _ ->
    let forgetting = Option.equal String.equal n.forgetting (Some note.id) in
    let name = Option.value note.title ~default:"Untitled note" in
    div
      ~cls:
        (if forgetting
         then "setting memory-note forgetting"
         else "setting memory-note")
      ~attrs:[ Attr.create "data-note" note.id ]
      [ div
          ~cls:"setting-text"
          [ (match note.title with
             | Some title -> span ~cls:"setting-name" title
             | None -> Node.none)
          ; span ~cls:"memory-note-text" note.text
          ; span
              ~cls:
                (if forgetting
                 then "setting-note warm"
                 else "setting-note muted")
              (if forgetting
               then "Delete this note? The model can no longer recall it."
               else "Saved " ^ Memory.Note.date note)
          ]
      ; div
          ~cls:"setting-control"
          (if forgetting
           then
             [ button
                 ~cls:"small ghost"
                 ~title:"Keep it (Esc)"
                 ~on_click:(inject Action.Memory_forget_cancel)
                 [ Node.text "Keep" ]
             ; button
                 ~cls:"small danger"
                 ~attrs:[ Attr.id "memory-forget-yes" ]
                 ~on_click:(inject Action.Memory_forget_confirm)
                 [ Node.text "Delete" ]
             ]
           else
             [ button
                 ~cls:"icon ghost"
                 ~label:("Edit " ^ name)
                 ~title:"Edit"
                 ~on_click:(inject (Action.Memory_edit (Some note.id)))
                 [ icon Pencil ]
             ; button
                 ~cls:"icon ghost memory-delete"
                 ~label:("Delete " ^ name)
                 ~title:"Delete"
                 ~on_click:(inject (Action.Memory_forget note.id))
                 [ icon Trash ]
             ])
      ]
;;

let memory_notes (m : App.Model.t) ~inject =
  let n = m.memory in
  let notes = n.memory.notes in
  let add =
    button
      ~cls:"small memory-add"
      ~title:"Write a note for the model yourself"
      ~disabled:(Option.is_some n.draft)
      ~on_click:(inject (Action.Memory_edit None))
      [ icon Plus; Node.text "Add note" ]
  in
  let toolbar =
    div
      ~cls:"setting memory-toolbar"
      [ Node.input
          ~attrs:
            [ Attr.id "memory-filter"
            ; Attr.class_ "text-input memory-filter"
            ; Attr.type_ "search"
            ; Attr.create "aria-label" "Filter notes"
            ; Attr.placeholder "Filter notes"
            ; Attr.value n.filter
            ; Attr.create "autocomplete" "off"
            ; Attr.create "spellcheck" "false"
            ; Attr.on_input (fun _ text -> inject (Action.Memory_filter text))
            ]
          ()
      ; add
      ]
  in
  let error =
    match n.error with
    | Some e ->
      [ div
          ~cls:"setting memory-error"
          ~attrs:[ Attr.role "alert" ]
          [ span ~cls:"setting-note warm" e ]
      ]
    | None -> []
  in
  let draft =
    match n.draft with
    | Some ({ id = None; _ } as d) -> [ memory_draft d ~inject ]
    | _ -> []
  in
  match n.status, notes with
  | (Unknown | Loading), _ -> []
  | Failed e, _ ->
    [ row
        ~cls:"memory-failed"
        ~name:"Your notes"
        ~about:"The backend could not read them."
        ~note:("warm", e)
        [ button
            ~cls:"small ghost"
            ~title:"Read them again (/memory)"
            ~on_click:(inject (Action.Run "/memory"))
            [ Node.text "Retry" ]
        ]
    ]
  | Loaded, [] ->
    error
    @ draft
    @ [ row
          ~cls:"memory-empty"
          ~name:"No notes yet"
          ~about:
            "Tell the model “remember that…” and it saves a note here. In a \
             later session, ask about it (“what did I say about deploys?”) and \
             it looks it up. It saves nothing you did not ask it to."
          (if List.is_empty draft then [ add ] else [])
      ]
  | Loaded, _ ->
    toolbar
    :: (error
        @ draft
        @
        match Memory_notes.visible n with
        | [] ->
          [ row
              ~cls:"memory-none"
              ~name:"No match"
              ~about:
                (sprintf
                   "No note matches “%s”: change or clear the filter."
                   (String.strip n.filter))
              []
          ]
        | visible -> List.map visible ~f:(memory_note n ~inject))
;;

module Section = Settings_section

(* A section's settings, in groups: a new setting is a row in its list.
   Where a setting is kept is its section's (Section.about) unless its row
   says otherwise. *)
let content (m : App.Model.t) (section : Section.t) ~inject =
  match section with
  | Appearance ->
    [ group ~title:"Look" (Theme_view.rows m ~inject)
    ; group ~title:"Layout" [ verbosity m ~inject ]
    ]
  | Sessions ->
    [ group
        ~title:"New sessions"
        [ default_model m ~inject
        ; default_mode m ~inject
        ; default_dir m ~inject
        ; fallback m ~inject
        ]
    ; group
        ~title:"Session list"
        [ worktree_hours m ~inject; auto_archive m ~inject ]
    ; group ~title:"Notifications" [ notifications m ~inject ]
    ]
  | Models ->
    [ group ~title:"Providers" [ providers m ~inject ]
    ; group
        ~title:"Models prigh uses"
        ~note:"Besides the conversation's"
        [ naming_model m ~inject
        ; summary_model m m.config ~inject
        ; dictation_model m ~inject
        ]
    ; group ~title:"Compaction" (compaction m ~inject)
    ]
  | Memory ->
    group [ memory_switch m ~inject ]
    ::
    (match memory_notes m ~inject with
     | [] -> []
     | rows ->
       [ group
           ~title:"Notes"
           ?note:
             (match m.memory.memory.notes with
              | [] -> None
              | [ _ ] -> Some "1 note"
              | notes -> Some (sprintf "%d notes" (List.length notes)))
           rows
       ])
  | About_you -> [ group [ about_you m ~inject ] ]
  | Git_hosting -> [ group ~title:"Hosts" (git_hosts m ~inject) ]
  | Nix_store -> [ group (nix_store m ~inject) ]
  | Account -> [ group (account m ~inject) ]
;;

let section_icon : Section.t -> Icon.t = function
  | Appearance -> Palette
  | Sessions -> Bot
  | Models -> Brain
  | Memory -> Book
  | About_you -> User
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

(* On a phone: the settings fill the screen, a list of the sections (each
   with what it is about) and a level down to one, with a way back, rather
   than a strip of tabs over a page. *)
let phone_home (m : App.Model.t) ~inject =
  let row (section : Section.t) =
    let face, about =
      match section, Account_view.who m with
      | Account, Some who ->
        span ~cls:"avatar" (Account_view.initial who.name), who.name
      | _ -> icon (section_icon section), Section.about section
    in
    Node.button
      ~attrs:
        [ classes
            [ "settings-home-row"; "settings-tab-" ^ Section.arg section ]
            []
        ; Attr.type_ "button"
        ; Attr.create "data-settings-tab" (Section.arg section)
        ; Attr.title ("/settings " ^ Section.arg section)
        ; Attr.on_click (fun _ -> inject (Action.Settings_section section))
        ]
      [ Node.span ~attrs:[ Attr.class_ "settings-home-icon" ] [ face ]
      ; Node.span
          ~attrs:[ Attr.class_ "settings-home-text" ]
          [ span ~cls:"settings-home-name" (Section.name section)
          ; span ~cls:"settings-home-about" about
          ]
      ; icon ~cls:"settings-home-chevron" Chevron
      ]
  in
  Node.nav
    ~attrs:
      [ Attr.class_ "settings-home"
      ; Attr.create "aria-label" "Settings sections"
      ]
    (List.map (Section.available ~superuser:(App.superuser m)) ~f:row)
;;

let panel (m : App.Model.t) current ~inject =
  Node.div
    ~key:(Section.arg current)
    ~attrs:
      [ classes [ "settings-panel"; "settings-panel-" ^ Section.arg current ] []
      ; Attr.id "settings-panel"
      ; (if m.narrow
         then Attr.create "aria-label" (Section.name current)
         else
           Attr.many
             [ Attr.role "tabpanel"
             ; Attr.create "aria-labelledby" (Section.tab_id current)
             ])
      ]
    (Node.header
       ~attrs:[ Attr.class_ "settings-panel-head" ]
       [ Node.h3 [ Node.text (Section.name current) ]
       ; Node.p [ Node.text (Section.about current) ]
       ]
     :: content m current ~inject)
;;

let view (m : App.Model.t) ~inject =
  let current =
    Section.visible m.settings_section ~superuser:(App.superuser m)
  in
  let footer =
    if m.narrow
    then []
    else
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
  in
  match m.narrow, m.settings_home with
  | true, true ->
    Modal.view
      ~cls:"settings-dialog phone-page settings-home-page"
      ~title:"Settings"
      ~on_close:(inject Action.Close_dialog)
      [ phone_home m ~inject ]
  | true, false ->
    Modal.view
      ~cls:"settings-dialog phone-page settings-section-page"
      ~title:(Section.name current)
      ~back:("Settings", inject Action.Settings_home)
      ~on_close:(inject Action.Close_dialog)
      [ panel m current ~inject ]
  | false, _ ->
    Modal.view
      ~cls:"settings-dialog"
      ~title:"Settings"
      ~on_close:(inject Action.Close_dialog)
      ~footer
      [ nav m ~current ~inject; panel m current ~inject ]
;;
