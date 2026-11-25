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

module Compaction_command = Prigh_ui.Compaction_command

let compact_at (m : App.Model.t) (config : Config.t option) ~inject =
  let on = Option.exists config ~f:(fun c -> c.compact_at_tokens > 0) in
  let error = Compact_at.error m.compact_at in
  let value =
    match Compact_at.draft m.compact_at, config with
    | Some draft, _ -> draft
    | None, Some c when c.compact_at_tokens > 0 ->
      Compaction_command.tokens c.compact_at_tokens
    | None, _ -> ""
  in
  row
    ~cls:"compact-at"
    ~name:"Compact at"
    ~about:
      "Also summarise once the context passes this many tokens (default 200k), \
       not only at 80% of the model's window."
    ?note:
      (match error, config with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some waiting
       | None, Some _ when not on ->
         Some ("muted", "Off: only at 80% of the window.")
       | None, Some _ -> None)
    [ number_switch
        ~disabled:(Option.is_none config)
        ~id:"compact-at"
        ~label:"Compact at"
        ~unit:"tokens"
        ~value
        ~placeholder:"200k"
        ~title:"/compaction at TOKENS, e.g. 150k or 200000"
        ~switch_title:
          (if on
           then "Only at 80% of the model's window (/compaction at off)"
           else "Also at a number of tokens (/compaction at TOKENS)")
        ~invalid:(Option.is_some error)
        ~on
        ~on_input:(fun text -> inject (Action.Compact_at_input text))
        ~on_commit:(inject Action.Compact_at_commit)
        ~on_switch:(fun on -> inject (Action.Switch_number (Compact_at, on)))
        ()
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

let auto_resume (config : Config.t option) ~inject =
  let on = Option.exists config ~f:(fun c -> c.resume_on_limit_reset) in
  row
    ~cls:"auto-resume"
    ~name:"Resume after a usage limit"
    ~about:
      "A run stopped by a provider's usage limit carries on by itself when the \
       limit resets, if the provider says when. Off: it stays stopped until \
       you send a message."
    [ switch
        ~disabled:(Option.is_none config)
        ~label:"Resume after a usage limit"
        ~title:
          (if on
           then "Stay stopped at a usage limit (/resume off)"
           else "Resume when a usage limit resets (/resume on)")
        ~on
        ~on_click:
          (inject (Action.Run (if on then "/resume off" else "/resume on")))
        ()
    ]
;;

let generate_model (m : App.Model.t) ~inject =
  let current =
    match Option.bind m.config ~f:(fun c -> c.generate_model) with
    | None -> "Default"
    | Some key -> model_name m key
  in
  row
    ~cls:"generate-model"
    ~name:"Drafting"
    ~about:
      (* A no-break space keeps "✦ Generate" on one line. *)
      "Drafts commit messages, pull request descriptions and branch names \
       (✦\xC2\xA0Generate). Default: the session's model."
    [ select
        ~cls:"generate-model-choice"
        ~title:"Choose the model (/generate-model)"
        ~label:("Drafting model: " ^ current)
        ~disabled:(Option.is_none m.config)
        ~on_click:(inject (Action.Run "/generate-model"))
        current
    ]
;;

let approval_model (m : App.Model.t) ~inject =
  let current =
    match Option.bind m.config ~f:(fun c -> c.approval_model) with
    | None -> "Default"
    | Some key -> model_name m key
  in
  row
    ~cls:"approval-model"
    ~name:"Approvals"
    ~about:
      "In smart mode, lets a tool call run or asks you, saying why. Default: \
       the session's model."
    [ select
        ~cls:"approval-model-choice"
        ~title:"Choose the model (/approval-model)"
        ~label:("Approval model: " ^ current)
        ~disabled:(Option.is_none m.config)
        ~on_click:(inject (Action.Run "/approval-model"))
        current
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
       Default: the drafting model, else the session's."
    [ select
        ~cls:"naming-model-choice"
        ~title:"Choose the model (/naming model)"
        ~label:("Naming model: " ^ current)
        ~disabled:(Option.is_none m.config)
        ~on_click:(inject (Action.Run "/naming model"))
        current
    ]
;;

let auto_archive (m : App.Model.t) ~inject =
  let days = Option.map m.config ~f:(fun c -> c.archive_after_days) in
  let on = Option.exists days ~f:(fun d -> Float.(d > 0.)) in
  let error = Archive_after.error m.archive_after in
  let value =
    match Archive_after.draft m.archive_after, days with
    | Some draft, _ -> draft
    | None, Some d when Float.(d > 0.) -> sprintf "%g" d
    | None, _ -> ""
  in
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
      (match error, days with
       | Some e, _ -> Some ("warm", e)
       | None, None -> Some waiting
       | None, Some _ when not on ->
         Some ("muted", "Off: sessions are archived only by hand.")
       | None, Some _ -> None)
    [ number_switch
        ~disabled:(Option.is_none days)
        ~id:"auto-archive"
        ~label:"Archive sessions after"
        ~unit:"days"
        ~value
        ~placeholder:"7"
        ~title:
          (sprintf
             "/auto-archive DAYS: up to %g days"
             Prigh_ui.Auto_archive_command.max_days)
        ~switch_title:
          (if on
           then "Archive only by hand (/auto-archive off)"
           else "Archive sessions without activity (/auto-archive DAYS)")
        ~invalid:(Option.is_some error)
        ~on
        ~on_input:(fun text -> inject (Action.Archive_after_input text))
        ~on_commit:(inject Action.Archive_after_commit)
        ~on_switch:(fun on -> inject (Action.Switch_number (Archive_days, on)))
        ()
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

(* Each provider's logins, once it has one, in a card under its name: open
   a login's menu (use it, make it the default, rename, log out...), share
   it, hide one shared with the user, or add another. *)
let logins (m : App.Model.t) ~inject =
  let module Logins = Prigh_ui.Provider_logins in
  let run command = inject (Action.Run command) in
  List.concat_map m.auth ~f:(fun (p : Auth_status.t) ->
    if Option.is_some p.custom || List.is_empty p.accounts
    then []
    else
      [ group
          ~title:p.name
          (List.map p.accounts ~f:(fun account ->
             Login_usage_view.row
               m
               ~inject
               { Logins.Login.provider = p; account })
           @ [ row
                 ~cls:"provider-login-add"
                 ~name:"Another login"
                 ~about:
                   "Another account, under a label: pin a session to it with \
                    /model MODEL@LABEL."
                 [ button
                     ~cls:"small"
                     ~title:(sprintf "/login %s as LABEL" p.provider)
                     ~label:(sprintf "Add a %s login" p.name)
                     ~on_click:(run (sprintf "/login %s as" p.provider))
                     [ Node.text "Add login" ]
                 ]
             ])
      ])
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
  custom
    ~cls:"git-host"
    [ text
        [ div
            ~cls:"git-host-name flex flex-wrap items-baseline gap-2"
            [ name a.label; host a.host ]
        ; about (String.capitalize (Forges.describe a) ^ ".")
        ; Option.value_map note ~default:Node.none ~f:(fun n ->
            Settings_row.note n)
        ]
    ; control
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
      custom
        ~cls:"nix-store"
        [ text
            ([ name "Disk usage"
             ; Settings_row.about
                 (String.capitalize
                    (sprintf
                       "%s; %s."
                       (Nix_gc_command.size s)
                       (Nix_gc_command.space s)))
             ]
             @ List.map
                 (Nix_gc_command.details ~utc_offset:m.utc_offset s)
                 ~f:(fun line -> note ("muted", String.capitalize line)))
        ; control [ refresh "Refresh" ]
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
    [ custom
        ~cls:"account-setting flex-nowrap items-start narrow:flex-wrap"
        [ Account_view.avatar ~size:`Large name
        ; text
            ~cls:"basis-0 narrow:basis-[calc(100%-54px)]"
            [ Settings_row.name ("Signed in as " ^ name)
            ; (match acting_as with
               | Some ns ->
                 note
                   ( "warm"
                   , sprintf
                       "Acting as %s: you see and work in their sessions."
                       ns )
               | None -> Node.none)
            ; (match host with
               | Some h -> Settings_row.host h
               | None -> Node.none)
            ]
        ; control
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
  custom
    ~cls:"memory-draft"
    ~stack:true
    [ Node.input
        ~attrs:
          [ Attr.id "memory-title"
          ; Attr.class_ (input_cls "memory-title-input")
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
          ; Attr.class_ (textarea_cls "memory-text-input min-h-[4.5em]")
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
       | Some e -> note ("warm", e)
       | None -> Node.none)
    ; buttons
        ~cls:"memory-draft-buttons"
        [ hint
            ~cls:"memory-draft-keys keyboard mr-auto"
            "Ctrl+Enter saves, Esc cancels"
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
let about_you_field
      (m : App.Model.t)
      (field : About_you.Field.t)
      ~inject
      ~disabled
  =
  let a = m.about_you in
  let id =
    "about-you-"
    ^ String.tr (About_you.Field.wire field) ~target:'_' ~replacement:'-'
  in
  let value = About_you.value a field in
  let on_input =
    Attr.on_input (fun _ v -> inject (Action.About_you_field (field, v)))
  in
  let common =
    [ Attr.id id
    ; (if disabled then Attr.disabled else Attr.empty)
    ; (match About_you.max a field with
       | 0 -> Attr.empty
       | max -> Attr.create "maxlength" (Int.to_string max))
    ]
  in
  let control =
    match About_you.Field.choices field with
    | Some choices ->
      div ~cls:"about-you-choice min-w-0" ~attrs:[ Attr.id id ]
      @@ [ segmented
             ~disabled
             ~label:(About_you.Field.label field)
             (List.map
                (("", "Not set", "Leave it out") :: choices)
                ~f:(fun (wire, short, label) ->
                  ( short
                  , label
                  , String.equal wire value
                  , inject (Action.About_you_field (field, wire)) )))
         ]
    | None when About_you.Field.multiline field ->
      Node.textarea
        ~attrs:
          (common
           @ [ Attr.class_ (textarea_cls "about-you-input min-h-[4.5em]")
             ; Attr.placeholder (About_you.Field.placeholder field)
             ; Attr.value value
             ; Attr.rows
                 (match field with
                  | Anything_else -> 5
                  | _ -> 3)
             ; on_input
             ])
        []
    | None ->
      Node.input
        ~attrs:
          (common
           @ [ Attr.class_ (input_cls "about-you-line w-full min-w-0 flex-1")
             ; Attr.type_ "text"
             ; Attr.placeholder (About_you.Field.placeholder field)
             ; Attr.value value
             ; Attr.create "autocomplete" "off"
             ; on_input
             ])
        ()
  in
  let detected =
    match field, m.browser_time_zone with
    | Time_zone, Some zone when not (String.equal (String.strip value) zone) ->
      Some
        (button
           ~cls:"small about-you-detect max-w-1/2 flex-none truncate"
           ~title:("Fill in this browser's time zone, " ^ zone)
           ~disabled
           ~on_click:(inject (Action.About_you_field (Time_zone, zone)))
           [ Node.text ("Use " ^ zone) ])
    | _ -> None
  in
  div
    ~cls:
      (if About_you.Field.multiline field
       then "about-you-field wide flex min-w-0 basis-full flex-col gap-1.5"
       else "about-you-field flex min-w-0 flex-[1_1_220px] flex-col gap-1.5")
    ~attrs:[ Attr.id (id ^ "-field") ]
    [ Node.label
        ~attrs:
          [ (match About_you.Field.choices field with
             | Some _ -> Attr.empty
             | None -> Attr.for_ id)
          ; Attr.class_
              "about-you-label text-[13px] font-medium text-muted-foreground"
          ]
        [ Node.text (About_you.Field.label field) ]
    ; (match detected with
       | None -> control
       | Some detected ->
         div
           ~cls:"about-you-with flex min-w-0 items-center gap-2"
           [ control; detected ])
    ]
;;

let about_you (m : App.Model.t) ~inject =
  let a = m.about_you in
  let size =
    List.sum
      (module Int)
      About_you.Field.all
      ~f:(fun field ->
        match About_you.Field.choices field with
        | Some _ -> 0
        | None -> String.length (String.strip (About_you.value a field)))
  in
  let max = a.info.max_total in
  let dirty = About_you.dirty a in
  let ready = About_you.Status.equal a.status Loaded in
  let note cls text =
    match String.lsplit2 cls ~on:' ' with
    | Some (level, extra) -> Settings_row.note ~cls:extra (level, text)
    | None -> Settings_row.note (cls, text)
  in
  let status =
    match a.status, a.error with
    | _, Some e -> note "warm about-you-error" e
    | Failed e, None ->
      note
        "warm about-you-error"
        (sprintf "Could not read it: %s. Open the section again to retry." e)
    | (Unknown | Loading), None -> note "muted" "Reading what you set…"
    | Loaded, None when a.elsewhere ->
      div
        ~cls:
          "about-you-elsewhere flex items-center gap-2 \
           [&>.setting-note]:min-w-0 [&>.setting-note]:flex-1"
        [ note
            "warm"
            "It was changed somewhere else while you were typing. Saving \
             replaces that with this."
        ; button
            ~cls:"small"
            ~title:"Drop what is typed here for the form as it is saved now"
            ~on_click:(inject Action.About_you_revert)
            [ Node.text "Load that one" ]
        ]
    | Loaded, None when a.saved ->
      note
        "good about-you-saved"
        (if String.is_empty a.info.text
         then "Cleared. Open sessions are told with your next message in them."
         else
           "Saved. New sessions start with it; open ones get it with your next \
            message in them.")
    | Loaded, None -> Node.none
  in
  let disabled = a.saving || not ready in
  custom
    ~cls:"about-you"
    ~stack:true
    [ text
        ~wide:true
        [ name "Tell the model about you"
        ; Settings_row.about
            "Every field is optional. What you fill in is given to the model \
             in every session: new ones start with it, and open ones get it \
             with your next message. Memory is for notes the model looks up \
             only when it needs them."
        ]
    ; div
        ~cls:"about-you-form flex flex-wrap items-start gap-x-4 gap-y-3"
        (List.map About_you.Field.all ~f:(fun field ->
           about_you_field m field ~inject ~disabled))
    ; status
    ; buttons
        ~cls:"about-you-buttons sticky bottom-0 z-1 -mb-2 bg-card py-2"
        [ (if max > 0
           then
             hint
               ~cls:
                 (if size * 10 >= max * 9
                  then
                    "about-you-count near mr-auto tabular-nums text-destructive"
                  else "about-you-count mr-auto tabular-nums")
               (sprintf "%d of %d bytes" size max)
           else Node.none)
        ; hint ~cls:"about-you-keys" "Ctrl+Enter saves"
        ; (if dirty
           then
             button
               ~cls:"small ghost"
               ~title:"Back to the form as it is saved"
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

let note_row_cls = "flex-nowrap items-start"

let forgetting_cls =
  "bg-destructive/10 narrow:flex-wrap narrow:[&>.setting-text]:basis-full \
   narrow:[&>.setting-control]:w-full narrow:[&>.setting-control>.btn]:flex-1"
;;

let note_text text =
  Node.span
    ~attrs:
      [ Attr.class_
          "memory-note-text whitespace-pre-wrap text-sm leading-normal \
           [overflow-wrap:anywhere]"
      ]
    [ Node.text text ]
;;

let memory_note (n : Memory_notes.t) (note : Memory.Note.t) ~inject =
  match n.draft with
  | Some ({ id = Some id; _ } as d) when String.equal id note.id ->
    memory_draft d ~inject
  | _ ->
    let forgetting = Option.equal String.equal n.forgetting (Some note.id) in
    let name = Option.value note.title ~default:"Untitled note" in
    custom
      ~cls:
        (if forgetting
         then "memory-note forgetting " ^ note_row_cls ^ " " ^ forgetting_cls
         else "memory-note " ^ note_row_cls)
      ~attrs:[ Attr.create "data-note" note.id ]
      [ text
          ~cls:"basis-0"
          [ (match note.title with
             | Some title -> Settings_row.name title
             | None -> Node.none)
          ; note_text note.text
          ; Settings_row.note
              (if forgetting
               then
                 "warm", "Delete this note? The model can no longer recall it."
               else "muted", "Saved " ^ Memory.Note.date note)
          ]
      ; control
          ~cls:(if forgetting then "self-center gap-2" else "gap-0.5")
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
                 ~cls:"icon ghost memory-delete hover:text-destructive"
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
      ~cls:"small memory-add whitespace-nowrap"
      ~title:"Write a note for the model yourself"
      ~disabled:(Option.is_some n.draft)
      ~on_click:(inject (Action.Memory_edit None))
      [ icon Plus; Node.text "Add note" ]
  in
  let toolbar =
    custom
      ~cls:"memory-toolbar flex-nowrap gap-2"
      [ Node.input
          ~attrs:
            [ Attr.id "memory-filter"
            ; Attr.class_ (input_cls "memory-filter min-w-0 flex-1")
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
      [ custom
          ~cls:"memory-error"
          ~attrs:[ Attr.role "alert" ]
          [ note ("warm", e) ]
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

(* The user's custom instructions: whether the model may change them, and
   the list, each to edit or delete, with what they cost every session. *)
let instructions_switch (m : App.Model.t) ~inject =
  let n = m.instructions in
  let on = n.instructions.enabled in
  row
    ~cls:"instructions-enabled"
    ~name:"Let the model change them"
    ~about:
      "When you state a lasting preference or standing instruction, the model \
       adds it here or edits the one that covers it, and tells you. Off, only \
       you change them; they stay in every session either way."
    ?note:
      (match n.status with
       | Unknown | Loading -> Some ("muted", "Reading your instructions…")
       | Loaded | Failed _ -> None)
    [ switch
        ~disabled:(not (Instruction_notes.Status.equal n.status Loaded))
        ~label:"Let the model change them"
        ~title:
          (if on
           then "Only you change them (/instructions off)"
           else "Let the model add and edit them (/instructions on)")
        ~on
        ~on_click:
          (inject
             (Action.Run
                (if on then "/instructions off" else "/instructions on")))
        ()
    ]
;;

let instruction_draft
      (n : Instruction_notes.t)
      (d : Instruction_notes.Draft.t)
      ~inject
  =
  let limits = n.instructions.limits in
  custom
    ~cls:"memory-draft instruction-draft"
    ~stack:true
    [ Node.input
        ~attrs:
          [ Attr.id "instruction-title"
          ; Attr.class_ (input_cls "memory-title-input")
          ; Attr.type_ "text"
          ; Attr.create "aria-label" "Title"
          ; Attr.placeholder "Title (optional): a few words"
          ; Attr.value d.title
          ; Attr.create "maxlength" (Int.to_string limits.title)
          ; Attr.create "autocomplete" "off"
          ; (if d.saving then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text -> inject (Action.Instruction_title text))
          ]
        ()
    ; Node.textarea
        ~attrs:
          [ Attr.id "instruction-text"
          ; Attr.class_ (textarea_cls "memory-text-input min-h-[4.5em]")
          ; Attr.create "aria-label" "Instruction"
          ; Attr.placeholder
              "What the model should always do, e.g. “Use Australian spelling.”"
          ; Attr.value d.text
          ; Attr.rows 3
          ; Attr.create "maxlength" (Int.to_string limits.text)
          ; (if d.saving then Attr.disabled else Attr.empty)
          ; Attr.on_input (fun _ text -> inject (Action.Instruction_text text))
          ]
        []
    ; (match d.error with
       | Some e -> note ("warm", e)
       | None -> Node.none)
    ; buttons
        ~cls:"memory-draft-buttons"
        [ hint
            ~cls:"memory-draft-keys keyboard mr-auto"
            "Ctrl+Enter saves, Esc cancels"
        ; button
            ~cls:"small ghost"
            ~title:"Esc"
            ~on_click:(inject Action.Instruction_cancel)
            [ Node.text "Cancel" ]
        ; button
            ~cls:"small primary"
            ~title:"Ctrl+Enter (Enter in the title)"
            ~disabled:d.saving
            ~attrs:(if d.saving then [ Attr.create "aria-busy" "true" ] else [])
            ~on_click:(inject Action.Instruction_save)
            [ Node.text (if d.saving then "Saving…" else "Save") ]
        ]
    ]
;;

let instruction_note
      (n : Instruction_notes.t)
      (note : Custom_instructions.Note.t)
      ~inject
  =
  match n.draft with
  | Some ({ id = Some id; _ } as d) when String.equal id note.id ->
    instruction_draft n d ~inject
  | _ ->
    let removing = Option.equal String.equal n.removing (Some note.id) in
    let name = Option.value note.title ~default:note.id in
    custom
      ~cls:
        (if removing
         then
           "memory-note instruction forgetting "
           ^ note_row_cls
           ^ " "
           ^ forgetting_cls
         else "memory-note instruction " ^ note_row_cls)
      ~attrs:[ Attr.create "data-note" note.id ]
      [ text
          ~cls:"basis-0"
          [ (match note.title with
             | Some title -> Settings_row.name title
             | None -> Node.none)
          ; note_text note.text
          ; Settings_row.note
              (if removing
               then
                 "warm", "Delete this instruction? No session gets it any more."
               else
                 ( "muted"
                 , sprintf "%s · saved %s" note.id (Memory.Note.date note) ))
          ]
      ; control
          ~cls:(if removing then "self-center gap-2" else "gap-0.5")
          (if removing
           then
             [ button
                 ~cls:"small ghost"
                 ~title:"Keep it (Esc)"
                 ~on_click:(inject Action.Instruction_remove_cancel)
                 [ Node.text "Keep" ]
             ; button
                 ~cls:"small danger"
                 ~attrs:[ Attr.id "instruction-remove-yes" ]
                 ~on_click:(inject Action.Instruction_remove_confirm)
                 [ Node.text "Delete" ]
             ]
           else
             [ button
                 ~cls:"icon ghost"
                 ~label:("Edit " ^ name)
                 ~title:"Edit"
                 ~on_click:(inject (Action.Instruction_edit (Some note.id)))
                 [ icon Pencil ]
             ; button
                 ~cls:"icon ghost memory-delete hover:text-destructive"
                 ~label:("Delete " ^ name)
                 ~title:"Delete"
                 ~on_click:(inject (Action.Instruction_remove note.id))
                 [ icon Trash ]
             ])
      ]
;;

let instructions_size (n : Instruction_notes.t) =
  let tokens = Instruction_notes.tokens n in
  let count = List.length n.instructions.notes in
  if tokens > Instruction_notes.warn_tokens
  then
    custom
      ~cls:
        "instructions-warning m-3 flex-nowrap items-start gap-2.5 rounded-md \
         border border-destructive/40 bg-destructive/10 px-3 py-2.5 \
         first:border-t"
      ~attrs:[ Attr.role "status" ]
      [ icon ~cls:"mt-0.5 size-4 flex-none text-destructive" Warning
      ; text
          [ name
              (sprintf
                 "About %s tokens in every session"
                 (Int.to_string_hum ~delimiter:',' tokens))
          ; Settings_row.about
              "They are added to the context of every session, before you say \
               anything: that is cost on every message and room the work no \
               longer has. Trim them: merge near-duplicates, drop what no \
               longer applies, and keep what is only sometimes needed in \
               memory."
          ]
      ]
  else
    custom
      ~cls:"instructions-size py-2"
      [ note
          ( "muted"
          , sprintf
              "%d of %d · about %s tokens in every session"
              count
              n.instructions.limits.count
              (Int.to_string_hum ~delimiter:',' tokens) )
      ]
;;

let instruction_rows (m : App.Model.t) ~inject =
  let n = m.instructions in
  let notes = n.instructions.notes in
  let add =
    button
      ~cls:"small memory-add instruction-add whitespace-nowrap"
      ~title:"Write an instruction for the model yourself"
      ~disabled:(Option.is_some n.draft)
      ~on_click:(inject (Action.Instruction_edit None))
      [ icon Plus; Node.text "Add instruction" ]
  in
  let error =
    match n.error with
    | Some e ->
      [ custom
          ~cls:"memory-error"
          ~attrs:[ Attr.role "alert" ]
          [ note ("warm", e) ]
      ]
    | None -> []
  in
  let draft =
    match n.draft with
    | Some ({ id = None; _ } as d) -> [ instruction_draft n d ~inject ]
    | _ -> []
  in
  match n.status, notes with
  | (Unknown | Loading), _ -> []
  | Failed e, _ ->
    [ row
        ~cls:"memory-failed"
        ~name:"Your instructions"
        ~about:"The backend could not read them."
        ~note:("warm", e)
        [ button
            ~cls:"small ghost"
            ~title:"Read them again (/instructions)"
            ~on_click:(inject (Action.Run "/instructions"))
            [ Node.text "Retry" ]
        ]
    ]
  | Loaded, [] ->
    error
    @ draft
    @ [ row
          ~cls:"instructions-empty"
          ~name:"No instructions yet"
          ~about:
            "Write what the model should always do (“Use Australian spelling”, \
             “Prefer OCaml examples”), or tell it a lasting preference and it \
             adds one here. Every session gets them."
          (if List.is_empty draft then [ add ] else [])
      ]
  | Loaded, _ ->
    (instructions_size n :: error)
    @ List.map notes ~f:(instruction_note n ~inject)
    @ draft
    @
    if List.is_empty draft
    then [ custom ~cls:"instructions-toolbar justify-end py-2.5" [ add ] ]
    else []
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
    [ group ~title:"Session list" [ auto_archive m ~inject ]
    ; group ~title:"Notifications" [ notifications m ~inject ]
    ]
  | New_sessions -> New_sessions_view.groups m ~inject
  | Models ->
    (group ~title:"Providers" [ providers m ~inject ] :: logins m ~inject)
    @ [ group
          ~title:"Models prigh uses"
          ~note:"Besides the conversation's"
          [ generate_model m ~inject
          ; approval_model m ~inject
          ; naming_model m ~inject
          ; summary_model m m.config ~inject
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
  | Custom_instructions ->
    group [ instructions_switch m ~inject ]
    ::
    (match instruction_rows m ~inject with
     | [] -> []
     | rows ->
       [ group
           ~title:"Instructions"
           ?note:
             (match m.instructions.instructions.notes with
              | [] -> None
              | [ _ ] -> Some "1 instruction"
              | notes -> Some (sprintf "%d instructions" (List.length notes)))
           rows
       ])
  | Mcp -> Mcp_settings_view.groups m ~inject
  | Git_hosting -> [ group ~title:"Hosts" (git_hosts m ~inject) ]
  | Nix_store -> [ group (nix_store m ~inject) ]
  | Engines -> Adapters_view.groups m ~inject
  | Usage ->
    group ~title:"At a limit" [ auto_resume m.config ~inject ]
    :: Usage_view.groups m ~inject
  | Account -> [ group (account m ~inject) ]
;;

let section_icon : Section.t -> Icon.t = function
  | Appearance -> Palette
  | Sessions -> Bot
  | New_sessions -> Plus
  | Models -> Brain
  | Engines -> Monitor
  | Memory -> Book
  | About_you -> User
  | Custom_instructions -> Toc
  | Mcp -> Plug
  | Git_hosting -> Repo
  | Nix_store -> Package
  | Usage -> Gauge
  | Account -> User
;;

(* The sections down the side in groups (in the sections' order), as tabs:
   the arrows move between them. The account's tab, last and apart, is the user. *)
let nav_groups : (string * Section.t list) list =
  [ "General", [ Appearance; Sessions; New_sessions ]
  ; "Models", [ Models; Engines; Usage ]
  ; "You", [ Memory; About_you; Custom_instructions ]
  ; "Integrations", [ Mcp; Git_hosting; Nix_store ]
  ]
;;

let face (m : App.Model.t) (section : Section.t) =
  match section, Account_view.who m with
  | Account, Some who -> Account_view.avatar ~size:`Small who.name, who.name
  | _ ->
    ( icon ~cls:"size-4 shrink-0 text-muted-foreground" (section_icon section)
    , Section.name section )
;;

let nav (m : App.Model.t) ~current ~inject =
  let available =
    Section.available
      ~composer:(App.composer_engine m)
      ~superuser:(App.superuser m)
      ()
  in
  let tab (section : Section.t) =
    let selected = Section.equal section current in
    let glyph, label = face m section in
    Node.button
      ~attrs:
        [ Attr.class_
            (Loon.Cn.cn
               [ "settings-tab settings-tab-" ^ Section.arg section
               ; (if selected then "on" else "")
               ; "flex h-8 w-full min-w-0 items-center gap-2 rounded-md px-2 \
                  text-left text-[13px] text-sidebar-foreground/80 \
                  outline-none hover:bg-sidebar-accent \
                  hover:text-sidebar-accent-foreground focus-visible:ring-2 \
                  focus-visible:ring-ring aria-selected:bg-sidebar-accent \
                  aria-selected:font-medium \
                  aria-selected:text-sidebar-accent-foreground \
                  aria-selected:[&>svg]:text-primary"
               ])
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
      [ glyph
      ; Node.span
          ~attrs:[ Attr.class_ "settings-tab-name min-w-0 truncate" ]
          [ Node.text label ]
      ]
  in
  let group (title, sections) =
    match List.filter sections ~f:(List.mem available ~equal:Section.equal) with
    | [] -> Node.none
    | sections ->
      Node.div
        ~attrs:[ Attr.class_ "flex flex-col gap-0.5"; Attr.role "presentation" ]
        (Node.div
           ~attrs:
             [ Attr.class_
                 "px-2 pt-3 pb-1 text-xs font-medium text-muted-foreground"
             ; Attr.create "aria-hidden" "true"
             ]
           [ Node.text title ]
         :: List.map sections ~f:tab)
  in
  Node.div
    ~attrs:
      [ Attr.class_
          "settings-nav loon flex min-h-0 flex-col gap-1 overflow-y-auto \
           border-r bg-sidebar px-2 pt-1 pb-2"
      ; Attr.role "tablist"
      ; Attr.create "aria-label" "Settings sections"
      ; Attr.create "aria-orientation" "vertical"
      ]
    (List.map nav_groups ~f:group
     @
     if List.mem available Account ~equal:Section.equal
     then
       [ Node.div ~attrs:[ Attr.class_ "mt-auto border-t pt-2" ] [ tab Account ]
       ]
     else [])
;;

(* On a phone: the settings fill the screen, a list of the sections (each
   with what it is about) and a level down to one, with a way back, rather
   than a strip of tabs over a page. *)
let phone_home (m : App.Model.t) ~inject =
  let available =
    Section.available
      ~composer:(App.composer_engine m)
      ~superuser:(App.superuser m)
      ()
  in
  let row (section : Section.t) =
    let glyph, about =
      match section, Account_view.who m with
      | Account, Some who -> Account_view.avatar who.name, who.name
      | _ ->
        ( icon ~cls:"size-[18px] text-muted-foreground" (section_icon section)
        , Section.about section )
    in
    Node.button
      ~attrs:
        [ Attr.class_
            ("settings-home-row settings-tab-"
             ^ Section.arg section
             ^ " flex min-h-14 w-full items-center gap-3 border-t px-3 py-2.5 \
                text-left first:border-t-0 active:bg-accent hover:bg-accent \
                focus-visible:bg-accent focus-visible:outline-none")
        ; Attr.type_ "button"
        ; Attr.create "data-settings-tab" (Section.arg section)
        ; Attr.title ("/settings " ^ Section.arg section)
        ; Attr.on_click (fun _ -> inject (Action.Settings_section section))
        ]
      [ Node.span
          ~attrs:
            [ Attr.class_
                "settings-home-icon flex size-9 flex-none items-center \
                 justify-center rounded-md bg-muted"
            ]
          [ glyph ]
      ; Node.span
          ~attrs:
            [ Attr.class_
                "settings-home-text flex min-w-0 flex-1 flex-col gap-0.5"
            ]
          [ Node.span
              ~attrs:[ Attr.class_ "settings-home-name text-base font-medium" ]
              [ Node.text (Section.name section) ]
          ; Node.span
              ~attrs:
                [ Attr.class_
                    "settings-home-about line-clamp-2 text-[13px] leading-snug \
                     text-muted-foreground"
                ]
              [ Node.text about ]
          ]
      ; icon
          ~cls:"settings-home-chevron size-4 flex-none text-muted-foreground"
          Chevron
      ]
  in
  let card sections =
    Node.div
      ~attrs:[ Attr.class_ "overflow-hidden rounded-lg border bg-card" ]
      (List.map sections ~f:row)
  in
  let group (title, sections) =
    match List.filter sections ~f:(List.mem available ~equal:Section.equal) with
    | [] -> []
    | sections ->
      [ Node.div
          ~attrs:
            [ Attr.class_
                "px-1 pt-4 pb-1.5 text-[13px] font-medium text-muted-foreground"
            ; Attr.create "aria-hidden" "true"
            ]
          [ Node.text title ]
      ; card sections
      ]
  in
  Node.nav
    ~attrs:
      [ Attr.class_
          "settings-home loon flex flex-col px-[max(12px,var(--safe-r))] \
           pb-[max(16px,var(--safe-b))] pl-[max(12px,var(--safe-l))]"
      ; Attr.create "aria-label" "Settings sections"
      ]
    (List.concat_map nav_groups ~f:group
     @
     if List.mem available Account ~equal:Section.equal
     then [ Node.div ~attrs:[ Attr.class_ "mt-5" ] [ card [ Account ] ] ]
     else [])
;;

let panel (m : App.Model.t) current ~inject =
  Node.div
    ~key:(Section.arg current)
    ~attrs:
      [ Attr.class_
          ("settings-panel settings-panel-"
           ^ Section.arg current
           ^ " loon min-h-0 overflow-y-auto px-6 pt-5 pb-6 \
              [scrollbar-gutter:stable] animate-in fade-in-0 \
              slide-in-from-bottom-1 duration-150 motion-reduce:animate-none \
              narrow:overflow-visible narrow:px-[max(16px,var(--safe-r))] \
              narrow:pt-4 narrow:pb-[max(24px,var(--safe-b))] \
              narrow:pl-[max(16px,var(--safe-l))] narrow:animate-none")
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
       ~attrs:[ Attr.class_ "settings-panel-head mb-1" ]
       [ Node.h3
           ~attrs:
             [ Attr.class_ "text-lg font-semibold tracking-tight narrow:hidden"
             ]
           [ Node.text (Section.name current) ]
       ; Node.p
           ~attrs:
             [ Attr.class_
                 "mt-1 max-w-[62ch] text-[13px] leading-normal \
                  text-muted-foreground narrow:mt-0 narrow:text-sm"
             ]
           [ Node.text (Section.about current) ]
       ]
     :: content m current ~inject)
;;

let view (m : App.Model.t) ~inject =
  let current =
    Section.visible
      ~composer:(App.composer_engine m)
      m.settings_section
      ~superuser:(App.superuser m)
  in
  let footer =
    if m.narrow
    then []
    else
      [ button
          ~cls:"ghost shortcuts mr-auto px-2"
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
      ~cls:
        "settings-dialog phone-page settings-home-page [&>.modal-body]:block \
         [&>.modal-body]:overflow-y-auto [&>.modal-body]:p-0"
      ~title:"Settings"
      ~on_close:(inject Action.Close_dialog)
      [ phone_home m ~inject ]
  | true, false ->
    Modal.view
      ~cls:
        "settings-dialog phone-page settings-section-page \
         [&>.modal-body]:block [&>.modal-body]:overflow-y-auto \
         [&>.modal-body]:p-0"
      ~title:(Section.name current)
      ~back:("Settings", inject Action.Settings_home)
      ~on_close:(inject Action.Close_dialog)
      [ panel m current ~inject ]
  | false, _ ->
    Modal.view
      ~cls:
        "settings-dialog h-[min(720px,88vh)] max-h-none w-[min(940px,100%)] \
         [&>.modal-body]:grid [&>.modal-body]:flex-auto \
         [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] \
         [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 \
         [&>.modal-buttons]:border-t [&>.modal-buttons]:border-border \
         [&>.modal-head]:border-b [&>.modal-head]:border-border"
      ~title:"Settings"
      ~on_close:(inject Action.Close_dialog)
      ~footer
      [ nav m ~current ~inject; panel m current ~inject ]
;;
