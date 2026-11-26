open! Core
open! Import
open Html
open Settings_row
module Action = App.Action
module S = Adapter_status

let command_in problem =
  match String.split problem ~on:'`' with
  | _ :: command :: _ :: _ when not (String.is_empty command) -> Some command
  | _ -> None
;;

(* [at] (Unix seconds) against the page's clock, else as the backend said. *)
let confirmed ~now ~at ~ago =
  match now with
  | Some now ->
    Some
      (Rel_time.ago
         ~now
         (Time_ns.of_span_since_epoch (Time_ns.Span.of_sec at)))
  | None -> ago
;;

let state ~now (s : S.t) =
  match s.installed, s.auth with
  | No, _ -> "bad", "Not installed"
  | Unknown _, _ -> "muted", "Unknown"
  | Yes, Logged_out -> "bad", "Not logged in"
  | Yes, Logged_in ->
    ( "good"
    , (match s.account, s.via with
       | Some account, _ -> "Logged in · " ^ account
       | None, Some (Prigh_login label) -> "Logged in · prigh login " ^ label
       | None, Some (Session { at; ago }) ->
         Option.value_map
           (confirmed ~now ~at ~ago)
           ~default:"Logged in"
           ~f:(sprintf "Logged in · confirmed %s")
       | None, Some (Login_file _) -> "Logged in · token file found"
       | None, (Some Status_command | None) -> "Logged in") )
  | Yes, Unknown when s.checking -> "muted", "Checking login…"
  | Yes, Unknown when s.checkable -> "muted", "Login not checked yet"
  | Yes, (Not_needed | Unknown) -> "good", "Installed"
;;

let check_button ~inject (s : S.t) =
  if s.checking
  then
    Some
      (button
         ~cls:"small adapter-check"
         ~disabled:true
         ~attrs:[ Attr.create "aria-busy" "true" ]
         ~on_click:Effect.Ignore
         [ Html.lamp (); Node.text "Checking…" ])
  else if s.checkable
  then
    Some
      (button
         ~cls:"small adapter-check"
         ~title:
           (sprintf
              "Start %s on %s briefly to see whether it is logged in \
               (/adapters refresh)"
              s.label
              s.host_name)
         ~on_click:(inject Action.Adapters_refresh)
         [ Node.text "Check" ])
  else None
;;

let adapter ~inject ~now (s : S.t) =
  let level, label = state ~now s in
  let detail =
    match s.installed, s.version with
    | Yes, Some v -> sprintf "%s · %s" v (Option.value s.path ~default:"")
    | Yes, None -> Option.value s.path ~default:""
    | No, _ -> "Not on the tool host's PATH"
    | Unknown "", _ -> "Could not tell whether it is installed"
    | Unknown why, _ -> String.capitalize why
  in
  let copy =
    Option.bind (Option.first_some s.problem s.hint) ~f:(fun problem ->
      let%map.Option text, what =
        match command_in problem, s.problem with
        | Some command, _ -> Some (command, "the command")
        | None, Some _ -> Some (problem, "what to do")
        | None, None -> None
      in
      button
        ~cls:"small"
        ~title:(sprintf "Copy %s: %s" what text)
        ~on_click:
          (inject
             (Action.Copy_text
                { text; copied = sprintf "Copied %s to the clipboard" what }))
        [ Node.text "Copy" ])
  in
  custom
    ~cls:
      ("adapter-status adapter-"
       ^ String.tr s.adapter ~target:':' ~replacement:'-')
    [ Settings_row.text
        ([ name s.label; Settings_row.about detail; note (level, label) ]
         @ Option.value_map s.problem ~default:[] ~f:(fun p ->
           [ note ~cls:"adapter-problem select-text" ("warm", p) ])
         @
         match s.problem, s.hint with
         | None, Some h -> [ note ~cls:"adapter-hint select-text" ("muted", h) ]
         | _ -> [])
    ; control (List.filter_opt [ check_button ~inject s; copy ])
    ]
;;

let refresh_row (m : App.Model.t) ~inject =
  let page = m.adapters in
  let about =
    match page.statuses with
    | None -> "Checking the tool hosts…"
    | Some (Ok _) ->
      "Checked on each connected tool host when you open this page, when a \
       host connects, and on Refresh. prigh never installs a CLI or logs in: \
       do that on the host."
    | Some (Error _) -> "Could not check the tool hosts."
  in
  let note =
    match page.statuses with
    | Some (Error e) -> Some ("warm", String.capitalize e ^ ": try Refresh.")
    | Some (Ok _) | None -> None
  in
  row
    ~cls:"adapters-refresh"
    ?note
    ~name:"Agent CLIs"
    ~about
    [ (if page.loading
       then
         button
           ~cls:"small"
           ~disabled:true
           ~attrs:[ Attr.create "aria-busy" "true" ]
           ~on_click:Effect.Ignore
           [ Html.lamp (); Node.text "Checking…" ]
       else
         button
           ~cls:"small"
           ~title:"Check the hosts again (/adapters refresh)"
           ~on_click:(inject Action.Adapters_refresh)
           [ Node.text "Refresh" ])
    ]
;;

let login_row ~inject ~importing (l : Adapter_login.t) =
  let who = Session_engine.adapter_label l.adapter in
  let current =
    match l.login with
    | None -> who ^ "'s own"
    | Some c -> c.label
  in
  row
    ~cls:
      ("adapter-login adapter-login-"
       ^ String.map l.adapter ~f:(function
         | ':' -> '-'
         | c -> c))
    ~name:(who ^ " login")
    ?note:(Option.map (Adapter_login.warning l) ~f:(fun w -> "warm", w))
    ~about:
      (sprintf
         "Your %s sessions run on %s's own login on their tool host, or on one \
          of your %s logins in prigh (yours or shared with you): prigh hands \
          the host short-lived tokens and counts the usage on that login. \
          Import moves the tool host's own login into prigh."
         who
         who
         (Adapter_login.provider_name l))
    [ select
        ~cls:"adapter-login-choice"
        ~title:(sprintf "Choose %s's login (/adapters login %s)" who l.adapter)
        ~label:(sprintf "%s login: %s" who current)
        ~on_click:(inject (Action.Run ("/adapters login " ^ l.adapter)))
        current
    ; (if Option.equal String.equal importing (Some l.adapter)
       then
         button
           ~cls:"small adapter-login-import"
           ~disabled:true
           ~attrs:[ Attr.create "aria-busy" "true" ]
           ~on_click:Effect.Ignore
           [ Html.lamp (); Node.text "Looking…" ]
       else
         button
           ~cls:"small adapter-login-import"
           ~disabled:(Option.is_some importing)
           ~title:
             (sprintf
                "Move %s's own login on the session's tool host into prigh \
                 (/adapters import %s)"
                who
                l.adapter)
           ~on_click:(inject (Action.Run ("/adapters import " ^ l.adapter)))
           [ Node.text "Import" ])
    ]
;;

let groups (m : App.Model.t) ~inject =
  (group [ refresh_row m ~inject ]
   ::
   (match m.adapters.logins with
    | [] -> []
    | logins ->
      [ group
          ~title:"Logins"
          (List.map
             logins
             ~f:(login_row ~inject ~importing:m.adapters.importing))
      ]))
  @
  match m.adapters.statuses with
  | None | Some (Error _) -> []
  | Some (Ok []) ->
    [ group
        [ row
            ~name:"No tool host"
            ~about:
              "The agent CLIs run on a tool host: connect one (the TUI, prigh \
               tool-host or the desktop app)."
            []
        ]
    ]
  | Some (Ok statuses) ->
    List.map (Adapters_page.by_host statuses) ~f:(fun (host, here) ->
      group ~title:host (List.map here ~f:(adapter ~inject ~now:m.now)))
;;
