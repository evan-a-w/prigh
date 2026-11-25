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

let state (s : S.t) =
  match s.installed, s.auth with
  | No, _ -> "bad", "Not installed"
  | Unknown _, _ -> "muted", "Unknown"
  | Yes, Logged_out -> "bad", "Not logged in"
  | Yes, Logged_in ->
    ( "good"
    , Option.value_map
        s.account
        ~default:"Logged in"
        ~f:(sprintf "Logged in · %s") )
  | Yes, (Not_needed | Unknown) -> "good", "Installed"
;;

let adapter ~inject (s : S.t) =
  let level, label = state s in
  let detail =
    match s.installed, s.version with
    | Yes, Some v -> sprintf "%s · %s" v (Option.value s.path ~default:"")
    | Yes, None -> Option.value s.path ~default:""
    | No, _ -> "Not on the tool host's PATH"
    | Unknown "", _ -> "Could not tell whether it is installed"
    | Unknown why, _ -> String.capitalize why
  in
  let copy =
    Option.map (Option.first_some s.problem s.hint) ~f:(fun problem ->
      let text, what =
        match command_in problem with
        | Some command -> command, "the command"
        | None -> problem, "what to do"
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
    ; control (Option.to_list copy)
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

let login_row ~inject (l : Adapter_login.t) =
  let who = Session_engine.adapter_label l.adapter in
  let current =
    match l.login with
    | None -> who ^ "'s own"
    | Some c -> c.label
  in
  row
    ~cls:("adapter-login adapter-login-" ^ l.adapter)
    ~name:(who ^ " login")
    ?note:(Option.map (Adapter_login.warning l) ~f:(fun w -> "warm", w))
    ~about:
      (sprintf
         "Your %s sessions run on %s's own login on their tool host, or on one \
          of your %s logins in prigh (yours or shared with you): prigh hands \
          the host short-lived tokens and counts the usage on that login."
         who
         who
         (Adapter_login.provider_name l))
    [ select
        ~cls:"adapter-login-choice"
        ~title:(sprintf "Choose %s's login (/adapters login %s)" who l.adapter)
        ~label:(sprintf "%s login: %s" who current)
        ~on_click:(inject (Action.Run ("/adapters login " ^ l.adapter)))
        current
    ; button
        ~cls:"small adapter-login-import"
        ~title:
          (sprintf
             "Move %s's own login on the session's tool host into prigh \
              (/adapters import %s)"
             who
             l.adapter)
        ~on_click:(inject (Action.Run ("/adapters import " ^ l.adapter)))
        [ Node.text "Import this host's login" ]
    ]
;;

let groups (m : App.Model.t) ~inject =
  (group [ refresh_row m ~inject ]
   ::
   (match m.adapters.logins with
    | [] -> []
    | logins ->
      [ group ~title:"Logins" (List.map logins ~f:(login_row ~inject)) ]))
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
      group ~title:host (List.map here ~f:(adapter ~inject)))
;;
