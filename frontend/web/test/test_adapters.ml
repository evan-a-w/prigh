open! Core
module H = Harness

(* Settings → Engines: the composer's agent CLIs on each tool host, checked
   as the page opens, on Refresh and when a host connects; only on a backend
   deploying the composer. *)

let hello ~features =
  Prigh_web.App.Action.Hello
    { client_id = "c1"
    ; namespace = None
    ; user = None
    ; superuser = false
    ; features
    ; deployed = Prigh_protocol.Deployed_engines.harness_only
    }
;;

let section h = H.text h ~selector:".settings-panel-engines"

let status
      ?(host = "h1")
      ?(host_name = "laptop")
      ?version
      ?account
      ?problem
      ?hint
      ?(auth = "unknown")
      ~installed
      adapter
      label
  =
  let opt key = function
    | None -> []
    | Some v -> [ sprintf "%S:%S" key v ]
  in
  "{"
  ^ String.concat
      ~sep:","
      ([ sprintf
           {|"adapter":%S,"label":%S,"host":%S,"host_name":%S|}
           adapter
           label
           host
           host_name
       ; sprintf
           {|"installed":%s,"auth":%S,"checked_at":1791194400|}
           installed
           auth
       ]
       @ opt "version" version
       @ opt "path" (Option.map version ~f:(fun _ -> "/usr/bin/" ^ adapter))
       @ opt "account" account
       @ opt "problem" problem
       @ opt "hint" hint)
  ^ "}"
;;

let statuses =
  sprintf
    {|{"adapters":[%s]}|}
    (String.concat
       ~sep:","
       [ status
           "codex"
           "Codex"
           ~installed:"true"
           ~version:"0.166.0"
           ~auth:"logged_in"
           ~account:"ChatGPT"
       ; status
           "claude"
           "Claude Code"
           ~installed:"true"
           ~version:"2.1.330"
           ~auth:"logged_out"
           ~problem:
             "Claude Code on laptop is not logged in: run `claude auth login` \
              there"
       ; status
           "codex"
           "Codex"
           ~host:"h2"
           ~host_name:"server"
           ~installed:"false"
           ~problem:"Codex is not installed on server, please install it"
       ; status
           "claude"
           "Claude Code"
           ~host:"h2"
           ~host_name:"server"
           ~installed:"null"
       ; status
           "acp:antigravity"
           "Antigravity"
           ~installed:"true"
           ~version:"1.5.13"
           ~hint:
             "prigh can't tell whether Antigravity is logged in on laptop: if \
              it asks, run `agy acp --login` there"
       ])
;;

let%expect_test "no Engines section unless the backend deploys the composer" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[]));
  H.act h (Settings_section Engines);
  H.texts h ~selector:".settings-tab" |> List.iter ~f:print_endline;
  H.act h (hello ~features:[ "engines" ]);
  H.act h (Settings_section Engines);
  H.texts h ~selector:".settings-tab" |> List.iter ~f:print_endline;
  [%expect
    {|
    (Focus dialog)
    Appearance
    Sessions
    New sessions
    Models
    Usage
    Memory
    About you
    Custom instructions
    MCP servers
    Git hosting
    Account
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_loaded))
    Appearance
    Sessions
    New sessions
    Models
    Engines
    Usage
    Memory
    About you
    Custom instructions
    MCP servers
    Git hosting
    Account
    |}]
;;

let%expect_test
    "opening Engines checks the hosts; a card per host; copy, refresh, events"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.act h (Settings_section Engines);
  section h;
  H.reply h "list_adapters" statuses;
  section h;
  H.click h ~selector:".adapter-claude button";
  H.click h ~selector:".adapter-codex button";
  H.click h ~selector:".adapters-refresh button";
  H.text h ~selector:".adapters-refresh";
  H.event
    h
    (sprintf
       {|{"type":"event","event":"adapters_changed","adapters":[%s]}|}
       (status
          "claude"
          "Claude Code"
          ~installed:"true"
          ~version:"2.1.330"
          ~auth:"logged_in"
          ~account:"l@example.com"));
  section h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_loaded))
    Engines
    The agent CLIs the composer engine drives (Codex, Claude Code, ACP agents) on each tool host: whether they are installed and logged in. prigh only checks: install them and log in on the host itself.
    Agent CLIs
    Checking the tool hosts…
    (Checking…)
    Engines
    The agent CLIs the composer engine drives (Codex, Claude Code, ACP agents) on each tool host: whether they are installed and logged in. prigh only checks: install them and log in on the host itself.
    Agent CLIs
    Checked on each connected tool host when you open this page, when a host connects, and on Refresh. prigh never installs a CLI or logs in: do that on the host.
    (Refresh)
    laptop
    Codex 0.166.0 · /usr/bin/codex Logged in · ChatGPT
    Claude Code 2.1.330 · /usr/bin/claude Not logged in Claude Code on laptop is not logged in: run `claude auth login` there
    (Copy)
    server
    Codex Not on the tool host's PATH Not installed Codex is not installed on server, please install it
    (Copy)
    Claude Code Could not tell whether it is installed Unknown
    laptop
    Antigravity 1.5.13 · /usr/bin/acp:antigravity Installed prigh can't tell whether Antigravity is logged in on laptop: if it asks, run `agy acp --login` there
    (Copy)
    (Copy_text (text "claude auth login")
     (copied "Copied the command to the clipboard"))
    (Copy (text "claude auth login")
     (copied "Copied the command to the clipboard"))
    (Expire_toast (id 0) (after_ms 2900))
    (Copy_text (text "Codex is not installed on server, please install it")
     (copied "Copied what to do to the clipboard"))
    (Copy (text "Codex is not installed on server, please install it")
     (copied "Copied what to do to the clipboard"))
    (Expire_toast (id 1) (after_ms 2860))
    Adapters_refresh
    (Rpc (method_ list_adapters) (params ((refresh true))) (tag Adapters_loaded))
    Agent CLIs
    Checked on each connected tool host when you open this page, when a host connects, and on Refresh. prigh never installs a CLI or logs in: do that on the host.
    (Checking…)
    Engines
    The agent CLIs the composer engine drives (Codex, Claude Code, ACP agents) on each tool host: whether they are installed and logged in. prigh only checks: install them and log in on the host itself.
    Agent CLIs
    Checked on each connected tool host when you open this page, when a host connects, and on Refresh. prigh never installs a CLI or logs in: do that on the host.
    (Checking…)
    laptop
    Claude Code 2.1.330 · /usr/bin/claude Logged in · l@example.com
    |}]
;;

let%expect_test "an error, and no host" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.quiet h (fun () -> H.act h (Settings_section Engines));
  H.fail h "list_adapters" "this backend does not run the composer engine";
  H.text h ~selector:".adapters-refresh";
  H.quiet h (fun () -> H.click h ~selector:".adapters-refresh button");
  H.reply h "list_adapters" {|{"adapters":[]}|};
  section h;
  [%expect
    {|
    Agent CLIs
    Could not check the tool hosts. This backend does not run the composer engine: try Refresh.
    (Refresh)
    Adapters_refresh
    Engines
    The agent CLIs the composer engine drives (Codex, Claude Code, ACP agents) on each tool host: whether they are installed and logged in. prigh only checks: install them and log in on the host itself.
    Agent CLIs
    Checked on each connected tool host when you open this page, when a host connects, and on Refresh. prigh never installs a CLI or logs in: do that on the host.
    (Refresh)
    No tool host
    The agent CLIs run on a tool host: connect one (the TUI, prigh tool-host or the desktop app).
    |}]
;;

let logins ~login =
  sprintf
    {|"adapter_logins":[{"adapter":"codex","provider":"openai-codex","login":%s,"label":%s,"choices":[{"ref":"work","label":"Work"},{"ref":"~alice/main","label":"Alice's ChatGPT"}]}]|}
    (if login then {|"work"|} else "null")
    (if login then {|"Work"|} else "null")
;;

let%expect_test "Engines: Codex's login, own or one of prigh's ChatGPT logins" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.quiet h (fun () -> H.act h (Settings_section Engines));
  H.reply
    h
    "list_adapters"
    (sprintf
       {|{"adapters":[%s],%s}|}
       (status "codex" "Codex" ~installed:"true" ~version:"0.166.0")
       (logins ~login:false));
  H.text h ~selector:".adapter-login";
  H.click h ~selector:".adapter-login-choice";
  H.text h ~selector:".picker-dialog";
  H.click h ~nth:1 ~selector:".picker-item";
  H.reply h "set_adapter_login" (sprintf "{%s}" (logins ~login:true));
  H.text h ~selector:".adapter-login";
  [%expect
    {|
    Codex login
    Your Codex sessions run on Codex's own login on their tool host, or on one of your ChatGPT logins in prigh (yours or shared with you): prigh hands the host short-lived tokens and counts the usage on that login. Import moves the tool host's own login into prigh.
    (Codex's own) (Import)
    (Run "/adapters login codex")
    (Focus picker-input)
    Codex runs on
    (Close (Esc))
    []
    Codex's own login its login on the session's tool host ✓
    Work prigh's: its host gets short-lived tokens
    Alice's ChatGPT prigh's: its host gets short-lived tokens
    ↑↓ to move, Enter to choose, Esc to close
    (Picker_choose work)
    (Rpc (method_ set_adapter_login) (params ((adapter codex) (login work)))
     (tag (Adapter_login_saved codex)))
    (Focus dialog)
    (Expire_toast (id 0) (after_ms 3740))
    Codex login
    Your Codex sessions run on Codex's own login on their tool host, or on one of your ChatGPT logins in prigh (yours or shared with you): prigh hands the host short-lived tokens and counts the usage on that login. Import moves the tool host's own login into prigh.
    (Work) (Import)
    |}]
;;

let%expect_test "a Codex session on prigh's login: Codex · MODEL@LOGIN" =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "engine", `String "composer"
             ; "adapter", `String "codex"
             ; "adapter_login", `String "Work"
             ]
           ())
      ()
  in
  H.text h ~selector:".model-chip";
  [%expect {| (Codex · Opus 5.5 @Work) |}]
;;

let%expect_test "Engines: Claude Code's login warns of Anthropic's terms" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.quiet h (fun () -> H.act h (Settings_section Engines));
  H.reply
    h
    "list_adapters"
    {|{"adapters":[],"adapter_logins":[{"adapter":"claude","provider":"anthropic","login":"max","label":"Max","choices":[{"ref":"max","label":"Max"}]}]}|};
  H.text h ~selector:".adapter-login";
  [%expect
    {|
    Claude Code login
    Your Claude Code sessions run on Claude Code's own login on their tool host, or on one of your Claude Pro/Max logins in prigh (yours or shared with you): prigh hands the host short-lived tokens and counts the usage on that login. Import moves the tool host's own login into prigh. Anthropic's terms keep Claude Pro/Max logins to Claude's own apps: prigh holding one for Claude Code may breach them
    (Max) (Import)
    |}]
;;

let%expect_test
    "a Claude Code session on prigh's login: Claude Code · MODEL@LOGIN"
  =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "engine", `String "composer"
             ; "adapter", `String "claude"
             ; "adapter_login", `String "Max"
             ]
           ())
      ()
  in
  H.text h ~selector:".model-chip";
  [%expect {| (Claude Code · Opus 5.5 @Max) |}]
;;

let import_reply ~confirmed =
  sprintf
    {|{"confirmed":%b,"adapter":"codex","provider":"openai-codex","host":"laptop","path":"/home/u/.codex/auth.json","label":"imported from laptop","warning":"Codex on laptop will stop being logged in.",%s}|}
    confirmed
    (if confirmed
     then
       {|"adapter_logins":[{"adapter":"codex","provider":"openai-codex","login":"imported-from-laptop","label":"imported from laptop","choices":[{"ref":"imported-from-laptop","label":"imported from laptop"}]}]|}
     else {|"note":null|})
;;

let%expect_test "Engines: importing the host's Codex login asks first" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.quiet h (fun () -> H.act h (Settings_section Engines));
  H.reply
    h
    "list_adapters"
    (sprintf {|{"adapters":[],%s}|} (logins ~login:false));
  H.click h ~selector:".adapter-login-import";
  H.reply h "import_adapter_login" (import_reply ~confirmed:false);
  H.text h ~selector:".import-login-confirm";
  (* Refused: the dialog says why and stays. *)
  H.click h ~selector:".import-login-confirm .danger";
  H.fail h "import_adapter_login" "laptop went away";
  H.text h ~selector:".import-login-confirm .dialog-error";
  H.click h ~selector:".import-login-confirm .danger";
  H.reply h "import_adapter_login" (import_reply ~confirmed:true);
  H.text h ~selector:".adapter-login";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Run "/adapters import codex")
    (Rpc (method_ import_adapter_login) (params ((adapter codex)))
     (tag Adapter_import_found))
    (Focus dialog)
    Move Codex's login from laptop into prigh?
    (Close (Esc))
    Codex on laptop will stop being logged in.
    prigh keeps it as "imported from laptop", refreshing it for every tool host that runs Codex on it.
    Enter moves it, Esc cancels (Cancel) (Move it)
    Dialog_accept
    (Rpc (method_ import_adapter_login)
     (params ((adapter codex) (host laptop) (confirm true)))
     (tag Adapter_imported))
    laptop went away
    Dialog_accept
    (Rpc (method_ import_adapter_login)
     (params ((adapter codex) (host laptop) (confirm true)))
     (tag Adapter_imported))
    (Focus dialog)
    (Expire_toast (id 0) (after_ms 6000))
    Codex login
    Your Codex sessions run on Codex's own login on their tool host, or on one of your ChatGPT logins in prigh (yours or shared with you): prigh hands the host short-lived tokens and counts the usage on that login. Import moves the tool host's own login into prigh.
    (imported from laptop) (Import)
    Codex runs on "imported from laptop" from its next turn: laptop's file is now /home/u/.codex/auth.json.moved-to-prigh
    |}]
;;

let%expect_test "Engines: Antigravity's login, on a shared Google login" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (hello ~features:[ "engines" ]));
  H.quiet h (fun () -> H.act h (Settings_section Engines));
  H.reply
    h
    "list_adapters"
    {|{"adapters":[],"adapter_logins":[{"adapter":"acp:antigravity","provider":"google-antigravity","login":"~alice/g","label":"Alice's Google","choices":[{"ref":"~alice/g","label":"Alice's Google"}]}]}|};
  H.text h ~selector:".adapter-login";
  H.click h ~selector:".adapter-login-acp-antigravity .adapter-login-import";
  [%expect
    {|
    Antigravity login
    Your Antigravity sessions run on Antigravity's own login on their tool host, or on one of your Antigravity (Google) logins in prigh (yours or shared with you): prigh hands the host short-lived tokens and counts the usage on that login. Import moves the tool host's own login into prigh.
    (Alice's Google) (Import)
    (Run "/adapters import acp:antigravity")
    (Rpc (method_ import_adapter_login) (params ((adapter acp:antigravity)))
     (tag Adapter_import_found))
    |}]
;;
