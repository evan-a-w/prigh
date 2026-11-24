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
