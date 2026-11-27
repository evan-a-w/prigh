open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Whether the composer's CLIs are installed and logged in on a host
   ([Adapter_health]), from [$probe_programs] replies, and [list_adapters] /
   [adapters_changed] over the RPC. *)

let adapters : Adapter_id.t list = [ Codex; Claude; Acp "gemini"; Prigh ]

let mask_version s =
  String.substr_replace_all s ~pattern:Version.to_string ~with_:"<version>"
;;

let show statuses =
  List.iter statuses ~f:(fun (s : Adapter_health.Status.t) ->
    let s = { s with checked_at = 0. } in
    printf
      "%s: %s\n  %s\n"
      (Adapter_id.to_string s.adapter)
      (mask_version
       @@ Sexp.to_string_hum
            [%sexp
              (s.install : Adapter_health.Install.t)
            , (s.auth : Adapter_health.Auth.t)])
      (match Adapter_health.Status.problem s, Adapter_health.Status.hint s with
       | Some problem, _ -> problem
       | None, Some hint -> "(ready) " ^ hint
       | None, None -> "(ready)"))
;;

let of_text ?(is_error = false) text =
  Adapter_health.of_result
    adapters
    ~host:"h1"
    ~host_name:"laptop"
    ~checked_at:0.
    { Tool.Result.text; is_error; images = [] }
;;

let%expect_test "the request: each CLI's --version and login-status command" =
  print_endline
    (Json.to_string
       (Host_probe.Request.to_json (Adapter_health.request adapters)));
  [%expect
    {| {"programs":[{"name":"codex","version":["--version"],"status":["login","status"]},{"name":"claude","version":["--version"],"status":["auth","status"]},{"name":"gemini","version":["--version"],"status":[]}],"paths":[],"env":[]} |}]
;;

let out ?(stderr = "") exit stdout =
  sprintf {|{"exit":%d,"stdout":%S,"stderr":%S}|} exit stdout stderr
;;

let%expect_test "installed and logged in" =
  show
    (of_text
       (sprintf
          {|{"programs":[
              {"name":"codex","path":"/usr/bin/codex","login_shell_path":null,
               "version":%s,"status":%s},
              {"name":"claude","path":"/home/u/.local/bin/claude","login_shell_path":null,
               "version":%s,"status":%s},
              {"name":"gemini","path":"/usr/bin/gemini","login_shell_path":null,
               "version":%s,"status":null}],
            "paths":[],"env":{}}|}
          (out 0 "codex-cli 0.166.0\n")
          (out 0 "" ~stderr:"Logged in using ChatGPT\n")
          (out 0 "2.1.330 (Claude Code)\n")
          (out
             0
             {|{"loggedIn":true,"authMethod":"claude.ai","email":"lloyd@example.com"}|})
          (out 0 "v0.9.1\n")));
  [%expect
    {|
    codex: ((Installed (version (0.166.0)) (path /usr/bin/codex))
     (Logged_in (account (ChatGPT)) (via Status_command)))
      (ready)
    claude: ((Installed (version (2.1.330)) (path /home/u/.local/bin/claude))
     (Logged_in (account (lloyd@example.com)) (via Status_command)))
      (ready)
    acp:gemini: ((Installed (version (0.9.1)) (path /usr/bin/gemini)) Unknown)
      (ready) Login not checked yet: Check (/adapters refresh) starts Gemini CLI on laptop briefly to see
    prigh: ((Installed (version (<version>)) (path prigh)) Not_needed)
      (ready)
    |}]
;;

let%expect_test "Codex's sandbox: probed with [codex sandbox -- true]" =
  print_s [%sexp (Codex_sandbox.request : Host_probe.Request.t)];
  let sandbox ?(is_error = false) text =
    let path, status =
      Codex_sandbox.of_result { Tool.Result.text; is_error; images = [] }
    in
    print_s [%sexp (path : string option), (status : Codex_sandbox.Status.t)]
  in
  let ran output =
    sandbox
      (sprintf
         {|{"programs":[{"name":"codex","path":"/usr/bin/codex","version":%s}],"paths":[],"env":{}}|}
         output)
  in
  ran (out 0 "");
  (* Docker's default seccomp profile *)
  ran
    (out
       1
       ""
       ~stderr:
         "WARNING: proceeding\n\
          bwrap: No permissions to create a new namespace, likely because the \
          kernel does not allow non-privileged user namespaces.\n");
  ran (out 1 "" ~stderr:"bwrap: setting up uid map: Permission denied\n");
  (* a Codex without [sandbox] keeps its sandbox *)
  ran (out 2 "" ~stderr:"error: unrecognized subcommand 'sandbox'\n");
  sandbox
    {|{"programs":[{"name":"codex","path":null,"version":null}],"paths":[],"env":{}}|};
  sandbox ~is_error:true {|tool host "h1" is not connected|};
  [%expect
    {|
    ((programs (((name codex) (version (sandbox -- true)) (status ()))))
     (paths ()) (env ()))
    ((/usr/bin/codex) Works)
    ((/usr/bin/codex) (Unavailable "no user namespaces"))
    ((/usr/bin/codex)
     (Unavailable "bwrap: setting up uid map: Permission denied"))
    ((/usr/bin/codex) (Unknown "error: unrecognized subcommand 'sandbox'"))
    (() (Unknown "codex is not installed"))
    (() (Unknown "tool host \"h1\" is not connected"))
    |}];
  (* Probed once per host and binary. *)
  let cache = Codex_sandbox.Cache.create () in
  let status ~host ~installed =
    Codex_sandbox.Cache.status cache ~host ~installed ~exec:(fun ~name:_ _ ->
      printf "probing %s\n" host;
      { Tool.Result.text =
          sprintf
            {|{"programs":[{"name":"codex","path":"/usr/bin/codex","version":%s}],"paths":[],"env":{}}|}
            (out 1 "" ~stderr:"bwrap: Creating new namespace failed\n")
      ; is_error = false
      ; images = []
      })
    |> [%sexp_of: Codex_sandbox.Status.t]
    |> print_s
  in
  status ~host:"h1" ~installed:None;
  status ~host:"h1" ~installed:(Some "/usr/bin/codex");
  status ~host:"h2" ~installed:None;
  status ~host:"h1" ~installed:(Some "/home/u/.nix-profile/bin/codex");
  Codex_sandbox.Cache.forget_host cache ~host:"h2";
  status ~host:"h2" ~installed:None;
  [%expect
    {|
    probing h1
    (Unavailable "no user namespaces")
    (Unavailable "no user namespaces")
    probing h2
    (Unavailable "no user namespaces")
    probing h1
    (Unavailable "no user namespaces")
    probing h2
    (Unavailable "no user namespaces")
    |}];
  (* What Settings → Engines says of it. *)
  show
    [ Adapter_health.Status.with_evidence
        ~no_sandbox:"no user namespaces"
        (List.hd_exn
           (of_text
              (sprintf
                 {|{"programs":[{"name":"codex","path":"/usr/bin/codex","version":%s,"status":%s}],"paths":[],"env":{}}|}
                 (out 0 "codex-cli 0.171.0\n")
                 (out 0 "" ~stderr:"Logged in using ChatGPT\n"))))
        ~prigh_login:None
        ~seen:None
        ~checking:false
    ];
  [%expect
    {|
    codex: ((Installed (version (0.171.0)) (path /usr/bin/codex))
     (Logged_in (account (ChatGPT)) (via Status_command)))
      (ready) Codex's sandbox cannot run on laptop (no user namespaces): it runs without it, and prigh still asks before its commands and edits as the session's mode says
    |}]
;;

let%expect_test "not installed (one only in a login shell), logged out" =
  show
    (of_text
       (sprintf
          {|{"programs":[
              {"name":"codex","path":null,"login_shell_path":null,"version":null,"status":null},
              {"name":"claude","path":"/usr/bin/claude","login_shell_path":null,
               "version":%s,"status":%s},
              {"name":"gemini","path":null,"login_shell_path":"/home/u/.npm/bin/gemini",
               "version":null,"status":null}],
            "paths":[],"env":{}}|}
          (out 0 "2.1.330 (Claude Code)\n")
          (out 1 {|{"loggedIn":false,"authMethod":"none"}|})));
  [%expect
    {|
    codex: ((Not_installed (login_shell_path ())) Unknown)
      Codex is not installed on laptop, please install it
    claude: ((Installed (version (2.1.330)) (path /usr/bin/claude)) Logged_out)
      Claude Code on laptop is not logged in: run `claude auth login` there
    acp:gemini: ((Not_installed (login_shell_path (/home/u/.npm/bin/gemini))) Unknown)
      Gemini CLI is installed on laptop at /home/u/.npm/bin/gemini, but not on the PATH of prigh's tool host there: add /home/u/.npm/bin to that PATH and restart the tool host
    prigh: ((Installed (version (<version>)) (path prigh)) Not_needed)
      (ready)
    |}];
  (* Codex logged out; a status command that timed out says nothing. *)
  show
    (List.take
       (of_text
          (sprintf
             {|{"programs":[
                 {"name":"codex","path":"/usr/bin/codex","login_shell_path":null,
                  "version":%s,"status":%s},
                 {"name":"claude","path":"/usr/bin/claude","login_shell_path":null,
                  "version":%s,"status":{"exit":"timed_out","stdout":"","stderr":""}}],
               "paths":[],"env":{}}|}
             (out 0 "codex-cli 0.166.0")
             (out 1 "" ~stderr:"Not logged in\n")
             (out 0 "2.1.330 (Claude Code)")))
       2);
  [%expect
    {|
    codex: ((Installed (version (0.166.0)) (path /usr/bin/codex)) Logged_out)
      Codex on laptop is not logged in: run `codex login` there
    claude: ((Installed (version (2.1.330)) (path /usr/bin/claude)) Unknown)
      (ready) prigh can't tell whether Claude Code is logged in on laptop: if it asks, run `claude auth login` there
    |}]
;;

let%expect_test
    "ACP agents: their login commands and the oldest version that speaks ACP"
  =
  let adapters : Adapter_id.t list =
    [ Acp "antigravity"; Acp "grok"; Acp "opencode" ]
  in
  let probe ~agy =
    show
      (Adapter_health.of_result
         adapters
         ~host:"h1"
         ~host_name:"laptop"
         ~checked_at:0.
         { Tool.Result.text =
             sprintf
               {|{"programs":[
                   {"name":"agy","path":"/usr/bin/agy","login_shell_path":null,"version":%s,"status":null},
                   {"name":"grok","path":"/usr/bin/grok","login_shell_path":null,"version":%s,"status":null},
                   {"name":"opencode","path":null,"login_shell_path":null,"version":null,"status":null}],
                 "paths":[],"env":{}}|}
               (out 0 agy)
               (out 0 "grok 1.0.41\n")
         ; is_error = false
         ; images = []
         })
  in
  probe ~agy:"1.4.12\n";
  probe ~agy:"agy 1.5.13\n";
  [%expect
    {|
    acp:antigravity: ((Installed (version (1.4.12)) (path /usr/bin/agy)) Unknown)
      Antigravity 1.4.12 on laptop is too old to drive: update it to 1.5.0 or later there
    acp:grok: ((Installed (version (1.0.41)) (path /usr/bin/grok)) Unknown)
      (ready) Login not checked yet: Check (/adapters refresh) starts Grok Build on laptop briefly to see
    acp:opencode: ((Not_installed (login_shell_path ())) Unknown)
      OpenCode is not installed on laptop, please install it
    acp:antigravity: ((Installed (version (1.5.13)) (path /usr/bin/agy)) Unknown)
      (ready) Login not checked yet: Check (/adapters refresh) starts Antigravity on laptop briefly to see
    acp:grok: ((Installed (version (1.0.41)) (path /usr/bin/grok)) Unknown)
      (ready) Login not checked yet: Check (/adapters refresh) starts Grok Build on laptop briefly to see
    acp:opencode: ((Not_installed (login_shell_path ())) Unknown)
      OpenCode is not installed on laptop, please install it
    |}]
;;

let%expect_test "a host that cannot say" =
  (* An older prigh without [$probe_programs]. *)
  show
    (List.take
       (of_text ~is_error:true {|unknown host tool "$probe_programs"|})
       2);
  [%expect
    {|
    codex: ((Unknown
      "its prigh is too old to check: update prigh there and restart its TUI or tool-host")
     Unknown)
      can't tell whether Codex is installed on laptop: its prigh is too old to check: update prigh there and restart its TUI or tool-host
    claude: ((Unknown
      "its prigh is too old to check: update prigh there and restart its TUI or tool-host")
     Unknown)
      can't tell whether Claude Code is installed on laptop: its prigh is too old to check: update prigh there and restart its TUI or tool-host
    |}];
  show
    (List.take (of_text ~is_error:true {|tool host "h1" is not connected|}) 1);
  [%expect
    {|
    codex: ((Unknown "tool host \"h1\" is not connected") Unknown)
      can't tell whether Codex is installed on laptop: tool host "h1" is not connected
    |}];
  show (List.take (of_text "not json") 1);
  [%expect
    {|
    codex: ((Unknown "bad reply from the tool host") Unknown)
      can't tell whether Codex is installed on laptop: bad reply from the tool host
    |}]
;;

let%expect_test "JSON" =
  List.iter
    (of_text
       (sprintf
          {|{"programs":[
              {"name":"codex","path":"/usr/bin/codex","login_shell_path":null,
               "version":%s,"status":%s},
              {"name":"claude","path":null,"login_shell_path":null,"version":null,"status":null}],
            "paths":[],"env":{}}|}
          (out 0 "codex-cli 0.166.0")
          (out 0 "Logged in using an API key - sk-***")))
    ~f:(fun s ->
      print_endline
        (mask_version (Json.to_string (Adapter_health.Status.to_json s))));
  [%expect
    {|
    {"adapter":"codex","label":"Codex","host":"h1","host_name":"laptop","installed":true,"version":"0.166.0","path":"/usr/bin/codex","auth":"logged_in","account":"API key","via":"status","checked_at":0}
    {"adapter":"claude","label":"Claude Code","host":"h1","host_name":"laptop","installed":false,"auth":"unknown","problem":"Claude Code is not installed on laptop, please install it","checked_at":0}
    {"adapter":"acp:gemini","label":"Gemini CLI","host":"h1","host_name":"laptop","installed":null,"unknown":"the host did not report it","auth":"unknown","problem":"can't tell whether Gemini CLI is installed on laptop: the host did not report it","checked_at":0}
    {"adapter":"prigh","label":"prigh","host":"h1","host_name":"laptop","installed":true,"version":"<version>","path":"prigh","auth":"not_needed","checked_at":0}
    |}]
;;

let%expect_test "the cache" =
  let cache = Adapter_health.Cache.create ~ttl:60. in
  let status host adapter checked_at : Adapter_health.Status.t =
    { adapter
    ; host
    ; host_name = host
    ; install = Not_installed { login_shell_path = None }
    ; auth = Unknown
    ; checked_at
    ; quirks = Acp_agent.Quirks.none
    ; checking = false
    ; check_failed = None
    ; no_sandbox = None
    }
  in
  Adapter_health.Cache.set
    cache
    [ status "a" Codex 100.; status "a" Claude 100.; status "b" Codex 130. ];
  let fresh now host adapter =
    printf
      "%s/%s at %g: %b\n"
      host
      (Adapter_id.to_string adapter)
      now
      (Adapter_health.Cache.fresh cache ~now ~host adapter)
  in
  fresh 150. "a" Codex;
  fresh 170. "a" Codex;
  fresh 170. "b" Codex;
  Adapter_health.Cache.forget_host cache ~host:"a";
  fresh 150. "a" Claude;
  Adapter_health.Cache.forget cache ~host:"b" Codex;
  fresh 150. "b" Codex;
  [%expect
    {|
    a/codex at 150: true
    a/codex at 170: false
    b/codex at 170: true
    a/claude at 150: false
    b/codex at 150: false
    |}]
;;

(* An ACP agent has no login-status command: its login shows when it is
   started (the model-list check, a session) or when it runs on one of
   prigh's logins. *)
let acp_status ?(files = "[]") name =
  let agent = Option.value_exn (Acp_agent.find_known name) in
  let program = List.hd_exn agent.command in
  List.hd_exn
    (Adapter_health.of_result
       [ Acp name ]
       ~host:"h1"
       ~host_name:"desktop"
       ~checked_at:0.
       { Tool.Result.text =
           sprintf
             {|{"programs":[{"name":%S,"path":"/bin/%s","login_shell_path":null,"version":%s,"status":null}],"paths":%s,"env":{}}|}
             program
             program
             (out 0 "1.5.13")
             files
       ; is_error = false
       ; images = []
       })
;;

let show_evidence ?(prigh_login = None) ?seen ?(checking = false) s =
  let s = Adapter_health.Status.with_evidence s ~prigh_login ~seen ~checking in
  match Adapter_health.Status.to_json ~now:600. s with
  | `Object fields ->
    List.iter fields ~f:(fun (k, v) ->
      match k with
      | "auth"
      | "account"
      | "via"
      | "prigh_login"
      | "confirmed_at"
      | "confirmed"
      | "login_file"
      | "checking"
      | "checkable"
      | "problem"
      | "hint" -> printf "  %s: %s\n" k (Json.to_string v)
      | _ -> ())
  | _ -> assert false
;;

let%expect_test "ACP agents' logins: unknown, checking, seen, prigh's" =
  let s = acp_status "antigravity" in
  print_endline "nothing known:";
  show_evidence s;
  print_endline "checking:";
  show_evidence s ~checking:true;
  print_endline "opened a session (the model list) at 300:";
  show_evidence s ~seen:(Opened { at = 300. });
  print_endline "refused (-32000 Authentication required):";
  show_evidence s ~seen:(Refused { at = 300. });
  print_endline "the check failed:";
  show_evidence s ~seen:(Check_failed { at = 300.; why = "it timed out" });
  print_endline "on one of prigh's logins (refused on its own):";
  show_evidence
    s
    ~prigh_login:(Some "Work Google")
    ~seen:(Refused { at = 300. });
  print_endline "its token file is there:";
  show_evidence
    (acp_status
       "antigravity"
       ~files:
         {|[{"path":"/home/l/.gemini/antigravity-cli/antigravity-oauth-token","exists":true}]|});
  print_endline "...but a session was refused since:";
  show_evidence
    (acp_status
       "antigravity"
       ~files:
         {|[{"path":"/home/l/.gemini/antigravity-cli/antigravity-oauth-token","exists":true}]|})
    ~seen:(Refused { at = 300. });
  [%expect
    {|
    nothing known:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts Antigravity on desktop briefly to see"
    checking:
      auth: "unknown"
      checking: true
      hint: "Checking whether Antigravity is logged in on desktop: prigh starts it briefly to see"
    opened a session (the model list) at 300:
      auth: "logged_in"
      via: "session"
      confirmed_at: 300
      confirmed: "5m ago"
    refused (-32000 Authentication required):
      auth: "logged_out"
      problem: "Antigravity on desktop is not logged in: run `agy acp --login` there"
    the check failed:
      auth: "unknown"
      checkable: true
      hint: "Couldn't check whether Antigravity is logged in on desktop (it timed out): Check tries again; if it asks you to log in, run `agy acp --login` there"
    on one of prigh's logins (refused on its own):
      auth: "logged_in"
      via: "prigh_login"
      prigh_login: "Work Google"
    its token file is there:
      auth: "logged_in"
      via: "login_file"
      login_file: "/home/l/.gemini/antigravity-cli/antigravity-oauth-token"
    ...but a session was refused since:
      auth: "logged_out"
      problem: "Antigravity on desktop is not logged in: run `agy acp --login` there"
    |}]
;;

let%expect_test "every ACP agent: unknown, logged in, logged out" =
  List.iter
    [ "opencode"; "gemini"; "antigravity"; "grok"; "goose" ]
    ~f:(fun name ->
      let s = acp_status name in
      printf "%s:\n" name;
      show_evidence s;
      show_evidence s ~seen:(Opened { at = 0. });
      show_evidence s ~seen:(Refused { at = 0. }));
  [%expect
    {|
    opencode:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts OpenCode on desktop briefly to see"
      auth: "logged_in"
      via: "session"
      confirmed_at: 0
      confirmed: "10m ago"
      auth: "logged_out"
      problem: "OpenCode on desktop is not logged in: run `opencode auth login` there"
    gemini:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts Gemini CLI on desktop briefly to see"
      auth: "logged_in"
      via: "session"
      confirmed_at: 0
      confirmed: "10m ago"
      auth: "logged_out"
      problem: "Gemini CLI on desktop is not logged in: run `gemini` there"
    antigravity:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts Antigravity on desktop briefly to see"
      auth: "logged_in"
      via: "session"
      confirmed_at: 0
      confirmed: "10m ago"
      auth: "logged_out"
      problem: "Antigravity on desktop is not logged in: run `agy acp --login` there"
    grok:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts Grok Build on desktop briefly to see"
      auth: "logged_in"
      via: "session"
      confirmed_at: 0
      confirmed: "10m ago"
      auth: "logged_out"
      problem: "Grok Build on desktop is not logged in: run `grok login` there"
    goose:
      auth: "unknown"
      checkable: true
      hint: "Login not checked yet: Check (/adapters refresh) starts Goose on desktop briefly to see"
      auth: "logged_in"
      via: "session"
      confirmed_at: 0
      confirmed: "10m ago"
      auth: "logged_out"
      problem: "Goose on desktop is not logged in: run `goose configure` there"
    |}]
;;

let%expect_test "what is seen: a failed check does not hide a result" =
  let table = Adapter_health.Seen.Table.create () in
  let record seen =
    Adapter_health.Seen.Table.record table ~host:"h1" (Acp "grok") seen;
    print_s
      [%sexp
        (Adapter_health.Seen.Table.find table ~host:"h1" (Acp "grok")
         : Adapter_health.Seen.t option)]
  in
  record (Check_failed { at = 1.; why = "timed out" });
  record (Opened { at = 2. });
  record (Check_failed { at = 3.; why = "timed out" });
  record (Refused { at = 4. });
  print_s
    [%sexp
      (Adapter_health.Seen.Table.find table ~host:"h2" (Acp "grok")
       : Adapter_health.Seen.t option)];
  [%expect
    {|
    ((Check_failed (at 1) (why "timed out")))
    ((Opened (at 2)))
    ((Opened (at 2)))
    ((Refused (at 4)))
    ()
    |}]
;;

(* Over the RPC, on the backend's own host, with fake CLIs on its PATH. *)
let with_server ?(engines = Engines.default) ?(home = false) ?spawn_external f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let provider = Faux_provider.create [] in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~backend_host:true
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:t.dir
      ~engines
      ?spawn_external:(Option.map spawn_external ~f:(fun f -> f ~sw))
      ~new_agent
      ?home:(Option.some_if home t.dir)
      ()
  in
  let events = Queue.create () in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      match json with
      | `Object fields ->
        (match List.Assoc.find fields "event" ~equal:String.equal with
         | Some (`String "adapters_changed") -> Queue.enqueue events json
         | _ -> ())
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
  in
  f t ~call ~events
;;

let script t name body =
  let path = Filename.concat "bin" name in
  write t path ("#!/bin/sh\n" ^ body);
  Core_unix.chmod (Filename.concat t.dir path) ~perm:0o755
;;

let with_path t f =
  let old_path = Sys.getenv "PATH" in
  let has_cli dir =
    List.exists [ "codex"; "claude"; "agy"; "grok" ] ~f:(fun name ->
      Sys_unix.file_exists_exn (Filename.concat dir name))
  in
  let clean_old =
    Option.value_map old_path ~default:[] ~f:(fun p ->
      String.split p ~on:':'
      |> List.filter ~f:(fun dir ->
        (not (String.is_empty dir)) && not (has_cli dir)))
  in
  Core_unix.putenv
    ~key:"PATH"
    ~data:(String.concat ~sep:":" (Filename.concat t.dir "bin" :: clean_old));
  Exn.protect ~f ~finally:(fun () ->
    Option.iter old_path ~f:(fun data -> Core_unix.putenv ~key:"PATH" ~data))
;;

(* Each status's adapter, installed, version, auth and problem: the host's
   id and name, paths and times vary. *)
let print_adapters t json =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields "error" ~equal:String.equal with
     | Some e -> print_endline (Json.to_string e)
     | None ->
       let adapters =
         match List.Assoc.find fields "result" ~equal:String.equal with
         | Some (`Object r) -> List.Assoc.find r "adapters" ~equal:String.equal
         | _ -> List.Assoc.find fields "adapters" ~equal:String.equal
       in
       (match adapters with
        | Some (`Array items) ->
          List.iter items ~f:(fun item ->
            let get k =
              match item with
              | `Object f ->
                Option.value_map
                  (List.Assoc.find f k ~equal:String.equal)
                  ~default:"-"
                  ~f:Json.to_string
              | _ -> "-"
            in
            printf
              "%s installed=%s version=%s auth=%s account=%s\n  problem=%s\n"
              (get "adapter")
              (get "installed")
              (get "version")
              (get "auth")
              (get "account")
              (mask t (get "problem")))
        | _ -> print_endline (Json.to_string json)))
  | _ -> print_endline (Json.to_string json)
;;

let%expect_test "list_adapters needs the composer" =
  with_server
  @@ fun t ~call ~events:_ ->
  ignore (call "hello" : Json.t);
  print_adapters t (call "list_adapters");
  [%expect
    {| "this backend does not run the composer engine, so it drives no agent CLIs: start one with prigh serve -engines composer" |}]
;;

let%expect_test "list_adapters: probed on the host, cached, refreshed" =
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Codex; Claude ]
         ())
  in
  with_server ~engines
  @@ fun t ~call ~events ->
  script
    t
    "codex"
    {|case "$1" in
  --version) echo "codex-cli 0.166.0";;
  login) echo "Logged in using ChatGPT" >&2;;
esac
|};
  script
    t
    "claude"
    {|case "$1" in
  --version) echo "2.1.330 (Claude Code)";;
  auth) echo '{"loggedIn":false,"authMethod":"none"}'; exit 1;;
esac
|};
  with_path t (fun () ->
    ignore (call "hello" : Json.t);
    Queue.clear events;
    print_adapters t (call "list_adapters");
    [%expect
      {|
      "codex" installed=true version="0.166.0" auth="logged_in" account="ChatGPT"
        problem=-
      "claude" installed=true version="2.1.330" auth="logged_out" account=-
        problem="Claude Code on <host> is not logged in: run `claude auth login` there"
      |}];
    printf "events: %d\n" (Queue.length events);
    Queue.iter events ~f:(print_adapters t);
    Queue.clear events;
    [%expect
      {|
      events: 1
      "codex" installed=true version="0.166.0" auth="logged_in" account="ChatGPT"
        problem=-
      "claude" installed=true version="2.1.330" auth="logged_out" account=-
        problem="Claude Code on <host> is not logged in: run `claude auth login` there"
      |}];
    (* Logged in meanwhile: the cache still says not, until a refresh. *)
    script
      t
      "claude"
      {|case "$1" in
  --version) echo "2.1.330 (Claude Code)";;
  auth) echo '{"loggedIn":true,"authMethod":"claude.ai","email":"l@example.com"}';;
esac
|};
    print_adapters t (call "list_adapters");
    printf "events: %d\n" (Queue.length events);
    [%expect
      {|
      "codex" installed=true version="0.166.0" auth="logged_in" account="ChatGPT"
        problem=-
      "claude" installed=true version="2.1.330" auth="logged_out" account=-
        problem="Claude Code on <host> is not logged in: run `claude auth login` there"
      events: 0
      |}];
    print_adapters t (call ~params:{|{"refresh": true}|} "list_adapters");
    printf "events: %d\n" (Queue.length events);
    [%expect
      {|
      "codex" installed=true version="0.166.0" auth="logged_in" account="ChatGPT"
        problem=-
      "claude" installed=true version="2.1.330" auth="logged_in" account="l@example.com"
        problem=-
      events: 1
      |}])
;;

(* A CLI that fails to start is checked again, and the frontends told. *)
let%expect_test "a CLI that fails to start is re-probed" =
  let engines =
    Or_error.ok_exn
      (Engines.create ~engines:[ Harness; Composer ] ~adapters:[ Codex ] ())
  in
  with_server ~engines
  @@ fun t ~call ~events ->
  script
    t
    "codex"
    {|case "$1" in
  --version) echo "codex-cli 0.166.0";;
  login) echo "Logged in using ChatGPT" >&2;;
esac
|};
  with_path t (fun () ->
    ignore (call "hello" : Json.t);
    print_adapters t (call "list_adapters");
    [%expect
      {|
      "codex" installed=true version="0.166.0" auth="logged_in" account="ChatGPT"
        problem=-
      |}];
    Core_unix.unlink (Filename.concat t.dir "bin/codex");
    ignore (call ~params:{|{"adapter": "codex"}|} "new_session" : Json.t);
    Queue.clear events;
    ignore (call ~params:{|{"text": "hi"}|} "prompt" : Json.t);
    let clock = Eio.Stdenv.clock t.env in
    let deadline = Eio.Time.now clock +. 20. in
    while Queue.is_empty events && Float.(Eio.Time.now clock < deadline) do
      Eio.Time.sleep clock 0.01
    done;
    Queue.iter events ~f:(print_adapters t);
    [%expect
      {|
      "codex" installed=false version=- auth="unknown" account=-
        problem="Codex is not installed on <host>, please install it"
      |}])
;;

let%expect_test "Codex on one of prigh's ChatGPT logins: chosen, listed, shown" =
  let engines =
    Or_error.ok_exn
      (Engines.create ~engines:[ Harness; Composer ] ~adapters:[ Codex ] ())
  in
  with_server ~engines ~home:true
  @@ fun t ~call ~events:_ ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  ignore
    (Or_error.ok_exn
       (Provider_accounts.save_login
          ~label:"Work"
          store
          Openai_codex
          (Oauth
             { access = "a"
             ; refresh = "r"
             ; expires_ms = Int.max_value
             ; account_id = Some "acct"
             }))
     : Account_id.t);
  script t "codex" {|echo "codex-cli 0.166.0"|};
  with_path t (fun () ->
    ignore (call "hello" : Json.t);
    let result json =
      match json with
      | `Object fields ->
        (match List.Assoc.find fields "result" ~equal:String.equal with
         | Some r -> r
         | None -> json)
      | _ -> json
    in
    let logins json =
      match Json.member "adapter_logins" (result json) with
      | Some l -> print_endline (Json.to_string l)
      | None -> print_endline (Json.to_string json)
    in
    let state_login () =
      let state = result (call "get_state") in
      print_s
        [%message
          ""
            ~adapter_login:
              (Option.map (Json.member "adapter_login" state) ~f:Json.to_string
               : string option)]
    in
    logins (call "list_adapters");
    logins
      (call ~params:{|{"adapter":"codex","login":"work"}|} "set_adapter_login");
    print_endline
      (In_channel.read_all (Filename.concat t.dir ".prigh/config.json"));
    state_login ();
    ignore (call ~params:{|{"adapter":"codex"}|} "set_adapter" : Json.t);
    state_login ();
    logins
      (call ~params:{|{"adapter":"codex","login":"home"}|} "set_adapter_login");
    logins
      (call ~params:{|{"adapter":"gemini","login":"work"}|} "set_adapter_login");
    logins
      (call ~params:{|{"adapter":"codex","login":"own"}|} "set_adapter_login");
    logins
      (call
         ~params:{|{"adapter":"codex","host":"nowhere"}|}
         "import_adapter_login");
    state_login ();
    print_endline
      (In_channel.read_all (Filename.concat t.dir ".prigh/config.json")));
  [%expect
    {|
    [{"adapter":"codex","provider":"openai-codex","login":null,"label":null,"choices":[{"ref":"main","label":"Work"}]},{"adapter":"claude","provider":"anthropic","login":null,"label":null,"choices":[]},{"adapter":"acp:antigravity","provider":"google-antigravity","login":null,"label":null,"choices":[]}]
    [{"adapter":"codex","provider":"openai-codex","login":"main","label":"Work","choices":[{"ref":"main","label":"Work"}]},{"adapter":"claude","provider":"anthropic","login":null,"label":null,"choices":[]},{"adapter":"acp:antigravity","provider":"google-antigravity","login":null,"label":null,"choices":[]}]
    {
      "adapters": {
        "codex": {
          "login": "main"
        }
      }
    }
    (adapter_login ())
    (adapter_login ("\"Work\""))
    {"type":"response","id":"r","ok":false,"error":"ChatGPT has no account \"home\": it has Work (main)"}
    {"type":"response","id":"r","ok":false,"error":"gemini runs only on its own login (prigh can hand one of its logins to: codex, claude, acp:antigravity)"}
    [{"adapter":"codex","provider":"openai-codex","login":null,"label":null,"choices":[{"ref":"main","label":"Work"}]},{"adapter":"claude","provider":"anthropic","login":null,"label":null,"choices":[]},{"adapter":"acp:antigravity","provider":"google-antigravity","login":null,"label":null,"choices":[]}]
    {"type":"response","id":"r","ok":false,"error":"no tool host \"nowhere\" is connected (connected: <host>)"}
    (adapter_login ())
    {}
    |}]
;;

(* An ACP agent that is logged in answers [session/new]; one that is not,
   [-32000 Authentication required]. *)
let fake_acp ~logged_in (p : Line_transport.Peer.t) =
  let module C = Test_adapter_catalog in
  let answer j field value =
    C.write
      p
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", Option.value_exn (Json.member "id" j)
          ; field, Json.of_string value
          ])
  in
  let rec loop () =
    match C.read p with
    | None -> ()
    | Some j ->
      (match C.string_at [ "method" ] j with
       | Some "initialize" ->
         answer j "result" {|{"protocolVersion":1,"agentCapabilities":{}}|}
       | Some "session/new" when logged_in ->
         answer j "result" {|{"sessionId":"s1"}|}
       | Some "session/new" ->
         answer
           j
           "error"
           {|{"code":-32000,"message":"Authentication required"}|}
       | _ -> ());
      loop ()
  in
  loop ()
;;

let print_logins json =
  let adapters =
    Option.bind (Json.member "result" json) ~f:(Json.member "adapters")
    |> Option.first_some (Json.member "adapters" json)
  in
  match adapters with
  | Some (`Array items) ->
    List.iter items ~f:(fun item ->
      let get k =
        Option.value_map (Json.member k item) ~default:"-" ~f:Json.to_string
      in
      printf
        "%s auth=%s via=%s checkable=%s\n  %s\n"
        (get "adapter")
        (get "auth")
        (get "via")
        (get "checkable")
        (match Json.member "problem" item, Json.member "hint" item with
         | Some p, _ -> "problem=" ^ Json.to_string p
         | None, Some h -> "hint=" ^ Json.to_string h
         | None, None -> "-"))
  | _ -> print_endline (Json.to_string json)
;;

let%expect_test "ACP agents' logins: checked by starting them, on Refresh" =
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Acp "antigravity"; Acp "grok" ]
         ())
  in
  let spawned = ref [] in
  let spawn_external ~sw _agent ~host:_ (launch : Adapter.Launch.t) =
    let program = List.hd_exn launch.argv in
    spawned := program :: !spawned;
    Ok
      (Line_transport.Peer.spawn
         ~sw
         (fake_acp ~logged_in:(String.equal program "agy")))
  in
  with_server ~engines ~spawn_external
  @@ fun t ~call ~events:_ ->
  script t "agy" {|echo "agy 1.5.13"|};
  script t "grok" {|echo "grok 1.0.41"|};
  with_path t (fun () ->
    ignore (call "hello" : Json.t);
    print_logins (call ~params:{|{"refresh": true}|} "list_adapters");
    [%expect
      {|
      "acp:antigravity" auth="logged_in" via="session" checkable=-
        -
      "acp:grok" auth="logged_out" via=- checkable=-
        problem="Grok Build on <host> is not logged in: run `grok login` there"
      |}];
    (* Known now: neither is started again. *)
    spawned := [];
    print_logins (call "list_adapters");
    print_s [%sexp (!spawned : string list)];
    [%expect
      {|
      "acp:antigravity" auth="logged_in" via="session" checkable=-
        -
      "acp:grok" auth="logged_out" via=- checkable=-
        problem="Grok Build on <host> is not logged in: run `grok login` there"
      ()
      |}])
;;
