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
     (Logged_in (account (ChatGPT))))
      (ready)
    claude: ((Installed (version (2.1.330)) (path /home/u/.local/bin/claude))
     (Logged_in (account (lloyd@example.com))))
      (ready)
    acp:gemini: ((Installed (version (0.9.1)) (path /usr/bin/gemini)) Unknown)
      (ready) prigh can't tell whether Gemini CLI is logged in on laptop: if it asks, run `gemini` there
    prigh: ((Installed (version (<version>)) (path prigh)) Not_needed)
      (ready)
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
      (ready) prigh can't tell whether Grok Build is logged in on laptop: if it asks, run `grok login` there
    acp:opencode: ((Not_installed (login_shell_path ())) Unknown)
      OpenCode is not installed on laptop, please install it
    acp:antigravity: ((Installed (version (1.5.13)) (path /usr/bin/agy)) Unknown)
      (ready) prigh can't tell whether Antigravity is logged in on laptop: if it asks, run `agy acp --login` there
    acp:grok: ((Installed (version (1.0.41)) (path /usr/bin/grok)) Unknown)
      (ready) prigh can't tell whether Grok Build is logged in on laptop: if it asks, run `grok login` there
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
    {"adapter":"codex","label":"Codex","host":"h1","host_name":"laptop","installed":true,"version":"0.166.0","path":"/usr/bin/codex","auth":"logged_in","account":"API key","checked_at":0}
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

(* Over the RPC, on the backend's own host, with fake CLIs on its PATH. *)
let with_server ?(engines = Engines.default) ?(home = false) f =
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
  Core_unix.putenv
    ~key:"PATH"
    ~data:(Filename.concat t.dir "bin" ^ ":" ^ Option.value old_path ~default:"");
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
