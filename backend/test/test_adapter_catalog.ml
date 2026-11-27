open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A Codex or Claude Code session's models and usage limits come from the
   harness's own login (its [model/list] and [account/rateLimits/read], its
   [initialize] and [get_usage]), asked by running it briefly before the
   session's first turn: never from prigh's provider logins, which these
   tests have none of. The harnesses are scripted peers. *)

let member path j =
  List.fold path ~init:(Some j) ~f:(fun j key ->
    Option.bind j ~f:(Json.member key))
;;

let string_at path j = Option.bind (member path j) ~f:Json.string

let read (p : Line_transport.Peer.t) =
  Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
    Json.parse line |> Or_error.ok_exn)
;;

let write (p : Line_transport.Peer.t) j =
  Line_transport.Peer.write_line p (Json.to_string j)
;;

(* Codex logged in to ChatGPT: two models, its rate limits. *)
let fake_codex ~logged_in (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      let meth = Option.value (string_at [ "method" ] j) ~default:"-" in
      if Option.is_some (Json.member "id" j) then printf "codex got %s\n" meth;
      (match meth with
       | "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | "model/list" ->
         let effort e = `Object [ "reasoningEffort", `String e ] in
         reply
           j
           (`Object
               [ ( "data"
                 , `Array
                     [ `Object
                         [ "model", `String "gpt-6.2-sol"
                         ; "displayName", `String "GPT-6.2-Sol"
                         ; "isDefault", `True
                         ; ( "supportedReasoningEfforts"
                           , `Array
                               [ effort "low"; effort "medium"; effort "high" ]
                           )
                         ]
                     ; `Object
                         [ "model", `String "gpt-6.2-mini"
                         ; "displayName", `String "GPT-6.2-Mini"
                         ; ( "supportedReasoningEfforts"
                           , `Array [ effort "low"; effort "xhigh" ] )
                         ]
                     ] )
               ; "nextCursor", `Null
               ])
       | "account/login/start" ->
         printf
           "codex was handed token %s\n"
           (Option.value (string_at [ "params"; "accessToken" ] j) ~default:"?");
         reply j (`Object [])
       | "account/rateLimits/read" when not logged_in ->
         write
           p
           (`Object
               [ "id", Option.value_exn (Json.member "id" j)
               ; ( "error"
                 , `Object
                     [ "code", `Number "-32600"
                     ; ( "message"
                       , `String
                           "codex account authentication required to read rate \
                            limits" )
                     ] )
               ])
       | "account/rateLimits/read" ->
         reply
           j
           (`Object
               [ ( "rateLimits"
                 , `Object
                     [ ( "primary"
                       , `Object
                           [ "usedPercent", `Number "42"
                           ; "windowDurationMins", `Number "300"
                           ; "resetsAt", `Number "1900000000"
                           ] )
                     ; ( "secondary"
                       , `Object
                           [ "usedPercent", `Number "7"
                           ; "windowDurationMins", `Number "10080"
                           ; "resetsAt", `Number "1900500000"
                           ] )
                     ; "planType", `String "pro"
                     ] )
               ])
       | _ -> ());
      loop ()
  in
  loop ()
;;

(* Claude Code logged in to Claude.ai: its models at [initialize], its plan
   limits from [get_usage]. *)
let fake_claude (p : Line_transport.Peer.t) =
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "type" ] j, string_at [ "request"; "subtype" ] j with
       | Some "control_request", Some subtype ->
         printf "claude got %s\n" subtype;
         let response =
           match subtype with
           | "initialize" ->
             `Object
               [ ( "models"
                 , `Array
                     (* As Claude Code 2.1 says them. *)
                     (let efforts =
                        ( "supportedEffortLevels"
                        , `Array
                            (List.map
                               [ "low"; "medium"; "high"; "xhigh"; "max" ]
                               ~f:(fun s -> `String s)) )
                      in
                      [ `Object
                          [ "value", `String "default"
                          ; "resolvedModel", `String "claude-opus-5-5[1m]"
                          ; "displayName", `String "Default (recommended)"
                          ; ( "description"
                            , `String
                                "Use the default model (currently Opus 5.5 (1M \
                                 context)) · $5/$25 per Mtok" )
                          ; efforts
                          ]
                      ; `Object
                          [ "value", `String "opus[1m]"
                          ; "resolvedModel", `String "claude-opus-5-5[1m]"
                          ; "displayName", `String "Opus (1M context)"
                          ; ( "description"
                            , `String
                                "Opus 5.5 with 1M context · Best for everyday, \
                                 complex tasks" )
                          ; efforts
                          ]
                      ; `Object
                          [ "value", `String "sonnet"
                          ; "resolvedModel", `String "claude-sonnet-5-5"
                          ; "displayName", `String "Sonnet"
                          ; ( "description"
                            , `String "Sonnet 5.5 · Efficient for routine tasks"
                            )
                          ; efforts
                          ]
                      ; `Object
                          [ "value", `String "haiku"
                          ; "resolvedModel", `String "claude-haiku-4-5"
                          ; "displayName", `String "Haiku"
                          ; ( "description"
                            , `String "Haiku 4.5 · Fastest for quick answers" )
                          ]
                      ]) )
               ]
           | "get_usage" ->
             `Object
               [ "subscription_type", `String "max"
               ; "rate_limits_available", `True
               ; ( "rate_limits"
                 , `Object
                     [ ( "five_hour"
                       , `Object
                           [ "utilization", `Number "12"
                           ; "resets_at", `String "2030-03-17T13:46:40Z"
                           ] )
                     ] )
               ]
           | _ -> `Object []
         in
         write
           p
           (`Object
               [ "type", `String "control_response"
               ; ( "response"
                 , `Object
                     [ "subtype", `String "success"
                     ; ( "request_id"
                       , `String
                           (Option.value
                              (string_at [ "request_id" ] j)
                              ~default:"") )
                     ; "response", response
                     ] )
               ])
       | _ -> ());
      loop ()
  in
  loop ()
;;

(* How many sessions the fake Antigravities opened, for a test to wait on. *)
let agy_sessions = ref 0
let agy_session_opened = Eio.Condition.create ()
let agy_model_sets = ref 0
let agy_model_set = Eio.Condition.create ()

let rec await_agy_sessions n =
  if !agy_sessions < n
  then (
    Eio.Condition.await_no_mutex agy_session_opened;
    await_agy_sessions n)
;;

let rec await_agy_model_sets n =
  if !agy_model_sets < n
  then (
    Eio.Condition.await_no_mutex agy_model_set;
    await_agy_model_sets n)
;;

(* Antigravity: its models through the session model API. *)
let fake_agy ~logged_in (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", Option.value_exn (Json.member "id" j)
          ; "result", Json.parse result |> Or_error.ok_exn
          ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      let meth = Option.value (string_at [ "method" ] j) ~default:"-" in
      printf "agy got %s\n" meth;
      (match meth with
       | "initialize" ->
         reply j {|{"protocolVersion":1,"agentCapabilities":{}}|}
       | "session/new" when not logged_in ->
         write
           p
           (`Object
               [ "jsonrpc", `String "2.0"
               ; "id", Option.value_exn (Json.member "id" j)
               ; ( "error"
                 , `Object
                     [ "code", `Number "-32000"
                     ; "message", `String "Authentication required"
                     ] )
               ])
       | "session/new" ->
         reply
           j
           {|{"sessionId":"s1","models":{"currentModelId":"flash","availableModels":[
               {"modelId":"gpt-oss","name":"GPT-OSS 120B (Medium)"},
               {"modelId":"flash","name":"Gemini 3.8 Flash"}]}}|};
         incr agy_sessions;
         Eio.Condition.broadcast agy_session_opened
       | "session/set_model" ->
         reply j "{}";
         incr agy_model_sets;
         Eio.Condition.broadcast agy_model_set
       | _ -> ());
      loop ()
  in
  loop ()
;;

let composer =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude; Acp "antigravity"; Acp "grok" ]
       ())
;;

let with_server
      ?(logged_in = true)
      ?(agy_logged_out = Fn.const false)
      ?(home = false)
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let provider = Faux_provider.create [] in
  let latest = ref None in
  let new_agent ?session ~cwd () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:Tools.interaction
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:true
        ~cwd
        ()
    in
    latest := Some agent;
    agent
  in
  let spawn_external _agent ~host (launch : Adapter.Launch.t) =
    (* Antigravity on a login of prigh's runs in a wrapper. *)
    let program =
      List.find_exn launch.argv ~f:(fun a ->
        List.mem [ "codex"; "claude"; "agy"; "grok" ] a ~equal:String.equal)
    in
    printf
      "spawn %s on %s%s\n"
      program
      host
      (match launch.secret with
       | Some s -> sprintf " with prigh's token in $%s" s.fd_env
       | None -> "");
    match program with
    | "grok" -> Or_error.error_string "grok: command not found"
    | _ ->
      Ok
        (Line_transport.Peer.spawn
           ~sw
           (match program with
            | "codex" -> fake_codex ~logged_in
            | "agy" -> fake_agy ~logged_in:(not (agy_logged_out host))
            | _ -> fake_claude))
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
      ~engines:composer
      ~spawn_external
      ~new_agent
      ?home:(Option.some_if home t.dir)
      ()
  in
  let note_changed = ref None in
  let last_harness_models = ref None in
  let next_change () =
    let changed, note = Eio.Promise.create () in
    note_changed := Some note;
    changed
  in
  let changed = next_change () in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      match string_at [ "event" ] json with
      | Some "notice" ->
        printf
          "notice: %s\n"
          (String.substr_replace_all
             (Option.value_exn (string_at [ "text" ] json))
             ~pattern:(Machine_name.get ())
             ~with_:"<machine>"
           |> String.substr_replace_all ~pattern:t.dir ~with_:"$DIR")
      | Some "state" ->
        (* Where the harness is with listing its models, when it says. *)
        Option.iter
          (member [ "state"; "harness_models" ] json)
          ~f:(fun status ->
            let line =
              sprintf
                "event: state, harness_models %s"
                (match string_at [ "state" ] status with
                 | Some "failed" ->
                   let text key =
                     String.substr_replace_all
                       (Option.value_exn (string_at [ key ] status))
                       ~pattern:(Machine_name.get ())
                       ~with_:"<machine>"
                   in
                   sprintf
                     "failed %s %S fixes=[%s]\n  %s"
                     (Option.value_exn (string_at [ "problem" ] status))
                     (text "summary")
                     (match member [ "fixes" ] status with
                      | Some (`Array fixes) ->
                        String.concat
                          ~sep:" "
                          (List.map fixes ~f:(fun f ->
                             Option.value_exn (string_at [ "kind" ] f)
                             ^ Option.value_map
                                 (string_at [ "provider" ] f)
                                 ~default:""
                                 ~f:(fun p -> ":" ^ p)))
                      | _ -> "?")
                     (text "message")
                 | state -> Option.value state ~default:"?")
            in
            if not ([%equal: string option] !last_harness_models (Some line))
            then (
              last_harness_models := Some line;
              print_endline line))
      | Some "models_changed" ->
        print_endline "event: models_changed";
        Option.iter !note_changed ~f:(fun u ->
          note_changed := None;
          Eio.Promise.resolve u ())
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    match
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    with
    | `Object fields ->
      (match List.Assoc.find fields "result" ~equal:String.equal with
       | Some result -> result
       | None ->
         raise_s [%message "failed" meth (Json.to_string (`Object fields))])
    | _ -> assert false
  in
  let models () =
    match call "list_models" with
    | `Array models ->
      List.iter models ~f:(fun m ->
        printf
          "  %s %S thinking=[%s]\n"
          (Option.value_exn (string_at [ "key" ] m))
          (Option.value_exn (string_at [ "name" ] m))
          (match member [ "thinking_options" ] m with
           | Some (`Array l) ->
             String.concat
               ~sep:" "
               (List.filter_map l ~f:(fun o ->
                  match o with
                  | `String s -> Some s
                  | o -> Option.bind (Json.member "level" o) ~f:Json.string))
           | _ -> "?"))
    | _ -> print_endline "models: ?"
  in
  let chip () =
    let (state : Agent.State.t) = Agent.state (Option.value_exn !latest) in
    printf "chip: %s (%s)\n" (Model.key state.model) state.model.name
  in
  let usage () =
    match member [ "providers" ] (call "usage_limits") with
    | Some (`Array providers) ->
      List.iter providers ~f:(fun p ->
        printf
          "usage %s %S %s%s\n"
          (Option.value_exn (string_at [ "provider" ] p))
          (Option.value_exn (string_at [ "name" ] p))
          (Option.value_exn (string_at [ "status" ] p))
          (match member [ "windows" ] p with
           | Some (`Array windows) ->
             String.concat
               (List.map windows ~f:(fun w ->
                  sprintf
                    " %s=%s%%"
                    (Option.value_exn (string_at [ "id" ] w))
                    (match member [ "used_percent" ] w with
                     | Some (`Number n) -> n
                     | _ -> "?")))
           | _ ->
             Option.value_map
               (string_at [ "error" ] p)
               ~default:""
               ~f:(sprintf " %S")))
    | _ -> print_endline "usage: ?"
  in
  let connect_host ~id ~name =
    let host = Rpc_server.connect server ~send:ignore in
    ignore
      (Rpc_server.handle
         server
         host
         (Json.of_string
            (sprintf
               {|{"id": 1, "method": "hello", "params": {"name": "%s", "tools": true, "host_id": "%s", "cwd": "%s"}}|}
               name
               id
               t.dir))
       : Json.t)
  in
  let save_login provider =
    ignore
      (Or_error.ok_exn
         (Provider_accounts.save_login
            ~label:"Work"
            (Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
            provider
            (Oauth
               { access = "tok-" ^ Provider_id.to_string provider
               ; refresh = "r"
               ; expires_ms = Int.max_value
               ; account_id = Some "acct"
               }))
       : Account_id.t)
  in
  f ~call ~models ~chip ~usage ~changed ~next_change ~connect_host ~save_login;
  Rpc_server.shutdown server
;;

let%expect_test "codex: the picker lists Codex's models before the first turn" =
  with_server
  @@ fun ~call
       ~models
       ~chip
       ~usage
       ~changed
       ~next_change:_
       ~connect_host:_
       ~save_login:_ ->
  ignore (call ~params:{|{"adapter": "codex"}|} "new_session" : Json.t);
  chip ();
  print_endline "list_models, before Codex said:";
  models ();
  Eio.Promise.await changed;
  print_endline "list_models, after:";
  models ();
  chip ();
  [%expect
    {|
    event: state, harness_models loading
    chip: codex/default (loading models…)
    list_models, before Codex said:
    spawn codex on backend
      codex/default "loading models\226\128\166" thinking=[default low medium high]
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
    list_models, after:
      codex/gpt-6.2-sol "GPT-6.2-Sol" thinking=[default low medium high]
      codex/gpt-6.2-mini "GPT-6.2-Mini" thinking=[default low xhigh]
    chip: codex/gpt-6.2-sol (GPT-6.2-Sol)
    |}];
  ignore (call ~params:{|{"model": "gpt-6.2-mini"}|} "set_model" : Json.t);
  chip ();
  (* Kept: no second run of Codex. *)
  usage ();
  [%expect
    {|
    chip: codex/gpt-6.2-mini (GPT-6.2-Mini)
    usage codex "Codex" limits primary=42% secondary=7%
    |}]
;;

let%expect_test "claude: its models and plan limits from its own login" =
  with_server
  @@ fun ~call
       ~models
       ~chip
       ~usage
       ~changed
       ~next_change:_
       ~connect_host:_
       ~save_login:_ ->
  ignore (call ~params:{|{"adapter": "claude"}|} "new_session" : Json.t);
  usage ();
  Eio.Promise.await changed;
  models ();
  chip ();
  [%expect
    {|
    event: state, harness_models loading
    spawn claude on backend
    claude got initialize
    claude got get_usage
    event: models_changed
    usage claude "Claude Code" limits five_hour=12%
      claude/opus[1m] "Opus 5.5 (1M context)" thinking=[off default low medium high xhigh max]
      claude/sonnet "Sonnet 5.5" thinking=[off default low medium high xhigh max]
      claude/haiku "Haiku 4.5" thinking=[off default low medium high max]
    chip: claude/opus[1m] (Opus 5.5 (1M context))
    |}]
;;

let%expect_test
    "codex logged out: its models still, and what it said of its limits"
  =
  with_server ~logged_in:false
  @@ fun ~call
       ~models:_
       ~chip:_
       ~usage
       ~changed:_
       ~next_change:_
       ~connect_host:_
       ~save_login:_ ->
  ignore (call ~params:{|{"adapter": "codex"}|} "new_session" : Json.t);
  usage ();
  [%expect
    {|
    event: state, harness_models loading
    spawn codex on backend
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
    usage codex "Codex" error "Codex: codex account authentication required to read rate limits (codex login, on the session's tool host)"
    |}]
;;

let%expect_test
    "an ACP agent's models before its first turn; switching to Codex lists \
     Codex's"
  =
  with_server
  @@ fun ~call
       ~models
       ~chip
       ~usage:_
       ~changed
       ~next_change
       ~connect_host:_
       ~save_login:_ ->
  ignore
    (call ~params:{|{"adapter": "acp:antigravity"}|} "new_session" : Json.t);
  print_endline "list_models, before Antigravity said:";
  models ();
  Eio.Promise.await changed;
  print_endline "list_models, after:";
  models ();
  chip ();
  [%expect
    {|
    event: state, harness_models loading
    spawn agy on backend
    list_models, before Antigravity said:
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
    list_models, after:
      acp:antigravity/flash "Gemini 3.8 Flash" thinking=[off]
      acp:antigravity/gpt-oss "GPT-OSS 120B (Medium)" thinking=[off]
    chip: acp:antigravity/flash (Gemini 3.8 Flash)
    |}];
  let changed = next_change () in
  ignore (call ~params:{|{"adapter": "codex"}|} "set_adapter" : Json.t);
  print_endline "list_models, on Codex:";
  models ();
  Eio.Promise.await changed;
  let changed = next_change () in
  Eio.Promise.await changed;
  models ();
  chip ();
  [%expect
    {|
    event: models_changed
    list_models, on Codex:
    spawn codex on backend
      codex/default "loading models\226\128\166" thinking=[default low medium high]
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
      codex/gpt-6.2-sol "GPT-6.2-Sol" thinking=[default low medium high]
      codex/gpt-6.2-mini "GPT-6.2-Mini" thinking=[default low xhigh]
    chip: codex/gpt-6.2-sol (GPT-6.2-Sol)
    |}]
;;

let%expect_test
    "switching engine: the session's model is one the new engine has, the one \
     it last ran with there"
  =
  with_server
  @@ fun ~call
       ~models:_
       ~chip
       ~usage:_
       ~changed
       ~next_change
       ~connect_host:_
       ~save_login:_ ->
  ignore
    (call ~params:{|{"adapter": "acp:antigravity"}|} "new_session" : Json.t);
  (* Asking its models has prigh ask Antigravity. *)
  ignore (call "list_models" : Json.t);
  Eio.Promise.await changed;
  ignore
    (call ~params:{|{"model": "acp:antigravity/gpt-oss"}|} "set_model" : Json.t);
  chip ();
  let switch adapter =
    let changed = next_change () in
    ignore
      (call ~params:(sprintf {|{"adapter": "%s"}|} adapter) "set_adapter"
       : Json.t);
    Eio.Promise.await changed;
    printf "%s: " adapter;
    chip ()
  in
  [%expect
    {|
    event: state, harness_models loading
    spawn agy on backend
    agy got initialize
    agy got session/new
    event: models_changed
    chip: acp:antigravity/gpt-oss (GPT-OSS 120B (Medium))
    |}];
  switch "harness";
  [%expect
    {|
    event: models_changed
    harness: chip: deepseek/deepseek-flash (DeepSeek V4.1 Flash)
    |}];
  let opened = !agy_sessions in
  let model_sets = !agy_model_sets in
  switch "acp:antigravity";
  (* Switched to, it is started at once (Runner.prewarm) and applies the
     remembered model during startup. *)
  await_agy_sessions (opened + 1);
  await_agy_model_sets (model_sets + 1);
  [%expect
    {|
    spawn agy on backend
    event: models_changed
    acp:antigravity: chip: acp:antigravity/gpt-oss (GPT-OSS 120B (Medium))
    agy got initialize
    agy got session/new
    agy got session/set_model
    |}]
;;

let%expect_test
    "a harness that cannot list its models: the chip says why, retry asks again"
  =
  with_server
  @@ fun ~call
       ~models
       ~chip:_
       ~usage:_
       ~changed:_
       ~next_change:_
       ~connect_host:_
       ~save_login:_ ->
  ignore (call ~params:{|{"adapter": "acp:grok"}|} "new_session" : Json.t);
  models ();
  (* The fetch failed at once: asked again within the minute, it is not
     run again. *)
  models ();
  models ();
  (* Retry asks again at once. *)
  ignore (call "retry_models" : Json.t);
  [%expect
    {|
    event: state, harness_models loading
    spawn grok on backend
    event: models_changed
    event: state, harness_models failed not_installed "not installed on <machine>" fixes=[engines retry]
      Grok Build on <machine> could not list its models: grok: command not found. Install it there (prigh never installs it), then retry; Settings → Engines says what is missing.
      acp:grok/default "loading models\226\128\166" thinking=[off]
      acp:grok/default "loading models\226\128\166" thinking=[off]
      acp:grok/default "loading models\226\128\166" thinking=[off]
    spawn grok on backend
    event: models_changed
    |}]
;;

let%expect_test
    "a new chat moving to the host the user picked asks its models there"
  =
  with_server ~agy_logged_out:(String.equal "backend")
  @@ fun ~call
       ~models
       ~chip:_
       ~usage:_
       ~changed
       ~next_change
       ~connect_host
       ~save_login:_ ->
  (* A page whose machine's tool host has not connected yet: the chat
     starts on the backend, where agy is not logged in; it lists its models
     on the desktop. *)
  ignore
    (call
       ~params:{|{"name": "prigh-web", "prefer_host": "host-desktop"}|}
       "hello"
     : Json.t);
  ignore
    (call ~params:{|{"adapter": "acp:antigravity"}|} "new_session" : Json.t);
  models ();
  Eio.Promise.await changed;
  models ();
  [%expect
    {|
    event: state, harness_models loading
    spawn agy on backend
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
    event: state, harness_models failed not_logged_in "not logged in on <machine>" fixes=[engines retry]
      Antigravity on <machine> could not list its models: Antigravity is not logged in on this host: run `agy acp --login` there. Check that it is installed and logged in there (Settings → Engines).
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    |}];
  let changed = next_change () in
  connect_host ~id:"host-desktop" ~name:"desktop";
  Eio.Promise.await changed;
  let changed = next_change () in
  models ();
  Eio.Promise.await changed;
  models ();
  [%expect
    {|
    notice: tools now run on desktop in $DIR
    event: state, harness_models loading
    event: models_changed
    spawn agy on host-desktop
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
      acp:antigravity/flash "Gemini 3.8 Flash" thinking=[off]
      acp:antigravity/gpt-oss "GPT-OSS 120B (Medium)" thinking=[off]
    |}]
;;

let%expect_test
    "on prigh's login: the catalog is asked on it, and again on another"
  =
  with_server ~home:true
  @@ fun ~call
       ~models
       ~chip:_
       ~usage:_
       ~changed
       ~next_change
       ~connect_host:_
       ~save_login ->
  save_login Google_antigravity;
  save_login Openai_codex;
  save_login Anthropic;
  List.iter [ "acp:antigravity"; "codex"; "claude" ] ~f:(fun adapter ->
    ignore
      (call
         ~params:(sprintf {|{"adapter":"%s","login":"work"}|} adapter)
         "set_adapter_login"
       : Json.t));
  ignore
    (call ~params:{|{"adapter": "acp:antigravity"}|} "new_session" : Json.t);
  models ();
  Eio.Promise.await changed;
  models ();
  [%expect
    {|
    event: state, harness_models loading
    spawn agy on backend with prigh's token in $PRIGH_AGY_TOKEN_FD
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
      acp:antigravity/flash "Gemini 3.8 Flash" thinking=[off]
      acp:antigravity/gpt-oss "GPT-OSS 120B (Medium)" thinking=[off]
    |}];
  (* Back on agy's own login: asked again, on it. *)
  let changed = next_change () in
  ignore
    (call
       ~params:{|{"adapter":"acp:antigravity","login":"own"}|}
       "set_adapter_login"
     : Json.t);
  Eio.Promise.await changed;
  let changed = next_change () in
  models ();
  Eio.Promise.await changed;
  [%expect
    {|
    event: models_changed
    spawn agy on backend
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
    |}];
  (* Not [usage]: it would also ask prigh's logins' usage, over the
     network. *)
  let switch adapter =
    let changed = next_change () in
    ignore
      (call ~params:(sprintf {|{"adapter": "%s"}|} adapter) "set_adapter"
       : Json.t);
    Eio.Promise.await changed;
    let changed = next_change () in
    models ();
    Eio.Promise.await changed
  in
  switch "codex";
  switch "claude";
  [%expect
    {|
    event: models_changed
    event: models_changed
    spawn codex on backend
      codex/default "loading models\226\128\166" thinking=[default low medium high]
    codex got initialize
    codex got account/login/start
    codex was handed token tok-openai-codex
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
    event: models_changed
    event: models_changed
    spawn claude on backend with prigh's token in $CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR
      claude/default "loading models\226\128\166" thinking=[off default low medium high max]
    claude got initialize
    claude got get_usage
    event: models_changed
    |}]
;;

let%expect_test "prigh's login refused: what to do is about that login" =
  with_server ~home:true ~agy_logged_out:(Fn.const true)
  @@ fun ~call
       ~models
       ~chip:_
       ~usage:_
       ~changed
       ~next_change:_
       ~connect_host:_
       ~save_login ->
  save_login Google_antigravity;
  ignore
    (call
       ~params:{|{"adapter":"acp:antigravity","login":"work"}|}
       "set_adapter_login"
     : Json.t);
  ignore
    (call ~params:{|{"adapter": "acp:antigravity"}|} "new_session" : Json.t);
  models ();
  Eio.Promise.await changed;
  models ();
  [%expect
    {|
    event: state, harness_models loading
    spawn agy on backend with prigh's token in $PRIGH_AGY_TOKEN_FD
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    agy got initialize
    agy got session/new
    event: models_changed
    event: state, harness_models failed login_refused "login refused" fixes=[login:google-antigravity engines retry]
      Antigravity on <machine> could not list its models: it was refused prigh's google-antigravity login "Work", or could not use it: /login google-antigravity logs in again, or Settings → Engines runs Antigravity on its own login.
      acp:antigravity/default "loading models\226\128\166" thinking=[off]
    |}]
;;

let%expect_test
    "catalog fetch skips MCP sockets; new_session prewarms and cwd change \
     re-prewarms"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.interaction
      ~sessions_dir
      ~home:t.dir
      ?session
      ~backend_host:true
      ~cwd
      ()
  in
  let spawn_external _agent ~host:_ (launch : Adapter.Launch.t) =
    let program =
      List.find_exn launch.argv ~f:(fun a ->
        List.mem [ "codex"; "agy" ] a ~equal:String.equal)
    in
    printf "spawn %s mcp=%b cwd=%s\n" program launch.mcp (mask t launch.cwd);
    Ok
      (Line_transport.Peer.spawn
         ~sw
         (match program with
          | "codex" -> fake_codex ~logged_in:true
          | _ -> fake_agy ~logged_in:true))
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
      ~engines:composer
      ~spawn_external
      ~new_agent
      ()
  in
  let note_changed = ref None in
  let next_change () =
    let p, r = Eio.Promise.create () in
    note_changed := Some r;
    p
  in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      match string_at [ "event" ] json with
      | Some "models_changed" ->
        Option.iter !note_changed ~f:(fun r ->
          note_changed := None;
          Eio.Promise.resolve r ())
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    match
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
    with
    | `Object fields
      when Option.is_some (List.Assoc.find fields "result" ~equal:String.equal)
      -> ()
    | json -> raise_s [%message "rpc failed" meth (Json.to_string json)]
  in
  (* Codex catalog fetch uses mcp=false. *)
  call "hello";
  call ~params:{|{"adapter":"codex"}|} "new_session";
  let changed = next_change () in
  call "list_models";
  Eio.Promise.await changed;
  (* ACP new_session prewarms immediately (mcp=true), and changing cwd
     re-prewarms in the new directory. *)
  let opened = !agy_sessions in
  call ~params:{|{"adapter":"acp:antigravity"}|} "new_session";
  await_agy_sessions (opened + 1);
  call ~params:(sprintf {|{"path":%S}|} sub) "set_cwd";
  await_agy_sessions (opened + 2);
  Rpc_server.shutdown server;
  [%expect
    {|
    spawn codex mcp=false cwd=$DIR
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    spawn agy mcp=true cwd=$DIR
    agy got initialize
    agy got session/new
    spawn agy mcp=true cwd=$DIR/sub
    agy got initialize
    agy got session/new
    |}]
;;
