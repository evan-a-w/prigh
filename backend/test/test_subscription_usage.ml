open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Settings → Usage: the subscription limits of the Claude and ChatGPT
   logins, from recorded answers of their usage endpoints. *)

let utc seconds =
  Time_float.of_span_since_epoch (Time_float.Span.of_sec seconds)
  |> Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc
;;

let print_status (status : Subscription_usage.Status.t) =
  match status with
  | Not_signed_in -> print_endline "not signed in"
  | Api_key -> print_endline "API key"
  | Failed e -> printf "error: %s\n" e
  | Limits { plan; windows } ->
    printf "plan: %s\n" (Option.value plan ~default:"-");
    List.iter windows ~f:(fun w ->
      printf
        "  %-28s %-34s %5.1f%%  resets %s\n"
        w.label
        ("[" ^ w.id ^ "]")
        w.used_percent
        (Option.value_map w.resets_at ~default:"-" ~f:utc))
;;

(* Claude Max, before [limits]: windows by name, some null or without a
   utilization (not on this plan), and one we do not know. *)
let anthropic_named =
  {|{"five_hour": {"utilization": 33.0, "resets_at": "2026-11-20T14:00:00.444694+00:00"},
     "seven_day": {"utilization": 13.0, "resets_at": "2026-11-24T09:00:00+00:00"},
     "seven_day_oauth_apps": null,
     "seven_day_opus": {"utilization": 1.0, "resets_at": null},
     "seven_day_sonnet": {"utilization": null, "resets_at": null},
     "cinder_cove": {"utilization": 5, "resets_at": 1795700000},
     "extra_usage": {"is_enabled": false, "monthly_limit": null, "used_credits": null, "utilization": null}}|}
;;

(* Lately: no 5-hour window, and the server's own rows in [limits]. *)
let anthropic_limits =
  {|{"five_hour": null,
     "seven_day": {"utilization": 29.0, "resets_at": "2026-11-25T01:00:00.123+00:00"},
     "limits": [
       {"kind": "weekly_all", "group": "weekly", "percent": 29,
        "resets_at": "2026-11-25T01:00:00.123+00:00", "scope": null,
        "is_active": true, "severity": "normal"},
       {"kind": "weekly_scoped", "group": "weekly", "percent": 2,
        "resets_at": "2026-11-25T01:00:00+00:00",
        "scope": {"model": {"display_name": "Fable"}}, "is_active": false},
       {"kind": "daily_surface", "group": "daily", "percent": 60.5,
        "resets_at": null, "scope": {"surface": {"display_name": "Claude Code"}}},
       {"kind": "broken"},
       "junk"]}|}
;;

(* ChatGPT Pro, as the Codex CLI reads it: the weekly window's [reset_at]
   is missing, so [reset_after_seconds] counts from now. *)
let codex =
  {|{"plan_type": "pro",
     "rate_limit": {"allowed": true, "limit_reached": false,
       "primary_window": {"used_percent": 42, "limit_window_seconds": 18000,
                          "reset_after_seconds": 3600, "reset_at": 1795683600},
       "secondary_window": {"used_percent": 84, "limit_window_seconds": 604800,
                            "reset_after_seconds": 200000, "reset_at": 0}},
     "credits": {"has_credits": true, "unlimited": false, "balance": "9.99"},
     "spend_control": null,
     "additional_rate_limits": [
       {"limit_name": "GPT-5.3-Codex-Spark", "metered_feature": "codex_bengalfox",
        "normal_model_slug": null,
        "rate_limit": {"allowed": true, "limit_reached": false,
          "primary_window": {"used_percent": 70, "limit_window_seconds": 18000,
                             "reset_after_seconds": 900, "reset_at": 1795680000},
          "secondary_window": null}},
       {"limit_name": "codex_other", "metered_feature": "codex_other",
        "rate_limit": {"primary_window": {"used_percent": 3, "limit_window_seconds": 900,
                                          "reset_after_seconds": 60, "reset_at": 1795680060},
                       "secondary_window": {"used_percent": 1.5,
                                            "reset_after_seconds": 60}}}],
     "account_id": "acc", "user_id": "user"}|}
;;

let now = 1795670000.

let%expect_test "Claude's usage: named windows" =
  print_status (Subscription_usage.parse_anthropic anthropic_named);
  [%expect
    {|
    plan: -
      5-hour session               [five_hour]                         33.0%  resets 2026-11-20T14:00:00.444694Z
      Weekly · all models         [seven_day]                         13.0%  resets 2026-11-24T09:00:00.000000Z
      Weekly · Opus               [seven_day_opus]                     1.0%  resets -
      Cinder cove                  [cinder_cove]                        5.0%  resets 2026-11-26T13:33:20.000000Z
    |}]
;;

let%expect_test "Claude's usage: the server's rows" =
  print_status (Subscription_usage.parse_anthropic anthropic_limits);
  [%expect
    {|
    plan: -
      Weekly · all models         [weekly_all]                        29.0%  resets 2026-11-25T01:00:00.123000Z
      Weekly · Fable              [weekly_scoped:Fable]                2.0%  resets 2026-11-25T01:00:00.000000Z
      Daily surface · Claude Code [daily_surface:Claude Code]         60.5%  resets -
    |}]
;;

let%expect_test "ChatGPT's usage" =
  print_status (Subscription_usage.parse_codex ~now codex);
  [%expect
    {|
    plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
    |}]
;;

let%expect_test "odd answers" =
  let both body =
    print_status (Subscription_usage.parse_anthropic body);
    print_status (Subscription_usage.parse_codex ~now body)
  in
  both "{}";
  both {|{"rate_limit": null, "five_hour": {"resets_at": "soon"}}|};
  both "[1]";
  both "<html>";
  [%expect
    {|
    plan: -
    plan: -
    plan: -
    plan: -
    error: Claude's usage endpoint answered something unexpected (not a JSON object): try Refresh later
    error: ChatGPT's usage endpoint answered something unexpected (not a JSON object): try Refresh later
    error: Claude's usage endpoint answered something unexpected (json: unexpected character: '<' (does your string contain HTML instead of JSON?)): try Refresh later
    error: ChatGPT's usage endpoint answered something unexpected (json: unexpected character: '<' (does your string contain HTML instead of JSON?)): try Refresh later
    |}];
  both "<html>";
  [%expect
    {|
    error: Claude's usage endpoint answered something unexpected (json: unexpected character: '<' (does your string contain HTML instead of JSON?)): try Refresh later
    error: ChatGPT's usage endpoint answered something unexpected (json: unexpected character: '<' (does your string contain HTML instead of JSON?)): try Refresh later
    |}]
;;

(* Requests are answered by URL; they run at once, so they are printed in
   order of URL afterwards. *)
let fake_http answers =
  let requests = ref [] in
  let http : Subscription_usage.Http.t =
    fun ~url ~headers () ->
    requests := (url, headers) :: !requests;
    match List.Assoc.find answers ~equal:String.equal url with
    | Some answer -> answer
    | None -> Error "no answer"
  in
  let print () =
    List.iter
      (List.sort !requests ~compare:[%compare: string * (string * string) list])
      ~f:(fun (url, headers) ->
        printf "GET %s\n" url;
        List.iter headers ~f:(fun (k, v) -> printf "  %s: %s\n" k v));
    requests := []
  in
  http, print
;;

let anthropic_url = "https://api.anthropic.com/api/oauth/usage"
let codex_url = "https://chatgpt.com/backend-api/wham/usage"

let oauth ?(expires_ms = Int.max_value / 2) ?account_id access : Credential.t =
  Oauth { access; refresh = "refresh-" ^ access; expires_ms; account_id }
;;

let fetch
      ?(env_vars = [])
      ?(answers = [])
      ?(labelled = [])
      ~(store_with : (Provider_id.t * Credential.t) list)
      ()
  =
  with_sandbox
  @@ fun t ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  List.iter store_with ~f:(fun (provider, credential) ->
    ok_exn (Auth_store.set store provider credential));
  List.iter labelled ~f:(fun (provider, label, credential) ->
    ignore
      (ok_exn (Provider_accounts.save_login ~label store provider credential)
       : Account_id.t));
  let http, print_requests = fake_http answers in
  let usage =
    Subscription_usage.fetch
      ~env:t.env
      ~getenv:(fun name -> List.Assoc.find env_vars ~equal:String.equal name)
      ~refresh:(fun provider ~refresh_token ->
        printf
          "(refreshing %s with %s)\n"
          (Provider_id.to_string provider)
          refresh_token;
        Ok
          { Credential.Oauth.access = "fresh"
          ; refresh = "refresh-fresh"
          ; expires_ms = Int.max_value / 2
          ; account_id = Some "acc-fresh"
          })
      ~http
      ~now:(fun () -> now)
      store
  in
  print_requests ();
  printf "fetched at %s\n" (utc usage.fetched_at);
  List.iter usage.providers ~f:(fun p ->
    printf "%s: " (Subscription_usage.name p.provider);
    print_status p.status;
    if List.length p.accounts > 1
    then
      List.iter p.accounts ~f:(fun a ->
        printf
          "  account %s%s (%s): "
          (Account_id.to_string a.id)
          (if a.default then "*" else "")
          a.label;
        print_status a.status))
;;

let%expect_test "nobody signed in, or with API keys" =
  fetch ~store_with:[] ();
  [%expect
    {|
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: not signed in
    ChatGPT: not signed in
    |}];
  fetch ~store_with:[ Anthropic, Api_key "sk-ant-api" ] ();
  [%expect
    {|
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: API key
    ChatGPT: not signed in
    |}];
  (* A Claude subscription token from the environment is a login. *)
  fetch
    ~env_vars:[ "ANTHROPIC_OAUTH_TOKEN", "sk-ant-oat01-env" ]
    ~answers:
      [ anthropic_url, Ok (200, [], {|{"five_hour": {"utilization": 7}}|}) ]
    ~store_with:[]
    ();
  [%expect
    {|
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer sk-ant-oat01-env
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: plan: -
      5-hour session               [five_hour]                          7.0%  resets -
    ChatGPT: not signed in
    |}]
;;

let%expect_test "both signed in" =
  fetch
    ~answers:
      [ anthropic_url, Ok (200, [], anthropic_named)
      ; codex_url, Ok (200, [], codex)
      ]
    ~store_with:
      [ Anthropic, oauth "claude-token"
      ; Openai_codex, oauth "codex-token" ~account_id:"acc-1"
      ]
    ();
  [%expect
    {|
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer claude-token
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-token
      chatgpt-account-id: acc-1
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: plan: -
      5-hour session               [five_hour]                         33.0%  resets 2026-11-20T14:00:00.444694Z
      Weekly · all models         [seven_day]                         13.0%  resets 2026-11-24T09:00:00.000000Z
      Weekly · Opus               [seven_day_opus]                     1.0%  resets -
      Cinder cove                  [cinder_cove]                        5.0%  resets 2026-11-26T13:33:20.000000Z
    ChatGPT: plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
    |}]
;;

let%expect_test "expired tokens are refreshed first" =
  fetch
    ~answers:
      [ anthropic_url, Ok (200, [], "{}"); codex_url, Ok (200, [], "{}") ]
    ~store_with:
      [ Anthropic, oauth "old" ~expires_ms:0
      ; Openai_codex, oauth "old-codex" ~expires_ms:0 ~account_id:"acc-old"
      ]
    ();
  [%expect
    {|
    (refreshing anthropic with refresh-old)
    (refreshing openai-codex with refresh-old-codex)
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer fresh
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer fresh
      chatgpt-account-id: acc-fresh
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: plan: -
    ChatGPT: plan: -
    |}]
;;

let%expect_test "failures say what to do" =
  let store_with : (Provider_id.t * Credential.t) list =
    [ Anthropic, oauth "claude-token"
    ; Openai_codex, oauth "codex-token" ~account_id:"a"
    ]
  in
  let run anthropic codex =
    fetch ~answers:[ anthropic_url, anthropic; codex_url, codex ] ~store_with ()
  in
  run
    (Ok
       ( 403
       , []
       , {|{"type": "error", "error": {"type": "permission_error", "message": "OAuth token does not meet scope requirement user:profile"}}|}
       ))
    (Ok (401, [], {|{"detail": "Could not parse your authentication token."}|}));
  [%expect
    {|
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer claude-token
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-token
      chatgpt-account-id: a
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: error: Claude did not accept the sign-in (HTTP 403: OAuth token does not meet scope requirement user:profile): sign in again with /login anthropic
    ChatGPT: error: ChatGPT did not accept the sign-in (HTTP 401: Could not parse your authentication token.): sign in again with /login openai-codex
    |}];
  run
    (Ok
       ( 429
       , [ "Retry-After", "95" ]
       , {|{"type": "error", "error": {"type": "rate_limit_error", "message": "Rate limited"}}|}
       ))
    (Ok (502, [], "<html>bad gateway</html>"));
  [%expect
    {|
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer claude-token
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-token
      chatgpt-account-id: a
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: error: Claude is limiting how often usage is read (HTTP 429): Refresh in 95 s
    ChatGPT: error: ChatGPT's usage endpoint answered HTTP 502: Refresh later
    |}];
  run (Ok (429, [], "")) (Error "connection refused");
  [%expect
    {|
    GET https://api.anthropic.com/api/oauth/usage
      Authorization: Bearer claude-token
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-token
      chatgpt-account-id: a
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: error: Claude is limiting how often usage is read (HTTP 429): Refresh in a minute
    ChatGPT: error: Could not reach ChatGPT's usage endpoint (connection refused): check the backend's network and Refresh
    |}]
;;

let%expect_test "a ChatGPT login without an account id, or whose refresh fails" =
  fetch ~store_with:[ Openai_codex, oauth "codex-token" ] ();
  [%expect
    {|
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: not signed in
    ChatGPT: error: The ChatGPT sign-in has no account id: sign in again with /login openai-codex
    |}];
  with_sandbox
  @@ fun t ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  ok_exn (Auth_store.set store Anthropic (oauth "old" ~expires_ms:0));
  let http, _ = fake_http [] in
  let usage =
    Subscription_usage.fetch
      ~env:t.env
      ~getenv:(fun _ -> None)
      ~refresh:(fun _ ~refresh_token:_ -> Or_error.error_string "invalid_grant")
      ~http
      ~now:(fun () -> now)
      store
  in
  List.iter usage.providers ~f:(fun p -> print_status p.status);
  [%expect
    {|
    error: invalid_grant: sign in again with /login anthropic
    not signed in
    |}]
;;

let%expect_test "rpc: usage_limits" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  ok_exn (Auth_store.set store Anthropic (oauth "claude-token"));
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv:(fun _ -> None) ~store ()
  in
  let provider = Faux_provider.create [] in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let usage_http, _ =
    fake_http [ anthropic_url, Ok (200, [], anthropic_named) ]
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~usage_http
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let response =
    Rpc_server.handle
      server
      client
      (Json.of_string {|{"id": "r", "method": "usage_limits", "params": {}}|})
  in
  let result = Option.value_exn (Json.member "result" response) in
  (match result with
   | `Object fields ->
     printf
       "fetched_at_ms is a number: %b\n"
       (match List.Assoc.find fields ~equal:String.equal "fetched_at_ms" with
        | Some (`Number _) -> true
        | _ -> false);
     List.iter
       (match List.Assoc.find fields ~equal:String.equal "providers" with
        | Some (`Array ps) -> ps
        | _ -> [])
       ~f:(fun p -> print_endline (Json.to_string p))
   | _ -> print_endline (Json.to_string response));
  [%expect
    {|
    fetched_at_ms is a number: true
    {"provider":"anthropic","name":"Claude","status":"limits","plan":null,"windows":[{"id":"five_hour","label":"5-hour session","used_percent":33,"resets_at_ms":1795183200445},{"id":"seven_day","label":"Weekly · all models","used_percent":13,"resets_at_ms":1795510800000},{"id":"seven_day_opus","label":"Weekly · Opus","used_percent":1,"resets_at_ms":null},{"id":"cinder_cove","label":"Cinder cove","used_percent":5,"resets_at_ms":1795700000000}],"accounts":[{"ref":"anthropic/main","id":"main","label":"Anthropic (Claude Pro/Max)","default":true,"status":"limits","plan":null,"windows":[{"id":"five_hour","label":"5-hour session","used_percent":33,"resets_at_ms":1795183200445},{"id":"seven_day","label":"Weekly · all models","used_percent":13,"resets_at_ms":1795510800000},{"id":"seven_day_opus","label":"Weekly · Opus","used_percent":1,"resets_at_ms":null},{"id":"cinder_cove","label":"Cinder cove","used_percent":5,"resets_at_ms":1795700000000}]}]}
    {"provider":"openai-codex","name":"ChatGPT","status":"not_signed_in","accounts":[]}
    |}]
;;

let%expect_test "PRIGH_USAGE_ORIGIN stands in for both origins" =
  fetch
    ~env_vars:[ "PRIGH_USAGE_ORIGIN", "http://127.0.0.1:9" ]
    ~answers:
      [ "http://127.0.0.1:9/api/oauth/usage", Ok (200, [], "{}")
      ; "http://127.0.0.1:9/backend-api/wham/usage", Ok (200, [], "{}")
      ]
    ~store_with:
      [ Anthropic, oauth "claude-token"
      ; Openai_codex, oauth "codex-token" ~account_id:"a"
      ]
    ();
  [%expect
    {|
    GET http://127.0.0.1:9/api/oauth/usage
      Authorization: Bearer claude-token
      anthropic-beta: oauth-2025-04-20
      Content-Type: application/json
      User-Agent: claude-code/2.1.322
    GET http://127.0.0.1:9/backend-api/wham/usage
      Authorization: Bearer codex-token
      chatgpt-account-id: a
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: plan: -
    ChatGPT: plan: -
    |}]
;;

let%expect_test "each account's usage; the provider's is the default's" =
  fetch
    ~answers:[ codex_url, Ok (200, [], codex) ]
    ~store_with:[ Openai_codex, oauth "codex-main" ~account_id:"acc-1" ]
    ~labelled:
      [ Openai_codex, "Work", oauth "codex-work" ~account_id:"acc-2"
      ; Openai_codex, "No id", oauth "codex-noid"
      ]
    ();
  [%expect
    {|
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-main
      chatgpt-account-id: acc-1
      originator: prigh
      User-Agent: prigh/0.1.0
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer codex-work
      chatgpt-account-id: acc-2
      originator: prigh
      User-Agent: prigh/0.1.0
    fetched at 2026-11-26T05:13:20.000000Z
    Claude: not signed in
    ChatGPT: plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
      account main* (ChatGPT Plus/Pro): plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
      account work (Work): plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
      account no-id (No id): error: The ChatGPT sign-in has no account id: sign in again with /login openai-codex
    |}]
;;

let%expect_test "logins shared with the user: asked with the owner's token" =
  with_sandbox
  @@ fun t ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let http, print_requests = fake_http [ codex_url, Ok (200, [], codex) ] in
  let shared ~owner resolve =
    { Subscription_usage.Shared.owner
    ; provider = Openai_codex
    ; id = Or_error.ok_exn (Account_id.of_string "work")
    ; label = "Work"
    ; resolve
    }
  in
  let usage =
    Subscription_usage.fetch
      ~env:t.env
      ~getenv:(fun _ -> None)
      ~shared:
        [ shared ~owner:"alice" (fun () ->
            Ok
              (Some
                 { Provider_auth.Resolved.token = "alice-token"
                 ; method_ = Oauth
                 ; account_id = Some "acct-alice"
                 ; source = "oauth"
                 ; account = None
                 ; expires_ms = None
                 }))
        ; shared ~owner:"carol" (fun () ->
            Or_error.error_string
              "carol does not share the login with you (any more)")
        ]
      ~http
      ~now:(fun () -> now)
      store
  in
  print_requests ();
  List.iter usage.providers ~f:(fun p ->
    printf "%s: " (Subscription_usage.name p.provider);
    print_status p.status;
    List.iter p.accounts ~f:(fun a ->
      printf
        "  %s's %s%s: "
        (Option.value a.shared_by ~default:"own")
        a.label
        (if a.default then "*" else "");
      print_status a.status));
  (match Subscription_usage.to_json usage with
   | `Object fields ->
     (match List.Assoc.find fields ~equal:String.equal "providers" with
      | Some (`Array [ _; `Object codex ]) ->
        (match List.Assoc.find codex ~equal:String.equal "accounts" with
         | Some (`Array (`Object first :: _)) ->
           List.iter [ "ref"; "id"; "shared_by" ] ~f:(fun k ->
             printf
               "%s: %s\n"
               k
               (Json.to_string
                  (List.Assoc.find_exn first ~equal:String.equal k)))
         | _ -> print_endline "?")
      | _ -> print_endline "?")
   | _ -> print_endline "?");
  [%expect
    {|
    GET https://chatgpt.com/backend-api/wham/usage
      Authorization: Bearer alice-token
      chatgpt-account-id: acct-alice
      originator: prigh
      User-Agent: prigh/0.1.0
    Claude: not signed in
    ChatGPT: plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
      alice's Work*: plan: pro
      5-hour session               [primary]                           42.0%  resets 2026-11-26T09:00:00.000000Z
      Weekly                       [secondary]                         84.0%  resets 2026-11-28T12:46:40.000000Z
      GPT-5.3-Codex-Spark · 5-hour session [GPT-5.3-Codex-Spark.primary]       70.0%  resets 2026-11-26T08:00:00.000000Z
      Codex other · 15-minute window [codex_other.primary]                3.0%  resets 2026-11-26T08:01:00.000000Z
      Codex other · Long window   [codex_other.secondary]              1.5%  resets 2026-11-26T05:14:20.000000Z
      carol's Work: error: carol does not share the login with you (any more)
    ref: "~alice/openai-codex/work"
    id: "~alice/work"
    shared_by: "alice"
    |}]
;;
