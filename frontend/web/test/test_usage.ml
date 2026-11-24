open! Core
module H = Harness

(* Settings → Usage: the subscriptions' limits, read as the page opens and on
   Refresh. *)

let section h = H.text h ~selector:".settings-panel-usage"

let usage_json =
  (* [H.now] is 1791194400000. *)
  {|{"fetched_at_ms":1791194400000,"providers":[
      {"provider":"anthropic","name":"Claude","status":"limits","plan":null,
       "windows":[
         {"id":"five_hour","label":"5-hour session","used_percent":33,
          "resets_at_ms":1791202440000},
         {"id":"seven_day","label":"Weekly · all models","used_percent":91.5,
          "resets_at_ms":1791468000000},
         {"id":"seven_day_opus","label":"Weekly · Opus","used_percent":1.5,
          "resets_at_ms":null}]},
      {"provider":"openai-codex","name":"ChatGPT","status":"limits",
       "plan":"pro",
       "windows":[
         {"id":"primary","label":"5-hour session","used_percent":72,
          "resets_at_ms":1791196920000},
         {"id":"secondary","label":"Weekly","used_percent":0,
          "resets_at_ms":1791712800000}]}]}|}
;;

let signed_out_json =
  {|{"fetched_at_ms":1791194400000,"providers":[
      {"provider":"anthropic","name":"Claude","status":"api_key"},
      {"provider":"openai-codex","name":"ChatGPT","status":"not_signed_in"}]}|}
;;

let failed_json =
  {|{"fetched_at_ms":1791194400000,"providers":[
      {"provider":"anthropic","name":"Claude","status":"error",
       "error":"Claude refused the login (HTTP 401): sign in again with /login anthropic"},
      {"provider":"openai-codex","name":"ChatGPT","status":"limits",
       "plan":"plus","windows":[]}]}|}
;;

let opened () =
  let h = H.create () in
  H.act h (Clock H.now);
  H.act h (Settings_section Usage);
  h
;;

let%expect_test "Opening Usage asks once; loading, then each provider's windows"
  =
  let h = opened () in
  section h;
  H.reply h "usage_limits" usage_json;
  section h;
  H.elements h ~selector:".usage-track";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Usage
    Each login's usage: how much of its subscription's limits it has used, as the provider reports it (read when you open this page), and what ran on it. And whether a run stopped by a limit carries on when it resets.
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Reading your usage from the providers…
    (Reading…)
    Usage
    Each login's usage: how much of its subscription's limits it has used, as the provider reports it (read when you open this page), and what ran on it. And whether a run stopped by a limit carries on when it resets.
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Read 10:00 (just now). Updated when you open this page or press Refresh.
    (Refresh)
    Claude
    5-hour session
    Resets in 2h 14m · 12:14
    33% used
    Weekly · all models
    Resets in 3d 4h · 8 Oct 14:00
    92% used
    Weekly · Opus
    Reset time not reported
    1.5% used
    ChatGPT Pro
    5-hour session
    Resets in 42m · 10:42
    72% used
    Weekly
    Resets in 6d · 11 Oct 10:00
    0% used
    <div class="[&>[data-slot=progress-indicator]]:bg-muted-foreground [&>[data-slot=progress-indicator]]:motion-reduce:transition-none bg-muted flex flex-1 h-1.5 items-center overflow-x-hidden relative rounded-full usage-track w-full">
    <div class="[&>[data-slot=progress-indicator]]:bg-destructive [&>[data-slot=progress-indicator]]:motion-reduce:transition-none bg-muted flex flex-1 h-1.5 items-center overflow-x-hidden relative rounded-full usage-track w-full">
    <div class="[&>[data-slot=progress-indicator]]:bg-muted-foreground [&>[data-slot=progress-indicator]]:motion-reduce:transition-none bg-muted flex flex-1 h-1.5 items-center overflow-x-hidden relative rounded-full usage-track w-full">
    <div class="[&>[data-slot=progress-indicator]]:bg-primary [&>[data-slot=progress-indicator]]:motion-reduce:transition-none bg-muted flex flex-1 h-1.5 items-center overflow-x-hidden relative rounded-full usage-track w-full">
    <div class="[&>[data-slot=progress-indicator]]:bg-muted-foreground [&>[data-slot=progress-indicator]]:motion-reduce:transition-none bg-muted flex flex-1 h-1.5 items-center overflow-x-hidden relative rounded-full usage-track w-full">
    |}]
;;

let%expect_test "Refresh asks again and keeps the last answer meanwhile" =
  let h = opened () in
  H.quiet h (fun () -> H.reply h "usage_limits" usage_json);
  H.act h (Clock (Time_ns.add H.now (Time_ns.Span.of_min 5.)));
  H.text h ~selector:".usage-refresh";
  H.click h ~selector:".usage-refresh button";
  H.text h ~selector:".usage-refresh";
  H.text h ~selector:".usage-window";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Subscription limits
    Read 10:00 (5m ago). Updated when you open this page or press Refresh.
    (Refresh)
    Usage_refresh
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Subscription limits
    Read 10:00 (5m ago). Updated when you open this page or press Refresh.
    (Reading…)
    5-hour session
    Resets in 2h 9m · 12:14
    33% used
    Weekly · all models
    Resets in 3d 3h · 8 Oct 14:00
    92% used
    Weekly · Opus
    Reset time not reported
    1.5% used
    5-hour session
    Resets in 37m · 10:42
    72% used
    Weekly
    Resets in 5d 23h · 11 Oct 10:00
    0% used
    |}]
;;

let%expect_test "Not signed in and API keys say how to sign in" =
  let h = opened () in
  H.quiet h (fun () -> H.reply h "usage_limits" signed_out_json);
  section h;
  H.click h ~nth:1 ~selector:".usage-signin button";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Usage
    Each login's usage: how much of its subscription's limits it has used, as the provider reports it (read when you open this page), and what ran on it. And whether a run stopped by a limit carries on when it resets.
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Read 10:00 (just now). Updated when you open this page or press Refresh.
    (Refresh)
    Claude
    Not signed in
    Signed in with an API key, billed per token: no subscription limits. To see them, sign in with the subscription: /login anthropic.
    (Sign in)
    ChatGPT
    Not signed in
    Sign in with your ChatGPT subscription to see its limits: /login openai-codex.
    (Sign in)
    (Run "/login openai-codex")
    (Focus dialog)
    (Rpc (method_ login) (params ((provider openai-codex))) (tag Login_started))
    |}]
;;

let%expect_test "A provider's error, a plan without windows, a failed call" =
  let h = opened () in
  H.quiet h (fun () -> H.reply h "usage_limits" failed_json);
  section h;
  let h = opened () in
  H.quiet h (fun () -> H.fail h "usage_limits" "unknown method usage_limits");
  section h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Usage
    Each login's usage: how much of its subscription's limits it has used, as the provider reports it (read when you open this page), and what ran on it. And whether a run stopped by a limit carries on when it resets.
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Read 10:00 (just now). Updated when you open this page or press Refresh.
    (Refresh)
    Claude
    Could not read the limits
    Claude refused the login (HTTP 401): sign in again with /login anthropic
    ChatGPT Plus
    No limits reported
    The provider reported no usage windows for this plan.
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Usage
    Each login's usage: how much of its subscription's limits it has used, as the provider reports it (read when you open this page), and what ran on it. And whether a run stopped by a limit carries on when it resets.
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Could not read your usage. Unknown method usage_limits: try Refresh.
    (Refresh)
    |}]
;;

let%expect_test "/usage opens the section; switching users forgets it" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "alice"
         ; user = Some "alice"
         ; superuser = true
         ; features = []
         }));
  H.act h (Run "/usage");
  H.reply h "usage_limits" usage_json;
  H.text h ~selector:".settings-panel-head h3";
  H.act h (Run "/usage now");
  H.text h ~selector:".toast";
  let known () =
    print_s [%sexp (Option.is_some (H.model h).usage.usage : bool)]
  in
  known ();
  H.quiet h (fun () ->
    H.act h (Run "/setusr bob");
    H.reply h "set_user" {|{"client_id":"c1","namespace":"bob","user":"alice"}|});
  known ();
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings false))))
    Usage
    /usage takes no argument: it opens Settings, Usage, with Refresh. (Dismiss)
    true
    false
    |}]
;;

let%expect_test "until" =
  List.iter
    [ 0.; 0.5; 1.; 59.; 60.; 134.; 1440.; 1440. +. 240.; 2880. ]
    ~f:(fun min ->
      print_endline (Prigh_ui.Login_usage_text.until (Time_ns.Span.of_min min)));
  [%expect
    {|
    under a minute
    1m
    1m
    59m
    1h
    2h 14m
    1d
    1d 4h
    2d
    |}]
;;

(* A card per login with usage to show, by provider, as Settings → Models
   lists them: a subscription's limits, an API key's requests and who used
   it, a login shared with the user; a provider with none says how to sign
   in. *)
let logins_json =
  let totals r =
    sprintf
      {|{"requests":%d,"input":%d,"output":%d,"cache_read":0,"cost_usd":%d}|}
      r
      (r * 4000)
      (r * 300)
      r
  in
  let usage a b =
    sprintf
      {|"usage":{"today":%s,"last_7_days":%s,"last_30_days":%s}|}
      (totals a)
      (totals b)
      (totals b)
  in
  sprintf
    {|[{"provider":"anthropic","name":"Claude","methods":[{"method":"oauth","label":"Claude Pro/Max"}],"configured":{"method":"oauth","source":"oauth"},"accounts":[
         {"ref":"anthropic/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true,"shareable":true},
         {"ref":"anthropic/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":false,"shareable":true}]},
       {"provider":"openai-codex","name":"ChatGPT","methods":[{"method":"oauth","label":"ChatGPT Plus/Pro"}],"configured":null},
       {"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":{"method":"api_key","source":"auth.json"},"accounts":[
         {"ref":"deepseek/default","id":"default","label":"DeepSeek API key","method":"api_key","source":"auth.json","default":true,"fallback":true,"shareable":true,"shared_with":[{"user":"bob","since_ms":1700000000000}],%s},
         {"ref":"~carol/deepseek/team","id":"~carol/team","label":"Team","method":"api_key","source":"shared by carol","default":false,"fallback":false,"shared_by":"carol",%s},
         {"ref":"~dave/deepseek/old","id":"~dave/old","label":"Old","method":"api_key","source":"shared by dave","default":false,"fallback":false,"shared_by":"dave","hidden":true,%s}]}]|}
    (usage 2 5)
    (usage 0 1)
    (usage 1 1)
;;

let limits_json =
  {|{"fetched_at_ms":1791194400000,"providers":[
      {"provider":"anthropic","name":"Claude","status":"limits","plan":"max","windows":[],"accounts":[
        {"ref":"anthropic/main","id":"main","label":"Personal","default":true,"status":"limits","plan":"max","windows":[{"id":"five_hour","label":"5-hour session","used_percent":42,"resets_at_ms":1791199800000}]},
        {"ref":"anthropic/work","id":"work","label":"Work","default":false,"status":"limits","plan":"pro","windows":[]}]},
      {"provider":"openai-codex","name":"ChatGPT","status":"not_signed_in","accounts":[]}]}|}
;;

let%expect_test "a card per login, by provider; shared ones from their owner" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Clock H.now);
    H.act h (Reply (Auth_status Refresh, Ok (Jsonaf.of_string logins_json))));
  H.act h (Settings_section Usage);
  H.reply h "auth_status" logins_json;
  H.reply h "usage_limits" limits_json;
  H.reply
    h
    "account_usage"
    {|[{"user":"alice","you":true,"today":{"requests":1,"input":4000,"output":300,"cache_read":0,"cost_usd":1},"last_7_days":{"requests":3,"input":12000,"output":900,"cache_read":0,"cost_usd":3},"last_30_days":{"requests":3,"input":12000,"output":900,"cache_read":0,"cost_usd":3}},
       {"user":"bob","today":{"requests":1,"input":4000,"output":300,"cache_read":0,"cost_usd":1},"last_7_days":{"requests":2,"input":8000,"output":600,"cache_read":0,"cost_usd":2},"last_30_days":{"requests":2,"input":8000,"output":600,"cache_read":0,"cost_usd":2}}]|};
  H.text h ~selector:".settings-panel-usage .settings-group:not(:first-child)";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    (Rpc (method_ account_usage) (params ((ref deepseek/default)))
     (tag (Login_users deepseek/default)))
    (Rpc (method_ account_share_targets) (params ()) (tag Share_targets))
    At a limit
    Resume after a usage limit
    A run stopped by a provider's usage limit carries on by itself when the limit resets, if the provider says when. Off: it stays stopped until you send a message.
    (Stay stopped at a usage limit (/resume off))
    Subscription limits
    Read 10:00 (just now). Updated when you open this page or press Refresh.
    (Refresh)
    Claude
    Personal
    Subscription, default, takes over at usage limits. Max plan.
    (Share…) (Manage)
    5-hour session Resets in 1h 30m · 11:30
    42% used
    ChatGPT
    Not signed in
    Sign in with your ChatGPT subscription to see its limits: /login openai-codex.
    (Sign in)
    DeepSeek
    API key
    API key, default, takes over at usage limits, shared with bob.
    (Share…) (Manage)
    Today 2 req 8.6k tok $2.00
    7 days 5 req 22k tok $5.00
    30 days 5 req 22k tok $5.00
    You today 1 req 4.3k tok $1.00 · 30 days 3 req 13k tok $3.00
    bob today 1 req 4.3k tok $1.00 · 30 days 2 req 8.6k tok $2.00
    Team (from carol)
    API key, shared by carol.
    (Hide) (Manage)
    Today 0 req 0 tok $0.00
    7 days 1 req 4.3k tok $1.00
    30 days 1 req 4.3k tok $1.00
    |}]
;;
