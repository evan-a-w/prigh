open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let no_env _ = None
let store t = Auth_store.create ~path:(Filename.concat t.dir "auth.json")

let show_resolved r =
  print_s
    [%sexp
      (Or_error.map
         r
         ~f:
           (Option.map ~f:(fun (r : Provider_auth.Resolved.t) ->
              r.token, r.method_, r.account_id, r.source))
       : (string * Provider_auth.Method.t * string option * string) option
           Or_error.t)]
;;

let%expect_test "methods, env vars and labels per provider" =
  List.iter Provider_id.all ~f:(fun p ->
    printf
      "%-13s methods=%s env=%s\n"
      (Provider_id.to_string p)
      (String.concat
         ~sep:","
         (List.map (Provider_auth.methods p) ~f:(fun m ->
            sprintf
              "%s(%s)"
              (Provider_auth.Method.to_string m)
              (Provider_auth.Method.label p m))))
      (String.concat ~sep:"," (Provider_auth.env_vars p)));
  [%expect
    {|
    anthropic     methods=oauth(Anthropic (Claude Pro/Max)),api_key(Anthropic API key) env=ANTHROPIC_OAUTH_TOKEN,ANTHROPIC_API_KEY
    openai        methods=api_key(OpenAI API key) env=OPENAI_API_KEY
    openai-codex  methods=oauth(OpenAI (ChatGPT Plus/Pro)) env=
    deepseek      methods=api_key(DeepSeek API key) env=DEEPSEEK_API_KEY
    |}]
;;

let%expect_test "resolve: stored credential wins over env; env only as fallback"
  =
  with_sandbox
  @@ fun t ->
  let store = store t in
  let env = function
    | "DEEPSEEK_API_KEY" -> Some "sk-env"
    | "ANTHROPIC_API_KEY" -> Some "sk-ant-env"
    | _ -> None
  in
  let resolve = Provider_auth.resolve ~env:t.env ~getenv:env store in
  show_resolved (resolve Deepseek);
  show_resolved (resolve Anthropic);
  show_resolved (resolve Openai);
  show_resolved (resolve Openai_codex);
  [%expect
    {|
    (Ok ((sk-env Api_key () DEEPSEEK_API_KEY)))
    (Ok ((sk-ant-env Api_key () ANTHROPIC_API_KEY)))
    (Ok ())
    (Ok ())
    |}];
  Or_error.ok_exn (Auth_store.set store Deepseek (Api_key "sk-stored"));
  show_resolved (resolve Deepseek);
  [%expect {| (Ok ((sk-stored Api_key () "stored api key"))) |}];
  print_s
    [%sexp
      (Or_error.map
         (Provider_auth.status ~getenv:env store)
         ~f:
           (List.map ~f:(fun (s : Provider_auth.Status.t) ->
              s.provider, s.configured))
       : (Provider_id.t * (Provider_auth.Method.t * string) option) list
           Or_error.t)];
  [%expect
    {|
    (Ok
     ((Anthropic ((Api_key ANTHROPIC_API_KEY))) (Openai ()) (Openai_codex ())
      (Deepseek ((Api_key "stored api key")))))
    |}]
;;

let%expect_test
    "resolve: oauth refresh only when expiring, persisted, errors surface"
  =
  with_sandbox
  @@ fun t ->
  let store = store t in
  let refreshes = ref 0 in
  let refresh provider ~refresh_token =
    incr refreshes;
    printf
      "refreshing %s with %s\n"
      (Provider_id.to_string provider)
      refresh_token;
    Ok
      { Credential.Oauth.access = "access-new"
      ; refresh = "refresh-new"
      ; expires_ms = Credential.now_ms () + 3_600_000
      ; account_id = Some "acct"
      }
  in
  let resolve =
    Provider_auth.resolve ~env:t.env ~getenv:no_env ~refresh store
  in
  let fresh =
    { Credential.Oauth.access = "access-1"
    ; refresh = "refresh-1"
    ; expires_ms = Credential.now_ms () + 3_600_000
    ; account_id = Some "acct"
    }
  in
  Or_error.ok_exn (Auth_store.set store Openai_codex (Oauth fresh));
  show_resolved (resolve Openai_codex);
  [%expect {| (Ok ((access-1 Oauth (acct) oauth))) |}];
  Or_error.ok_exn
    (Auth_store.set
       store
       Openai_codex
       (Oauth { fresh with expires_ms = Credential.now_ms () + 60_000 }));
  show_resolved (resolve Openai_codex);
  show_resolved (resolve Openai_codex);
  printf "refreshes=%d\n" !refreshes;
  print_s
    [%sexp
      (Or_error.map
         (Auth_store.read store Openai_codex)
         ~f:
           (Option.map ~f:(function
              | Credential.Oauth o -> o.refresh
              | Api_key k -> k))
       : string option Or_error.t)];
  [%expect
    {|
    refreshing openai-codex with refresh-1
    (Ok ((access-new Oauth (acct) oauth)))
    (Ok ((access-new Oauth (acct) oauth)))
    refreshes=1
    (Ok (refresh-new))
    |}];
  (* A failed refresh is an error, not an env fallback. *)
  Or_error.ok_exn
    (Auth_store.set store Anthropic (Oauth { fresh with expires_ms = 0 }));
  show_resolved
    (Provider_auth.resolve
       ~env:t.env
       ~getenv:(fun _ -> Some "sk-env")
       ~refresh:(fun _ ~refresh_token:_ ->
         Or_error.error_string "invalid_grant")
       store
       Anthropic);
  [%expect {| (Error invalid_grant) |}];
  (* Two fibers racing to refresh: only one network call. *)
  Or_error.ok_exn
    (Auth_store.set store Anthropic (Oauth { fresh with expires_ms = 0 }));
  refreshes := 0;
  let slow_refresh provider ~refresh_token =
    Eio.Fiber.yield ();
    refresh provider ~refresh_token
  in
  let resolve =
    Provider_auth.resolve ~env:t.env ~getenv:no_env ~refresh:slow_refresh store
  in
  let results =
    Eio.Fiber.List.map (fun () -> resolve Anthropic) [ (); (); () ]
  in
  List.iter results ~f:show_resolved;
  printf "refreshes=%d\n" !refreshes;
  [%expect
    {|
    refreshing anthropic with refresh-1
    (Ok ((access-new Oauth (acct) oauth)))
    (Ok ((access-new Oauth (acct) oauth)))
    (Ok ((access-new Oauth (acct) oauth)))
    refreshes=1
    |}]
;;

let%expect_test "login with an api key prompt; logout" =
  with_sandbox
  @@ fun t ->
  let store = store t in
  let login provider method_ answers =
    print_s
      [%sexp
        (Provider_auth.login
           ~env:t.env
           store
           provider
           method_
           (Auth_interaction.scripted answers)
         : unit Or_error.t)]
  in
  login Deepseek Api_key [ "  sk-typed  " ];
  print_s
    [%sexp (Auth_store.read store Deepseek : Credential.t option Or_error.t)];
  login Openai Api_key [ "" ];
  login Openai Api_key [];
  login Deepseek Oauth [];
  login Openai_codex Api_key [];
  print_s [%sexp (Provider_auth.logout store Deepseek : unit Or_error.t)];
  print_s
    [%sexp (Auth_store.read store Deepseek : Credential.t option Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Ok ((Api_key sk-typed)))
    (Error "empty API key")
    (Error "login cancelled")
    (Error
     ("login method not supported by provider" (provider Deepseek)
      (method_ Oauth)))
    (Error
     ("login method not supported by provider" (provider Openai_codex)
      (method_ Api_key)))
    (Ok ())
    (Ok ())
    |}]
;;

let%expect_test
    "login manager: prompt/respond round trip, cancel, logout events"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv:no_env ~store:(store t) ()
  in
  Login_manager.subscribe login ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.login_event e)));
  print_s [%sexp (Login_manager.start login Deepseek Api_key : unit Or_error.t)];
  print_s [%sexp (Login_manager.start login Openai Api_key : unit Or_error.t)];
  Eio.Fiber.yield ();
  print_s [%sexp (Login_manager.respond login ~id:"nope" "x" : unit Or_error.t)];
  print_s
    [%sexp (Login_manager.respond login ~id:"p1" "sk-1" : unit Or_error.t)];
  Login_manager.wait login;
  [%expect
    {|
    {"type":"event","event":"auth","kind":"prompt","id":"p1","prompt":"secret","message":"Enter DeepSeek API key"}
    (Ok ())
    (Error "a login is already in progress")
    (Error "no pending login prompt \"nope\"")
    (Ok ())
    {"type":"event","event":"auth","kind":"done","provider":"deepseek","method":"api_key"}
    |}];
  print_s [%sexp (Login_manager.start login Openai Api_key : unit Or_error.t)];
  Eio.Fiber.yield ();
  Login_manager.cancel login;
  Login_manager.wait login;
  print_s
    [%sexp (Login_manager.respond login ~id:"p2" "late" : unit Or_error.t)];
  [%expect
    {|
    {"type":"event","event":"auth","kind":"prompt","id":"p2","prompt":"secret","message":"Enter OpenAI API key"}
    (Ok ())
    {"type":"event","event":"auth","kind":"failed","provider":"openai","error":"login cancelled"}
    (Error "no login in progress")
    |}];
  print_s [%sexp (Login_manager.logout login Deepseek : unit Or_error.t)];
  print_s
    [%sexp
      (Or_error.map
         (Login_manager.status login)
         ~f:
           (List.map ~f:(fun (s : Provider_auth.Status.t) ->
              s.provider, s.configured))
       : (Provider_id.t * (Provider_auth.Method.t * string) option) list
           Or_error.t)];
  [%expect
    {|
    {"type":"event","event":"auth","kind":"logged_out","provider":"deepseek"}
    (Ok ())
    (Ok ((Anthropic ()) (Openai ()) (Openai_codex ()) (Deepseek ())))
    |}]
;;

let%expect_test "provider router: unauthenticated request fails with a hint" =
  with_sandbox
  @@ fun t ->
  let provider =
    Provider_router.create ~env:t.env ~getenv:no_env ~store:(store t) ()
  in
  List.iter Provider_id.all ~f:(fun p ->
    let message =
      provider.stream
        { model = Model.default_for p
        ; system = None
        ; messages = [ Message.user "hi" ]
        ; tools = []
        ; thinking = Off
        ; max_tokens = None
        }
        ~cancel:Cancellation.never
        ~on_event:ignore
    in
    print_s [%sexp (message.stop_reason : Stop_reason.t)]);
  [%expect
    {|
    (Error
     "not logged in to Anthropic: use /login anthropic or set ANTHROPIC_OAUTH_TOKEN/ANTHROPIC_API_KEY")
    (Error "not logged in to OpenAI: use /login openai or set OPENAI_API_KEY")
    (Error "not logged in to OpenAI Codex (ChatGPT): use /login openai-codex")
    (Error
     "not logged in to DeepSeek: use /login deepseek or set DEEPSEEK_API_KEY")
    |}]
;;
