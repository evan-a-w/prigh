open! Core
open! Prigh
open Eio.Std
module Server = Fake_http_server
module Json = Jsonaf

let run f = Eio_main.run @@ fun env -> Switch.run @@ fun sw -> f ~env ~sw
let port_re = Re.compile (Re.Perl.re {|(localhost|127\.0\.0\.1):\d+|})
let verifier_re = Re.compile (Re.Perl.re {|code_verifier=[A-Za-z0-9_-]+|})

let mask s =
  Re.replace_string port_re ~by:"<host>:<port>" s
  |> Re.replace_string verifier_re ~by:"code_verifier=<verifier>"
;;

let print_masked s = print_endline (mask s)
let print_s_masked sexp = print_masked (Sexp.to_string_hum sexp)

let%expect_test "pkce: S256 challenge matches the RFC 7636 appendix vector" =
  print_endline
    (Pkce.challenge_of_verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk");
  [%expect {| E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM |}];
  let pair = Pkce.generate () in
  printf
    "verifier=%d chars, challenge=%d chars, consistent=%b, urlsafe=%b\n"
    (String.length pair.verifier)
    (String.length pair.challenge)
    (String.equal pair.challenge (Pkce.challenge_of_verifier pair.verifier))
    (String.for_all (pair.verifier ^ pair.challenge) ~f:(fun c ->
       Char.is_alphanum c || Char.equal c '-' || Char.equal c '_'));
  printf
    "hex=%s\n"
    (String.map (Pkce.random_hex 16) ~f:(fun c ->
       if Char.is_hex_digit c then 'x' else '?'));
  [%expect
    {|
    verifier=43 chars, challenge=43 chars, consistent=true, urlsafe=true
    hex=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    |}]
;;

let%expect_test "authorization input: url, code#state, query, bare code" =
  List.iter
    [ "https://localhost:1455/auth/callback?code=abc&state=xyz"
    ; "  http://localhost:53692/callback?state=s1&code=c1  "
    ; "code123#state456"
    ; "code=c2&state=s2"
    ; "plaincode"
    ; ""
    ]
    ~f:(fun input ->
      print_s
        [%sexp
          (Oauth_common.Authorization_input.parse input
           : Oauth_common.Authorization_input.t)]);
  [%expect
    {|
    ((code (abc)) (state (xyz)))
    ((code (c1)) (state (s1)))
    ((code (code123)) (state (state456)))
    ((code (c2)) (state (s2)))
    ((code (plaincode)) (state ()))
    ((code ()) (state ()))
    |}]
;;

let%expect_test "callback server: request classification" =
  let classify =
    Oauth_callback_server.For_testing.classify
      ~path:"/callback"
      ~expected_state:"good"
  in
  List.iter
    [ "/callback?code=c&state=good"
    ; "/callback?code=c&state=bad"
    ; "/callback?state=good"
    ; "/callback?error=access_denied"
    ; "/elsewhere?code=c&state=good"
    ]
    ~f:(fun target ->
      match classify target with
      | Success r ->
        print_s [%sexp "success", (r : Oauth_callback_server.Result_.t)]
      | Failure { status; message } ->
        print_s [%sexp "failure", (status : int), (message : string)]);
  [%expect
    {|
    (success ((code c) (state good)))
    (failure 400 "State mismatch.")
    (failure 400 "Missing code or state parameter.")
    (failure 400 "Authentication did not complete. Error: access_denied")
    (failure 404 "Callback route not found.")
    |}]
;;

let fixed_pkce =
  { Pkce.Pair.verifier = "VERIFIER-abc_123"; challenge = "CHALLENGE-xyz" }
;;

let%expect_test "anthropic: authorize url and token parsing" =
  let url =
    Oauth_anthropic.For_testing.authorize_url
      Oauth_anthropic.Config.default
      fixed_pkce
  in
  let uri = Uri.of_string url in
  printf
    "%s://%s%s\n"
    (Option.value_exn (Uri.scheme uri))
    (Option.value_exn (Uri.host uri))
    (Uri.path uri);
  List.iter (Uri.query uri) ~f:(fun (k, v) ->
    printf "  %s = %s\n" k (String.concat ~sep:"," v));
  [%expect
    {|
    https://claude.ai/oauth/authorize
      code = true
      client_id = 9d1c250a-e61b-44d9-88ed-5944d1962f5e
      response_type = code
      redirect_uri = http://localhost:53692/callback
      scope = org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload
      code_challenge = CHALLENGE-xyz
      code_challenge_method = S256
      state = VERIFIER-abc_123
    |}];
  print_s
    [%sexp
      (Oauth_anthropic.For_testing.credential_of_body
         ~now_ms:1_000_000
         {|{"access_token":"sk-ant-oat01-x","refresh_token":"sk-ant-ort01-y","expires_in":3600}|}
       : Credential.Oauth.t Or_error.t)];
  print_s
    [%sexp
      (Oauth_anthropic.For_testing.credential_of_body
         ~now_ms:0
         {|{"access_token":"a"}|}
       : Credential.Oauth.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((access sk-ant-oat01-x) (refresh sk-ant-ort01-y) (expires_ms 4300000)
      (account_id ())))
    (Error "token response: missing \"refresh_token\"")
    |}]
;;

let jwt_with claims =
  let b64 s = Pkce.base64url s in
  b64 {|{"alg":"none"}|} ^ "." ^ b64 claims ^ "." ^ b64 "sig"
;;

let%expect_test "openai codex: authorize url, jwt account id, token parsing" =
  let url =
    Oauth_openai_codex.For_testing.authorize_url
      Oauth_openai_codex.Config.default
      fixed_pkce
      ~state:"STATE1"
  in
  let uri = Uri.of_string url in
  printf
    "%s://%s%s\n"
    (Option.value_exn (Uri.scheme uri))
    (Option.value_exn (Uri.host uri))
    (Uri.path uri);
  List.iter (Uri.query uri) ~f:(fun (k, v) ->
    printf "  %s = %s\n" k (String.concat ~sep:"," v));
  [%expect
    {|
    https://auth.openai.com/oauth/authorize
      response_type = code
      client_id = app_EMoamEEZ73f0CkXaXp7hrann
      redirect_uri = http://localhost:1455/auth/callback
      scope = openid profile email offline_access
      code_challenge = CHALLENGE-xyz
      code_challenge_method = S256
      state = STATE1
      id_token_add_organizations = true
      codex_cli_simplified_flow = true
      originator = prigh
    |}];
  let token =
    jwt_with
      {|{"https://api.openai.com/auth":{"chatgpt_account_id":"acct-42"}}|}
  in
  print_s
    [%sexp
      (Oauth_openai_codex.For_testing.account_id_of_token token : string option)];
  print_s
    [%sexp
      (Oauth_openai_codex.For_testing.account_id_of_token
         (jwt_with {|{"sub":"x"}|})
       : string option)];
  print_s
    [%sexp
      (Oauth_openai_codex.For_testing.account_id_of_token "garbage"
       : string option)];
  [%expect
    {|
    (acct-42)
    ()
    ()
    |}];
  let body =
    sprintf {|{"access_token":"%s","refresh_token":"r","expires_in":600}|} token
  in
  print_s
    [%sexp
      (Or_error.map
         (Oauth_openai_codex.For_testing.credential_of_body ~now_ms:0 body)
         ~f:(fun c -> c.refresh, c.expires_ms, c.account_id)
       : (string * int * string option) Or_error.t)];
  print_s
    [%sexp
      (Oauth_openai_codex.For_testing.credential_of_body
         ~now_ms:0
         {|{"access_token":"not-a-jwt","refresh_token":"r","expires_in":600}|}
       : Credential.Oauth.t Or_error.t)];
  [%expect
    {|
    (Ok (r 600000 (acct-42)))
    (Error "failed to extract accountId from token")
    |}]
;;

(* Token endpoint stub that records the request and answers with a fixed
   token. *)
let token_server ~sw ~env ?(access = "sk-ant-oat01-access") () =
  Server.start ~sw ~env ~handler:(fun _ ->
    Server.Reply.simple
      ~headers:[ "Content-Type", "application/json" ]
      200
      (sprintf
         {|{"access_token":"%s","refresh_token":"refresh-1","expires_in":3600}|}
         access))
;;

let show_token_request (r : Server.Request.t) =
  [%sexp
    { request_line = (r.request_line : string)
    ; content_type = (Server.Request.header r "content-type" : string option)
    ; body = (r.body : string)
    }]
;;

let anthropic_config ~token_url ~port =
  { Oauth_anthropic.Config.default with token_url; callback_port = port }
;;

let free_port ~env ~sw =
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  Eio.Net.close socket;
  port
;;

let show_credential (c : Credential.t Or_error.t) =
  match c with
  | Ok (Oauth o) ->
    print_s
      [%sexp
        "oauth"
      , { access = (o.access : string)
        ; refresh = (o.refresh : string)
        ; expires_in_future = (o.expires_ms > Credential.now_ms () : bool)
        ; account_id = (o.account_id : string option)
        }]
  | other -> print_s_masked [%sexp (other : Credential.t Or_error.t)]
;;

(* Simulates the browser being redirected to the loopback server. *)
let hit_callback ~env ~port ~path ~code ~state =
  ignore
    (Http_client.post
       ~env
       ~url:
         (sprintf "http://127.0.0.1:%d%s?code=%s&state=%s" port path code state)
       ~headers:[]
       ~body:""
       ()
     : _ result)
;;

let state_of_auth_url url =
  Option.value_exn (Uri.get_query_param (Uri.of_string url) "state")
;;

let%expect_test "anthropic login: browser callback wins the race" =
  run
  @@ fun ~env ~sw ->
  let tokens = token_server ~sw ~env () in
  let port = free_port ~env ~sw in
  let config =
    anthropic_config ~token_url:(Server.url tokens "/v1/oauth/token") ~port
  in
  let interaction =
    { Auth_interaction.prompt =
        (fun p ->
          print_s_masked [%sexp "prompt", (p : Auth_interaction.Prompt.t)];
          (* Never answered: the callback resolves the flow. *)
          Fiber.await_cancel ())
    ; notify =
        (function
          | Auth_url { url; _ } ->
            Fiber.fork ~sw (fun () ->
              hit_callback
                ~env
                ~port
                ~path:"/callback"
                ~code:"CODE1"
                ~state:(state_of_auth_url url))
          | Progress m -> print_endline ("progress: " ^ m))
    ; cancel = Cancellation.create ()
    }
  in
  show_credential (Oauth_anthropic.login ~env ~config interaction);
  List.iter (Server.requests tokens) ~f:(fun r ->
    let json = Json.of_string r.body in
    let field k =
      Option.value_map (Json.member k json) ~default:"-" ~f:Json.to_string
    in
    ksprintf
      print_masked
      "%s %s grant=%s code=%s state_is_verifier=%b"
      r.request_line
      (Option.value (Server.Request.header r "content-type") ~default:"-")
      (field "grant_type")
      (field "code")
      (String.equal (field "state") (field "code_verifier")));
  [%expect
    {|
    (prompt
     (Manual_code
      (message
       "Complete login in your browser, or paste the authorization code / redirect URL here:")
      (placeholder http://<host>:<port>/callback)))
    progress: Exchanging authorization code for tokens...
    (oauth
     ((access sk-ant-oat01-access) (refresh refresh-1) (expires_in_future true)
      (account_id ())))
    POST /v1/oauth/token HTTP/1.1 application/json grant="authorization_code" code="CODE1" state_is_verifier=true
    |}]
;;

let%expect_test "anthropic login: pasted redirect URL, state checked" =
  run
  @@ fun ~env ~sw ->
  let tokens = token_server ~sw ~env () in
  let port = free_port ~env ~sw in
  let config =
    anthropic_config ~token_url:(Server.url tokens "/v1/oauth/token") ~port
  in
  let login answer =
    let state = ref "" in
    let interaction =
      { Auth_interaction.prompt = (fun _ -> Ok (answer !state))
      ; notify =
          (function
            | Auth_url { url; _ } -> state := state_of_auth_url url
            | Progress _ -> ())
      ; cancel = Cancellation.create ()
      }
    in
    show_credential (Oauth_anthropic.login ~env ~config interaction)
  in
  login (fun state ->
    sprintf "http://localhost:%d/callback?code=PASTED&state=%s" port state);
  login (fun _ -> "BARE-CODE");
  login (fun _ -> "http://localhost/callback?code=X&state=WRONG");
  login (fun _ -> "");
  [%expect
    {|
    (oauth
     ((access sk-ant-oat01-access) (refresh refresh-1) (expires_in_future true)
      (account_id ())))
    (oauth
     ((access sk-ant-oat01-access) (refresh refresh-1) (expires_in_future true)
      (account_id ())))
    (Error "OAuth state mismatch")
    (Error "missing authorization code")
    |}];
  List.iter (Server.requests tokens) ~f:(fun r ->
    printf
      "code=%s\n"
      (Json.to_string
         (Option.value_exn (Json.member "code" (Json.of_string r.body)))));
  [%expect
    {|
    code="PASTED"
    code="BARE-CODE"
    |}]
;;

let%expect_test "anthropic login: cancellation and token endpoint failure" =
  run
  @@ fun ~env ~sw ->
  let tokens =
    Server.start ~sw ~env ~handler:(fun _ ->
      Server.Reply.simple 401 {|{"error":"invalid_grant"}|})
  in
  let port = free_port ~env ~sw in
  let config =
    anthropic_config ~token_url:(Server.url tokens "/v1/oauth/token") ~port
  in
  let cancel = Cancellation.create () in
  let interaction =
    { Auth_interaction.prompt =
        (fun _ ->
          Cancellation.cancel cancel;
          Fiber.await_cancel ())
    ; notify = ignore
    ; cancel
    }
  in
  show_credential (Oauth_anthropic.login ~env ~config interaction);
  show_credential
    (Oauth_anthropic.login ~env ~config (Auth_interaction.scripted [ "CODE" ]));
  show_credential
    (Oauth_anthropic.login ~env ~config (Auth_interaction.scripted []));
  [%expect
    {|
    (Error "login cancelled")
    (Error
     ("Anthropic token exchange failed"
      (url http://<host>:<port>/v1/oauth/token) (status 401)
      (body "{\"error\":\"invalid_grant\"}")))
    (Error "login cancelled")
    |}]
;;

let%expect_test "anthropic refresh" =
  run
  @@ fun ~env ~sw ->
  let tokens = token_server ~sw ~env ~access:"sk-ant-oat01-new" () in
  let config =
    anthropic_config ~token_url:(Server.url tokens "/v1/oauth/token") ~port:0
  in
  print_s
    [%sexp
      (Or_error.map
         (Oauth_anthropic.refresh ~env ~config ~refresh_token:"old-refresh" ())
         ~f:(fun c -> c.access, c.refresh)
       : (string * string) Or_error.t)];
  List.iter (Server.requests tokens) ~f:(fun r ->
    print_masked (Sexp.to_string_hum (show_token_request r)));
  [%expect
    {|
    (Ok (sk-ant-oat01-new refresh-1))
    ((request_line "POST /v1/oauth/token HTTP/1.1")
     (content_type (application/json))
     (body
      "{\"grant_type\":\"refresh_token\",\"client_id\":\"9d1c250a-e61b-44d9-88ed-5944d1962f5e\",\"refresh_token\":\"old-refresh\"}"))
    |}]
;;

let%expect_test
    "openai codex login and refresh (form-encoded, account id from jwt)"
  =
  run
  @@ fun ~env ~sw ->
  let access =
    jwt_with {|{"https://api.openai.com/auth":{"chatgpt_account_id":"acct-7"}}|}
  in
  let tokens = token_server ~sw ~env ~access () in
  let port = free_port ~env ~sw in
  let config =
    { Oauth_openai_codex.Config.default with
      token_url = Server.url tokens "/oauth/token"
    ; callback_port = port
    }
  in
  let interaction =
    { Auth_interaction.prompt = (fun _ -> Fiber.await_cancel ())
    ; notify =
        (function
          | Auth_url { url; _ } ->
            Fiber.fork ~sw (fun () ->
              hit_callback
                ~env
                ~port
                ~path:"/auth/callback"
                ~code:"OAI-CODE"
                ~state:(state_of_auth_url url))
          | Progress _ -> ())
    ; cancel = Cancellation.create ()
    }
  in
  show_credential (Oauth_openai_codex.login ~env ~config interaction);
  print_s
    [%sexp
      (Or_error.map
         (Oauth_openai_codex.refresh ~env ~config ~refresh_token:"old" ())
         ~f:(fun c -> c.account_id)
       : string option Or_error.t)];
  List.iter (Server.requests tokens) ~f:(fun r ->
    print_masked (Sexp.to_string_hum (show_token_request r)));
  [%expect
    {|
    (oauth
     ((access
       eyJhbGciOiJub25lIn0.eyJodHRwczovL2FwaS5vcGVuYWkuY29tL2F1dGgiOnsiY2hhdGdwdF9hY2NvdW50X2lkIjoiYWNjdC03In19.c2ln)
      (refresh refresh-1) (expires_in_future true) (account_id (acct-7))))
    (Ok (acct-7))
    ((request_line "POST /oauth/token HTTP/1.1")
     (content_type (application/x-www-form-urlencoded))
     (body
      grant_type=authorization_code&client_id=app_EMoamEEZ73f0CkXaXp7hrann&code=OAI-CODE&code_verifier=<verifier>&redirect_uri=http://<host>:<port>/auth/callback))
    ((request_line "POST /oauth/token HTTP/1.1")
     (content_type (application/x-www-form-urlencoded))
     (body
      grant_type=refresh_token&refresh_token=old&client_id=app_EMoamEEZ73f0CkXaXp7hrann))
    |}]
;;
