open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Signing in to git hosts against fake ones: GitHub's device flow,
   Forgejo's authorization code with PKCE (pasted, and through the loopback
   server), pasted tokens, refreshing, the accounts list and signing out. *)

let clock = ref 1_000_000

let with_forges ?(getenv = fun _ -> None) ?config ?listen handler f =
  with_sandbox
  @@ fun t ->
  clock := 1_000_000;
  Option.iter config ~f:(write t ".prigh/config.json");
  let fake = Fake_forge.create handler in
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let forges =
    Forges.create
      ~http:(Fake_forge.http fake)
      ~sleep:(fun s ->
        printf "(sleep %gs)\n" s;
        clock := !clock + Float.to_int (s *. 1000.))
      ?listen:(Option.map listen ~f:(fun f -> f t))
      ~getenv
      ~home:t.dir
      ~now_ms:(fun () -> !clock)
      ~store
      ()
  in
  f t fake forges
;;

let mask_random s =
  s
  |> Re.replace_string
       (Re.Perl.compile_pat "state=[0-9a-f]{32}")
       ~by:"state=<state>"
  |> Re.replace_string
       (Re.Perl.compile_pat "code_challenge=[A-Za-z0-9_-]{43}")
       ~by:"code_challenge=<challenge>"
  |> Re.replace_string
       (Re.Perl.compile_pat "127\\.0\\.0\\.1:[0-9]+")
       ~by:"127.0.0.1:<port>"
;;

let show_prompt (p : Auth_interaction.Prompt.t) =
  match p with
  | Secret { message; _ } -> printf "[secret] %s\n" message
  | Text { message; _ } -> printf "[text] %s\n" message
  | Manual_code { message; placeholder } ->
    printf "[paste] %s (%s)\n" message (mask_random placeholder)
  | Select { message; _ } -> printf "[select] %s\n" message
;;

let interaction ?(on_url = ignore) answer : Auth_interaction.t =
  { prompt =
      (fun p ->
        show_prompt p;
        answer p)
  ; notify =
      (function
        | Auth_url { url; instructions } ->
          printf "[url] %s\n  %s\n" (mask_random url) instructions;
          on_url url
        | Progress m -> printf "[progress] %s\n" m)
  ; cancel = Cancellation.create ()
  }
;;

let no_answer _ = Auth_interaction.cancelled ()

let login ?(method_ = None) ?(answer = no_answer) ?on_url forges host =
  let result =
    Or_error.bind (Forges.instance forges host) ~f:(fun instance ->
      Forges.login forges instance ~method_ (interaction ?on_url answer))
  in
  match result with
  | Ok (c : Forge_credential.t) ->
    print_s
      [%message
        "signed in"
          ~user:(c.user : string option)
          ~method_:(c.method_ : Forge_credential.Method.t)
          ~expires_ms:(c.expires_ms : int option)]
  | Error e -> printf "error: %s\n" (Error.to_string_hum e)
;;

let show_store t =
  let path = Filename.concat t.dir "auth.json" in
  match Sys_unix.file_exists path with
  | `Yes ->
    printf
      "auth.json (%o):\n%s"
      (Core_unix.stat path).st_perm
      (In_channel.read_all path)
  | `No | `Unknown -> print_endline "no auth.json"
;;

let form_field body name =
  List.Assoc.find (Uri.query_of_encoded body) ~equal:String.equal name
  |> Option.bind ~f:List.hd
;;

let%expect_test
    "GitHub: the device flow, waiting as told; the sign-in is stored next to \
     the providers' keys"
  =
  let polls = ref 0 in
  with_forges
    ~getenv:(function
      | "PRIGH_GITHUB_CLIENT_ID" -> Some "Ov23-prigh"
      | _ -> None)
    (fun r ->
       match r.meth, Fake_forge.path_of r.url with
       | Post, "/login/device/code" ->
         ( 200
         , {|{"device_code":"dc-1","user_code":"ABCD-1234","verification_uri":"https://github.com/login/device","expires_in":900,"interval":5}|}
         )
       | Post, "/login/oauth/access_token" ->
         incr polls;
         (match !polls with
          | 1 -> 200, {|{"error":"authorization_pending"}|}
          | 2 -> 200, {|{"error":"slow_down","interval":10}|}
          | _ ->
            ( 200
            , {|{"access_token":"gho_new","token_type":"bearer","scope":"repo"}|}
            ))
       | Get, "/user" -> 200, {|{"login":"octocat","id":1}|}
       | _ -> 404, {|{"message":"Not Found"}|})
  @@ fun t fake forges ->
  write t "auth.json" {|{"anthropic": {"type": "api_key", "key": "sk-ant"}}|};
  login forges "github.com";
  Fake_forge.print_requests fake;
  show_store t;
  [%expect
    {|
    [url] https://github.com/login/device
      Enter the code ABCD-1234 at https://github.com/login/device to sign in to github.com (the code expires in 15 minutes).
    [progress] Waiting for you to authorize prigh...
    (sleep 5s)
    (sleep 5s)
    (sleep 10s)
    [progress] Checking the sign-in with github.com...
    ("signed in" (user (octocat)) (method_ Oauth) (expires_ms ()))
    POST https://github.com/login/device/code
      client_id=Ov23-prigh&scope=repo
    POST https://github.com/login/oauth/access_token
      client_id=Ov23-prigh&device_code=dc-1&grant_type=urn:ietf:params:oauth:grant-type:device_code
    POST https://github.com/login/oauth/access_token
      client_id=Ov23-prigh&device_code=dc-1&grant_type=urn:ietf:params:oauth:grant-type:device_code
    POST https://github.com/login/oauth/access_token
      client_id=Ov23-prigh&device_code=dc-1&grant_type=urn:ietf:params:oauth:grant-type:device_code
    GET https://api.github.com/user
      Authorization: Bearer gho_new
    auth.json (600):
    {
      "anthropic": {
        "type": "api_key",
        "key": "sk-ant"
      },
      "forge:github.com": {
        "type": "forge",
        "kind": "github",
        "method": "oauth",
        "token": "gho_new",
        "client_id": "Ov23-prigh",
        "user": "octocat"
      }
    }
    |}]
;;

let%expect_test "GitHub: the device flow's failures say what to do" =
  let reply = ref (200, "") in
  let code = ref (200, "") in
  with_forges
    ~config:{|{"forges": {"github.com": {"client_id": "Ov23-wrong"}}}|}
    (fun r ->
       match Fake_forge.path_of r.url with
       | "/login/device/code" -> !code
       | _ -> !reply)
  @@ fun _ _ forges ->
  code := 404, {|{"error":"Not Found"}|};
  login forges "github.com";
  code
  := ( 200
     , {|{"device_code":"d","user_code":"WXYZ-0000","verification_uri":"https://github.com/login/device","expires_in":60,"interval":5}|}
     );
  reply
  := ( 200
     , {|{"error":"access_denied","error_description":"The user has denied your application access."}|}
     );
  login forges "github.com";
  reply := 200, {|{"error":"expired_token"}|};
  login forges "github.com";
  reply := 200, {|{"error":"authorization_pending"}|};
  login forges "github.com";
  [%expect
    {|
    error: github.com did not start the device flow: check the OAuth app's client id Ov23-wrong and that its "Enable Device Flow" setting is on
    [url] https://github.com/login/device
      Enter the code WXYZ-0000 at https://github.com/login/device to sign in to github.com (the code expires in 60 seconds).
    [progress] Waiting for you to authorize prigh...
    (sleep 5s)
    error: you declined the authorization on github
    [url] https://github.com/login/device
      Enter the code WXYZ-0000 at https://github.com/login/device to sign in to github.com (the code expires in 60 seconds).
    [progress] Waiting for you to authorize prigh...
    (sleep 5s)
    error: the code expired: /login github gives a new one
    [url] https://github.com/login/device
      Enter the code WXYZ-0000 at https://github.com/login/device to sign in to github.com (the code expires in 60 seconds).
    [progress] Waiting for you to authorize prigh...
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    (sleep 5s)
    error: the code expired: /login github gives a new one
    |}]
;;

let%expect_test
    "a pasted token when no OAuth app is configured, or when asked for; a \
     rejected one is not stored"
  =
  let user = ref (200, {|{"login":"octocat"}|}) in
  with_forges (fun r ->
    match Fake_forge.path_of r.url with
    | "/user" | "/api/v1/user" -> !user
    | _ -> 404, "")
  @@ fun t fake forges ->
  login forges "github.com" ~answer:(fun _ -> Ok " ghp_pasted \n");
  Fake_forge.print_requests fake;
  login forges "codeberg.org" ~method_:(Some Token) ~answer:(fun _ ->
    Ok "fj_token");
  Fake_forge.print_requests fake;
  user := 401, {|{"message":"Bad credentials"}|};
  login forges "git.lan" ~method_:(Some Token) ~answer:(fun _ -> Ok "bad");
  login forges "github.com" ~method_:(Some Oauth);
  show_store t;
  [%expect
    {|
    [secret] No OAuth app is configured for github.com (forges."github.com".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID). Paste a GitHub personal access token for github.com: create one at https://github.com/settings/tokens/new?scopes=repo&description=prigh (classic, scope repo; or a fine-grained token with Contents and Pull requests: read and write)
    [progress] Checking the sign-in with github.com...
    ("signed in" (user (octocat)) (method_ Token) (expires_ms ()))
    GET https://api.github.com/user
      Authorization: Bearer ghp_pasted
    [secret] Paste a Forgejo personal access token for codeberg.org: create one at https://codeberg.org/user/settings/applications (scopes read:user and write:repository)
    [progress] Checking the sign-in with codeberg.org...
    ("signed in" (user (octocat)) (method_ Token) (expires_ms ()))
    GET https://codeberg.org/api/v1/user
      Authorization: token fj_token
    [secret] Paste a Forgejo personal access token for git.lan: create one at https://git.lan/user/settings/applications (scopes read:user and write:repository)
    [progress] Checking the sign-in with git.lan...
    error: git.lan refused the sign-in (HTTP 401: Bad credentials): sign in again with /login forgejo git.lan
    error: cannot sign in to github.com with OAuth: no OAuth app for github.com: set forges."github.com".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID (README: Git hosting), or sign in with a token
    auth.json (600):
    {
      "forge:github.com": {
        "type": "forge",
        "kind": "github",
        "method": "token",
        "token": "ghp_pasted",
        "user": "octocat"
      },
      "forge:codeberg.org": {
        "type": "forge",
        "kind": "forgejo",
        "method": "token",
        "token": "fj_token",
        "user": "octocat"
      }
    }
    |}]
;;

let authorize_query url =
  Uri.query (Uri.of_string url)
  |> List.map ~f:(fun (k, v) -> k, String.concat ~sep:"," v)
;;

let%expect_test "Forgejo: authorization code with PKCE, the redirect URL pasted"
  =
  let state = ref "" in
  let redirect = ref "" in
  with_forges (fun r ->
    match Fake_forge.path_of r.url with
    | "/login/oauth/access_token" ->
      let body = Option.value r.body ~default:"" in
      let verifier =
        Option.value (form_field body "code_verifier") ~default:""
      in
      printf
        "(exchange: code=%s, redirect_uri=%s, verifier matches the challenge: \
         %b)\n"
        (Option.value (form_field body "code") ~default:"")
        (Option.value (form_field body "redirect_uri") ~default:"")
        (String.length verifier >= 43);
      ( 200
      , {|{"access_token":"jwt-access","token_type":"bearer","expires_in":3600,"refresh_token":"jwt-refresh"}|}
      )
    | "/api/v1/user" -> 200, {|{"login":"lloyd"}|}
    | _ -> 404, "")
  @@ fun _ _ forges ->
  login
    forges
    "codeberg.org"
    ~on_url:(fun url ->
      let q = authorize_query url in
      let get k = List.Assoc.find_exn q ~equal:String.equal k in
      state := get "state";
      redirect := get "redirect_uri";
      print_s
        [%message
          ""
            ~client_id:(get "client_id")
            ~redirect_uri:(get "redirect_uri")
            ~scope:(get "scope")
            ~code_challenge_method:(get "code_challenge_method")])
    ~answer:(function
      | Manual_code _ ->
        Ok (sprintf "%s?code=the-code&state=%s" !redirect !state)
      | _ -> no_answer ());
  login forges "codeberg.org" ~answer:(function
    | Manual_code _ -> Ok "http://127.0.0.1/?code=c&state=forged"
    | _ -> no_answer ());
  [%expect
    {|
    [url] https://codeberg.org/login/oauth/authorize?client_id=a4792ccc-144e-407e-86c9-5e7d8d9c3269&redirect_uri=http://127.0.0.1/&response_type=code&scope=read:user%20write:repository&code_challenge=<challenge>&code_challenge_method=S256&state=<state>
      Open this page and authorize git-credential-oauth to sign in to codeberg.org. If the browser then cannot load 127.0.0.1 (prigh runs on another machine), paste that page's address here.
    ((client_id a4792ccc-144e-407e-86c9-5e7d8d9c3269)
     (redirect_uri http://127.0.0.1/) (scope "read:user write:repository")
     (code_challenge_method S256))
    [paste] Complete login in your browser, or paste the authorization code / redirect URL here: (http://127.0.0.1/?code=...&state=...)
    [progress] Exchanging the authorization code...
    (exchange: code=the-code, redirect_uri=http://127.0.0.1/, verifier matches the challenge: true)
    [progress] Checking the sign-in with codeberg.org...
    ("signed in" (user (lloyd)) (method_ Oauth) (expires_ms (4600000)))
    [url] https://codeberg.org/login/oauth/authorize?client_id=a4792ccc-144e-407e-86c9-5e7d8d9c3269&redirect_uri=http://127.0.0.1/&response_type=code&scope=read:user%20write:repository&code_challenge=<challenge>&code_challenge_method=S256&state=<state>
      Open this page and authorize git-credential-oauth to sign in to codeberg.org. If the browser then cannot load 127.0.0.1 (prigh runs on another machine), paste that page's address here.
    [paste] Complete login in your browser, or paste the authorization code / redirect URL here: (http://127.0.0.1/?code=...&state=...)
    error: OAuth state mismatch
    |}]
;;

let%expect_test "Forgejo: the browser comes back to the loopback server" =
  with_forges
    ~config:
      {|{"forges": {"git.lan": {"kind": "forgejo", "client_id": "prigh-app"}}}|}
    ~listen:(fun t -> Forge_login.Listen.real ~env:t.env)
    (fun r ->
       match Fake_forge.path_of r.url with
       | "/login/oauth/access_token" ->
         200, {|{"access_token":"jwt","expires_in":3600,"refresh_token":"rt"}|}
       | "/api/v1/user" -> 200, {|{"login":"lloyd"}|}
       | _ -> 404, "")
  @@ fun t _ forges ->
  let url = ref "" in
  login
    forges
    "git.lan"
    ~on_url:(fun u -> url := u)
    ~answer:(function
      | Manual_code _ ->
        (* The browser, sent back by the forge. *)
        let q = authorize_query !url in
        let get k = List.Assoc.find_exn q ~equal:String.equal k in
        let callback =
          sprintf "%s?code=c&state=%s" (get "redirect_uri") (get "state")
        in
        (match Http_client.get ~env:t.env ~url:callback ~headers:[] () with
         | Ok (response, _) -> printf "(browser: HTTP %d)\n" response.status
         | Error e -> printf "(browser: %s)\n" (Http_client.Error.to_string e));
        Eio.Fiber.await_cancel ()
      | _ -> no_answer ());
  [%expect
    {|
    [url] https://git.lan/login/oauth/authorize?client_id=prigh-app&redirect_uri=http://127.0.0.1:<port>/&response_type=code&scope=read:user%20write:repository&code_challenge=<challenge>&code_challenge_method=S256&state=<state>
      Open this page and authorize the app to sign in to git.lan. If the browser then cannot load 127.0.0.1 (prigh runs on another machine), paste that page's address here.
    [paste] Complete login in your browser, or paste the authorization code / redirect URL here: (http://127.0.0.1:<port>/?code=...&state=...)
    (browser: HTTP 200)
    [progress] Exchanging the authorization code...
    [progress] Checking the sign-in with git.lan...
    ("signed in" (user (lloyd)) (method_ Oauth) (expires_ms (4600000)))
    |}]
;;

let%expect_test
    "expiring OAuth tokens are refreshed under the lock; the accounts list; \
     signing out"
  =
  let refreshes =
    ref
      ( 200
      , {|{"access_token":"jwt-2","expires_in":3600,"refresh_token":"rt-2"}|} )
  in
  with_forges ~config:{|{"forges": {"git.lan": {"kind": "forgejo"}}}|} (fun r ->
    match Fake_forge.path_of r.url with
    | "/login/oauth/access_token" -> !refreshes
    | _ -> 404, "")
  @@ fun t fake forges ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let put host (c : Forge_credential.t) =
    ok_exn
      (Auth_store.modify_entry store (Forge_credential.key ~host) ~f:(fun _ ->
         Ok (Some (Forge_credential.to_json c))))
    |> ignore
  in
  put
    "codeberg.org"
    { kind = Forgejo
    ; method_ = Oauth
    ; token = "jwt-1"
    ; refresh = Some "rt-1"
    ; expires_ms = Some (!clock + 60_000)
    ; client_id = Some Forge_instance.forgejo_default_client_id
    ; user = Some "lloyd"
    };
  put
    "ghe.corp"
    { kind = Github
    ; method_ = Token
    ; token = "ghp"
    ; refresh = None
    ; expires_ms = None
    ; client_id = None
    ; user = Some "me"
    };
  let api host =
    match
      Or_error.bind (Forges.instance forges host) ~f:(fun i ->
        Forges.api forges i)
    with
    | Ok api -> printf "%s: Authorization %s\n" host api.authorization
    | Error e -> printf "%s: %s\n" host (Error.to_string_hum e)
  in
  api "codeberg.org";
  Fake_forge.print_requests fake;
  api "codeberg.org";
  api "ghe.corp";
  api "github.com";
  clock := !clock + 7_200_000;
  refreshes
  := ( 400
     , {|{"error":"invalid_grant","error_description":"refresh token expired"}|}
     );
  api "codeberg.org";
  (match Forges.accounts forges with
   | Ok accounts ->
     List.iter accounts ~f:(fun a ->
       print_endline (Json.to_string (Forges.Account.to_json a)))
   | Error e -> print_s [%sexp (e : Error.t)]);
  print_s [%sexp (Forges.logout forges "https://ghe.corp/x" : unit Or_error.t)];
  print_s [%sexp (Forges.logout forges "ghe.corp" : unit Or_error.t)];
  show_store t;
  [%expect
    {|
    codeberg.org: Authorization Bearer jwt-2
    POST https://codeberg.org/login/oauth/access_token
      client_id=a4792ccc-144e-407e-86c9-5e7d8d9c3269&grant_type=refresh_token&refresh_token=rt-1
    codeberg.org: Authorization Bearer jwt-2
    ghe.corp: Authorization Bearer ghp
    github.com: not signed in to github.com: /login github signs in
    codeberg.org: codeberg.org did not refresh the sign-in (invalid_grant (refresh token expired)): sign in again with /login forgejo codeberg.org
    {"host":"github.com","kind":"github","label":"GitHub","user":null,"method":null,"expires_ms":null,"oauth":false,"oauth_note":"no OAuth app for github.com: set forges.\"github.com\".client_id in ~/.prigh/config.json or $PRIGH_GITHUB_CLIENT_ID (README: Git hosting), or sign in with a token"}
    {"host":"codeberg.org","kind":"forgejo","label":"Forgejo","user":"lloyd","method":"oauth","expires_ms":4600000,"oauth":true,"oauth_note":"signs in through codeberg.org's pre-registered git-credential-oauth app (its consent page names it); forges.\"codeberg.org\".client_id sets your own"}
    {"host":"git.lan","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":true,"oauth_note":"signs in through git.lan's pre-registered git-credential-oauth app (its consent page names it); forges.\"git.lan\".client_id sets your own"}
    {"host":"ghe.corp","kind":"github","label":"GitHub","user":"me","method":"token","expires_ms":null,"oauth":false,"oauth_note":"no OAuth app for ghe.corp: set forges.\"ghe.corp\".client_id in ~/.prigh/config.json (README: Git hosting), or sign in with a token"}
    (Ok ())
    (Error "not signed in to ghe.corp")
    auth.json (600):
    {
      "forge:codeberg.org": {
        "type": "forge",
        "kind": "forgejo",
        "method": "oauth",
        "token": "jwt-2",
        "refresh": "rt-2",
        "expires": 4600000,
        "client_id": "a4792ccc-144e-407e-86c9-5e7d8d9c3269",
        "user": "lloyd"
      }
    }
    |}]
;;
