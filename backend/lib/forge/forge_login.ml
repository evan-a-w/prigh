open! Core
open! Import

module Listen = struct
  type t =
    sw:Switch.t -> expected_state:string -> Oauth_callback_server.t Or_error.t

  let real ~env ~sw ~expected_state =
    Oauth_callback_server.start
      ~sw
      ~env
      ~host:"127.0.0.1"
      ~port:0
      ~path:"/"
      ~expected_state
      ()
  ;;
end

let github_scope = "repo"
let forgejo_scope = "read:user write:repository"

let form fields =
  Uri.encoded_of_query (List.map fields ~f:(fun (k, v) -> k, [ v ]))
;;

let post_form ~(http : Forge_http.t) ~cancel ~url ~what fields =
  match
    http
      ~cancel
      { meth = Post
      ; url
      ; headers =
          [ "Content-Type", "application/x-www-form-urlencoded"
          ; "Accept", "application/json"
          ; "User-Agent", "prigh"
          ]
      ; body = Some (form fields)
      }
  with
  | Error e -> Error e
  | Ok { status; body } ->
    (match Json.parse body with
     | Ok json
       when status / 100 = 2 || Option.is_some (Json.member "error" json) ->
       Ok json
     | Ok _ | Error _ -> Or_error.errorf "%s failed (HTTP %d)" what status)
;;

let str json name =
  match Json.member name json with
  | Some (`String s) when not (String.is_empty s) -> Some s
  | _ -> None
;;

let int json name = Option.bind (Json.member name json) ~f:Json.int

(* An OAuth error reply's [error] and [error_description], never the
   request (it may carry a code or a refresh token). *)
let oauth_error json =
  Option.map (str json "error") ~f:(fun error ->
    match str json "error_description" with
    | Some d -> sprintf "%s (%s)" error d
    | None -> error)
;;

let credential_of_tokens ~now_ms (instance : Forge_instance.t) ~client_id json =
  match str json "access_token" with
  | None ->
    Or_error.errorf
      "%s gave no access token%s"
      instance.host
      (Option.value_map (oauth_error json) ~default:"" ~f:(fun e -> ": " ^ e))
  | Some token ->
    Ok
      { Forge_credential.kind = instance.kind
      ; method_ = Oauth
      ; token
      ; refresh = str json "refresh_token"
      ; expires_ms =
          Option.map (int json "expires_in") ~f:(fun s -> now_ms + (s * 1000))
      ; client_id = Some client_id
      ; user = None
      }
;;

let with_user ~http ~cancel instance (credential : Forge_credential.t) =
  Or_error.map
    (Forge_api.current_user
       (Forge_api.create ~cancel ~http instance credential))
    ~f:(fun user -> { credential with user = Some user })
;;

(* ---- GitHub: device flow --------------------------------------------- *)

let device_flow
      ~http
      ~sleep
      ~now_ms
      (instance : Forge_instance.t)
      ~client_id
      (interaction : Auth_interaction.t)
  =
  let cancel = interaction.cancel in
  let open Or_error.Let_syntax in
  let%bind codes =
    post_form
      ~http
      ~cancel
      ~url:(instance.web_url ^ "/login/device/code")
      ~what:"asking github for a device code"
      [ "client_id", client_id; "scope", github_scope ]
  in
  let%bind device_code, user_code, verification_uri =
    match str codes "device_code", str codes "user_code", oauth_error codes with
    | Some d, Some u, _ ->
      Ok
        ( d
        , u
        , Option.value
            (str codes "verification_uri")
            ~default:(instance.web_url ^ "/login/device") )
    | _, _, Some ("Not Found" | "not_found") | _, _, None ->
      Or_error.errorf
        "%s did not start the device flow: check the OAuth app's client id %s \
         and that its \"Enable Device Flow\" setting is on"
        instance.host
        client_id
    | _, _, Some e ->
      Or_error.errorf
        "%s did not start the device flow (%s): check the OAuth app's client \
         id %s and that its \"Enable Device Flow\" setting is on"
        instance.host
        e
        client_id
  in
  let expires_in = Option.value (int codes "expires_in") ~default:900 in
  interaction.notify
    (Auth_url
       { url = verification_uri
       ; instructions =
           sprintf
             "Enter the code %s at %s to sign in to %s (the code expires in \
              %s)."
             user_code
             verification_uri
             instance.host
             (if expires_in >= 120
              then sprintf "%d minutes" (expires_in / 60)
              else sprintf "%d seconds" expires_in)
       });
  interaction.notify (Progress "Waiting for you to authorize prigh...");
  let deadline = now_ms () + (expires_in * 1000) in
  let rec poll interval =
    sleep (Float.of_int interval);
    if now_ms () > deadline
    then
      Or_error.errorf
        "the code expired: /login %s gives a new one"
        (Forge_instance.login_arg instance)
    else (
      let%bind reply =
        post_form
          ~http
          ~cancel
          ~url:(instance.web_url ^ "/login/oauth/access_token")
          ~what:"polling github for the token"
          [ "client_id", client_id
          ; "device_code", device_code
          ; "grant_type", "urn:ietf:params:oauth:grant-type:device_code"
          ]
      in
      match str reply "error" with
      | None ->
        credential_of_tokens ~now_ms:(now_ms ()) instance ~client_id reply
      | Some "authorization_pending" -> poll interval
      | Some "slow_down" ->
        poll (Option.value (int reply "interval") ~default:(interval + 5))
      | Some "expired_token" ->
        Or_error.errorf
          "the code expired: /login %s gives a new one"
          (Forge_instance.login_arg instance)
      | Some "access_denied" ->
        Or_error.error_string "you declined the authorization on github"
      | Some _ ->
        Or_error.errorf
          "%s refused the device flow: %s"
          instance.host
          (Option.value (oauth_error reply) ~default:"unknown error"))
  in
  poll (Option.value (int codes "interval") ~default:5)
;;

(* ---- Forgejo: authorization code with PKCE --------------------------- *)

let authorization_code
      ~http
      ~listen
      ~now_ms
      (instance : Forge_instance.t)
      ~client_id
      (interaction : Auth_interaction.t)
  =
  Switch.run
  @@ fun sw ->
  let pkce = Pkce.generate () in
  let state = Pkce.random_hex 16 in
  let server =
    match listen with
    | None -> None
    | Some listen ->
      (match listen ~sw ~expected_state:state with
       | Ok server -> Some server
       | Error e ->
         interaction.notify
           (Progress
              (sprintf
                 "%s; paste the redirect URL instead"
                 (Error.to_string_hum e)));
         None)
  in
  let redirect_uri =
    match server with
    | Some server ->
      sprintf "http://127.0.0.1:%d/" (Oauth_callback_server.port server)
    | None -> "http://127.0.0.1/"
  in
  let url =
    instance.web_url
    ^ "/login/oauth/authorize?"
    ^ form
        [ "client_id", client_id
        ; "redirect_uri", redirect_uri
        ; "response_type", "code"
        ; "scope", forgejo_scope
        ; "code_challenge", pkce.challenge
        ; "code_challenge_method", "S256"
        ; "state", state
        ]
  in
  interaction.notify
    (Auth_url
       { url
       ; instructions =
           sprintf
             "Open this page and authorize %s to sign in to %s. If the browser \
              then cannot load 127.0.0.1 (prigh runs on another machine), \
              paste that page's address here."
             (if instance.default_client
              then "git-credential-oauth"
              else "the app")
             instance.host
       });
  let open Or_error.Let_syntax in
  let%bind code =
    Oauth_common.wait_for_code
      ~interaction
      ~server
      ~expected_state:state
      ~placeholder:(redirect_uri ^ "?code=...&state=...")
  in
  interaction.notify (Progress "Exchanging the authorization code...");
  let%bind reply =
    post_form
      ~http
      ~cancel:interaction.cancel
      ~url:(instance.web_url ^ "/login/oauth/access_token")
      ~what:(sprintf "the token exchange with %s" instance.host)
      [ "client_id", client_id
      ; "code", code
      ; "grant_type", "authorization_code"
      ; "redirect_uri", redirect_uri
      ; "code_verifier", pkce.verifier
      ]
  in
  credential_of_tokens ~now_ms:(now_ms ()) instance ~client_id reply
;;

(* ---- a pasted token -------------------------------------------------- *)

let token_page (instance : Forge_instance.t) =
  match instance.kind with
  | Github ->
    sprintf
      "%s/settings/tokens/new?scopes=repo&description=prigh (classic, scope \
       repo; or a fine-grained token with Contents and Pull requests: read and \
       write)"
      instance.web_url
  | Forgejo ->
    sprintf
      "%s/user/settings/applications (scopes read:user and write:repository)"
      instance.web_url
;;

let oauth_note (instance : Forge_instance.t) =
  match instance.client_id, instance.kind with
  | None, Github ->
    Some
      (sprintf
         "no OAuth app for %s: set forges.\"%s\".client_id in \
          ~/.prigh/config.json%s (README: Git hosting), or sign in with a \
          token"
         instance.host
         instance.host
         (if String.equal instance.host "github.com"
          then " or $PRIGH_GITHUB_CLIENT_ID"
          else ""))
  | Some _, Forgejo when instance.default_client ->
    Some
      (sprintf
         "signs in through %s's pre-registered git-credential-oauth app (its \
          consent page names it); forges.\"%s\".client_id sets your own"
         instance.host
         instance.host)
  | _ -> None
;;

let paste_token
      (instance : Forge_instance.t)
      ~why
      (interaction : Auth_interaction.t)
  =
  let open Or_error.Let_syntax in
  let%map token =
    interaction.prompt
      (Secret
         { message =
             sprintf
               "%sPaste a %s personal access token for %s: create one at %s"
               why
               (Forge_kind.label instance.kind)
               instance.host
               (token_page instance)
         ; allow_empty = false
         })
  in
  { Forge_credential.kind = instance.kind
  ; method_ = Token
  ; token = String.strip token
  ; refresh = None
  ; expires_ms = None
  ; client_id = None
  ; user = None
  }
;;

let login
      ~http
      ~sleep
      ?listen
      ?(now_ms = Credential.now_ms)
      (instance : Forge_instance.t)
      ~(method_ : Forge_credential.Method.t option)
      (interaction : Auth_interaction.t)
  =
  Auth_interaction.run interaction ~f:(fun () ->
    let open Or_error.Let_syntax in
    let%bind credential =
      match method_, instance.client_id with
      | Some Token, _ -> paste_token instance ~why:"" interaction
      | (None | Some Oauth), Some client_id ->
        (match instance.kind with
         | Github ->
           device_flow ~http ~sleep ~now_ms instance ~client_id interaction
         | Forgejo ->
           authorization_code
             ~http
             ~listen
             ~now_ms
             instance
             ~client_id
             interaction)
      | Some Oauth, None ->
        Or_error.errorf
          "cannot sign in to %s with OAuth: %s"
          instance.host
          (Option.value (oauth_note instance) ~default:"no OAuth client id")
      | None, None ->
        paste_token
          instance
          ~why:
            (sprintf
               "No OAuth app is configured for %s (forges.\"%s\".client_id in \
                ~/.prigh/config.json%s). "
               instance.host
               instance.host
               (if String.equal instance.host "github.com"
                then " or $PRIGH_GITHUB_CLIENT_ID"
                else ""))
          interaction
    in
    interaction.notify
      (Progress (sprintf "Checking the sign-in with %s..." instance.host));
    with_user ~http ~cancel:interaction.cancel instance credential)
;;

let refresh
      ~http
      ?(cancel = Cancellation.never)
      ?(now_ms = Credential.now_ms ())
      (instance : Forge_instance.t)
      (credential : Forge_credential.t)
  =
  let again =
    sprintf "sign in again with /login %s" (Forge_instance.login_arg instance)
  in
  match credential.refresh, credential.client_id with
  | None, _ | _, None ->
    Or_error.errorf "the sign-in to %s has expired: %s" instance.host again
  | Some refresh_token, Some client_id ->
    let open Or_error.Let_syntax in
    let%bind reply =
      post_form
        ~http
        ~cancel
        ~url:(instance.web_url ^ "/login/oauth/access_token")
        ~what:(sprintf "refreshing the sign-in to %s" instance.host)
        [ "client_id", client_id
        ; "grant_type", "refresh_token"
        ; "refresh_token", refresh_token
        ]
    in
    (match str reply "access_token" with
     | None ->
       Or_error.errorf
         "%s did not refresh the sign-in (%s): %s"
         instance.host
         (Option.value (oauth_error reply) ~default:"no token")
         again
     | Some _ ->
       let%map fresh = credential_of_tokens ~now_ms instance ~client_id reply in
       { fresh with
         user = credential.user
       ; refresh = Option.first_some fresh.refresh credential.refresh
       })
;;
