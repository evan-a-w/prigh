open! Core
open! Import

module Config = struct
  type t =
    { client_id : string
    ; authorize_url : string
    ; token_url : string
    ; callback_host : string
    ; callback_port : int
    ; callback_path : string
    ; originator : string
    }

  let default =
    { client_id = "app_EMoamEEZ73f0CkXaXp7hrann"
    ; authorize_url = "https://auth.openai.com/oauth/authorize"
    ; token_url = "https://auth.openai.com/oauth/token"
    ; callback_host =
        (match Sys.getenv "PRIGH_OAUTH_CALLBACK_HOST" with
         | Some h when not (String.is_empty h) -> h
         | _ -> "127.0.0.1")
    ; callback_port = 1455
    ; callback_path = "/auth/callback"
    ; originator = "prigh"
    }
  ;;

  let redirect_uri t =
    sprintf "http://localhost:%d%s" t.callback_port t.callback_path
  ;;
end

let scope = "openid profile email offline_access"
let jwt_claim_path = "https://api.openai.com/auth"

let decode_jwt_payload token =
  match String.split token ~on:'.' with
  | [ _; payload; _ ] ->
    let padded =
      payload ^ String.make ((4 - (String.length payload % 4)) % 4) '='
    in
    (match Base64.decode ~alphabet:Base64.uri_safe_alphabet padded with
     | Ok s -> Result.ok (Json.parse s)
     | Error _ ->
       (match Base64.decode padded with
        | Ok s -> Result.ok (Json.parse s)
        | Error _ -> None))
  | _ -> None
;;

let account_id_of_token token =
  let open Option.Let_syntax in
  let%bind payload = decode_jwt_payload token in
  let%bind auth = Json.member jwt_claim_path payload in
  match Json.member "chatgpt_account_id" auth with
  | Some (`String id) when not (String.is_empty id) -> Some id
  | _ -> None
;;

let credential_of_body ?now_ms body =
  let open Or_error.Let_syntax in
  let%bind t = Oauth_common.Token_response.parse body in
  match account_id_of_token t.access_token with
  | None -> Or_error.error_string "failed to extract accountId from token"
  | Some account_id ->
    Ok (Oauth_common.Token_response.to_credential ~account_id ?now_ms t)
;;

let authorize_url (config : Config.t) (pkce : Pkce.Pair.t) ~state =
  config.authorize_url
  ^ "?"
  ^ Oauth_common.query_string
      [ "response_type", "code"
      ; "client_id", config.client_id
      ; "redirect_uri", Config.redirect_uri config
      ; "scope", scope
      ; "code_challenge", pkce.challenge
      ; "code_challenge_method", "S256"
      ; "state", state
      ; "id_token_add_organizations", "true"
      ; "codex_cli_simplified_flow", "true"
      ; "originator", config.originator
      ]
;;

let exchange ~env ~cancel (config : Config.t) ~code ~verifier =
  Or_error.bind
    (Oauth_common.post_form
       ~env
       ~cancel
       ~url:config.token_url
       ~fields:
         [ "grant_type", "authorization_code"
         ; "client_id", config.client_id
         ; "code", code
         ; "code_verifier", verifier
         ; "redirect_uri", Config.redirect_uri config
         ]
       ~what:"OpenAI Codex token exchange")
    ~f:credential_of_body
;;

let refresh
      ~env
      ?(cancel = Cancellation.never)
      ?(config = Config.default)
      ~refresh_token
      ()
  =
  Or_error.bind
    (Oauth_common.post_form
       ~env
       ~cancel
       ~url:config.token_url
       ~fields:
         [ "grant_type", "refresh_token"
         ; "refresh_token", refresh_token
         ; "client_id", config.client_id
         ]
       ~what:"OpenAI Codex token refresh")
    ~f:credential_of_body
;;

let login ~env ?(config = Config.default) (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    Switch.run
    @@ fun sw ->
    let pkce = Pkce.generate () in
    let state = Pkce.random_hex 16 in
    let server =
      match
        Oauth_callback_server.start
          ~sw
          ~env
          ~host:config.callback_host
          ~port:config.callback_port
          ~path:config.callback_path
          ~expected_state:state
          ()
      with
      | Ok server -> Some server
      | Error e ->
        interaction.notify
          (Progress
             (sprintf
                "%s; paste the redirect URL manually"
                (Error.to_string_hum e)));
        None
    in
    interaction.notify
      (Auth_url
         { url = authorize_url config pkce ~state
         ; instructions =
             "A browser window should open. Complete login to finish."
         });
    let open Or_error.Let_syntax in
    let%bind code =
      Oauth_common.wait_for_code
        ~interaction
        ~server
        ~expected_state:state
        ~placeholder:(Config.redirect_uri config)
    in
    interaction.notify (Progress "Exchanging authorization code for tokens...");
    let%map credential =
      exchange
        ~env
        ~cancel:interaction.cancel
        config
        ~code
        ~verifier:pkce.verifier
    in
    Credential.Oauth credential)
;;

module For_testing = struct
  let authorize_url = authorize_url
  let account_id_of_token = account_id_of_token
  let credential_of_body = credential_of_body
end
