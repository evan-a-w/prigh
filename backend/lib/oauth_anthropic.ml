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
    ; scopes : string
    }

  let callback_port = 53692
  let callback_path = "/callback"

  let default =
    { client_id =
        Base64.decode_exn "OWQxYzI1MGEtZTYxYi00NGQ5LTg4ZWQtNTk0NGQxOTYyZjVl"
    ; authorize_url = "https://claude.ai/oauth/authorize"
    ; token_url = "https://platform.claude.com/v1/oauth/token"
    ; callback_host =
        (match Sys.getenv "PRIGH_OAUTH_CALLBACK_HOST" with
         | Some h when not (String.is_empty h) -> h
         | _ -> "127.0.0.1")
    ; callback_port
    ; callback_path
    ; scopes =
        "org:create_api_key user:profile user:inference \
         user:sessions:claude_code user:mcp_servers user:file_upload"
    }
  ;;

  (* Anthropic registers the redirect as localhost, whatever host we bind. *)
  let redirect_uri t =
    sprintf "http://localhost:%d%s" t.callback_port t.callback_path
  ;;
end

let authorize_url (config : Config.t) (pkce : Pkce.Pair.t) =
  config.authorize_url
  ^ "?"
  ^ Oauth_common.query_string
      [ "code", "true"
      ; "client_id", config.client_id
      ; "response_type", "code"
      ; "redirect_uri", Config.redirect_uri config
      ; "scope", config.scopes
      ; "code_challenge", pkce.challenge
      ; "code_challenge_method", "S256"
      ; "state", pkce.verifier
      ]
;;

let credential_of_body ?now_ms body =
  Or_error.map (Oauth_common.Token_response.parse body) ~f:(fun t ->
    let c = Oauth_common.Token_response.to_credential ?now_ms t in
    (* pi subtracts five minutes from the reported lifetime. *)
    { c with expires_ms = c.expires_ms - Credential.refresh_margin_ms })
;;

let exchange ~env ~cancel (config : Config.t) ~code ~state ~verifier =
  Or_error.bind
    (Oauth_common.post_json
       ~env
       ~cancel
       ~url:config.token_url
       ~fields:
         [ "grant_type", "authorization_code"
         ; "client_id", config.client_id
         ; "code", code
         ; "state", state
         ; "redirect_uri", Config.redirect_uri config
         ; "code_verifier", verifier
         ]
       ~what:"Anthropic token exchange")
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
    (Oauth_common.post_json
       ~env
       ~cancel
       ~url:config.token_url
       ~fields:
         [ "grant_type", "refresh_token"
         ; "client_id", config.client_id
         ; "refresh_token", refresh_token
         ]
       ~what:"Anthropic token refresh")
    ~f:credential_of_body
;;

let login ~env ?(config = Config.default) (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    Switch.run
    @@ fun sw ->
    let pkce = Pkce.generate () in
    let server =
      match
        Oauth_callback_server.start
          ~sw
          ~env
          ~host:config.callback_host
          ~port:config.callback_port
          ~path:config.callback_path
          ~expected_state:pkce.verifier
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
         { url = authorize_url config pkce
         ; instructions =
             "Complete login in your browser. If the browser is on another \
              machine, paste the final redirect URL here."
         });
    let open Or_error.Let_syntax in
    let%bind code =
      Oauth_common.wait_for_code
        ~interaction
        ~server
        ~expected_state:pkce.verifier
        ~placeholder:(Config.redirect_uri config)
    in
    interaction.notify (Progress "Exchanging authorization code for tokens...");
    let%map credential =
      exchange
        ~env
        ~cancel:interaction.cancel
        config
        ~code
        ~state:pkce.verifier
        ~verifier:pkce.verifier
    in
    Credential.Oauth credential)
;;

module For_testing = struct
  let authorize_url = authorize_url
  let credential_of_body = credential_of_body
end
