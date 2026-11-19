open! Core
open! Import

module Config = struct
  type t =
    { client_id : string
    ; client_secret : string
    ; project : string
    }

  let of_env ?(getenv = Sys.getenv) () =
    let open Or_error.Let_syntax in
    let require name =
      match Option.map (getenv name) ~f:String.strip with
      | None | Some "" ->
        Or_error.errorf "Gemini OAuth is not configured: set %s on the backend" name
      | Some value -> Ok value
    in
    let%bind client_id = require "PRIGH_GEMINI_OAUTH_CLIENT_ID" in
    let%bind client_secret = require "PRIGH_GEMINI_OAUTH_CLIENT_SECRET" in
    let%bind project = require "PRIGH_GEMINI_OAUTH_PROJECT" in
    if not
         (String.for_all project ~f:(function
            | 'a' .. 'z' | '0' .. '9' | '-' -> true
            | _ -> false))
    then
      Or_error.error_string
        "PRIGH_GEMINI_OAUTH_PROJECT must be a Google Cloud project ID or number"
    else Ok { client_id; client_secret; project }
  ;;
end

let callback_path = "/oauth/gemini/callback"
let redirect_uri port = sprintf "http://127.0.0.1:%d%s" port callback_path
let token_url = "https://oauth2.googleapis.com/token"

let headers (config : Config.t) ~access_token =
  [ "Authorization", "Bearer " ^ access_token
  ; "x-goog-user-project", config.project
  ]
;;

let authorize_url (config : Config.t) (pkce : Pkce.Pair.t) ~state ~redirect_uri =
  "https://accounts.google.com/o/oauth2/v2/auth?"
  ^ Oauth_common.query_string
      [ "response_type", "code"
      ; "client_id", config.client_id
      ; "redirect_uri", redirect_uri
      ; ( "scope"
        , "https://www.googleapis.com/auth/cloud-platform \
           https://www.googleapis.com/auth/generative-language.retriever" )
      ; "code_challenge", pkce.challenge
      ; "code_challenge_method", "S256"
      ; "state", state
      ; "access_type", "offline"
      ; "prompt", "consent"
      ]
;;

let manual_code ~redirect_uri ~expected_state input =
  let uri = Uri.of_string (String.strip input) in
  let target = Uri.with_query uri [] |> Uri.to_string in
  let one name =
    match Uri.get_query_param' uri name with
    | Some [ value ] when not (String.is_empty value) -> Some value
    | _ -> None
  in
  if not (String.equal target redirect_uri)
  then Or_error.error_string "Paste the complete Gemini OAuth redirect URL"
  else (
    match one "state", one "code" with
    | Some state, Some code when String.equal state expected_state -> Ok code
    | _ ->
      Or_error.error_string
        "Gemini OAuth callback has a missing code or invalid state; start sign-in again")
;;

let credential_of_body
      ?(now_ms = Credential.now_ms ())
      ?previous_refresh
      body
  =
  let open Or_error.Let_syntax in
  let%bind json =
    match Json.parse body with
    | Error _ -> Or_error.error_string "Google OAuth returned invalid JSON"
    | Ok json -> Ok json
  in
  let string name =
    match Json.member name json with
    | Some (`String value) when not (String.is_empty value) -> Some value
    | _ -> None
  in
  match
    ( string "access_token"
    , Option.first_some (string "refresh_token") previous_refresh
    , Option.bind (Json.member "expires_in" json) ~f:Json.int )
  with
  | Some access, Some refresh, Some expires_in
    when expires_in > 0 && expires_in <= 7 * 24 * 60 * 60 ->
    Ok
      { Credential.Oauth.access
      ; refresh
      ; expires_ms = now_ms + (expires_in * 1000)
      ; account_id = None
      }
  | _ ->
    Or_error.error_string
      "Google OAuth returned incomplete credentials; sign in again and grant offline access"
;;

let request ~env ~cancel (config : Config.t) fields =
  match
    Http_client.post
      ~env
      ~cancel
      ~timeout:(Time_ns.Span.of_sec 30.)
      ~url:token_url
      ~headers:
        [ "Content-Type", "application/x-www-form-urlencoded"
        ; "Accept", "application/json"
        ]
      ~body:
        (Oauth_common.query_string
           (("client_id", config.client_id)
            :: ("client_secret", config.client_secret)
            :: fields))
      ()
  with
  | Error Cancelled -> Auth_interaction.cancelled ()
  | Error _ ->
    Or_error.error_string "Could not reach the Google OAuth token endpoint; try again"
  | Ok (response, _) when response.status / 100 <> 2 ->
    Or_error.errorf "Google OAuth token request failed (HTTP %d); sign in again and check the backend's OAuth client configuration" response.status
  | Ok (_, body) -> Ok body
;;

let refresh ~env ~cancel config ~refresh_token =
  Or_error.bind
    (request
       ~env
       ~cancel
       config
       [ "grant_type", "refresh_token"; "refresh_token", refresh_token ])
    ~f:(credential_of_body ~previous_refresh:refresh_token)
;;

let login ~env config (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    Switch.run
    @@ fun sw ->
    let open Or_error.Let_syntax in
    let pkce = Pkce.generate () in
    let state = Pkce.random_hex 32 in
    let%bind server =
      Oauth_callback_server.start
        ~sw
        ~env
        ~host:"127.0.0.1"
        ~port:0
        ~path:callback_path
        ~expected_state:state
        ()
    in
    let redirect_uri = redirect_uri (Oauth_callback_server.port server) in
    interaction.notify
      (Auth_url
         { url = authorize_url config pkce ~state ~redirect_uri
         ; instructions =
             "Sign in with Google. If your browser is on a different machine \
              and the loopback page cannot open, paste its full address here."
         });
    let%bind code =
      Fiber.first
        (fun () -> Ok (Oauth_callback_server.wait server).code)
        (fun () ->
          Or_error.bind
            (interaction.prompt
               (Manual_code
                  { message = "Paste the full Google OAuth redirect URL:"
                  ; placeholder = redirect_uri
                  }))
            ~f:(manual_code ~redirect_uri ~expected_state:state))
    in
    let%bind body =
      request
        ~env
        ~cancel:interaction.cancel
        config
        [ "grant_type", "authorization_code"
        ; "code", code
        ; "code_verifier", pkce.verifier
        ; "redirect_uri", redirect_uri
        ]
    in
    let%map credential = credential_of_body body in
    Credential.Oauth credential)
;;

module For_testing = struct
  let authorize_url = authorize_url
  let redirect_uri = redirect_uri
  let manual_code = manual_code
  let credential_of_body = credential_of_body
end
