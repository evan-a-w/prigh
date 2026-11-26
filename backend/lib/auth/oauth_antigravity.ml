open! Core
open! Import

let sha256 s = Digestif.SHA256.(to_hex (digest_string s))

module Client = struct
  type t =
    { id : string
    ; secret : string [@sexp.opaque]
    }
  [@@deriving sexp_of, equal]

  let pattern =
    {|[0-9]{6,}-[a-z0-9]{32}\.apps\.googleusercontent\.com|GOCSPX-[A-Za-z0-9_-]{28}|}
  ;;

  (* The consumer (oauth-personal) client's, as agy 1.5 embeds it. *)
  let id_sha256 =
    "bf00c418024ba6bf606ccdc37120976e41bc429dd1d46ecf16a729aa532626ea"
  ;;

  let secret_sha256 =
    "1d2f041093fd95aa8995a038c711d50a7960da09a505381c09a745d6ad0ecc60"
  ;;

  let of_candidates candidates =
    let find hash =
      List.find candidates ~f:(fun c ->
        String.equal (sha256 (String.strip c)) hash)
      |> Option.map ~f:String.strip
    in
    match find id_sha256, find secret_sha256 with
    | Some id, Some secret -> Some { id; secret }
    | _ -> None
  ;;

  let file store =
    Filename.dirname (Auth_store.path store) ^/ "antigravity-client.json"
  ;;

  let of_json json =
    match Json.member "client_id" json, Json.member "client_secret" json with
    | Some (`String id), Some (`String secret) -> Some { id; secret }
    | _ -> None
  ;;

  let save store t =
    Or_error.try_with (fun () ->
      let path = file store in
      let tmp = path ^ ".tmp" in
      Out_channel.with_file tmp ~perm:0o600 ~f:(fun oc ->
        Out_channel.output_string
          oc
          (Json.to_string
             (`Object
                 [ "client_id", `String t.id
                 ; "client_secret", `String t.secret
                 ])));
      Core_unix.rename ~src:tmp ~dst:path)
  ;;

  let find_script =
    sprintf
      {|b=$(command -v agy 2>/dev/null) && b=$(readlink -f "$b")
for f in "$b" "$(dirname "$b")/.agy-wrapped"; do
  [ -f "$f" ] && grep -aoE '%s' "$f" 2>/dev/null | sort -u | sed 's/^/client /'
done
true|}
      pattern
  ;;

  let of_lines lines =
    of_candidates
      (List.filter_map lines ~f:(String.chop_prefix ~prefix:"client "))
  ;;

  let discover_local () =
    match
      Core_unix.open_process_in
        (sprintf "bash -c %s" (Filename.quote find_script))
    with
    | exception _ -> None
    | ic ->
      let lines = In_channel.input_lines ic in
      ignore (Core_unix.close_process_in ic : Core_unix.Exit_or_signal.t);
      of_lines lines
  ;;

  let load ?(getenv = Sys.getenv) store =
    match
      ( getenv "PRIGH_ANTIGRAVITY_CLIENT_ID"
      , getenv "PRIGH_ANTIGRAVITY_CLIENT_SECRET" )
    with
    | Some id, Some secret
      when (not (String.is_empty id)) && not (String.is_empty secret) ->
      Some { id; secret }
    | _ ->
      (match
         match In_channel.read_all (file store) with
         | exception _ -> None
         | text -> Result.ok (Json.parse text) |> Option.bind ~f:of_json
       with
       | Some t -> Some t
       | None ->
         let found = discover_local () in
         Option.iter found ~f:(fun t -> ignore (save store t : unit Or_error.t));
         found)
  ;;

  let missing =
    "prigh does not know Antigravity's OAuth client yet: import a tool host's \
     login once (/adapters import antigravity, which reads it from that host's \
     agy), or set PRIGH_ANTIGRAVITY_CLIENT_ID and \
     PRIGH_ANTIGRAVITY_CLIENT_SECRET"
  ;;
end

let authorize_endpoint = "https://accounts.google.com/o/oauth2/auth"
let token_url = "https://oauth2.googleapis.com/token"
let agy_version = "1.5.13"
let redirect_uri = "https://antigravity.google/oauth-callback"

let scopes =
  "https://www.googleapis.com/auth/cloud-platform \
   https://www.googleapis.com/auth/userinfo.email \
   https://www.googleapis.com/auth/userinfo.profile \
   https://www.googleapis.com/auth/cclog \
   https://www.googleapis.com/auth/experimentsandconfigs \
   https://www.googleapis.com/auth/aicode openid"
;;

let credential_of_body ~now_ms ~refresh_token body =
  let open Or_error.Let_syntax in
  let%bind json =
    Json.parse body
    |> Result.map_error ~f:(fun _ ->
      Error.of_string "Google's token endpoint answered something not JSON")
  in
  let%bind access =
    match Json.member "access_token" json with
    | Some (`String s) -> Ok s
    | _ -> Or_error.error_string "Google's token answer has no access_token"
  in
  let expires_in =
    Option.bind (Json.member "expires_in" json) ~f:Json.int
    |> Option.value ~default:3600
  in
  let refresh =
    match Json.member "refresh_token" json with
    | Some (`String s) when not (String.is_empty s) -> s
    | _ -> refresh_token
  in
  if String.is_empty refresh
  then Or_error.error_string "Google's token answer has no refresh_token"
  else
    Ok
      { Credential.Oauth.access
      ; refresh
      ; expires_ms = now_ms + (expires_in * 1000)
      ; account_id = None
      }
;;

let refresh
      ~env
      ?(cancel = Cancellation.never)
      ?(token_url = token_url)
      ~(client : Client.t)
      ~refresh_token
      ()
  =
  Or_error.bind
    (Oauth_common.post_form
       ~env
       ~cancel
       ~url:token_url
       ~fields:
         [ "grant_type", "refresh_token"
         ; "client_id", client.id
         ; "client_secret", client.secret
         ; "refresh_token", refresh_token
         ]
       ~what:"Google token refresh")
    ~f:(credential_of_body ~now_ms:(Credential.now_ms ()) ~refresh_token)
;;

let authorize_url ~(client : Client.t) ~challenge ~state =
  authorize_endpoint
  ^ "?"
  ^ Oauth_common.query_string
      [ "client_id", client.id
      ; "response_type", "code"
      ; "redirect_uri", redirect_uri
      ; "scope", scopes
      ; "code_challenge", challenge
      ; "code_challenge_method", "S256"
      ; "state", state
      ; "access_type", "offline"
      ; "prompt", "consent"
      ]
;;

let login ~env ~(client : Client.t) (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    let pkce = Pkce.generate () in
    let state = Pkce.random_hex 16 in
    interaction.notify
      (Auth_url
         { url = authorize_url ~client ~challenge:pkce.challenge ~state
         ; instructions =
             "Sign in with Google in your browser, then paste the code \
              antigravity.google shows (or its whole URL) here."
         });
    let open Or_error.Let_syntax in
    let%bind code =
      Oauth_common.wait_for_code
        ~interaction
        ~server:None
        ~expected_state:state
        ~placeholder:redirect_uri
    in
    interaction.notify (Progress "Exchanging the code for tokens...");
    let%bind body =
      Oauth_common.post_form
        ~env
        ~cancel:interaction.cancel
        ~url:token_url
        ~fields:
          [ "grant_type", "authorization_code"
          ; "client_id", client.id
          ; "client_secret", client.secret
          ; "code", code
          ; "code_verifier", pkce.verifier
          ; "redirect_uri", redirect_uri
          ]
        ~what:"Google token exchange"
    in
    let%map c =
      credential_of_body ~now_ms:(Credential.now_ms ()) ~refresh_token:"" body
    in
    Credential.Oauth c)
;;

let agy_token_document ~access ~expires_ms =
  let expiry =
    Time_ns.of_int_ns_since_epoch (expires_ms * 1_000_000)
    |> Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc
  in
  Json.to_string
    (`Object
        [ ( "token"
          , `Object
              [ "access_token", `String access
              ; "token_type", `String "Bearer"
              ; "expiry", `String expiry
              ] )
        ; "auth_method", `String "consumer"
        ])
;;

module Agy_token = struct
  type t =
    { access : string option [@sexp.opaque]
    ; refresh : string option [@sexp.opaque]
    ; auth_method : string option
    ; email : string option
    }
  [@@deriving sexp_of]

  let str json key =
    match Json.member key json with
    | Some (`String s) when not (String.is_empty (String.strip s)) ->
      Some (String.strip s)
    | _ -> None
  ;;

  let parse text =
    match Json.parse (String.strip text) with
    | Error _ -> Or_error.error_string "not agy's token document (not JSON)"
    | Ok json ->
      let token = Option.value (Json.member "token" json) ~default:`Null in
      Ok
        { access = str token "access_token"
        ; refresh = str token "refresh_token"
        ; auth_method = str json "auth_method"
        ; email = str json "id_token_email"
        }
  ;;
end

module For_testing = struct
  let credential_of_body = credential_of_body
  let authorize_url = authorize_url
end
