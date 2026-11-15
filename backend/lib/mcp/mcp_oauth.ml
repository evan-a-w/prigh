open! Core
open! Import

let timeout = Time_ns.Span.of_int_sec 15

module Challenge = struct
  type t =
    { resource_metadata : string option
    ; scope : string option
    ; error : string option
    }
  [@@deriving sexp_of]

  let bearer =
    Re.compile (Re.Perl.re ~opts:[ `Caseless ] {|(^|[\s,])Bearer(\s|$)|})
  ;;

  let param =
    Re.compile
      (Re.Perl.re
         {|([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?:"((?:[^"\\]|\\.)*)"|([^\s,]+))|})
  ;;

  let parse header =
    match Re.exec_opt bearer header with
    | None -> None
    | Some group ->
      let rest = String.drop_prefix header (Re.Group.stop group 0) in
      let params =
        Re.all param rest
        |> List.map ~f:(fun g ->
          ( String.lowercase (Re.Group.get g 1)
          , match Re.Group.get_opt g 2 with
            | Some quoted ->
              String.substr_replace_all quoted ~pattern:{|\"|} ~with_:{|"|}
            | None -> Re.Group.get g 3 ))
      in
      let find name = List.Assoc.find params ~equal:String.equal name in
      Some
        { resource_metadata = find "resource_metadata"
        ; scope = find "scope"
        ; error = find "error"
        }
  ;;
end

module Server_metadata = struct
  type t =
    { issuer : string
    ; authorization_endpoint : string
    ; token_endpoint : string
    ; registration_endpoint : string option
    ; revocation_endpoint : string option
    ; scopes_supported : string list
    }
  [@@deriving sexp_of]
end

module Discovery = struct
  type t =
    { resource : string
    ; resource_metadata : string option
    ; server : Server_metadata.t
    ; scopes : string list
    }
  [@@deriving sexp_of]
end

let string_member json name = Option.bind (Json.member name json) ~f:Json.string

let strings_member json name =
  Option.bind (Json.member name json) ~f:Json.list
  |> Option.value ~default:[]
  |> List.filter_map ~f:Json.string
;;

let origin uri =
  Uri.to_string
    (Uri.make
       ?scheme:(Uri.scheme uri)
       ?host:(Uri.host uri)
       ?port:(Uri.port uri)
       ())
;;

let well_known uri name =
  let base = origin uri ^ "/.well-known/" ^ name in
  match String.strip ~drop:(Char.equal '/') (Uri.path uri) with
  | "" -> base, None
  | path -> base, Some path
;;

let resource_metadata_urls ~url challenge =
  let uri = Uri.of_string url in
  let base, path = well_known uri "oauth-protected-resource" in
  List.filter_map
    ~f:Fn.id
    [ Option.bind challenge ~f:(fun (c : Challenge.t) -> c.resource_metadata)
    ; Option.map path ~f:(fun path -> base ^ "/" ^ path)
    ; Some base
    ]
  |> List.stable_dedup ~compare:String.compare
;;

let server_metadata_urls ~issuer =
  let uri = Uri.of_string issuer in
  let oauth, path = well_known uri "oauth-authorization-server" in
  let oidc, _ = well_known uri "openid-configuration" in
  match path with
  | None -> [ oauth; oidc ]
  | Some path ->
    [ oauth ^ "/" ^ path
    ; oidc ^ "/" ^ path
    ; origin uri ^ "/" ^ path ^ "/.well-known/openid-configuration"
    ]
;;

let get_json ~env url =
  match
    Http_client.get
      ~env
      ~timeout
      ~url
      ~headers:[ "Accept", "application/json" ]
      ()
  with
  | Ok (response, body) when response.status = 200 ->
    Result.ok (Json.parse body)
  | Ok _ | Error _ -> None
;;

let server_metadata_of_json json =
  match
    ( string_member json "authorization_endpoint"
    , string_member json "token_endpoint" )
  with
  | Some authorization_endpoint, Some token_endpoint ->
    Some
      { Server_metadata.issuer =
          Option.value (string_member json "issuer") ~default:""
      ; authorization_endpoint
      ; token_endpoint
      ; registration_endpoint = string_member json "registration_endpoint"
      ; revocation_endpoint = string_member json "revocation_endpoint"
      ; scopes_supported = strings_member json "scopes_supported"
      }
  | _ -> None
;;

let probe ~env ~url =
  match
    Http_client.post
      ~env
      ~timeout
      ~url
      ~headers:
        [ "Content-Type", "application/json"
        ; "Accept", "application/json, text/event-stream"
        ]
      ~body:
        {|{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"prigh","version":"0"}}}|}
      ()
  with
  | Ok (response, _) when response.status = 401 ->
    Option.bind
      (Http_client.Response.header response "www-authenticate")
      ~f:Challenge.parse
  | Ok _ | Error _ -> None
;;

let discover ~env ~url ?challenge () =
  let tried = Queue.create () in
  let first urls ~f =
    List.find_map urls ~f:(fun url ->
      Queue.enqueue tried url;
      Option.bind (get_json ~env url) ~f)
  in
  let resource_metadata =
    first (resource_metadata_urls ~url challenge) ~f:(fun json ->
      match strings_member json "authorization_servers" with
      | [] -> None
      | issuer :: _ -> Some (json, issuer))
  in
  let issuer, found_at =
    match resource_metadata with
    | Some (_, issuer) -> issuer, Some (Queue.last_exn tried)
    | None -> origin (Uri.of_string url), None
  in
  match
    first (server_metadata_urls ~issuer) ~f:(fun json ->
      Option.map
        (server_metadata_of_json json)
        ~f:(fun (m : Server_metadata.t) ->
          if String.is_empty m.issuer then { m with issuer } else m))
  with
  | None ->
    Or_error.errorf
      "found no OAuth metadata for %s (tried %s)"
      url
      (String.concat ~sep:", " (Queue.to_list tried))
  | Some server ->
    let prm = Option.map resource_metadata ~f:fst in
    let scopes =
      match Option.bind challenge ~f:(fun (c : Challenge.t) -> c.scope) with
      | Some scope ->
        String.split scope ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
      | None ->
        Option.value_map prm ~default:[] ~f:(fun json ->
          strings_member json "scopes_supported")
    in
    Ok
      { Discovery.resource =
          Option.bind prm ~f:(fun json -> string_member json "resource")
          |> Option.value ~default:url
      ; resource_metadata = found_at
      ; server
      ; scopes
      }
;;

module Client = struct
  module Auth_method = struct
    type t =
      | None_
      | Basic
      | Post
    [@@deriving sexp_of, equal]

    let to_string = function
      | None_ -> "none"
      | Basic -> "client_secret_basic"
      | Post -> "client_secret_post"
    ;;

    let of_string = function
      | "none" -> Some None_
      | "client_secret_basic" -> Some Basic
      | "client_secret_post" -> Some Post
      | _ -> None
    ;;
  end

  type t =
    { id : string
    ; secret : string option
    ; auth_method : Auth_method.t
    }

  let sexp_of_t { id; secret; auth_method } =
    [%sexp
      { id : string
      ; secret = (Option.map secret ~f:(Fn.const "<hidden>") : string option)
      ; auth_method : Auth_method.t
      }]
  ;;

  let to_json { id; secret; auth_method } =
    `Object
      ([ "client_id", `String id
       ; "auth_method", `String (Auth_method.to_string auth_method)
       ]
       @ Option.value_map secret ~default:[] ~f:(fun s ->
         [ "client_secret", `String s ]))
  ;;

  let of_json json =
    match string_member json "client_id" with
    | None -> Or_error.error_string "no client_id"
    | Some id ->
      let secret = string_member json "client_secret" in
      Ok
        { id
        ; secret
        ; auth_method =
            (match
               Option.bind
                 (Option.first_some
                    (string_member json "auth_method")
                    (string_member json "token_endpoint_auth_method"))
                 ~f:Auth_method.of_string
             with
             | Some m -> m
             | None -> if Option.is_some secret then Basic else None_)
        }
  ;;

  (* The request's headers and form fields that authenticate it. *)
  let credentials t =
    match t.auth_method, t.secret with
    | Basic, Some secret ->
      let encode = Uri.pct_encode ~component:`Userinfo in
      ( [ ( "Authorization"
          , "Basic " ^ Base64.encode_string (encode t.id ^ ":" ^ encode secret)
          )
        ]
      , [] )
    | Post, Some secret -> [], [ "client_id", t.id; "client_secret", secret ]
    | (None_ | Basic | Post), _ -> [], [ "client_id", t.id ]
  ;;
end

let oauth_error ~what ~status body =
  let detail =
    match Json.parse body with
    | Ok json ->
      (match
         string_member json "error", string_member json "error_description"
       with
       | Some e, Some d -> sprintf "%s (%s)" e d
       | Some e, None -> e
       | None, _ -> String.prefix (String.strip body) 200)
    | Error _ -> String.prefix (String.strip body) 200
  in
  Or_error.errorf "%s failed: HTTP %d: %s" what status detail
;;

let post ~env ~what ~url ~headers ~body ~ok =
  match Http_client.post ~env ~timeout ~url ~headers ~body () with
  | Error e ->
    Or_error.errorf "%s failed: %s: %s" what url (Http_client.Error.to_string e)
  | Ok (response, body) when response.status >= 200 && response.status < 300 ->
    ok body
  | Ok (response, body) -> oauth_error ~what ~status:response.status body
;;

let register ~env (server : Server_metadata.t) ~redirect_uri =
  match server.registration_endpoint with
  | None ->
    Or_error.errorf
      "%s does not offer dynamic client registration: register an OAuth client \
       there and give its id in the server's \"oauth\": {\"clientId\": ...}"
      server.issuer
  | Some url ->
    post
      ~env
      ~what:"client registration"
      ~url
      ~headers:
        [ "Content-Type", "application/json"; "Accept", "application/json" ]
      ~body:
        (Json.to_string
           (`Object
               [ "client_name", `String "prigh"
               ; "redirect_uris", `Array [ `String redirect_uri ]
               ; ( "grant_types"
                 , `Array
                     [ `String "authorization_code"; `String "refresh_token" ] )
               ; "response_types", `Array [ `String "code" ]
               ; "token_endpoint_auth_method", `String "none"
               ]))
      ~ok:(fun body -> Or_error.bind (Json.parse body) ~f:Client.of_json)
;;

let authorize_url
      (server : Server_metadata.t)
      (client : Client.t)
      ~redirect_uri
      ~challenge
      ~state
      ~resource
      ~scopes
  =
  Uri.add_query_params'
    (Uri.of_string server.authorization_endpoint)
    ([ "response_type", "code"
     ; "client_id", client.id
     ; "redirect_uri", redirect_uri
     ; "code_challenge", challenge
     ; "code_challenge_method", "S256"
     ; "state", state
     ; "resource", resource
     ]
     @
     if List.is_empty scopes
     then []
     else [ "scope", String.concat ~sep:" " scopes ])
  |> Uri.to_string
;;

module Token = struct
  type t =
    { access : string
    ; refresh : string option
    ; expires_ms : int option
    }

  let sexp_of_t { access = _; refresh; expires_ms } =
    [%sexp
      { access = "<hidden>"
      ; refresh = (Option.map refresh ~f:(Fn.const "<hidden>") : string option)
      ; expires_ms : int option
      }]
  ;;

  let to_json { access; refresh; expires_ms } =
    `Object
      ([ "access", `String access ]
       @ Option.value_map refresh ~default:[] ~f:(fun r ->
         [ "refresh", `String r ])
       @ Option.value_map expires_ms ~default:[] ~f:(fun e ->
         [ "expires", `Number (Int.to_string e) ]))
  ;;

  let of_json json =
    match string_member json "access" with
    | None -> Or_error.error_string "no access token"
    | Some access ->
      Ok
        { access
        ; refresh = string_member json "refresh"
        ; expires_ms = Option.bind (Json.member "expires" json) ~f:Json.int
        }
  ;;

  let expiring t ~now_ms =
    match t.expires_ms with
    | None -> false
    | Some expires -> expires - now_ms < 60_000
  ;;

  let of_response ~now_ms ?previous_refresh body =
    let open Or_error.Let_syntax in
    let%bind json = Json.parse body in
    match string_member json "access_token" with
    | None -> Or_error.error_string "the token endpoint sent no access_token"
    | Some access ->
      Ok
        { access
        ; refresh =
            Option.first_some
              (string_member json "refresh_token")
              previous_refresh
        ; expires_ms =
            Option.map
              (Option.bind (Json.member "expires_in" json) ~f:Json.int)
              ~f:(fun s -> now_ms + (s * 1000))
        }
  ;;
end

let token_request ~env ~what ~token_endpoint client fields ~ok =
  let headers, credentials = Client.credentials client in
  post
    ~env
    ~what
    ~url:token_endpoint
    ~headers:
      ([ "Content-Type", "application/x-www-form-urlencoded"
       ; "Accept", "application/json"
       ]
       @ headers)
    ~body:
      (Uri.encoded_of_query
         (List.map (fields @ credentials) ~f:(fun (k, v) -> k, [ v ])))
    ~ok
;;

let exchange
      ~env
      ~now_ms
      ~token_endpoint
      client
      ~code
      ~verifier
      ~redirect_uri
      ~resource
  =
  token_request
    ~env
    ~what:"the token exchange"
    ~token_endpoint
    client
    [ "grant_type", "authorization_code"
    ; "code", code
    ; "code_verifier", verifier
    ; "redirect_uri", redirect_uri
    ; "resource", resource
    ]
    ~ok:(Token.of_response ~now_ms)
;;

let refresh ~env ~now_ms ~token_endpoint client ~refresh ~resource =
  token_request
    ~env
    ~what:"refreshing the token"
    ~token_endpoint
    client
    [ "grant_type", "refresh_token"
    ; "refresh_token", refresh
    ; "resource", resource
    ]
    ~ok:(Token.of_response ~now_ms ~previous_refresh:refresh)
;;

let revoke ~env ~revocation_endpoint client ~token =
  token_request
    ~env
    ~what:"revoking the token"
    ~token_endpoint:revocation_endpoint
    client
    [ "token", token ]
    ~ok:(fun _ -> Ok ())
;;
