open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf
module Server = Fake_http_server

(* ---- a fake MCP server and its authorization server, on one port ------- *)

module Fake = struct
  type t =
    { mutable base : string
    ; mutable metadata : [ `Oauth | `Oidc | `None ]
    ; mutable resource_metadata : [ `Header | `Well_known | `None ]
    ; mutable registration : bool
    ; mutable revocation : bool
    ; mutable challenge : string option (** the browser's code_challenge *)
    ; mutable access_seq : int
    ; mutable refresh_ok : bool
    ; valid : String.Hash_set.t (** access tokens the MCP server takes *)
    ; log : string Queue.t
    }

  let create () =
    { base = ""
    ; metadata = `Oauth
    ; resource_metadata = `Header
    ; registration = true
    ; revocation = true
    ; challenge = None
    ; access_seq = 0
    ; refresh_ok = true
    ; valid = String.Hash_set.create ()
    ; log = Queue.create ()
    }
  ;;

  let json status json =
    Server.Reply.simple
      ~headers:[ "Content-Type", "application/json" ]
      status
      (Json.to_string json)
  ;;

  let not_found = Server.Reply.simple 404 "not found"

  let as_metadata t =
    `Object
      ([ "issuer", `String (t.base ^ "/as")
       ; "authorization_endpoint", `String (t.base ^ "/as/authorize")
       ; "token_endpoint", `String (t.base ^ "/as/token")
       ; "code_challenge_methods_supported", `Array [ `String "S256" ]
       ]
       @ (if t.registration
          then [ "registration_endpoint", `String (t.base ^ "/as/register") ]
          else [])
       @
       if t.revocation
       then [ "revocation_endpoint", `String (t.base ^ "/as/revoke") ]
       else [])
  ;;

  let issue t ~refresh =
    t.access_seq <- t.access_seq + 1;
    let access = sprintf "access-%d" t.access_seq in
    Hash_set.add t.valid access;
    `Object
      [ "access_token", `String access
      ; "token_type", `String "Bearer"
      ; "expires_in", `Number "3600"
      ; "refresh_token", `String refresh
      ]
  ;;

  let mcp t (request : Server.Request.t) =
    let bearer =
      Option.bind (Server.Request.header request "authorization") ~f:(fun h ->
        String.chop_prefix h ~prefix:"Bearer ")
    in
    match bearer with
    | Some token when Hash_set.mem t.valid token ->
      let body = Or_error.ok_exn (Json.parse request.body) in
      let result result =
        json
          200
          (`Object
              [ "jsonrpc", `String "2.0"
              ; "id", Option.value_exn (Json.member "id" body)
              ; "result", result
              ])
      in
      (match Option.bind (Json.member "method" body) ~f:Json.string with
       | Some "initialize" ->
         result (`Object [ "protocolVersion", `String "2025-06-18" ])
       | Some "tools/list" ->
         result
           (`Object
               [ ( "tools"
                 , `Array
                     [ `Object
                         [ "name", `String "whoami"
                         ; "description", `String ("as " ^ token)
                         ]
                     ] )
               ])
       | _ -> Server.Reply.simple 202 "")
    | _ ->
      let challenge =
        match t.resource_metadata with
        | `Header ->
          sprintf
            {|Bearer error="invalid_token", resource_metadata="%s/meta/prm", scope="mcp:read mcp:write"|}
            t.base
        | `Well_known | `None -> {|Bearer realm="mcp"|}
      in
      Server.Reply.simple
        ~headers:[ "WWW-Authenticate", challenge ]
        401
        "unauthorized"
  ;;

  let form body =
    List.map (Uri.query_of_encoded body) ~f:(fun (k, vs) ->
      k, String.concat ~sep:"," vs)
  ;;

  let token t (request : Server.Request.t) =
    let form = form request.body in
    let param name = List.Assoc.find form ~equal:String.equal name in
    let client =
      match Server.Request.header request "authorization" with
      | Some basic ->
        let decoded =
          Base64.decode_exn (String.chop_prefix_exn basic ~prefix:"Basic ")
        in
        sprintf "basic %s" decoded
      | None ->
        sprintf
          "client_id=%s%s"
          (Option.value (param "client_id") ~default:"-")
          (Option.value_map (param "client_secret") ~default:"" ~f:(fun s ->
             " client_secret=" ^ s))
    in
    match param "grant_type" with
    | Some "authorization_code" ->
      let verified =
        match param "code_verifier", t.challenge with
        | Some verifier, Some challenge ->
          String.equal (Pkce.challenge_of_verifier verifier) challenge
        | _ -> false
      in
      Queue.enqueue
        t.log
        (sprintf
           "token: authorization_code code=%s verifier %s redirect_uri=%s \
            resource=%s %s"
           (Option.value (param "code") ~default:"-")
           (if verified then "matches" else "DOES NOT MATCH")
           (Option.value (param "redirect_uri") ~default:"-")
           (Option.value (param "resource") ~default:"-")
           client);
      if verified && Option.equal String.equal (param "code") (Some "code-1")
      then json 200 (issue t ~refresh:"refresh-1")
      else
        json
          400
          (`Object
              [ "error", `String "invalid_grant"
              ; "error_description", `String "bad code or verifier"
              ])
    | Some "refresh_token" ->
      Queue.enqueue
        t.log
        (sprintf
           "token: refresh_token %s resource=%s %s"
           (Option.value (param "refresh_token") ~default:"-")
           (Option.value (param "resource") ~default:"-")
           client);
      if t.refresh_ok
      then json 200 (issue t ~refresh:"refresh-2")
      else json 400 (`Object [ "error", `String "invalid_grant" ])
    | _ -> json 400 (`Object [ "error", `String "unsupported_grant_type" ])
  ;;

  let handler t (request : Server.Request.t) =
    let meth, path =
      match String.split request.request_line ~on:' ' with
      | meth :: target :: _ -> meth, List.hd_exn (String.split target ~on:'?')
      | _ -> "?", "?"
    in
    let prm =
      `Object
        [ "resource", `String (t.base ^ "/mcp")
        ; "authorization_servers", `Array [ `String (t.base ^ "/as") ]
        ; "scopes_supported", `Array [ `String "mcp:all" ]
        ]
    in
    let reply : Server.Reply.t =
      match meth, path, t.resource_metadata, t.metadata with
      | "POST", "/mcp", _, _ -> mcp t request
      | "GET", "/meta/prm", `Header, _ -> json 200 prm
      | "GET", "/.well-known/oauth-protected-resource/mcp", `Well_known, _ ->
        json 200 prm
      | "GET", "/.well-known/oauth-authorization-server/as", _, `Oauth ->
        json 200 (as_metadata t)
      | "GET", "/.well-known/openid-configuration/as", _, `Oidc ->
        json 200 (as_metadata t)
      | "POST", "/as/register", _, _ ->
        Queue.enqueue t.log ("register: " ^ request.body);
        json
          201
          (`Object
              [ "client_id", `String "client-1"
              ; "token_endpoint_auth_method", `String "none"
              ])
      | "POST", "/as/token", _, _ -> token t request
      | "POST", "/as/revoke", _, _ ->
        Queue.enqueue
          t.log
          ("revoke: "
           ^ Option.value
               (List.Assoc.find (form request.body) ~equal:String.equal "token")
               ~default:"-");
        Server.Reply.simple 200 ""
      | _ -> not_found
    in
    if not (String.equal path "/mcp")
    then Queue.enqueue t.log (sprintf "%s %s -> %d" meth path reply.status);
    reply
  ;;

  let start ~sw ~env =
    let t = create () in
    let server = Server.start ~sw ~env ~handler:(handler t) in
    t.base <- Server.url server "";
    t
  ;;

  let print_log t ~mask =
    Queue.iter t.log ~f:(fun line -> print_endline (mask line));
    Queue.clear t.log
  ;;
end

let masker (fake : Fake.t) s =
  String.substr_replace_all s ~pattern:fake.base ~with_:"$BASE"
;;

let url_params url =
  List.map
    (Uri.query (Uri.of_string url))
    ~f:(fun (k, vs) -> k, String.concat ~sep:"," vs)
;;

(* Follows the authorization URL as a browser would, to the point where the
   authorization server sends it back with [code-1]. *)
let authorize (fake : Fake.t) url =
  let params = url_params url in
  let param name = List.Assoc.find_exn params ~equal:String.equal name in
  fake.challenge <- Some (param "code_challenge");
  List.map params ~f:(fun (k, v) ->
    ( k
    , match k with
      | "state" -> "<state>"
      | "code_challenge" -> "<challenge>"
      | _ -> masker fake v ))
  |> List.iter ~f:(fun (k, v) -> printf "  %s=%s\n" k v);
  param "state"
;;

let mask_ports s =
  Re.replace_string
    (Re.compile (Re.Perl.re "http://127\\.0\\.0\\.1:[0-9]+"))
    ~by:"$BASE"
    s
;;

let print_page (response : Static_site.Response.t) =
  let text =
    Re.replace_string (Re.compile (Re.Perl.re "<[^>]*>")) ~by:" " response.body
  in
  let lines =
    String.split_lines text
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun l ->
      (not (String.is_empty l)) && not (String.is_prefix l ~prefix:"body{"))
  in
  printf "%d %s\n" response.status (mask_ports (String.concat ~sep:" | " lines))
;;

(* ---- the protocol --------------------------------------------------------- *)

let%expect_test "WWW-Authenticate challenges" =
  List.iter
    [ {|Bearer resource_metadata="https://x.example/.well-known/oauth-protected-resource", scope="a b"|}
    ; {|Basic realm="x", Bearer error="invalid_token", error_description="The \"token\" expired"|}
    ; {|bearer resource_metadata=https://x.example/m|}
    ; {|Basic realm="x"|}
    ; {|Bearerish x=1|}
    ]
    ~f:(fun header ->
      print_s
        [%sexp
          (Mcp_oauth.Challenge.parse header : Mcp_oauth.Challenge.t option)]);
  [%expect
    {|
    (((resource_metadata
       (https://x.example/.well-known/oauth-protected-resource))
      (scope ("a b")) (error ())))
    (((resource_metadata ()) (scope ()) (error (invalid_token))))
    (((resource_metadata (https://x.example/m)) (scope ()) (error ())))
    ()
    ()
    |}]
;;

let%expect_test "where metadata is looked for" =
  let show urls = List.iter urls ~f:print_endline in
  show
    (Mcp_oauth.resource_metadata_urls
       ~url:"https://mcp.example.com/v1/mcp"
       None);
  show
    (Mcp_oauth.resource_metadata_urls
       ~url:"https://mcp.example.com/"
       (Mcp_oauth.Challenge.parse
          {|Bearer resource_metadata="https://m.example/prm"|}));
  show (Mcp_oauth.server_metadata_urls ~issuer:"https://auth.example.com");
  show
    (Mcp_oauth.server_metadata_urls ~issuer:"https://auth.example.com/tenant1/");
  [%expect
    {|
    https://mcp.example.com/.well-known/oauth-protected-resource/v1/mcp
    https://mcp.example.com/.well-known/oauth-protected-resource
    https://m.example/prm
    https://mcp.example.com/.well-known/oauth-protected-resource
    https://auth.example.com/.well-known/oauth-authorization-server
    https://auth.example.com/.well-known/openid-configuration
    https://auth.example.com/.well-known/oauth-authorization-server/tenant1
    https://auth.example.com/.well-known/openid-configuration/tenant1
    https://auth.example.com/tenant1/.well-known/openid-configuration
    |}]
;;

let%expect_test
    "discovery: from the challenge, the well-known URIs, OpenID, none"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let mask = masker fake in
  let url = fake.base ^ "/mcp" in
  let discover () =
    let challenge = Mcp_oauth.probe ~env:t.env ~url in
    (match Mcp_oauth.discover ~env:t.env ~url ?challenge () with
     | Ok d ->
       print_endline
         (mask (Sexp.to_string_hum [%sexp (d : Mcp_oauth.Discovery.t)]))
     | Error e -> print_endline (mask (Error.to_string_hum e)));
    Fake.print_log fake ~mask
  in
  discover ();
  [%expect
    {|
    ((resource $BASE/mcp)
     (resource_metadata ($BASE/meta/prm))
     (server
      ((issuer $BASE/as)
       (authorization_endpoint $BASE/as/authorize)
       (token_endpoint $BASE/as/token)
       (registration_endpoint ($BASE/as/register))
       (revocation_endpoint ($BASE/as/revoke))
       (scopes_supported ())))
     (scopes (mcp:read mcp:write)))
    GET /meta/prm -> 200
    GET /.well-known/oauth-authorization-server/as -> 200
    |}];
  fake.resource_metadata <- `Well_known;
  fake.metadata <- `Oidc;
  discover ();
  [%expect
    {|
    ((resource $BASE/mcp)
     (resource_metadata
      ($BASE/.well-known/oauth-protected-resource/mcp))
     (server
      ((issuer $BASE/as)
       (authorization_endpoint $BASE/as/authorize)
       (token_endpoint $BASE/as/token)
       (registration_endpoint ($BASE/as/register))
       (revocation_endpoint ($BASE/as/revoke))
       (scopes_supported ())))
     (scopes (mcp:all)))
    GET /.well-known/oauth-protected-resource/mcp -> 200
    GET /.well-known/oauth-authorization-server/as -> 404
    GET /.well-known/openid-configuration/as -> 200
    |}];
  fake.resource_metadata <- `None;
  fake.metadata <- `None;
  discover ();
  [%expect
    {|
    found no OAuth metadata for $BASE/mcp (tried $BASE/.well-known/oauth-protected-resource/mcp, $BASE/.well-known/oauth-protected-resource, $BASE/.well-known/oauth-authorization-server, $BASE/.well-known/openid-configuration)
    GET /.well-known/oauth-protected-resource/mcp -> 404
    GET /.well-known/oauth-protected-resource -> 404
    GET /.well-known/oauth-authorization-server -> 404
    GET /.well-known/openid-configuration -> 404
    |}]
;;

(* ---- the client: 401 means a sign-in, tokens are sent ------------------- *)

let http_server ?(headers = []) ?oauth url ~dir =
  { Mcp_config.Server.name = "remote"
  ; source = Filename.concat dir ".mcp.json"
  ; project = false
  ; dir
  ; transport = Http { url; headers; oauth }
  ; approval = ""
  }
;;

let%expect_test "client: 401 from a server with OAuth wants a sign-in" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let mask s = masker fake (Tool_test_helpers.mask t s) in
  let url = fake.base ^ "/mcp" in
  let connect ?headers ?bearer () =
    match
      Mcp_client.connect
        ~env:t.env
        ~sw
        ?bearer
        (http_server ?headers url ~dir:t.dir)
    with
    | Ok client ->
      (match Mcp_client.tools client with
       | Ok tools ->
         List.iter tools ~f:(fun (tool : Mcp_client.Tool.t) ->
           print_endline ("tool " ^ tool.name ^ ": " ^ tool.description))
       | Error e -> print_endline (Error.to_string_hum e));
      Mcp_client.close client
    | Error e ->
      print_s
        (Sexp.of_string
           (mask (Sexp.to_string [%sexp (e : Mcp_client.Connect_error.t)])))
  in
  connect ();
  [%expect
    {|
    ((message "$BASE/mcp needs you to sign in (HTTP 401)") (needs_sign_in true))
    |}];
  Hash_set.add fake.valid "good";
  connect ~bearer:(fun () -> Some "good") ();
  [%expect {| tool whoami: as good |}];
  connect ~bearer:(fun () -> Some "stale") ();
  [%expect
    {|
    ((message "$BASE/mcp refused the sign-in (HTTP 401): sign in again")
     (needs_sign_in true))
    |}];
  (* An explicit header wins over the sign-in. *)
  connect
    ~headers:[ "authorization", "Bearer good" ]
    ~bearer:(fun () -> Some "stale")
    ();
  [%expect {| tool whoami: as good |}];
  (* Without OAuth metadata, the hint is still the header. *)
  fake.resource_metadata <- `None;
  fake.metadata <- `None;
  connect ();
  [%expect
    {|
    ((message
      "HTTP 401 from $BASE/mcp: unauthorized; if it needs credentials, add an Authorization header to the server's \"headers\" in $DIR/.mcp.json")
     (needs_sign_in false))
    |}];
  Queue.clear fake.log
;;

(* ---- signing in -------------------------------------------------------- *)

let auth_setup t ~sw ?(listen = fun ~sw:_ -> failwith "no loopback") () =
  let now = ref 1_000_000 in
  let path = Filename.concat t.dir "auth.json" in
  let store = Auth_store.create ~path in
  let auth =
    Mcp_auth.create ~env:t.env ~sw ~store ~listen ~now_ms:(fun () -> !now) ()
  in
  Mcp_auth.subscribe auth ~f:(fun () -> print_endline "(sign-ins changed)");
  now, path, auth
;;

let print_store ~fake path =
  let json = Or_error.ok_exn (Json.parse (In_channel.read_all path)) in
  print_endline (masker fake (Json.to_string json));
  printf "mode %o\n" (Core_unix.stat path).st_perm
;;

let%expect_test "sign in: registration, PKCE, state, the callback, tokens" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let mask = masker fake in
  let url = fake.base ^ "/mcp" in
  let now, path, auth = auth_setup t ~sw () in
  let sign_in () =
    Or_error.ok_exn
      (Mcp_auth.sign_in
         auth
         ~url
         ~oauth:None
         ~redirect_base:(Some "https://prigh.example/"))
  in
  let link = sign_in () in
  print_endline (mask (List.hd_exn (String.split link ~on:'?')));
  let state = authorize fake link in
  Fake.print_log fake ~mask;
  [%expect
    {|
    $BASE/as/authorize
      response_type=code
      client_id=client-1
      redirect_uri=https://prigh.example/mcp/oauth/callback
      code_challenge=<challenge>
      code_challenge_method=S256
      state=<state>
      resource=$BASE/mcp
      scope=mcp:read mcp:write
    GET /meta/prm -> 200
    GET /.well-known/oauth-authorization-server/as -> 200
    register: {"client_name":"prigh","redirect_uris":["https://prigh.example/mcp/oauth/callback"],"grant_types":["authorization_code","refresh_token"],"response_types":["code"],"token_endpoint_auth_method":"none"}
    POST /as/register -> 201
    |}];
  print_s [%sexp (Mcp_auth.signed_in auth ~url : bool)];
  [%expect {| false |}];
  (* A wrong state is refused and the sign-in keeps waiting. *)
  print_page (Mcp_auth.callback [ "code", "code-1"; "state", "forged" ]);
  [%expect
    {| 400 Unknown sign-in | Unknown sign-in  This sign-in is unknown or has expired. Start the sign-in again from /mcp or Settings → MCP servers in prigh. |}];
  print_page (Mcp_auth.callback [ "code", "code-1"; "state", state ]);
  Fake.print_log fake ~mask;
  [%expect
    {|
    (sign-ins changed)
    200 Signed in | Signed in  prigh is signed in to $BASE/mcp and reconnects to it. You can close this tab.
    token: authorization_code code=code-1 verifier matches redirect_uri=https://prigh.example/mcp/oauth/callback resource=$BASE/mcp client_id=client-1
    POST /as/token -> 200
    |}];
  (* The state is used up. *)
  print_page (Mcp_auth.callback [ "code", "code-1"; "state", state ]);
  [%expect
    {| 400 Unknown sign-in | Unknown sign-in  This sign-in is unknown or has expired. Start the sign-in again from /mcp or Settings → MCP servers in prigh. |}];
  print_s [%sexp (Mcp_auth.signed_in auth ~url : bool)];
  print_store ~fake path;
  print_endline (mask (Json.to_string (Mcp_auth.tokens auth)));
  [%expect
    {|
    true
    {"mcp:$BASE/mcp":{"token":{"access":"access-1","refresh":"refresh-1","expires":4600000},"client":{"client_id":"client-1","auth_method":"none"},"token_endpoint":"$BASE/as/token","revocation_endpoint":"$BASE/as/revoke","resource":"$BASE/mcp"}}
    mode 600
    {"$BASE/mcp":"access-1"}
    |}];
  (* Close to expiry, the token is refreshed before it is sent. *)
  now := 4_570_000;
  print_endline (mask (Json.to_string (Mcp_auth.tokens auth)));
  Fake.print_log fake ~mask;
  [%expect
    {|
    {"$BASE/mcp":"access-2"}
    token: refresh_token refresh-1 resource=$BASE/mcp client_id=client-1
    POST /as/token -> 200
    |}];
  (* A rejected token is refreshed once; a token that changed since is just
     tried again. *)
  print_s [%sexp (Mcp_auth.renew auth ~url ~rejected:"access-1" : bool)];
  Fake.print_log fake ~mask;
  [%expect {| true |}];
  print_s [%sexp (Mcp_auth.renew auth ~url ~rejected:"access-2" : bool)];
  Fake.print_log fake ~mask;
  [%expect
    {|
    true
    token: refresh_token refresh-2 resource=$BASE/mcp client_id=client-1
    POST /as/token -> 200
    |}];
  (* When the refresh fails, the sign-in is gone. *)
  fake.refresh_ok <- false;
  print_s [%sexp (Mcp_auth.renew auth ~url ~rejected:"access-3" : bool)];
  print_s [%sexp (Mcp_auth.signed_in auth ~url : bool)];
  Fake.print_log fake ~mask;
  [%expect
    {|
    (sign-ins changed)
    false
    false
    token: refresh_token refresh-2 resource=$BASE/mcp client_id=client-1
    POST /as/token -> 400
    |}];
  (* Signing in again reuses the registered client; the authorization
     server's error comes back on the page. *)
  let state = authorize fake (sign_in ()) in
  ignore ([%expect.output] : string);
  Fake.print_log fake ~mask;
  [%expect
    {|
    GET /meta/prm -> 200
    GET /.well-known/oauth-authorization-server/as -> 200
    |}];
  print_page
    (Mcp_auth.callback
       [ "error", "access_denied"
       ; "error_description", "The user said no"
       ; "state", state
       ]);
  [%expect
    {| 400 Sign-in failed | Sign-in failed  $BASE/as refused the sign-in: access_denied (The user said no). Start the sign-in again from /mcp or Settings → MCP servers in prigh. |}]
  |> ignore;
  (* A code the server does not take. *)
  let state = authorize fake (sign_in ()) in
  ignore ([%expect.output] : string);
  Queue.clear fake.log;
  print_page (Mcp_auth.callback [ "code", "stolen"; "state", state ]);
  Fake.print_log fake ~mask;
  [%expect
    {|
    400 Sign-in failed | Sign-in failed  the token exchange failed: HTTP 400: invalid_grant (bad code or verifier) Start the sign-in again from /mcp or Settings → MCP servers in prigh.
    token: authorization_code code=stolen verifier matches redirect_uri=https://prigh.example/mcp/oauth/callback resource=$BASE/mcp client_id=client-1
    POST /as/token -> 400
    |}]
;;

let%expect_test "sign out revokes; a configured client, no registration" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let mask = masker fake in
  let url = fake.base ^ "/mcp" in
  fake.registration <- false;
  let _now, path, auth = auth_setup t ~sw () in
  (match
     Mcp_auth.sign_in
       auth
       ~url
       ~oauth:None
       ~redirect_base:(Some "https://p.example")
   with
   | Ok _ -> print_endline "started"
   | Error e -> print_endline (mask (Error.to_string_hum e)));
  Queue.clear fake.log;
  [%expect
    {| $BASE/as does not offer dynamic client registration: register an OAuth client there and give its id in the server's "oauth": {"clientId": ...} |}];
  let oauth =
    { Mcp_config.Oauth.client_id = "my-app"
    ; client_secret = Some "s3cret"
    ; scopes = Some [ "tools" ]
    }
  in
  let link =
    Or_error.ok_exn
      (Mcp_auth.sign_in
         auth
         ~url
         ~oauth:(Some oauth)
         ~redirect_base:(Some "https://p.example"))
  in
  let state = authorize fake link in
  Queue.clear fake.log;
  print_page (Mcp_auth.callback [ "code", "code-1"; "state", state ]);
  Fake.print_log fake ~mask;
  [%expect
    {|
      response_type=code
      client_id=my-app
      redirect_uri=https://p.example/mcp/oauth/callback
      code_challenge=<challenge>
      code_challenge_method=S256
      state=<state>
      resource=$BASE/mcp
      scope=tools
    (sign-ins changed)
    200 Signed in | Signed in  prigh is signed in to $BASE/mcp and reconnects to it. You can close this tab.
    token: authorization_code code=code-1 verifier matches redirect_uri=https://p.example/mcp/oauth/callback resource=$BASE/mcp basic my-app:s3cret
    POST /as/token -> 200
    |}];
  Or_error.ok_exn (Mcp_auth.sign_out auth ~url);
  Fake.print_log fake ~mask;
  print_s [%sexp (Mcp_auth.signed_in auth ~url : bool)];
  print_store ~fake path;
  [%expect
    {|
    (sign-ins changed)
    revoke: refresh-1
    POST /as/revoke -> 200
    revoke: access-1
    POST /as/revoke -> 200
    false
    {}
    mode 600
    |}];
  print_endline
    (mask_ports
       (Sexp.to_string [%sexp (Mcp_auth.sign_out auth ~url : unit Or_error.t)]));
  [%expect {| (Error"not signed in to $BASE/mcp") |}]
;;

let%expect_test "sign in from a terminal: a one-shot loopback listener" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let url = fake.base ^ "/mcp" in
  let _now, _path, auth =
    auth_setup t ~sw ~listen:(fun ~sw -> Mcp_callback.listen ~env:t.env ~sw) ()
  in
  let link =
    Or_error.ok_exn (Mcp_auth.sign_in auth ~url ~oauth:None ~redirect_base:None)
  in
  let redirect_uri =
    List.Assoc.find_exn (url_params link) ~equal:String.equal "redirect_uri"
  in
  let state = authorize fake link in
  ignore ([%expect.output] : string);
  print_endline
    (Re.replace_string
       (Re.compile (Re.Perl.re ":[0-9]+/"))
       ~by:":PORT/"
       redirect_uri);
  let response, body =
    Or_error.ok_exn
      (Result.map_error
         (Http_client.get
            ~env:t.env
            ~url:(sprintf "%s?code=code-1&state=%s" redirect_uri state)
            ~headers:[]
            ())
         ~f:(fun e -> Error.of_string (Http_client.Error.to_string e)))
  in
  print_page (Static_site.Response.text ~status:response.status ~reason:"" body);
  print_s [%sexp (Mcp_auth.signed_in auth ~url : bool)];
  [%expect
    {|
    http://127.0.0.1:PORT/mcp/oauth/callback
    (sign-ins changed)
    200 Signed in | Signed in  prigh is signed in to $BASE/mcp and reconnects to it. You can close this tab.
    true
    |}]
;;

(* ---- the hub and the RPC ------------------------------------------------ *)

let%expect_test "hub: a new token reconnects, a lost one disconnects" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let url = fake.base ^ "/mcp" in
  let mask s = masker fake (Tool_test_helpers.mask t s) in
  let hub = Mcp_hub.create ~env:t.env ~sw () in
  let settings = `Object [ "remote", `Object [ "url", `String url ] ] in
  let list ?tokens () =
    let statuses, _ =
      Mcp_hub.servers hub ~settings ?tokens ~cwd:t.dir ~home:t.dir ()
    in
    List.iter statuses ~f:(fun { server; status } ->
      print_endline
        (mask
           (Sexp.to_string_hum
              [%sexp (server.name : string), (status : Mcp_hub.Status.t)])))
  in
  list ();
  [%expect
    {|
    (remote
     (Needs_sign_in "$BASE/mcp needs you to sign in (HTTP 401)"))
    |}];
  (* The same tokens: the failure is remembered. *)
  list ~tokens:[] ();
  [%expect
    {|
    (remote
     (Needs_sign_in "$BASE/mcp needs you to sign in (HTTP 401)"))
    |}];
  Hash_set.add fake.valid "access-9";
  list ~tokens:[ url, "access-9" ] ();
  [%expect
    {|
    (remote
     (Ready
      (((name whoami) (description "as access-9")
        (input_schema (Object ((type (String object))))) (read_only false)))))
    |}];
  list ~tokens:[] ();
  [%expect
    {|
    (remote
     (Needs_sign_in "$BASE/mcp needs you to sign in (HTTP 401)"))
    |}];
  Mcp_hub.close hub
;;

let%expect_test "RPC: list_mcp, mcp_signin, the callback, mcp_signout" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.start ~sw ~env:t.env in
  let mask s = masker fake (Tool_test_helpers.mask t s) in
  let home = Filename.concat t.dir "home" in
  Core_unix.mkdir_p home;
  let store = Auth_store.create ~path:(Filename.concat home "auth.json") in
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv:(fun _ -> None) ~store ()
  in
  let mcp = Mcp_hub.create ~env:t.env ~sw () in
  let mcp_settings = Mcp_settings.create ~home in
  ignore
    (Mcp_settings.add
       mcp_settings
       [ "remote", `Object [ "url", `String (fake.base ^ "/mcp") ] ]
     : _ Or_error.t);
  let mcp_auth = Mcp_auth.create ~env:t.env ~sw ~store () in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat home "sessions")
      ~home
      ~mcp
      ~mcp_settings
      ~mcp_auth
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home
      ~login
      ~mcp_settings
      ~mcp_auth
      ~sessions_dir:(Filename.concat home "sessions")
      ~cwd:home
      ~new_agent
      ()
  in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "mcp_auth_changed") ->
        print_endline "event mcp_auth_changed"
      | _ -> ())
  in
  let call meth params =
    let response =
      Rpc_server.handle
        server
        client
        (`Object [ "id", `Number "1"; "method", `String meth; "params", params ])
    in
    match Json.member "ok" response, Json.member "result" response with
    | Some `True, Some result -> result
    | _ -> Option.value_exn (Json.member "error" response)
  in
  let print json = print_endline (mask (Json.to_string json)) in
  ignore (call "hello" (`Object []) : Json.t);
  print (call "list_mcp" (`Object []));
  [%expect
    {| {"servers":[{"name":"remote","source":"settings","project":false,"status":"needs_sign_in","error":"$BASE/mcp needs you to sign in (HTTP 401)","url":"$BASE/mcp","signed_in":false,"tools":[]}],"problems":[]} |}];
  print
    (call
       "mcp_signin"
       (`Object [ "source", `String "nowhere"; "server", `String "remote" ]));
  [%expect
    {| "there is no MCP server \"remote\" from nowhere; /mcp lists them" |}];
  let link =
    call
      "mcp_signin"
      (`Object
          [ "source", `String "settings"
          ; "server", `String "remote"
          ; "redirect_base", `String "https://phone.example"
          ])
  in
  let link =
    Option.value_exn (Option.bind (Json.member "url" link) ~f:Json.string)
  in
  let state = authorize fake link in
  ignore ([%expect.output] : string);
  Queue.clear fake.log;
  print_page (Mcp_auth.callback [ "code", "code-1"; "state", state ]);
  [%expect
    {|
    event mcp_auth_changed
    200 Signed in | Signed in  prigh is signed in to $BASE/mcp and reconnects to it. You can close this tab.
    |}];
  print (call "list_mcp" (`Object []));
  [%expect
    {| {"servers":[{"name":"remote","source":"settings","project":false,"status":"ready","url":"$BASE/mcp","signed_in":true,"tools":[{"name":"mcp__remote__whoami","description":"as access-1"}]}],"problems":[]} |}];
  print
    (call
       "mcp_signout"
       (`Object [ "source", `String "settings"; "server", `String "remote" ]));
  [%expect
    {|
    event mcp_auth_changed
    {"servers":[{"name":"remote","source":"settings","project":false,"status":"needs_sign_in","error":"$BASE/mcp needs you to sign in (HTTP 401)","url":"$BASE/mcp","signed_in":false,"tools":[]}],"problems":[]}
    |}];
  Mcp_hub.close mcp
;;
