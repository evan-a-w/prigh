open! Core
open! Import

let callback_path = "/mcp/oauth/callback"
let key ~url = "mcp:" ^ url
let flow_lifetime_ms = 10 * 60 * 1000

module Grant = struct
  type t =
    { token : Mcp_oauth.Token.t
    ; client : Mcp_oauth.Client.t
    ; token_endpoint : string
    ; revocation_endpoint : string option
    ; resource : string
    }

  let to_json t =
    `Object
      [ "token", Mcp_oauth.Token.to_json t.token
      ; "client", Mcp_oauth.Client.to_json t.client
      ; "token_endpoint", `String t.token_endpoint
      ; ( "revocation_endpoint"
        , Option.value_map t.revocation_endpoint ~default:`Null ~f:(fun s ->
            `String s) )
      ; "resource", `String t.resource
      ]
  ;;

  let of_json json =
    let open Or_error.Let_syntax in
    let member name =
      match Json.member name json with
      | Some v -> Ok v
      | None -> Or_error.errorf "no %s" name
    in
    let string name =
      match Option.bind (Json.member name json) ~f:Json.string with
      | Some s -> Ok s
      | None -> Or_error.errorf "no %s" name
    in
    let%bind token = member "token" >>= Mcp_oauth.Token.of_json in
    let%bind client = member "client" >>= Mcp_oauth.Client.of_json in
    let%bind token_endpoint = string "token_endpoint" in
    let%map resource = string "resource" in
    { token
    ; client
    ; token_endpoint
    ; revocation_endpoint =
        Option.bind (Json.member "revocation_endpoint" json) ~f:Json.string
    ; resource
    }
  ;;
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; store : Auth_store.t
  ; now_ms : unit -> int
  ; listen : (sw:Switch.t -> int) option
  ; clients : Mcp_oauth.Client.t String.Table.t
    (** registered clients, by issuer, redirect URI and configured name *)
  ; mutable subscribers : (unit -> unit) list
  }

module Pending = struct
  type nonrec t =
    { auth : t
    ; url : string
    ; discovery : Mcp_oauth.Discovery.t
    ; client : Mcp_oauth.Client.t
    ; verifier : string
    ; redirect_uri : string
    ; started_ms : int
    ; finished : unit -> unit
    }
end

(* Every world's sign-ins, by state: the callback route is the web
   server's, shared by all of them. *)
let pending : Pending.t String.Table.t = String.Table.create ()
let web_base = ref None

let set_web_base base =
  web_base := Some (String.rstrip ~drop:(Char.equal '/') base)
;;

let create ~env ~sw ~store ?listen ?(now_ms = Credential.now_ms) () =
  { env
  ; sw
  ; store
  ; now_ms
  ; listen
  ; clients = String.Table.create ()
  ; subscribers = []
  }
;;

let subscribe t ~f = t.subscribers <- t.subscribers @ [ f ]
let notify t = List.iter t.subscribers ~f:(fun f -> f ())

let read t ~url =
  match Auth_store.read_entry t.store (key ~url) with
  | Ok (Some json) -> Result.ok (Grant.of_json json)
  | Ok None | Error _ -> None
;;

let signed_in t ~url = Option.is_some (read t ~url)

let remove t ~url =
  ignore
    (Auth_store.modify_entry t.store (key ~url) ~f:(fun _ -> Ok None)
     : Json.t option Or_error.t)
;;

(* Refreshes the grant under the store's lock, when [needed] says so of
   what is on disk; [None] when it is gone or cannot be refreshed. *)
let refresh t ~url ~needed =
  let result =
    Auth_store.modify_entry t.store (key ~url) ~f:(fun json ->
      match Option.map json ~f:Grant.of_json with
      | None | Some (Error _) -> Ok None
      | Some (Ok grant) when not (needed grant) -> Ok json
      | Some (Ok grant) ->
        (match grant.token.refresh with
         | None -> Ok None
         | Some refresh ->
           (match
              Mcp_oauth.refresh
                ~env:t.env
                ~now_ms:(t.now_ms ())
                ~token_endpoint:grant.token_endpoint
                grant.client
                ~refresh
                ~resource:grant.resource
            with
            | Ok token -> Ok (Some (Grant.to_json { grant with token }))
            | Error _ -> Ok None)))
  in
  match result with
  | Ok (Some json) -> Result.ok (Grant.of_json json)
  | Ok None | Error _ -> None
;;

let tokens t =
  let entries =
    Auth_store.entries t.store ~prefix:"mcp:"
    |> Result.ok
    |> Option.value ~default:[]
  in
  let dropped = ref false in
  let tokens =
    List.filter_map entries ~f:(fun (k, json) ->
      let url = String.chop_prefix_exn k ~prefix:"mcp:" in
      match Grant.of_json json with
      | Error _ -> None
      | Ok grant ->
        let now_ms = t.now_ms () in
        let grant =
          if Mcp_oauth.Token.expiring grant.token ~now_ms
          then (
            let refreshed =
              refresh t ~url ~needed:(fun grant ->
                Mcp_oauth.Token.expiring grant.token ~now_ms)
            in
            if Option.is_none refreshed then dropped := true;
            refreshed)
          else Some grant
        in
        Option.map grant ~f:(fun grant -> url, `String grant.token.access))
  in
  if !dropped then notify t;
  `Object tokens
;;

let renew t ~url ~rejected =
  match read t ~url with
  | None -> false
  | Some grant when not (String.equal grant.token.access rejected) -> true
  | Some _ ->
    (match
       refresh t ~url ~needed:(fun grant ->
         String.equal grant.token.access rejected)
     with
     | Some _ -> true
     | None ->
       notify t;
       false)
;;

let sign_out t ~url =
  match read t ~url with
  | None -> Or_error.errorf "not signed in to %s" url
  | Some grant ->
    remove t ~url;
    Option.iter grant.revocation_endpoint ~f:(fun revocation_endpoint ->
      List.iter
        (Option.to_list grant.token.refresh @ [ grant.token.access ])
        ~f:(fun token ->
          ignore
            (Mcp_oauth.revoke
               ~env:t.env
               ~revocation_endpoint
               grant.client
               ~token
             : unit Or_error.t)));
    notify t;
    Ok ()
;;

(* ---- the redirect ----------------------------------------------------- *)

let escape s =
  String.concat_map s ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | '\'' -> "&#39;"
    | c -> String.of_char c)
;;

let page ~status ~title text =
  let body =
    sprintf
      {|<!doctype html>
<html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>%s</title>
<style>body{font:16px/1.5 system-ui,sans-serif;max-width:32rem;margin:15vh auto;padding:0 1.5rem;color:#222;background:#faf8f5}h1{font-size:1.4rem}@media (prefers-color-scheme:dark){body{color:#ddd;background:#1c1b1a}}</style>
</head><body><h1>%s</h1><p>%s</p></body></html>
|}
      (escape title)
      (escape title)
      (escape text)
  in
  { (Static_site.Response.text
       ~status
       ~reason:(if status = 200 then "OK" else "Bad Request")
       body)
    with
    headers = [ "Content-Type", "text/html; charset=utf-8" ]
  }
;;

let again =
  "Start the sign-in again from /mcp or Settings → MCP servers in prigh."
;;

let expire now_ms =
  Hashtbl.filteri_inplace pending ~f:(fun ~key:_ ~data:(p : Pending.t) ->
    let alive = now_ms - p.started_ms < flow_lifetime_ms in
    if not alive then p.finished ();
    alive)
;;

let callback query =
  let param name = List.Assoc.find query ~equal:String.equal name in
  match Option.bind (param "state") ~f:(Hashtbl.find_and_remove pending) with
  | None ->
    page
      ~status:400
      ~title:"Unknown sign-in"
      ("This sign-in is unknown or has expired. " ^ again)
  | Some p ->
    Exn.protect ~finally:p.finished ~f:(fun () ->
      let t = p.auth in
      match param "error", param "code" with
      | Some error, _ ->
        let detail =
          match param "error_description" with
          | Some d -> sprintf "%s (%s)" error d
          | None -> error
        in
        page
          ~status:400
          ~title:"Sign-in failed"
          (sprintf
             "%s refused the sign-in: %s. %s"
             p.discovery.server.issuer
             detail
             again)
      | None, None ->
        page
          ~status:400
          ~title:"Sign-in failed"
          ("The redirect had no code. " ^ again)
      | None, Some code ->
        (match
           Mcp_oauth.exchange
             ~env:t.env
             ~now_ms:(t.now_ms ())
             ~token_endpoint:p.discovery.server.token_endpoint
             p.client
             ~code
             ~verifier:p.verifier
             ~redirect_uri:p.redirect_uri
             ~resource:p.discovery.resource
         with
         | Error e ->
           page
             ~status:400
             ~title:"Sign-in failed"
             (sprintf "%s %s" (Error.to_string_hum e) again)
         | Ok token ->
           let grant =
             { Grant.token
             ; client = p.client
             ; token_endpoint = p.discovery.server.token_endpoint
             ; revocation_endpoint = p.discovery.server.revocation_endpoint
             ; resource = p.discovery.resource
             }
           in
           (match
              Auth_store.modify_entry t.store (key ~url:p.url) ~f:(fun _ ->
                Ok (Some (Grant.to_json grant)))
            with
            | Error e ->
              page
                ~status:500
                ~title:"Sign-in failed"
                (sprintf
                   "Could not save the sign-in: %s"
                   (Error.to_string_hum e))
            | Ok _ ->
              notify t;
              page
                ~status:200
                ~title:"Signed in"
                (sprintf
                   "prigh is signed in to %s and reconnects to it. You can \
                    close this tab."
                   p.url))))
;;

(* A listener on 127.0.0.1 for one sign-in, when there is no web server to
   come back to. *)
let loopback t =
  match t.listen with
  | None ->
    Or_error.error_string
      "there is no web server for the sign-in to come back to: sign in from \
       prigh-web, or start the backend with -web"
  | Some listen ->
    let started, set_started = Promise.create () in
    let finished, set_finished = Promise.create () in
    Fiber.fork ~sw:t.sw (fun () ->
      match
        Switch.run (fun sw ->
          let port = listen ~sw in
          Promise.resolve set_started (Ok port);
          ignore
            (Eio.Time.with_timeout
               (Eio.Stdenv.clock t.env)
               (Float.of_int flow_lifetime_ms /. 1000.)
               (fun () -> Ok (Promise.await finished))
             : (unit, [ `Timeout ]) Result.t);
          Switch.fail sw Exit)
      with
      | () -> ()
      | exception Exit -> ()
      | exception exn ->
        if Option.is_none (Promise.peek started)
        then Promise.resolve set_started (Error (Error.of_exn exn)));
    Or_error.map (Promise.await started) ~f:(fun port ->
      ( sprintf "http://127.0.0.1:%d" port
      , fun () ->
          if Option.is_none (Promise.peek finished)
          then Promise.resolve set_finished () ))
;;

let sign_in t ~url ~oauth ~redirect_base =
  let open Or_error.Let_syntax in
  expire (t.now_ms ());
  let client_name =
    Option.bind oauth ~f:(fun (o : Mcp_config.Oauth.t) -> o.client_name)
  in
  let%bind discovery =
    Or_error.tag
      (Mcp_oauth.discover
         ~env:t.env
         ~url
         ?challenge:(Mcp_oauth.probe ~env:t.env ~url ~client_name)
         ())
      ~tag:"cannot sign in"
  in
  let%bind base, finished =
    match Option.first_some redirect_base !web_base with
    | Some base -> Ok (String.rstrip ~drop:(Char.equal '/') base, Fn.id)
    | None -> loopback t
  in
  let redirect_uri = base ^ callback_path in
  let client =
    match (oauth : Mcp_config.Oauth.t option) with
    | Some { client_id = Some client_id; client_secret; _ } ->
      Ok
        { Mcp_oauth.Client.id = client_id
        ; secret = client_secret
        ; auth_method = (if Option.is_some client_secret then Basic else None_)
        }
    | Some { client_id = None; _ } | None ->
      let cache =
        String.concat
          ~sep:" "
          [ discovery.server.issuer
          ; redirect_uri
          ; Option.value client_name ~default:""
          ]
      in
      (match Hashtbl.find t.clients cache with
       | Some client -> Ok client
       | None ->
         let%map client =
           Mcp_oauth.register
             ~env:t.env
             discovery.server
             ~redirect_uri
             ~client_name
         in
         Hashtbl.set t.clients ~key:cache ~data:client;
         client)
  in
  match client with
  | Error e ->
    finished ();
    Error e
  | Ok client ->
    let pkce = Pkce.generate () in
    let state = Pkce.random_hex 16 in
    Hashtbl.set
      pending
      ~key:state
      ~data:
        { Pending.auth = t
        ; url
        ; discovery
        ; client
        ; verifier = pkce.verifier
        ; redirect_uri
        ; started_ms = t.now_ms ()
        ; finished
        };
    Ok
      (Mcp_oauth.authorize_url
         discovery.server
         client
         ~redirect_uri
         ~challenge:pkce.challenge
         ~state
         ~resource:discovery.resource
         ~scopes:
           (match oauth with
            | Some { scopes = Some scopes; _ } -> scopes
            | Some { scopes = None; _ } | None -> discovery.scopes))
;;
