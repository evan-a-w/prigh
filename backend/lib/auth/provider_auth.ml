open! Core
open! Import
module Method = Login_method

let methods : Provider_id.t -> Method.t list = function
  | Anthropic -> [ Oauth; Api_key ]
  | Openai -> [ Api_key ]
  | Openai_codex -> [ Oauth ]
  | Deepseek | Custom _ -> [ Api_key ]
;;

let env_vars : Provider_id.t -> string list = function
  | Anthropic -> [ "ANTHROPIC_OAUTH_TOKEN"; "ANTHROPIC_API_KEY" ]
  | Openai -> [ "OPENAI_API_KEY" ]
  | Openai_codex -> []
  | Deepseek -> [ "DEEPSEEK_API_KEY" ]
  | Custom name -> [ Custom_provider.env_var name ]
;;

module Resolved = struct
  type t =
    { token : string
    ; method_ : Method.t
    ; account_id : string option
    ; source : string
    ; account : Account_id.t option
    }
  [@@deriving sexp_of]
end

module Account_status = struct
  type t =
    { id : Account_id.t
    ; label : string
    ; method_ : Method.t
    ; source : string
    ; expires_ms : int option
    ; default : bool
    ; fallback : bool
    ; shared_with : Account_meta.Grant.t list
    }
  [@@deriving sexp_of]
end

module Status = struct
  type t =
    { provider : Provider_id.t
    ; methods : Method.t list
    ; configured : (Method.t * string) option
    ; expires_ms : int option
    ; custom : Custom_provider.t option
    ; accounts : Account_status.t list
    }
  [@@deriving sexp_of]
end

let env_value ~getenv provider =
  List.find_map (env_vars provider) ~f:(fun name ->
    match getenv name with
    | Some v when not (String.is_empty v) -> Some (name, v)
    | _ -> None)
;;

let refresh_oauth ~env ~cancel provider ~refresh_token =
  match (provider : Provider_id.t) with
  | Anthropic -> Oauth_anthropic.refresh ~env ~cancel ~refresh_token ()
  | Openai_codex -> Oauth_openai_codex.refresh ~env ~cancel ~refresh_token ()
  | Openai | Deepseek | Custom _ ->
    Or_error.error_s
      [%message "provider has no OAuth support" (provider : Provider_id.t)]
;;

(* Double-checked: the cheap read decides whether to take the lock; the
   authoritative expiry check runs under it, so a concurrent refresh (this
   process or another) is observed instead of repeated. *)
let fresh_oauth ~refresh store provider id (stored : Credential.Oauth.t) =
  if not (Credential.oauth_needs_refresh stored)
  then Ok stored
  else
    Or_error.bind
      (Provider_accounts.modify store provider id ~f:(function
         | Some (Oauth current) when Credential.oauth_needs_refresh current ->
           Or_error.map
             (refresh provider ~refresh_token:current.refresh)
             ~f:(fun c -> Some (Credential.Oauth c))
         | other -> Ok other))
      ~f:(function
        | Some (Oauth c) -> Ok c
        | Some (Api_key _) | None ->
          Or_error.error_s
            [%message
              "credential changed during refresh; log in again"
                (provider : Provider_id.t)])
;;

let account_gone provider id accounts =
  Or_error.errorf
    "%s's account %S is not logged in (any more): /login %s logs it in, or \
     pick another (%s)"
    (Provider_id.display_name provider)
    (Account_id.to_string id)
    (Provider_id.to_string provider)
    (match accounts with
     | [] -> "it has none"
     | accounts ->
       String.concat
         ~sep:", "
         (List.map accounts ~f:(fun (a : Provider_accounts.Account.t) ->
            Account_id.to_string a.id)))
;;

let resolve
      ~env
      ?(cancel = Cancellation.never)
      ?(getenv = Sys.getenv)
      ?(refresh = refresh_oauth ~env ~cancel)
      ?account
      store
      provider
  =
  let open Or_error.Let_syntax in
  let%bind accounts = Provider_accounts.list store provider in
  let%bind chosen =
    match account with
    | None -> Ok (List.find accounts ~f:(fun a -> a.default))
    | Some id ->
      (match
         List.find accounts ~f:(fun (a : Provider_accounts.Account.t) ->
           Account_id.equal a.id id)
       with
       | Some a -> Ok (Some a)
       | None -> account_gone provider id accounts)
  in
  match chosen with
  | Some { credential = Api_key token; id; _ } ->
    Ok
      (Some
         { Resolved.token
         ; method_ = Api_key
         ; account_id = None
         ; source = "stored api key"
         ; account = Some id
         })
  | Some { credential = Oauth _; _ } when Provider_id.is_custom provider ->
    Or_error.errorf
      "auth.json's %S entry is an OAuth login, not an API key (another tool \
       may use that name): /logout %s, then /login %s"
      (Provider_id.to_string provider)
      (Provider_id.to_string provider)
      (Provider_id.to_string provider)
  | Some { credential = Oauth stored; id; _ } ->
    let%map c = fresh_oauth ~refresh store provider id stored in
    Some
      { Resolved.token = c.access
      ; method_ = Oauth
      ; account_id = c.account_id
      ; source = "oauth"
      ; account = Some id
      }
  | None ->
    Ok
      (Option.map (env_value ~getenv provider) ~f:(fun (name, token) ->
         { Resolved.token
         ; method_ = Api_key
         ; account_id = None
         ; source = name
         ; account = None
         }))
;;

let account_status ~oauth_source (a : Provider_accounts.Account.t) =
  { Account_status.id = a.id
  ; label = a.label
  ; method_ = Method.of_credential a.credential
  ; source =
      (match a.credential with
       | Api_key _ -> "stored api key"
       | Oauth _ -> oauth_source)
  ; expires_ms =
      (match a.credential with
       | Oauth o -> Some o.expires_ms
       | Api_key _ -> None)
  ; default = a.default
  ; fallback = a.fallback
  ; shared_with = a.shared_with
  }
;;

let status ?(getenv = Sys.getenv) ?(custom = []) store =
  let open Or_error.Let_syntax in
  let from_env provider =
    Option.map (env_value ~getenv provider) ~f:(fun (name, _) ->
      Method.Api_key, name)
  in
  let%bind builtins =
    (* The first error only: a broken file would repeat it per provider. *)
    Result.all
      (List.map Provider_id.builtins ~f:(fun provider ->
         let%map accounts = Provider_accounts.list store provider in
         let accounts =
           List.map accounts ~f:(account_status ~oauth_source:"oauth")
         in
         let configured, expires_ms =
           match List.find accounts ~f:(fun a -> a.default) with
           | Some a -> Some (a.method_, a.source), a.expires_ms
           | None -> from_env provider, None
         in
         { Status.provider
         ; methods = methods provider
         ; configured
         ; expires_ms
         ; custom = None
         ; accounts
         }))
  in
  let customs =
    List.map custom ~f:(fun (c : Custom_provider.t) ->
      let provider = Custom_provider.provider_id c in
      let accounts =
        (* A bad entry must not hide the provider from [/auth]. *)
        match Provider_accounts.list store provider with
        | Ok accounts ->
          List.map
            accounts
            ~f:(account_status ~oauth_source:"oauth (not usable)")
        | Error _ -> []
      in
      let configured =
        match List.find accounts ~f:(fun a -> a.default) with
        | Some a -> Some (a.method_, a.source)
        | None ->
          Some
            (Option.value
               (from_env provider)
               ~default:(Method.Api_key, "no key"))
      in
      { Status.provider
      ; methods = methods provider
      ; configured
      ; expires_ms = None
      ; custom = Some c
      ; accounts
      })
  in
  Ok (builtins @ customs)
;;

let login_api_key provider (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    Or_error.bind
      (interaction.prompt
         (Secret
            { message = sprintf "Enter %s" (Method.label provider Api_key)
            ; allow_empty = false
            }))
      ~f:(fun key ->
        let key = String.strip key in
        if String.is_empty key
        then Or_error.error_string "empty API key"
        else Ok (Credential.Api_key key)))
;;

let login ~env ?account ?label store provider (method_ : Method.t) interaction =
  let open Or_error.Let_syntax in
  let%bind credential =
    if not (List.mem (methods provider) method_ ~equal:Method.equal)
    then
      Or_error.error_s
        [%message
          "login method not supported by provider"
            (provider : Provider_id.t)
            (method_ : Method.t)]
    else (
      match method_, provider with
      | Api_key, _ -> login_api_key provider interaction
      | Oauth, Anthropic -> Oauth_anthropic.login ~env interaction
      | Oauth, Openai_codex -> Oauth_openai_codex.login ~env interaction
      | Oauth, (Openai | Deepseek | Custom _) -> assert false)
  in
  Provider_accounts.save_login ?account ?label store provider credential
;;

let logout ?account store provider =
  let open Or_error.Let_syntax in
  let%bind id =
    match account with
    | Some id -> Ok (Some id)
    | None ->
      Provider_accounts.default store provider
      >>| Option.map ~f:(fun (a : Provider_accounts.Account.t) -> a.id)
  in
  match id with
  | None -> Ok ()
  | Some id -> Provider_accounts.remove store provider id
;;
