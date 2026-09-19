open! Core
open! Import

module Method = struct
  type t =
    | Api_key
    | Oauth
  [@@deriving sexp, equal, enumerate]

  let to_string = function
    | Api_key -> "api_key"
    | Oauth -> "oauth"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

  let label provider t =
    match provider, t with
    | Provider_id.Anthropic, Oauth -> "Anthropic (Claude Pro/Max)"
    | Anthropic, Api_key -> "Anthropic API key"
    | Openai, Api_key -> "OpenAI API key"
    | Openai, Oauth -> "OpenAI OAuth"
    | Openai_codex, Oauth -> "OpenAI (ChatGPT Plus/Pro)"
    | Openai_codex, Api_key -> "OpenAI Codex API key"
    | Deepseek, Api_key -> "DeepSeek API key"
    | Deepseek, Oauth -> "DeepSeek OAuth"
  ;;
end

let methods : Provider_id.t -> Method.t list = function
  | Anthropic -> [ Oauth; Api_key ]
  | Openai -> [ Api_key ]
  | Openai_codex -> [ Oauth ]
  | Deepseek -> [ Api_key ]
;;

let env_vars : Provider_id.t -> string list = function
  | Anthropic -> [ "ANTHROPIC_OAUTH_TOKEN"; "ANTHROPIC_API_KEY" ]
  | Openai -> [ "OPENAI_API_KEY" ]
  | Openai_codex -> []
  | Deepseek -> [ "DEEPSEEK_API_KEY" ]
;;

module Resolved = struct
  type t =
    { token : string
    ; method_ : Method.t
    ; account_id : string option
    ; source : string
    }
  [@@deriving sexp_of]
end

module Status = struct
  type t =
    { provider : Provider_id.t
    ; methods : Method.t list
    ; configured : (Method.t * string) option
    ; expires_ms : int option
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
  | Openai | Deepseek ->
    Or_error.error_s
      [%message "provider has no OAuth support" (provider : Provider_id.t)]
;;

(* Double-checked: the cheap read decides whether to take the lock; the
   authoritative expiry check runs under it, so a concurrent refresh (this
   process or another) is observed instead of repeated. *)
let fresh_oauth ~refresh store provider (stored : Credential.Oauth.t) =
  if not (Credential.oauth_needs_refresh stored)
  then Ok stored
  else
    Or_error.bind
      (Auth_store.modify store provider ~f:(function
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

let resolve
      ~env
      ?(cancel = Cancellation.never)
      ?(getenv = Sys.getenv)
      ?(refresh = refresh_oauth ~env ~cancel)
      store
      provider
  =
  let open Or_error.Let_syntax in
  match%bind Auth_store.read store provider with
  | Some (Api_key token) ->
    Ok
      (Some
         { Resolved.token
         ; method_ = Api_key
         ; account_id = None
         ; source = "stored api key"
         })
  | Some (Oauth stored) ->
    let%map c = fresh_oauth ~refresh store provider stored in
    Some
      { Resolved.token = c.access
      ; method_ = Oauth
      ; account_id = c.account_id
      ; source = "oauth"
      }
  | None ->
    Ok
      (Option.map (env_value ~getenv provider) ~f:(fun (name, token) ->
         { Resolved.token; method_ = Api_key; account_id = None; source = name }))
;;

let status ?(getenv = Sys.getenv) store =
  Or_error.map (Auth_store.list store) ~f:(fun stored ->
    List.map Provider_id.all ~f:(fun provider ->
      let configured, expires_ms =
        match List.Assoc.find stored ~equal:Provider_id.equal provider with
        | Some (Oauth o) -> Some (Method.Oauth, "oauth"), Some o.expires_ms
        | Some (Api_key _) -> Some (Method.Api_key, "stored api key"), None
        | None ->
          ( Option.map (env_value ~getenv provider) ~f:(fun (name, _) ->
              Method.Api_key, name)
          , None )
      in
      { Status.provider; methods = methods provider; configured; expires_ms }))
;;

let login_api_key provider (interaction : Auth_interaction.t) =
  Auth_interaction.run interaction ~f:(fun () ->
    Or_error.bind
      (interaction.prompt
         (Secret
            { message = sprintf "Enter %s" (Method.label provider Api_key) }))
      ~f:(fun key ->
        let key = String.strip key in
        if String.is_empty key
        then Or_error.error_string "empty API key"
        else Ok (Credential.Api_key key)))
;;

let login ~env store provider (method_ : Method.t) interaction =
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
      | Oauth, (Openai | Deepseek) -> assert false)
  in
  Auth_store.set store provider credential
;;

let logout store provider = Auth_store.remove store provider
