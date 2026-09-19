open! Core
open! Import

let not_configured_message provider =
  let env_hint =
    match Provider_auth.env_vars provider with
    | [] -> ""
    | vars -> sprintf " or set %s" (String.concat ~sep:"/" vars)
  in
  sprintf
    "not logged in to %s: use /login %s%s"
    (Provider_id.display_name provider)
    (Provider_id.to_string provider)
    env_hint
;;

let provider_for
      ~env
      ?timeout
      (provider : Provider_id.t)
      (auth : Provider_auth.Resolved.t)
  : Provider.t Or_error.t
  =
  match provider with
  | Deepseek -> Ok (Deepseek.create ~env ?timeout ~api_key:auth.token ())
  | Anthropic ->
    Ok
      (Anthropic.create
         ~env
         ?timeout
         ~auth:(Anthropic.auth_of_token ~method_:auth.method_ auth.token)
         ())
  | Openai ->
    Ok
      (Openai_responses.create
         ~env
         ?timeout
         ~endpoint:(Openai { api_key = auth.token })
         ())
  | Openai_codex ->
    (match auth.account_id with
     | None ->
       Or_error.error_string
         "OpenAI Codex credential has no account id; log in again"
     | Some account_id ->
       Ok
         (Openai_responses.create
            ~env
            ?timeout
            ~endpoint:(Codex { access_token = auth.token; account_id })
            ()))
;;

let create ~env ?timeout ?getenv ~store () =
  let stream (request : Provider.Request.t) ~cancel ~on_event =
    let fail message =
      Assistant_builder.finish
        (Assistant_builder.create ~model:(Model.key request.model))
        ~stop_reason:(Error message)
        ~usage:Usage.zero
    in
    let provider = request.model.provider in
    match Provider_auth.resolve ~env ~cancel ?getenv store provider with
    | Error e -> fail ("auth: " ^ Error.to_string_hum e)
    | Ok None -> fail (not_configured_message provider)
    | Ok (Some auth) ->
      (match provider_for ~env ?timeout provider auth with
       | Error e -> fail (Error.to_string_hum e)
       | Ok p -> p.stream request ~cancel ~on_event)
  in
  { Provider.name = "router"; stream }
;;
