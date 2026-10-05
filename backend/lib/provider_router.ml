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

let custom_provider ~env ?timeout (p : Custom_provider.t) ~key : Provider.t =
  let bearer =
    Option.value_map key ~default:[] ~f:(fun key ->
      [ "Authorization", "Bearer " ^ key ])
  in
  match p.api with
  | Chat ->
    Openai_chat.create
      ~env
      ?timeout
      ~name:p.name
      ~url:(p.base_url ^ "/chat/completions")
      ~headers:(bearer @ p.headers)
      ~quirks:Openai_chat.Quirks.generic
      ()
  | Responses ->
    Openai_responses.create
      ~env
      ~base_url:p.base_url
      ?timeout
      ~endpoint:
        (Custom
           { provider = Custom_provider.provider_id p
           ; api_key = key
           ; headers = p.headers
           })
      ()
  | Anthropic ->
    Anthropic.create
      ~env
      ~base_url:p.base_url
      ~path:"/messages"
      ~extra_headers:p.headers
      ?timeout
      ~auth:(Gateway key)
      ()
;;

let builtin_provider
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
  | Custom _ -> assert false
;;

let create ~env ?timeout ?getenv ?(models = Model_registry.builtin ()) ~store ()
  =
  let stream (request : Provider.Request.t) ~cancel ~on_event =
    let fail message =
      Assistant_builder.finish
        (Assistant_builder.create ~model:(Model.key request.model))
        ~stop_reason:(Error message)
        ~usage:Usage.zero
    in
    let provider = request.model.provider in
    let resolve () =
      Provider_auth.resolve ~env ~cancel ?getenv store provider
    in
    match provider with
    | Custom name ->
      (match Model_registry.find_provider models name with
       | None ->
         fail
           (sprintf
              "custom provider %s is not configured: add it with /login custom \
               (or check providers.%s in config.json)"
              name
              name)
       | Some p ->
         (match resolve () with
          | Error e -> fail ("auth: " ^ Error.to_string_hum e)
          | Ok auth ->
            let key =
              Option.map auth ~f:(fun (r : Provider_auth.Resolved.t) -> r.token)
            in
            (custom_provider ~env ?timeout p ~key).stream
              request
              ~cancel
              ~on_event))
    | Anthropic | Openai | Openai_codex | Deepseek ->
      (match resolve () with
       | Error e -> fail ("auth: " ^ Error.to_string_hum e)
       | Ok None -> fail (not_configured_message provider)
       | Ok (Some auth) ->
         (match builtin_provider ~env ?timeout provider auth with
          | Error e -> fail (Error.to_string_hum e)
          | Ok p -> p.stream request ~cancel ~on_event))
  in
  { Provider.name = "router"; stream }
;;
