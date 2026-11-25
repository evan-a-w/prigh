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
         "ChatGPT credential has no account id; log in again"
     | Some account_id ->
       Ok
         (Openai_responses.create
            ~env
            ?timeout
            ~endpoint:(Codex { access_token = auth.token; account_id })
            ()))
  | Custom _ -> assert false
;;

let create
      ~env
      ?timeout
      ?getenv
      ?(models = Model_registry.builtin ())
      ?member
      ?ledger
      ~store
      ()
  =
  let stream (request : Provider.Request.t) ~cancel ~on_event =
    let fail message =
      Assistant_builder.finish
        (Assistant_builder.create ~model:(Model.key request.model))
        ~stop_reason:(Error message)
        ~usage:Usage.zero
    in
    let provider = request.model.provider in
    let shared ~owner id =
      match member with
      | None ->
        Or_error.errorf
          "%s's login %S is shared, and this server has no users to share \
           with: /accounts picks another"
          owner
          (Account_id.to_string id)
      | Some { Account_shares.Member.shares; user } ->
        Or_error.map
          (Account_shares.resolve
             shares
             ~env
             ~cancel
             ~recipient:user
             provider
             ~owner
             id)
          ~f:(fun r -> Some (r, Some (Account_ref.Shared { owner; id })))
    in
    let own account =
      Or_error.map
        (Provider_auth.resolve ~env ~cancel ?getenv ?account store provider)
        ~f:
          (Option.map ~f:(fun (r : Provider_auth.Resolved.t) ->
             r, Option.map r.account ~f:(fun id -> Account_ref.Own id)))
    in
    let resolve () =
      match request.account with
      | Some (Own id) -> own (Some id)
      | Some (Shared { owner; id }) -> shared ~owner id
      | None ->
        (match own None with
         | Ok None when not (Provider_id.is_custom provider) ->
           (* No login of their own: the first one shared with them. *)
           (match
              Option.bind member ~f:(fun member ->
                List.find_map
                  (Result.ok (Login_choice.list ~member store provider)
                   |> Option.value ~default:[])
                  ~f:(fun c ->
                    match c.ref_ with
                    | Shared { owner; id } when c.default -> Some (owner, id)
                    | _ -> None))
            with
            | Some (owner, id) -> shared ~owner id
            | None -> Ok None)
         | result -> result)
    in
    let stream_as (p : Provider.t) auth =
      let account =
        Option.bind auth ~f:(fun (_, (account : Account_ref.t option)) ->
          account)
      in
      let message = p.stream { request with account } ~cancel ~on_event in
      (match account with
       | Some account ->
         Account_shares.record_request
           ?member
           ?ledger
           provider
           account
           message.usage
           ~cost_usd:(Model.cost_usd request.model message.usage)
       | _ -> ());
      { message with
        account =
          Option.bind account ~f:(fun account ->
            Option.some_if
              (not (Account_ref.equal account Account_ref.main))
              (Account_ref.to_string account))
      }
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
              Option.map auth ~f:(fun ((r : Provider_auth.Resolved.t), _) ->
                r.token)
            in
            let message =
              stream_as (custom_provider ~env ?timeout p ~key) auth
            in
            (match message.stop_reason with
             | Error e
               when List.exists [ "HTTP 401"; "HTTP 403" ] ~f:(fun prefix ->
                      String.is_prefix e ~prefix) ->
               let hint =
                 match key with
                 | Some _ ->
                   sprintf
                     "the server refused the API key: /login %s changes it"
                     name
                 | None ->
                   sprintf
                     "the server wants an API key: /login %s adds one, or set \
                      %s"
                     name
                     (Custom_provider.env_var name)
               in
               { message with stop_reason = Error (sprintf "%s (%s)" e hint) }
             | _ -> message)))
    | Anthropic | Openai | Openai_codex | Deepseek ->
      (match resolve () with
       | Error e -> fail ("auth: " ^ Error.to_string_hum e)
       | Ok None -> fail (not_configured_message provider)
       | Ok (Some ((resolved, _) as auth)) ->
         (match builtin_provider ~env ?timeout provider resolved with
          | Error e -> fail (Error.to_string_hum e)
          | Ok p -> stream_as p (Some auth)))
  in
  { Provider.name = "router"; stream }
;;
