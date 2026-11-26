open! Core
open! Import

module Window = struct
  type t =
    { id : string
    ; label : string
    ; used_percent : float
    ; resets_at : float option
    }
  [@@deriving sexp_of, equal]
end

module Status = struct
  type t =
    | Not_signed_in
    | Api_key
    | Limits of
        { plan : string option
        ; windows : Window.t list
        }
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Account_usage = struct
  type t =
    { id : Account_id.t
    ; shared_by : string option
    ; label : string
    ; default : bool
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

module Provider_usage = struct
  type t =
    { provider : Provider_id.t
    ; status : Status.t
    ; accounts : Account_usage.t list
    }
  [@@deriving sexp_of, equal]
end

type t =
  { fetched_at : float
  ; providers : Provider_usage.t list
  }
[@@deriving sexp_of, equal]

let providers : Provider_id.t list = [ Anthropic; Openai_codex ]

let name : Provider_id.t -> string = function
  | Anthropic -> "Claude"
  | p -> Provider_id.display_name p
;;

let ms seconds =
  `Number (Int.to_string (Float.iround_nearest_exn (seconds *. 1000.)))
;;

let window_json (w : Window.t) =
  `Object
    [ "id", `String w.id
    ; "label", `String w.label
    ; ( "used_percent"
      , `Number (Float.to_string_hum ~strip_zero:true w.used_percent) )
    ; "resets_at_ms", Option.value_map w.resets_at ~default:`Null ~f:ms
    ]
;;

let status_fields : Status.t -> (string * Json.t) list = function
  | Not_signed_in -> [ "status", `String "not_signed_in" ]
  | Api_key -> [ "status", `String "api_key" ]
  | Limits { plan; windows } ->
    [ "status", `String "limits"
    ; "plan", Option.value_map plan ~default:`Null ~f:(fun p -> `String p)
    ; "windows", `Array (List.map windows ~f:window_json)
    ]
  | Failed error -> [ "status", `String "error"; "error", `String error ]
;;

module Harness_usage = struct
  type t =
    { adapter : string
    ; name : string
    ; status : Status.t
    }
end

let to_json ?(harnesses = []) t =
  `Object
    [ "fetched_at_ms", ms t.fetched_at
    ; ( "providers"
      , `Array
          (List.map harnesses ~f:(fun (h : Harness_usage.t) ->
             `Object
               ([ "provider", `String h.adapter; "name", `String h.name ]
                @ status_fields h.status))
           @ List.map t.providers ~f:(fun (p : Provider_usage.t) ->
             `Object
               ([ "provider", `String (Provider_id.to_string p.provider)
                ; "name", `String (name p.provider)
                ]
                @ status_fields p.status
                @ [ ( "accounts"
                    , `Array
                        (List.map p.accounts ~f:(fun (a : Account_usage.t) ->
                           `Object
                             ([ ( "ref"
                                , `String
                                    (match a.shared_by with
                                     | None ->
                                       sprintf
                                         "%s/%s"
                                         (Provider_id.to_string p.provider)
                                         (Account_id.to_string a.id)
                                     | Some owner ->
                                       sprintf
                                         "~%s/%s/%s"
                                         owner
                                         (Provider_id.to_string p.provider)
                                         (Account_id.to_string a.id)) )
                              ; ( "id"
                                , `String
                                    (match a.shared_by with
                                     | None -> Account_id.to_string a.id
                                     | Some owner ->
                                       sprintf
                                         "~%s/%s"
                                         owner
                                         (Account_id.to_string a.id)) )
                              ; "label", `String a.label
                              ; ("default", if a.default then `True else `False)
                              ]
                              @ Option.value_map
                                  a.shared_by
                                  ~default:[]
                                  ~f:(fun owner ->
                                    [ "shared_by", `String owner ])
                              @ status_fields a.status))) )
                  ]))) )
    ]
;;

module Http = struct
  type t =
    url:string
    -> headers:(string * string) list
    -> unit
    -> (int * (string * string) list * string, string) Result.t

  let real ~env ~url ~headers () =
    match
      Http_client.get
        ~env
        ~timeout:(Time_ns.Span.of_int_sec 20)
        ~url
        ~headers
        ()
    with
    | Ok (response, body) -> Ok (response.status, response.headers, body)
    | Error e -> Error (Http_client.Error.to_string e)
  ;;
end

(* Parsing: anything unexpected is skipped rather than failing the rest. *)

let number : Json.t -> float option = function
  | `Number n -> Float.of_string_opt n
  | `String s -> Float.of_string_opt (String.strip s)
  | _ -> None
;;

let field json name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | None | Some `Null -> None
     | Some v -> Some v)
  | _ -> None
;;

let number_field json name = Option.bind (field json name) ~f:number

let string_field json name =
  match field json name with
  | Some (`String s) when not (String.is_empty (String.strip s)) -> Some s
  | _ -> None
;;

(* Seconds since the epoch, or ISO 8601. *)
let time : Json.t -> float option = function
  | `Number _ as n -> number n
  | `String s ->
    (match Float.of_string_opt (String.strip s) with
     | Some f -> Some f
     | None ->
       Option.try_with (fun () ->
         Time_float.of_string_with_utc_offset (String.strip s)
         |> Time_float.to_span_since_epoch
         |> Time_float.Span.to_sec))
  | _ -> None
;;

(* [seven_day_oauth_apps] is "Seven day oauth apps"; names that are not
   snake case (e.g. a model's) are kept. *)
let humanize name =
  if
    String.for_all name ~f:(fun c ->
      Char.is_lowercase c || Char.is_digit c || Char.equal c '_')
  then
    String.capitalize
      (String.concat
         ~sep:" "
         (List.filter (String.split name ~on:'_') ~f:(Fn.non String.is_empty)))
  else name
;;

let parse_json body =
  match Json.parse body with
  | Ok (`Object _ as json) -> Ok json
  | Ok _ -> Error "not a JSON object"
  | Error e -> Error (Error.to_string_hum e)
;;

let anthropic_known =
  [ "five_hour", "5-hour session"
  ; "seven_day", "Weekly · all models"
  ; "seven_day_opus", "Weekly · Opus"
  ; "seven_day_sonnet", "Weekly · Sonnet"
  ; "seven_day_oauth_apps", "Weekly · OAuth apps"
  ]
;;

(* [limits]: the server's rows, in its order. *)
let anthropic_limit row =
  let open Option.Let_syntax in
  let%bind kind = string_field row "kind" in
  let%map used_percent =
    match number_field row "percent" with
    | Some p -> Some p
    | None -> number_field row "utilization"
  in
  let scope =
    let%bind scope = field row "scope" in
    List.find_map [ "model"; "surface" ] ~f:(fun key ->
      let%bind thing = field scope key in
      string_field thing "display_name")
  in
  let base =
    match kind with
    | "session" -> "Session"
    | "weekly_all" -> "Weekly · all models"
    | "weekly_scoped" -> "Weekly"
    | kind -> humanize kind
  in
  let label, id =
    match scope with
    | None -> base, kind
    | Some scope -> sprintf "%s · %s" base scope, sprintf "%s:%s" kind scope
  in
  { Window.id
  ; label
  ; used_percent
  ; resets_at = Option.bind (field row "resets_at") ~f:time
  }
;;

(* A named window: [{utilization, resets_at}]. [null] or without a
   [utilization] it does not apply to this plan. *)
let anthropic_window (key, value) =
  match key with
  | "limits" -> None
  | _ ->
    Option.map (number_field value "utilization") ~f:(fun used_percent ->
      { Window.id = key
      ; label =
          Option.value
            (List.Assoc.find anthropic_known ~equal:String.equal key)
            ~default:(humanize key)
      ; used_percent
      ; resets_at = Option.bind (field value "resets_at") ~f:time
      })
;;

let parse_anthropic body =
  match parse_json body with
  | Error e ->
    Status.Failed
      (sprintf
         "Claude's usage endpoint answered something unexpected (%s): try \
          Refresh later"
         e)
  | Ok json ->
    let limits =
      match field json "limits" with
      | Some (`Array rows) -> List.filter_map rows ~f:anthropic_limit
      | _ -> []
    in
    let windows =
      if not (List.is_empty limits)
      then limits
      else (
        let named =
          match json with
          | `Object fields -> List.filter_map fields ~f:anthropic_window
          | _ -> []
        in
        let rank (w : Window.t) =
          List.findi anthropic_known ~f:(fun _ (k, _) -> String.equal k w.id)
          |> Option.value_map ~default:Int.max_value ~f:fst
        in
        List.stable_sort named ~compare:(fun a b ->
          Int.compare (rank a) (rank b)))
    in
    Limits { plan = None; windows }
;;

let duration_label seconds =
  if seconds = 5 * 3600
  then "5-hour"
  else if seconds = 7 * 86400
  then "Weekly"
  else if seconds % 86400 = 0
  then sprintf "%d-day" (seconds / 86400)
  else if seconds % 3600 = 0
  then sprintf "%d-hour" (seconds / 3600)
  else sprintf "%d-minute" ((seconds + 59) / 60)
;;

let codex_window ~now ~id ~prefix ~fallback json =
  let open Option.Let_syntax in
  let%map used_percent = number_field json "used_percent" in
  let seconds =
    Option.bind (number_field json "limit_window_seconds") ~f:(fun s ->
      Option.some_if Float.(s > 0.) (Float.iround_nearest_exn s))
  in
  let what =
    match seconds with
    | None -> fallback
    | Some s when s = 5 * 3600 -> "5-hour session"
    | Some s ->
      (match duration_label s with
       | "Weekly" -> "Weekly"
       | d -> d ^ " window")
  in
  let resets_at =
    match number_field json "reset_at" with
    | Some at when Float.(at > 0.) -> Some at
    | _ ->
      Option.map (number_field json "reset_after_seconds") ~f:(fun s ->
        now +. s)
  in
  { Window.id
  ; label =
      (match prefix with
       | None -> what
       | Some p -> sprintf "%s · %s" p what)
  ; used_percent
  ; resets_at
  }
;;

let codex_windows ~now ~id_prefix ~prefix rate_limit =
  List.filter_map
    [ "primary_window", "primary", "Short window"
    ; "secondary_window", "secondary", "Long window"
    ]
    ~f:(fun (key, id, fallback) ->
      Option.bind (field rate_limit key) ~f:(fun w ->
        codex_window ~now ~id:(id_prefix ^ id) ~prefix ~fallback w))
;;

let parse_codex ~now body =
  match parse_json body with
  | Error e ->
    Status.Failed
      (sprintf
         "ChatGPT's usage endpoint answered something unexpected (%s): try \
          Refresh later"
         e)
  | Ok json ->
    let main =
      Option.value_map
        (field json "rate_limit")
        ~default:[]
        ~f:(codex_windows ~now ~id_prefix:"" ~prefix:None)
    in
    let additional =
      match field json "additional_rate_limits" with
      | Some (`Array limits) ->
        List.concat_map limits ~f:(fun limit ->
          let name =
            match string_field limit "limit_name" with
            | Some n -> n
            | None ->
              Option.value
                (string_field limit "metered_feature")
                ~default:"other"
          in
          Option.value_map
            (field limit "rate_limit")
            ~default:[]
            ~f:
              (codex_windows
                 ~now
                 ~id_prefix:(name ^ ".")
                 ~prefix:(Some (humanize name))))
      | _ -> []
    in
    Limits { plan = string_field json "plan_type"; windows = main @ additional }
;;

let anthropic_origin = "https://api.anthropic.com"
let codex_origin = "https://chatgpt.com"

(* [PRIGH_USAGE_ORIGIN] stands in for both providers' (prigh-web's e2e). *)
let url ?getenv origin path =
  let getenv = Option.value getenv ~default:Sys.getenv in
  Option.value (getenv "PRIGH_USAGE_ORIGIN") ~default:origin ^ path
;;

let retry_after headers =
  List.find_map headers ~f:(fun (k, v) ->
    if String.Caseless.equal k "retry-after"
    then Float.of_string_opt (String.strip v)
    else None)
;;

(* The error's own message, when the body has one. *)
let body_message body =
  match Json.parse body with
  | Ok json ->
    let error = Option.value (field json "error") ~default:json in
    (match string_field error "message" with
     | Some m -> Some m
     | None -> string_field json "detail")
  | Error _ -> None
;;

let answer provider ~parse result =
  let who = name provider in
  let login = "/login " ^ Provider_id.to_string provider in
  match result with
  | Error e ->
    Status.Failed
      (sprintf
         "Could not reach %s's usage endpoint (%s): check the backend's \
          network and Refresh"
         who
         e)
  | Ok (200, _, body) -> parse body
  | Ok (status, headers, body) ->
    let detail =
      Option.value_map (body_message body) ~default:"" ~f:(sprintf ": %s")
    in
    (match status with
     | 401 | 403 ->
       Failed
         (sprintf
            "%s did not accept the sign-in (HTTP %d%s): sign in again with %s"
            who
            status
            detail
            login)
     | 429 ->
       Failed
         (sprintf
            "%s is limiting how often usage is read (HTTP 429): Refresh %s"
            who
            (match retry_after headers with
             | Some s when Float.(s > 0.) ->
               sprintf "in %d s" (Float.iround_up_exn s)
             | _ -> "in a minute"))
     | _ ->
       Failed
         (sprintf
            "%s's usage endpoint answered HTTP %d%s: Refresh later"
            who
            status
            detail))
;;

let of_resolved ?getenv ~(http : Http.t) ~now provider resolved =
  let login = "/login " ^ Provider_id.to_string provider in
  match resolved with
  | Error e ->
    Status.Failed
      (sprintf "%s: sign in again with %s" (Error.to_string_hum e) login)
  | Ok None -> Not_signed_in
  | Ok (Some (resolved : Provider_auth.Resolved.t)) ->
    (match (provider : Provider_id.t) with
     | Anthropic ->
       (match
          Anthropic.auth_of_token ~method_:resolved.method_ resolved.token
        with
        | Api_key _ | Gateway _ -> Api_key
        | Oauth token ->
          answer
            provider
            ~parse:parse_anthropic
            (http
               ~url:(url ?getenv anthropic_origin "/api/oauth/usage")
               ~headers:
                 [ "Authorization", "Bearer " ^ token
                 ; "anthropic-beta", "oauth-2025-04-20"
                 ; "Content-Type", "application/json"
                 ; "User-Agent", "claude-code/" ^ Anthropic.claude_code_version
                 ]
               ()))
     | Openai_codex ->
       (match resolved.method_, resolved.account_id with
        | Api_key, _ -> Api_key
        | Oauth, None ->
          Failed
            (sprintf
               "The ChatGPT sign-in has no account id: sign in again with %s"
               login)
        | Oauth, Some account_id ->
          answer
            provider
            ~parse:(parse_codex ~now:(now ()))
            (http
               ~url:(url ?getenv codex_origin "/backend-api/wham/usage")
               ~headers:
                 [ "Authorization", "Bearer " ^ resolved.token
                 ; "chatgpt-account-id", account_id
                 ; "originator", "prigh"
                 ; "User-Agent", "prigh/" ^ Version.to_string
                 ]
               ()))
     | Google_antigravity -> Not_signed_in
     | Openai | Deepseek | Custom _ -> Not_signed_in)
;;

module Shared = struct
  type t =
    { owner : string
    ; provider : Provider_id.t
    ; id : Account_id.t
    ; label : string
    ; resolve : unit -> Provider_auth.Resolved.t option Or_error.t
    }
end

let fetch ~env ?getenv ?refresh ?(shared = []) ~http ~now store =
  let providers =
    Fiber.List.map
      (fun provider ->
         let fetch ?account () =
           of_resolved
             ?getenv
             ~http
             ~now
             provider
             (Provider_auth.resolve
                ~env
                ?getenv
                ?refresh
                ?account
                store
                provider)
         in
         let shared =
           Fiber.List.map
             (fun (s : Shared.t) ->
                { Account_usage.id = s.id
                ; shared_by = Some s.owner
                ; label = s.label
                ; default = false
                ; status =
                    (match s.resolve () with
                     | Error e -> Failed (Error.to_string_hum e)
                     | resolved ->
                       of_resolved ?getenv ~http ~now provider resolved)
                })
             (List.filter shared ~f:(fun (s : Shared.t) ->
                Provider_id.equal s.provider provider))
         in
         match Provider_accounts.list store provider with
         | Error _ | Ok [] ->
           (match shared with
            | [] ->
              { Provider_usage.provider; status = fetch (); accounts = [] }
            | first :: rest ->
              { provider
              ; status = first.status
              ; accounts = { first with default = true } :: rest
              })
         | Ok accounts ->
           let accounts =
             Fiber.List.map
               (fun (a : Provider_accounts.Account.t) ->
                  { Account_usage.id = a.id
                  ; shared_by = None
                  ; label = a.label
                  ; default = a.default
                  ; status = fetch ~account:a.id ()
                  })
               accounts
           in
           let status =
             match List.find accounts ~f:(fun a -> a.default) with
             | Some a -> a.status
             | None -> fetch ()
           in
           { provider; status; accounts = accounts @ shared })
      providers
  in
  { fetched_at = now (); providers }
;;
