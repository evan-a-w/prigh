open! Core
open! Import

module Api = struct
  type t =
    | Chat
    | Responses
    | Anthropic
  [@@deriving sexp, equal, enumerate]

  let to_string = function
    | Chat -> "chat"
    | Responses -> "responses"
    | Anthropic -> "anthropic"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

  let label = function
    | Chat -> "OpenAI chat completions (/chat/completions)"
    | Responses -> "OpenAI Responses (/responses)"
    | Anthropic -> "Anthropic messages (/messages)"
  ;;
end

module Model_override = struct
  type t =
    { id : string
    ; name : string option
    ; context_window : int option
    ; max_output : int option
    ; thinking : bool option
    ; images : bool option
    ; cost : Model.Cost.t option
    }
  [@@deriving sexp_of]
end

let positive_int json =
  match Json.int json with
  | Some n when n > 0 -> Some n
  | _ -> None
;;

module Listed_model = struct
  type t =
    { id : string
    ; context_window : int option
    }
  [@@deriving sexp_of, equal]

  let of_json json =
    match Json.member "id" json with
    | Some (`String id) when not (String.is_empty id) ->
      let context_window =
        List.find_map
          [ "context_length"; "context_window"; "max_model_len" ]
          ~f:(fun field -> Option.bind (Json.member field json) ~f:positive_int)
      in
      Some { id; context_window }
    | _ -> None
  ;;

  let list_of_json json =
    match Json.member "data" json, Json.member "models" json with
    | Some (`Array items), _ | None, Some (`Array items) ->
      Ok
        (List.filter_map items ~f:of_json
         |> List.stable_dedup ~compare:(fun (a : t) b -> String.compare a.id b.id))
    | _ ->
      Or_error.error_string
        "the response has no \"data\" array of models; is this an \
         OpenAI-compatible /models endpoint?"
  ;;
end

type t =
  { name : string
  ; base_url : string
  ; api : Api.t
  ; headers : (string * string) list
  ; models : Model_override.t list
  }
[@@deriving sexp_of]

let reserved = "custom" :: List.map Provider_id.builtins ~f:Provider_id.to_string

let validate_name raw =
  let name = String.strip raw in
  let valid_char c =
    Char.is_lowercase c || Char.is_digit c || Char.equal c '-' || Char.equal c '_'
  in
  if String.is_empty name
  then Or_error.error_string "the name is empty: type a short name such as aiproxy"
  else if List.mem reserved name ~equal:String.equal
  then
    Or_error.errorf
      "%S is a built-in provider: choose another name (e.g. my-%s)"
      name
      name
  else if not (Char.is_lowercase name.[0] && String.for_all name ~f:valid_char)
  then
    Or_error.errorf
      "%S is not a valid name: use lowercase letters, digits, - and _, starting \
       with a letter (e.g. %s)"
      name
      (let s =
         String.lowercase name
         |> String.map ~f:(fun c -> if valid_char c then c else '-')
         |> String.lstrip ~drop:(Fn.non Char.is_lowercase)
       in
       if String.is_empty s then "aiproxy" else s)
  else Ok name
;;

let endpoint_suffixes =
  [ "/chat/completions"; "/completions"; "/responses"; "/messages"; "/models" ]
;;

let validate_base_url raw =
  let url = String.strip raw |> String.rstrip ~drop:(Char.equal '/') in
  let url =
    List.find_map endpoint_suffixes ~f:(fun suffix ->
      String.chop_suffix url ~suffix)
    |> Option.value ~default:url
    |> String.rstrip ~drop:(Char.equal '/')
  in
  let uri = Uri.of_string url in
  match Uri.scheme uri, Uri.host uri with
  | Some ("http" | "https"), Some host when not (String.is_empty host) ->
    if Option.is_some (Uri.verbatim_query uri) || Option.is_some (Uri.fragment uri)
    then
      Or_error.errorf
        "%S has a query or fragment: give only the base URL, e.g. \
         http://localhost:3000/v1"
        raw
    else Ok url
  | _ ->
    Or_error.errorf
      "%S is not an http(s) URL: type the full base URL, e.g. \
       http://localhost:3000/v1"
      (String.strip raw)
;;

let env_var name =
  String.uppercase (String.map name ~f:(fun c -> if Char.equal c '-' then '_' else c))
  ^ "_API_KEY"
;;

let provider_id t = Provider_id.Custom t.name
let default_context_window = 128_000
let default_max_output = 16_384

let model t ?listed id =
  let o =
    List.find t.models ~f:(fun (o : Model_override.t) -> String.equal o.id id)
  in
  let get f = Option.bind o ~f in
  let context_window =
    match get (fun o -> o.context_window) with
    | Some n -> n
    | None ->
      Option.bind listed ~f:(fun (l : Listed_model.t) -> l.context_window)
      |> Option.value ~default:default_context_window
  in
  { Model.id
  ; provider = provider_id t
  ; name = Option.value (get (fun o -> o.name)) ~default:id
  ; context_window
  ; max_output =
      Option.value
        (get (fun o -> o.max_output))
        ~default:(Int.min default_max_output context_window)
  ; supports_thinking = Option.value (get (fun o -> o.thinking)) ~default:false
  ; thinking_style = Budget
  ; cost =
      Option.value
        (get (fun o -> o.cost))
        ~default:{ Model.Cost.input = 0.; output = 0.; cache_read = 0. }
  ; supports_images = Option.value (get (fun o -> o.images)) ~default:true
  }
;;

let models t ~listed =
  let find id =
    List.find listed ~f:(fun (l : Listed_model.t) -> String.equal l.id id)
  in
  let configured =
    List.map t.models ~f:(fun (o : Model_override.t) ->
      model t ?listed:(find o.id) o.id)
  in
  let others =
    List.filter listed ~f:(fun (l : Listed_model.t) ->
      not
        (List.exists t.models ~f:(fun (o : Model_override.t) ->
           String.equal o.id l.id)))
    |> List.map ~f:(fun (l : Listed_model.t) -> model t ~listed:l l.id)
  in
  configured @ others
;;

(* ---- JSON ---------------------------------------------------------------- *)

let fields_of ~what json =
  match json with
  | `Object fields -> Ok fields
  | _ -> Or_error.errorf "%s must be a JSON object" what
;;

let unknown_fields ~what ~known fields =
  List.filter_map fields ~f:(fun (name, _) ->
    if List.mem known name ~equal:String.equal
    then None
    else
      Some
        (sprintf
           "%s: unknown field %S ignored (known: %s)"
           what
           name
           (String.concat ~sep:", " known)))
;;

let cost_of_json ~what json =
  let open Or_error.Let_syntax in
  let%bind fields = fields_of ~what json in
  let price name ~required =
    match List.Assoc.find fields ~equal:String.equal name with
    | None when not required -> Ok 0.
    | Some json ->
      (match Json.float json with
       | Some f when Float.(f >= 0.) -> Ok f
       | _ ->
         Or_error.errorf "%s.%s must be a non-negative number (USD per million tokens)" what name)
    | None -> Or_error.errorf "%s.%s is missing (USD per million tokens)" what name
  in
  let%bind input = price "input" ~required:true in
  let%bind output = price "output" ~required:true in
  let%map cache_read = price "cache_read" ~required:false in
  { Model.Cost.input; output; cache_read }
;;

let override_known =
  [ "id"; "name"; "context_window"; "max_output"; "thinking"; "images"; "cost" ]
;;

let override_of_json ~what json =
  let open Or_error.Let_syntax in
  let%bind fields = fields_of ~what json in
  let find name = List.Assoc.find fields ~equal:String.equal name in
  let opt name ~f ~expected =
    match find name with
    | None | Some `Null -> Ok None
    | Some json ->
      (match f json with
       | Some v -> Ok (Some v)
       | None -> Or_error.errorf "%s.%s must be %s" what name expected)
  in
  let%bind id =
    match find "id" with
    | Some (`String id) when not (String.is_empty id) -> Ok id
    | _ -> Or_error.errorf "%s needs an \"id\" (the model id the server uses)" what
  in
  let%bind name = opt "name" ~f:Json.string ~expected:"a string" in
  let%bind context_window =
    opt "context_window" ~f:positive_int ~expected:"a positive integer (tokens)"
  in
  let%bind max_output =
    opt "max_output" ~f:positive_int ~expected:"a positive integer (tokens)"
  in
  let%bind thinking = opt "thinking" ~f:Json.bool ~expected:"true or false" in
  let%bind images = opt "images" ~f:Json.bool ~expected:"true or false" in
  let%map cost =
    match find "cost" with
    | None | Some `Null -> Ok None
    | Some json -> Or_error.map (cost_of_json ~what:(what ^ ".cost") json) ~f:Option.some
  in
  ( { Model_override.id; name; context_window; max_output; thinking; images; cost }
  , unknown_fields ~what ~known:override_known fields )
;;

let provider_known = [ "base_url"; "api"; "headers"; "models" ]

let of_json ~name json =
  let what = "providers." ^ name in
  let open Or_error.Let_syntax in
  let%bind name =
    Or_error.tag (validate_name name) ~tag:(what ^ ": bad provider name")
  in
  let%bind fields = fields_of ~what json in
  let find field = List.Assoc.find fields ~equal:String.equal field in
  let%bind base_url =
    match find "base_url" with
    | Some (`String url) ->
      Or_error.tag (validate_base_url url) ~tag:(what ^ ".base_url")
    | _ ->
      Or_error.errorf
        "%s needs a \"base_url\" such as \"http://localhost:3000/v1\""
        what
  in
  let%bind api =
    match find "api" with
    | None | Some `Null -> Ok Api.Chat
    | Some (`String s) when Option.is_some (Api.of_string s) ->
      Ok (Option.value_exn (Api.of_string s))
    | Some _ ->
      Or_error.errorf
        "%s.api must be one of: %s"
        what
        (String.concat ~sep:", " (List.map Api.all ~f:Api.to_string))
  in
  let%bind headers =
    match find "headers" with
    | None | Some `Null -> Ok []
    | Some (`Object headers) ->
      Or_error.all
        (List.map headers ~f:(fun (header, value) ->
           match value with
           | `String v -> Ok (header, v)
           | _ -> Or_error.errorf "%s.headers.%s must be a string" what header))
    | Some _ ->
      Or_error.errorf "%s.headers must be an object of header names to strings" what
  in
  let%map models, warnings =
    match find "models" with
    | None | Some `Null -> Ok ([], [])
    | Some (`Array items) ->
      let results =
        List.mapi items ~f:(fun i item ->
          override_of_json ~what:(sprintf "%s.models[%d]" what i) item)
      in
      Ok
        ( List.filter_map results ~f:(fun r -> Option.map (Result.ok r) ~f:fst)
        , List.concat_map results ~f:(function
            | Ok (_, warnings) -> warnings
            | Error e -> [ Error.to_string_hum e ^ "; that entry is ignored" ]) )
    | Some _ -> Or_error.errorf "%s.models must be an array of model objects" what
  in
  ( { name; base_url; api; headers; models }
  , unknown_fields ~what ~known:provider_known fields @ warnings )
;;

let override_to_json (o : Model_override.t) =
  let opt name f v = Option.value_map v ~default:[] ~f:(fun v -> [ name, f v ]) in
  let int n = `Number (Int.to_string n) in
  let float f = `Number (Float.to_string_hum ~strip_zero:true f) in
  let bool b = if b then `True else `False in
  `Object
    (List.concat
       [ [ "id", `String o.id ]
       ; opt "name" (fun s -> `String s) o.name
       ; opt "context_window" int o.context_window
       ; opt "max_output" int o.max_output
       ; opt "thinking" bool o.thinking
       ; opt "images" bool o.images
       ; opt
           "cost"
           (fun (c : Model.Cost.t) ->
             `Object
               [ "input", float c.input
               ; "output", float c.output
               ; "cache_read", float c.cache_read
               ])
           o.cost
       ])
;;

let to_json t =
  `Object
    (List.concat
       [ [ "base_url", `String t.base_url; "api", `String (Api.to_string t.api) ]
       ; (if List.is_empty t.headers
          then []
          else
            [ "headers", `Object (List.map t.headers ~f:(fun (k, v) -> k, `String v))
            ])
       ; (if List.is_empty t.models
          then []
          else [ "models", `Array (List.map t.models ~f:override_to_json) ])
       ])
;;

(* ---- config.json ------------------------------------------------------- *)

let hint ~home = sprintf " (in %s)" (Config.path ~home)

let load ~home =
  match Config.read_fields ~home with
  | Error e -> [], [ Error.to_string_hum e ^ "; custom providers are not loaded" ]
  | Ok fields ->
    (match List.Assoc.find fields ~equal:String.equal "providers" with
     | None | Some `Null -> [], []
     | Some (`Object entries) ->
       let results = List.map entries ~f:(fun (name, json) -> of_json ~name json) in
       let providers =
         List.filter_map results ~f:(fun r -> Option.map (Result.ok r) ~f:fst)
       in
       let problems =
         List.concat_map results ~f:(function
           | Ok (_, warnings) -> List.map warnings ~f:(fun w -> w ^ hint ~home)
           | Error e ->
             [ Error.to_string_hum e ^ hint ~home ^ "; that provider is skipped" ])
       in
       providers, problems
     | Some _ ->
       ( []
       , [ "\"providers\" must be an object of provider names to definitions"
           ^ hint ~home
         ] ))
;;

let update ~home ~f =
  Or_error.bind (Config.read_fields ~home) ~f:(fun fields ->
    let providers =
      match List.Assoc.find fields ~equal:String.equal "providers" with
      | Some (`Object entries) -> entries
      | _ -> []
    in
    let providers = f providers in
    let fields =
      if List.Assoc.mem fields ~equal:String.equal "providers"
      then
        List.map fields ~f:(fun (name, value) ->
          if String.equal name "providers" then name, `Object providers else name, value)
      else fields @ [ "providers", `Object providers ]
    in
    Config.write_fields ~home fields)
;;

let save ~home t =
  update ~home ~f:(fun entries ->
    if List.Assoc.mem entries ~equal:String.equal t.name
    then
      List.map entries ~f:(fun (name, json) ->
        if String.equal name t.name then name, to_json t else name, json)
    else entries @ [ t.name, to_json t ])
;;

let remove ~home name =
  update ~home ~f:(fun entries -> List.Assoc.remove entries ~equal:String.equal name)
;;
