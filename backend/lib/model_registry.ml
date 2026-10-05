open! Core
open! Import

module Listing = struct
  type t =
    | Unknown
    | Listed of Custom_provider.Listed_model.t list
    | Failed of string
end

module Source = struct
  type t =
    { env : Env.t
    ; sw : Switch.t
    ; timeout : Time_ns.Span.t
    ; home : string
    ; store : Auth_store.t
    ; getenv : string -> string option
    ; auto_fetch : bool
    }
end

type t =
  { source : Source.t option
  ; mutable providers : Custom_provider.t list
  ; mutable config_problems : string list
  ; listings : Listing.t String.Table.t
  ; mutable subscribers : (string -> unit) list
  }

let default_timeout = Time_ns.Span.of_int_sec 10

let builtin () =
  { source = None
  ; providers = []
  ; config_problems = []
  ; listings = String.Table.create ()
  ; subscribers = []
  }
;;

let home t = Option.map t.source ~f:(fun s -> s.home)
let subscribe t ~f = t.subscribers <- t.subscribers @ [ f ]
let announce t message = List.iter t.subscribers ~f:(fun f -> f message)
let providers t = t.providers

let find_provider t name =
  List.find t.providers ~f:(fun (p : Custom_provider.t) ->
    String.equal p.name name)
;;

let listing t name =
  Option.value (Hashtbl.find t.listings name) ~default:Listing.Unknown
;;

let listed t name =
  match listing t name with
  | Listed models -> models
  | Unknown | Failed _ -> []
;;

let models t =
  Model.all
  @ List.concat_map t.providers ~f:(fun p ->
    Custom_provider.models p ~listed:(listed t p.name))
;;

let custom_key t s =
  Option.bind (String.lsplit2 s ~on:'/') ~f:(fun (provider, id) ->
    Option.map (find_provider t provider) ~f:(fun p -> p, id))
;;

let find t s =
  match Model.find_in (models t) s with
  | Some m -> Some m
  | None ->
    Option.map (custom_key t s) ~f:(fun (p, id) ->
      Custom_provider.model p ?listed:None id)
;;

let resolve t query =
  let query = String.strip query in
  match Model.resolve_in (models t) query with
  | Ok m -> Ok m
  | Error _ as error ->
    (match custom_key t query with
     | Some (p, id) when not (String.is_empty id) ->
       (match listing t p.name with
        | Unknown | Failed _ -> Ok (Custom_provider.model p id)
        | Listed listed ->
          (* Suggest that provider's models only. *)
          (match Model.resolve_in (Custom_provider.models p ~listed) query with
           | Ok _ -> error
           | Error _ as narrower -> narrower))
     | _ -> error)
;;

let problems t =
  t.config_problems
  @ List.filter_map t.providers ~f:(fun p ->
    match listing t p.name with
    | Failed message -> Some message
    | Unknown | Listed _ -> None)
;;

let describe_http_error ~url (e : Http_client.Error.t) =
  match e with
  | Connection_failed message ->
    sprintf "could not connect to %s (%s)" url message
  | Timed_out -> sprintf "no answer from %s in time" url
  | Cancelled -> "cancelled"
;;

let fetch_models
      ~env
      ?cancel
      ?(timeout = default_timeout)
      (p : Custom_provider.t)
      ~key
  =
  let url = p.base_url ^ "/models" in
  let auth =
    match key with
    | None -> []
    | Some key ->
      [ "Authorization", "Bearer " ^ key ]
      @
        (match p.api with
        | Anthropic -> [ "x-api-key", key ]
        | Chat | Responses -> [])
  in
  let version =
    match p.api with
    | Anthropic -> [ "anthropic-version", "2023-06-01" ]
    | Chat | Responses -> []
  in
  match
    Http_client.get
      ~env
      ?cancel
      ~timeout
      ~url
      ~headers:(("Accept", "application/json") :: (auth @ version @ p.headers))
      ()
  with
  | Error e -> Or_error.error_string (describe_http_error ~url e)
  | Ok (response, body) when response.status / 100 <> 2 ->
    let message =
      Sse_request.error_message_of_body ~status:response.status body
    in
    let hint =
      match response.status with
      | 401 | 403 ->
        if Option.is_none key
        then " (the server wants an API key)"
        else " (the server refused the API key)"
      | 404 -> " (the base URL usually ends in /v1)"
      | _ -> ""
    in
    Or_error.errorf "GET %s: %s%s" url (String.prefix message 300) hint
  | Ok (_, body) ->
    (match Json.parse body with
     | Error _ ->
       Or_error.errorf
         "GET %s did not return JSON (is the base URL the API's, usually \
          ending in /v1?)"
         url
     | Ok json ->
       Or_error.tag
         (Custom_provider.Listed_model.list_of_json json)
         ~tag:(sprintf "GET %s" url))
;;

let resolve_key (source : Source.t) ?cancel (p : Custom_provider.t) =
  Or_error.map
    (Provider_auth.resolve
       ~env:source.env
       ?cancel
       ~getenv:source.getenv
       source.store
       (Custom_provider.provider_id p))
    ~f:(Option.map ~f:(fun (r : Provider_auth.Resolved.t) -> r.token))
;;

let fetch t ?cancel p =
  match t.source with
  | None -> Or_error.error_string "custom providers are not available here"
  | Some source ->
    Or_error.bind (resolve_key source ?cancel p) ~f:(fun key ->
      fetch_models ~env:source.env ?cancel ~timeout:source.timeout p ~key)
;;

let set_listed t name models =
  Hashtbl.set t.listings ~key:name ~data:(Listed models)
;;

let refresh_one t (p : Custom_provider.t) =
  match fetch t p with
  | Ok models ->
    let was_failing =
      match listing t p.name with
      | Failed _ -> true
      | Unknown | Listed _ -> false
    in
    (* The provider may have been removed or changed meanwhile. *)
    if Option.is_some (find_provider t p.name)
    then (
      set_listed t p.name models;
      if was_failing
      then
        announce
          t
          (sprintf
             "%s: the model list loaded again (%d models)"
             p.name
             (List.length models)))
  | Error e ->
    let message =
      sprintf
        "%s: no model list: %s. Check that the server is running and the base \
         URL and key are right (/login %s to change them); models named in \
         config.json still work."
        p.name
        (Error.to_string_hum e)
        p.name
    in
    if Option.is_some (find_provider t p.name)
    then (
      Hashtbl.set t.listings ~key:p.name ~data:(Failed message);
      announce t message)
;;

let refresh t ?only () =
  let selected =
    match only with
    | None -> t.providers
    | Some names ->
      List.filter t.providers ~f:(fun p ->
        List.mem names p.name ~equal:String.equal)
  in
  Fiber.List.iter (refresh_one t) selected
;;

let same_endpoint (a : Custom_provider.t) (b : Custom_provider.t) =
  String.equal a.base_url b.base_url
  && Custom_provider.Api.equal a.api b.api
  && [%equal: (string * string) list] a.headers b.headers
;;

let reload ?listed t =
  match t.source with
  | None -> ()
  | Some source ->
    let providers, problems = Custom_provider.load ~home:source.home in
    let just_listed p =
      Option.value_map listed ~default:false ~f:(fun (name, _) ->
        String.equal name p.Custom_provider.name)
    in
    let changed =
      List.filter providers ~f:(fun p ->
        (not (just_listed p))
        &&
        match find_provider t p.name with
        | Some old -> not (same_endpoint old p)
        | None -> true)
    in
    let new_problems =
      List.filter problems ~f:(fun p ->
        not (List.mem t.config_problems p ~equal:String.equal))
    in
    t.providers <- providers;
    t.config_problems <- problems;
    Hashtbl.filter_keys_inplace t.listings ~f:(fun name ->
      Option.is_some (find_provider t name));
    List.iter changed ~f:(fun p -> Hashtbl.remove t.listings p.name);
    Option.iter listed ~f:(fun (name, models) ->
      if Option.is_some (find_provider t name) then set_listed t name models);
    List.iter new_problems ~f:(announce t);
    if source.auto_fetch && not (List.is_empty changed)
    then
      Fiber.fork ~sw:source.sw (fun () ->
        refresh t ~only:(List.map changed ~f:(fun p -> p.name)) ())
;;

let create
      ~env
      ~sw
      ?(timeout = default_timeout)
      ?(auto_fetch = true)
      ~home
      ~store
      ~getenv
      ()
  =
  let t =
    { (builtin ()) with
      source = Some { env; sw; timeout; home; store; getenv; auto_fetch }
    }
  in
  reload t;
  t
;;
