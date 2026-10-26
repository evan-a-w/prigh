open! Core
open! Import

type t =
  { host : string
  ; kind : Forge_kind.t
  ; web_url : string
  ; client_id : string option
  ; default_client : bool
  }
[@@deriving sexp_of]

let forgejo_default_client_id = "a4792ccc-144e-407e-86c9-5e7d8d9c3269"

module Entry = struct
  type t =
    { kind : Forge_kind.t option
    ; client_id : string option
    ; url : string option
    }
  [@@deriving sexp_of]

  let of_json ~host json =
    let str name =
      match Json.member name json with
      | None | Some `Null -> Ok None
      | Some (`String s) when not (String.is_empty (String.strip s)) ->
        Ok (Some (String.strip s))
      | Some _ ->
        Or_error.errorf "forges.%s.%s must be a non-empty string" host name
    in
    let open Or_error.Let_syntax in
    let%bind kind = str "kind" in
    let%bind kind =
      match kind with
      | None -> Ok None
      | Some k ->
        Or_error.map (Forge_kind.of_string k) ~f:Option.some
        |> Or_error.tag ~tag:(sprintf "forges.%s.kind" host)
    in
    let%bind client_id = str "client_id" in
    let%map url = str "url" in
    { kind; client_id; url }
  ;;
end

module Settings = struct
  type t = (string * Entry.t) list [@@deriving sexp_of]

  let empty = []

  let of_config_fields fields =
    match List.Assoc.find fields ~equal:String.equal "forges" with
    | None | Some `Null -> Ok []
    | Some (`Object hosts) ->
      Or_error.all
        (List.map hosts ~f:(fun (host, json) ->
           Or_error.map (Entry.of_json ~host json) ~f:(fun e -> host, e)))
    | Some _ ->
      Or_error.error_string
        "\"forges\" in config.json must be an object of hosts to settings"
  ;;

  let load ~home =
    match Or_error.bind (Config.read_fields ~home) ~f:of_config_fields with
    | Ok t -> t
    | Error _ -> []
  ;;

  let hosts t = List.map t ~f:fst
end

let normalize_host input =
  let s = String.strip input in
  let s =
    match String.substr_index s ~pattern:"://" with
    | Some i -> String.drop_prefix s (i + 3)
    | None -> s
  in
  let s =
    match String.lsplit2 s ~on:'/' with
    | Some (host, _) -> host
    | None -> s
  in
  let s =
    match String.rsplit2 s ~on:'@' with
    | Some (_, host) -> host
    | None -> s
  in
  let s = String.lowercase s in
  if
    String.is_empty s
    || not
         (String.for_all s ~f:(fun c ->
            Char.is_alphanum c
            || List.mem [ '.'; '-'; ':'; '_' ] c ~equal:Char.equal))
  then Or_error.errorf "not a host name: %S (e.g. codeberg.org)" input
  else Ok s
;;

let resolve ?(getenv = Sys.getenv) ?kind ?stored settings ~host =
  let entry = List.Assoc.find settings ~equal:String.equal host in
  let kind =
    match kind, Option.bind entry ~f:(fun (e : Entry.t) -> e.kind), stored with
    | Some k, _, _ | None, Some k, _ | None, None, Some k -> k
    | None, None, None ->
      if String.equal host "github.com" then Github else Forgejo
  in
  let configured = Option.bind entry ~f:(fun e -> e.client_id) in
  let configured =
    match configured, kind, host with
    | None, Github, "github.com" ->
      Option.filter (getenv "PRIGH_GITHUB_CLIENT_ID") ~f:(fun s ->
        not (String.is_empty (String.strip s)))
      |> Option.map ~f:String.strip
    | c, _, _ -> c
  in
  let client_id, default_client =
    match configured, kind with
    | Some c, _ -> Some c, false
    | None, Forgejo -> Some forgejo_default_client_id, true
    | None, Github -> None, false
  in
  let web_url =
    match Option.bind entry ~f:(fun e -> e.url) with
    | Some url -> String.rstrip url ~drop:(Char.equal '/')
    | None -> "https://" ^ host
  in
  { host; kind; web_url; client_id; default_client }
;;

let api_url t =
  match t.kind, t.host with
  | Github, "github.com" -> "https://api.github.com"
  | Github, _ -> t.web_url ^ "/api/v3"
  | Forgejo, _ -> t.web_url ^ "/api/v1"
;;

let login_arg t =
  match t.kind, t.host with
  | Github, "github.com" -> "github"
  | kind, host -> Forge_kind.to_string kind ^ " " ^ host
;;
