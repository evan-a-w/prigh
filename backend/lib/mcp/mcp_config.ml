open! Core
open! Import

module Transport = struct
  type t =
    | Stdio of
        { command : string
        ; args : string list
        ; env : (string * string) list
        }
    | Http of
        { url : string
        ; headers : (string * string) list
        }
  [@@deriving sexp_of, compare]

  (* Identifies the definition, secrets included: never shown. *)
  let fingerprint = sexp_of_t

  let sexp_of_t t =
    let hide = List.map ~f:(fun (k, _) -> k, "<hidden>") in
    sexp_of_t
      (match t with
       | Stdio s -> Stdio { s with env = hide s.env }
       | Http h -> Http { h with headers = hide h.headers })
  ;;
end

module Server = struct
  type t =
    { name : string
    ; source : string
    ; project : bool
    ; dir : string
    ; transport : Transport.t
    ; approval : string
    }
  [@@deriving sexp_of]

  let key t =
    Md5.to_hex
      (Md5.digest_string
         (Sexp.to_string
            [%sexp
              ((t.name, t.source, t.dir, Transport.fingerprint t.transport)
               : string * string * string * Sexp.t)]))
  ;;
end

module Discovered = struct
  type t =
    { servers : Server.t list
    ; problems : string list
    }
  [@@deriving sexp_of]
end

let user_file ~home = Filename.concat home ".prigh/mcp.json"
let approvals_file ~home = Filename.concat home ".prigh/mcp-approvals.json"

let ancestors dir =
  let rec go dir acc =
    let parent = Filename.dirname dir in
    if String.equal parent dir
    then List.rev (dir :: acc)
    else go parent (dir :: acc)
  in
  go dir []
;;

(* [${VAR}] and [${VAR:-default}]; an unset variable without a default is
   an error naming it. *)
let expand ~getenv s =
  let re =
    Re.compile (Re.Perl.re {|\$\{([A-Za-z_][A-Za-z0-9_]*)(:-([^}]*))?\}|})
  in
  let missing = ref [] in
  let expanded =
    Re.replace re s ~f:(fun g ->
      let var = Re.Group.get g 1 in
      match getenv var, Re.Group.get_opt g 3 with
      | Some value, _ -> value
      | None, Some default -> default
      | None, None ->
        missing := var :: !missing;
        "")
  in
  match List.rev !missing with
  | [] -> Ok expanded
  | var :: _ ->
    Or_error.errorf "${%s} is not set; set it or write ${%s:-default}" var var
;;

let strings json ~what =
  match json with
  | `Array items ->
    List.map items ~f:(function
      | `String s -> Ok s
      | _ -> Or_error.errorf "%s must be a list of strings" what)
    |> Result.all
  | _ -> Or_error.errorf "%s must be a list of strings" what
;;

let string_map json ~what =
  match json with
  | `Object fields ->
    List.map fields ~f:(fun (k, v) ->
      match v with
      | `String s -> Ok (k, s)
      | _ -> Or_error.errorf "%s.%s must be a string" what k)
    |> Result.all
  | _ -> Or_error.errorf "%s must be an object of strings" what
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let transport ~getenv json =
  let open Or_error.Let_syntax in
  let expand = expand ~getenv in
  let expand_pairs = List.map ~f:(fun (k, v) -> expand v >>| fun v -> k, v) in
  let opt name ~default ~f =
    match field json name with
    | None -> Ok default
    | Some v -> f v ~what:name
  in
  let type_ =
    match field json "type" with
    | Some (`String s) -> Some s
    | _ -> None
  in
  match type_, field json "command", field json "url" with
  | (None | Some "stdio"), Some (`String command), _
    when String.is_empty (String.strip command) ->
    Or_error.error_string "the \"command\" is empty: give the program to run"
  | (None | Some ("http" | "streamable-http")), _, Some (`String url)
    when String.is_empty (String.strip url) ->
    Or_error.error_string "the \"url\" is empty: give the server's address"
  | (None | Some "stdio"), Some (`String command), _ ->
    let%bind args = opt "args" ~default:[] ~f:strings in
    let%bind env = opt "env" ~default:[] ~f:string_map in
    let%bind command = expand command in
    let%bind args = List.map args ~f:expand |> Result.all in
    let%map env = expand_pairs env |> Result.all in
    Transport.Stdio { command; args; env }
  | (None | Some ("http" | "streamable-http")), _, Some (`String url) ->
    let%bind headers = opt "headers" ~default:[] ~f:string_map in
    let%bind url = expand url in
    let%map headers = expand_pairs headers |> Result.all in
    Transport.Http { url; headers }
  | Some "stdio", _, _ ->
    Or_error.error_string "give the \"command\" to run (a string)"
  | Some ("http" | "streamable-http"), _, _ ->
    Or_error.error_string "give the server's \"url\" (a string)"
  | Some "sse", _, _ ->
    Or_error.error_string
      "the legacy SSE transport is not supported; use the server's streamable \
       HTTP endpoint (\"type\": \"http\")"
  | Some other, _, _ ->
    Or_error.errorf "unknown type %S; use stdio or http" other
  | None, _, _ ->
    Or_error.error_string "give a \"command\" (stdio) or a \"url\" (http)"
;;

let read_file path =
  match In_channel.read_all path with
  | exception _ -> None
  | text -> Some text
;;

let settings_source = "settings"
let settings_label = "Settings → MCP servers"

let label source =
  if String.equal source settings_source then settings_label else source
;;

let approval_of ~source ~name spec =
  Md5.to_hex
    (Md5.digest_string (source ^ "\000" ^ name ^ "\000" ^ Json.to_string spec))
;;

let servers_of ~getenv ~source ~project ~dir entries =
  List.partition_map entries ~f:(fun (name, spec) ->
    match transport ~getenv spec with
    | Ok transport ->
      First
        { Server.name
        ; source
        ; project
        ; dir
        ; transport
        ; approval = approval_of ~source ~name spec
        }
    | Error e ->
      Second
        (sprintf
           "%s: server %S: %s"
           (label source)
           name
           (Error.to_string_hum e)))
;;

let servers_in ~getenv ~source ~project ~dir =
  match read_file source with
  | None -> [], []
  | Some text ->
    (match Json.parse text with
     | Error e ->
       [], [ sprintf "%s: invalid JSON: %s" source (Error.to_string_hum e) ]
     | Ok json ->
       (match field json "mcpServers" with
        | None -> [], []
        | Some (`Object entries) ->
          servers_of ~getenv ~source ~project ~dir entries
        | Some _ -> [], [ sprintf "%s: mcpServers must be an object" source ]))
;;

let settings_entries = function
  | `Object entries -> entries
  | _ -> []
;;

let settings_servers ?(getenv = Sys.getenv) ~home settings =
  let servers, problems =
    servers_of
      ~getenv
      ~source:settings_source
      ~project:false
      ~dir:home
      (settings_entries settings)
  in
  { Discovered.servers; problems }
;;

let discover ?(getenv = Sys.getenv) ?(settings = `Object []) ~cwd ~home () =
  let project =
    List.map (ancestors cwd) ~f:(fun dir ->
      servers_in
        ~getenv
        ~source:(Filename.concat dir ".mcp.json")
        ~project:true
        ~dir)
  in
  let user =
    servers_in ~getenv ~source:(user_file ~home) ~project:false ~dir:home
  in
  let settings =
    servers_of
      ~getenv
      ~source:settings_source
      ~project:false
      ~dir:home
      (settings_entries settings)
  in
  let found = project @ [ user; settings ] in
  { Discovered.servers =
      List.concat_map found ~f:fst
      |> List.stable_dedup ~compare:(fun (a : Server.t) b ->
        String.compare a.name b.name)
  ; problems = List.concat_map found ~f:snd
  }
;;

let find ?(getenv = Sys.getenv) ?(settings = `Object []) ~home ~source name =
  let servers, problems =
    if String.equal source settings_source
    then
      servers_of
        ~getenv
        ~source
        ~project:false
        ~dir:home
        (settings_entries settings)
    else (
      let project = not (String.equal source (user_file ~home)) in
      let dir = if project then Filename.dirname source else home in
      servers_in ~getenv ~source ~project ~dir)
  in
  match List.find servers ~f:(fun s -> String.equal s.name name) with
  | Some server -> Ok server
  | None ->
    (match
       List.find problems ~f:(fun p ->
         String.is_substring p ~substring:(sprintf "server %S:" name))
     with
     | Some problem -> Error (Error.of_string problem)
     | None ->
       Or_error.errorf
         "%s no longer defines the MCP server %S"
         (label source)
         name)
;;

let check_name name =
  if String.is_empty name
  then Or_error.error_string "give the server a name"
  else if String.length name > 32
  then
    Or_error.errorf
      "the name %S is longer than 32 characters; shorten it (its tools are \
       named mcp__NAME__TOOL, at most 64 characters)"
      name
  else if
    not
      (String.for_all name ~f:(fun c ->
         Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'))
  then
    Or_error.errorf
      "the name %S may only have letters, digits, - and _ (its tools are named \
       mcp__NAME__TOOL)"
      name
  else if String.is_substring name ~substring:"__"
  then
    Or_error.errorf
      "the name %S has \"__\", which separates the server from the tool in \
       mcp__NAME__TOOL; use a single _"
      name
  else Ok ()
;;

let check spec =
  match spec with
  | `Object _ ->
    Or_error.bind
      (transport ~getenv:(fun _ -> Some "x") spec)
      ~f:(function
        | Stdio _ -> Ok ()
        | Http _ ->
          (match field spec "url" with
           | Some (`String url)
             when not
                    (List.exists
                       [ "https://"; "http://"; "${" ]
                       ~f:(fun prefix ->
                         String.is_prefix (String.strip url) ~prefix)) ->
             Or_error.errorf
               "the url %S must start with https:// (or http://)"
               url
           | _ -> Ok ()))
  | _ ->
    Or_error.error_string
      "a server is an object with a \"command\" (stdio) or a \"url\" (http)"
;;

let approvals ~home =
  match Option.map (read_file (approvals_file ~home)) ~f:Json.parse with
  | Some (Ok (`Array items)) ->
    List.filter_map items ~f:(function
      | `String s -> Some s
      | _ -> None)
  | Some (Ok _ | Error _) | None -> []
;;

let is_approved ~home (server : Server.t) =
  (not server.project)
  || List.mem (approvals ~home) server.approval ~equal:String.equal
;;

let approve ~home (server : Server.t) =
  if is_approved ~home server
  then Ok ()
  else (
    let path = approvals_file ~home in
    let items = server.approval :: approvals ~home in
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p (Filename.dirname path);
      let tmp = path ^ ".tmp" in
      Out_channel.write_all
        tmp
        ~data:
          (Json.to_string (`Array (List.map items ~f:(fun s -> `String s)))
           ^ "\n");
      Core_unix.rename ~src:tmp ~dst:path))
;;
