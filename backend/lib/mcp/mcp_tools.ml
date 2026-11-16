open! Core
open! Import

module Status = struct
  type t =
    | Ready
    | Failed of string
    | Needs_approval
    | Needs_sign_in of string
  [@@deriving sexp_of]
end

module Server = struct
  type t =
    { name : string
    ; source : string
    ; project : bool
    ; status : Status.t
    ; tools : Mcp_client.Tool.t list
    ; url : string option [@sexp.option]
    ; oauth : Mcp_config.Oauth.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

let tool_name ~server ~tool =
  let clean s =
    String.map s ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'
      then c
      else '_')
  in
  String.prefix (sprintf "mcp__%s__%s" (clean server) (clean tool)) 64
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let bool json = if json then `True else `False

module Listing = struct
  type t =
    { servers : Server.t list
    ; problems : string list
    }
  [@@deriving sexp_of]

  let empty = { servers = []; problems = [] }

  let keep_tools t ~previous =
    let same (a : Server.t) (b : Server.t) =
      String.equal a.name b.name && String.equal a.source b.source
    in
    { t with
      servers =
        List.map t.servers ~f:(fun (s : Server.t) ->
          match s.status with
          | (Failed _ | Needs_sign_in _) when List.is_empty s.tools ->
            (match List.find previous.servers ~f:(same s) with
             | Some p -> { s with tools = p.tools }
             | None -> s)
          | Ready | Failed _ | Needs_sign_in _ | Needs_approval -> s)
    }
  ;;

  let of_hub (statuses, problems) =
    { servers =
        List.map statuses ~f:(fun { Mcp_hub.Server_status.server; status } ->
          let status, tools =
            match status with
            | Ready tools -> Status.Ready, tools
            | Failed e -> Failed e, []
            | Needs_approval -> Needs_approval, []
            | Needs_sign_in e -> Needs_sign_in e, []
          in
          let url, oauth =
            match server.transport with
            | Http { url; oauth; headers = _ } -> Some url, oauth
            | Stdio _ -> None, None
          in
          { Server.name = server.name
          ; source = server.source
          ; project = server.project
          ; status
          ; tools
          ; url
          ; oauth
          })
    ; problems
    }
  ;;

  let status_fields : Status.t -> _ = function
    | Ready -> [ "status", `String "ready" ]
    | Failed e -> [ "status", `String "failed"; "error", `String e ]
    | Needs_approval -> [ "status", `String "needs_approval" ]
    | Needs_sign_in e ->
      [ "status", `String "needs_sign_in"; "error", `String e ]
  ;;

  let to_json' t ~tool ~extra =
    `Object
      [ ( "servers"
        , `Array
            (List.map t.servers ~f:(fun (s : Server.t) ->
               `Object
                 ([ "name", `String s.name
                  ; "source", `String s.source
                  ; "project", bool s.project
                  ]
                  @ status_fields s.status
                  @ Option.value_map s.url ~default:[] ~f:(fun url ->
                    [ "url", `String url ])
                  @ extra s
                  @ [ "tools", `Array (List.map s.tools ~f:(tool s)) ]))) )
      ; "problems", `Array (List.map t.problems ~f:(fun p -> `String p))
      ]
  ;;

  let to_json =
    to_json'
      ~extra:(fun s ->
        Option.value_map s.oauth ~default:[] ~f:(fun oauth ->
          [ "oauth", Mcp_config.Oauth.to_json oauth ]))
      ~tool:(fun _ (tool : Mcp_client.Tool.t) ->
        `Object
          [ "name", `String tool.name
          ; "description", `String tool.description
          ; "input_schema", tool.input_schema
          ; "read_only", bool tool.read_only
          ])
  ;;

  let to_rpc_json ?(signed_in = fun _ -> false) t =
    to_json'
      t
      ~extra:(fun s ->
        match s.url with
        | Some url -> [ "signed_in", bool (signed_in url) ]
        | None -> [])
      ~tool:(fun (s : Server.t) (tool : Mcp_client.Tool.t) ->
        `Object
          [ "name", `String (tool_name ~server:s.name ~tool:tool.name)
          ; "description", `String tool.description
          ])
  ;;

  let of_json json =
    let strings name =
      match field json name with
      | Some (`Array items) ->
        List.filter_map items ~f:(function
          | `String s -> Some s
          | _ -> None)
      | _ -> []
    in
    let tool json =
      Option.map (string_field json "name") ~f:(fun name ->
        { Mcp_client.Tool.name
        ; description =
            Option.value (string_field json "description") ~default:""
        ; input_schema =
            Option.value
              (field json "input_schema")
              ~default:(`Object [ "type", `String "object" ])
        ; read_only =
            (match field json "read_only" with
             | Some `True -> true
             | _ -> false)
        })
    in
    let server json =
      match string_field json "name", string_field json "source" with
      | Some name, Some source ->
        let status : Status.t =
          match string_field json "status" with
          | Some "ready" -> Ready
          | Some "needs_approval" -> Needs_approval
          | Some "needs_sign_in" ->
            Needs_sign_in
              (Option.value (string_field json "error") ~default:"sign in")
          | _ ->
            Failed
              (Option.value
                 (string_field json "error")
                 ~default:"unknown status")
        in
        Some
          { Server.name
          ; source
          ; project =
              (match field json "project" with
               | Some `True -> true
               | _ -> false)
          ; status
          ; tools =
              (match field json "tools" with
               | Some (`Array items) -> List.filter_map items ~f:tool
               | _ -> [])
          ; url = string_field json "url"
          ; oauth =
              Option.bind (field json "oauth") ~f:(fun oauth ->
                Option.map (string_field oauth "clientId") ~f:(fun client_id ->
                  { Mcp_config.Oauth.client_id
                  ; client_secret = string_field oauth "clientSecret"
                  ; scopes =
                      (match field oauth "scopes" with
                       | Some (`Array items) ->
                         Some (List.filter_map items ~f:Json.string)
                       | _ -> None)
                  }))
          }
      | _ -> None
    in
    match field json "servers" with
    | Some (`Array items) ->
      Ok
        { servers = List.filter_map items ~f:server
        ; problems = strings "problems"
        }
    | _ -> Or_error.error_string "an MCP listing needs \"servers\""
  ;;

  let notices t =
    t.problems
    @ List.filter_map t.servers ~f:(fun s ->
      match s.status with
      | Ready -> None
      | Failed e ->
        Some
          (sprintf
             "MCP server %s (%s) failed: %s; once that is fixed, /mcp \
              reconnect starts it"
             s.name
             s.source
             e)
      | Needs_approval ->
        Some
          (sprintf
             "MCP server %s from %s is not started until you approve it: /mcp"
             s.name
             s.source)
      | Needs_sign_in e ->
        Some
          (sprintf
             "MCP server %s (%s) needs you to sign in: %s; /mcp signin %s (or \
              Sign in in Settings → MCP servers)"
             s.name
             s.source
             e
             s.name))
  ;;
end

let tools (listing : Listing.t) ~call =
  List.concat_map listing.servers ~f:(fun (s : Server.t) ->
    match s.status with
    | Needs_approval -> []
    | Ready | Failed _ | Needs_sign_in _ ->
      List.map s.tools ~f:(fun (tool : Mcp_client.Tool.t) ->
        { Tool.spec =
            { Tool_spec.name = tool_name ~server:s.name ~tool:tool.name
            ; description =
                (if String.is_empty tool.description
                 then sprintf "%s (MCP server %s)" tool.name s.name
                 else tool.description)
            ; parameters = tool.input_schema
            ; parallel_safe = tool.read_only
            ; destructive = Tool_spec.Destructive.of_bool (not tool.read_only)
            ; on_host = false
            ; freeform = None
            ; native = None
            }
        ; run =
            (fun context args ->
              call context ~source:s.source ~server:s.name ~tool:tool.name args)
        }))
  |> List.stable_dedup ~compare:(fun (a : Tool.t) b ->
    String.compare a.spec.name b.spec.name)
;;
