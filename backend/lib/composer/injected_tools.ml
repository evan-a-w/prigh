open! Core
open! Import

let server_name = "prigh"

let default_names ~native_questions =
  [ Tool_render_ui.name
  ; Tool_memory.name
  ; Tool_custom_instructions.name
  ; Tool_sessions.name
  ; Tool_ask_session.name
  ; Tool_answer_session.name
  ; Tool_pull_request.name
  ; Tool_issue.name
  ; Tool_repo.name
  ; Tool_worktree.name
  ; Tool_review_reply.name
  ; Goal.tool_name
  ]
  @ if native_questions then [] else [ Tool_ask_user.name ]
;;

let select ~native_questions tools =
  List.filter_map (default_names ~native_questions) ~f:(fun name ->
    List.find tools ~f:(fun t -> String.equal (Tool.name t) name))
;;

let canonical_of_mcp ~server ~tool =
  if
    String.equal server server_name
    && List.mem (default_names ~native_questions:false) tool ~equal:String.equal
  then Some tool
  else None
;;

let result_to_mcp (result : Tool_result.t) =
  let text =
    if String.is_empty result.text && not (List.is_empty result.images)
    then []
    else [ `Object [ "type", `String "text"; "text", `String result.text ] ]
  in
  let images =
    List.map result.images ~f:(fun (image : Image.t) ->
      `Object
        [ "type", `String "image"
        ; "data", `String image.data
        ; "mimeType", `String image.mime_type
        ])
  in
  `Object
    [ "content", `Array (text @ images)
    ; ("isError", if result.is_error then `True else `False)
    ]
;;

type t =
  { tools : Tool.t list
  ; execute : Tool.t -> Json.t -> Tool_result.t
  }

let create ~tools ~execute = { tools; execute }
let tools t = t.tools

(* MCP revisions we can speak; the client's is echoed when it is one. *)
let protocol_versions = [ "2025-06-18"; "2025-03-26"; "2024-11-05" ]

module Rpc_error = struct
  let parse = -32700
  let invalid_request = -32600
  let method_not_found = -32601
  let invalid_params = -32602
end

let response ~id result =
  `Object [ "jsonrpc", `String "2.0"; "id", id; "result", result ]
;;

let error ~id code message =
  `Object
    [ "jsonrpc", `String "2.0"
    ; "id", id
    ; ( "error"
      , `Object
          [ "code", `Number (Int.to_string code); "message", `String message ] )
    ]
;;

let string_member name json =
  match Json.member name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let initialize params =
  let version =
    match Option.bind params ~f:(string_member "protocolVersion") with
    | Some v when List.mem protocol_versions v ~equal:String.equal -> v
    | Some _ | None -> List.hd_exn protocol_versions
  in
  `Object
    [ "protocolVersion", `String version
    ; "capabilities", `Object [ "tools", `Object [] ]
    ; ( "serverInfo"
      , `Object
          [ "name", `String server_name; "version", `String Version.to_string ]
      )
    ; ( "instructions"
      , `String
          "prigh's tools for the user's session: components shown in the chat, \
           the user's memory, other sessions, pull requests, issues and CI \
           through the user's sign-in, and worktrees." )
    ]
;;

let tool_json (tool : Tool.t) =
  `Object
    [ "name", `String tool.spec.name
    ; "description", `String tool.spec.description
    ; "inputSchema", tool.spec.parameters
    ]
;;

let call t ~id params =
  let name = Option.bind params ~f:(string_member "name") in
  let arguments =
    match Option.bind params ~f:(Json.member "arguments") with
    | None | Some `Null -> Ok (`Object [])
    | Some (`Object _ as args) -> Ok args
    | Some _ -> Error "arguments must be an object"
  in
  match name, arguments with
  | None, _ -> error ~id Rpc_error.invalid_params "tools/call needs a tool name"
  | Some _, Error message -> error ~id Rpc_error.invalid_params message
  | Some name, Ok arguments ->
    (match
       List.find t.tools ~f:(fun tool -> String.equal (Tool.name tool) name)
     with
     | None ->
       error
         ~id
         Rpc_error.invalid_params
         (sprintf
            "unknown tool %s: prigh offers %s"
            name
            (String.concat ~sep:", " (List.map t.tools ~f:Tool.name)))
     | Some tool ->
       let result =
         match t.execute tool arguments with
         | result -> result
         | exception exn -> Tool_result.error (Exn.to_string exn)
       in
       response ~id (result_to_mcp result))
;;

let handle t (message : Json.t) =
  let id = Json.member "id" message in
  let method_ = string_member "method" message in
  let params = Json.member "params" message in
  match method_, id with
  | None, _ ->
    (* A response to a request we never send, or garbage: nothing to say
       unless it carries an id we could answer. *)
    (match id, Json.member "result" message, Json.member "error" message with
     | Some id, None, None ->
       Some (error ~id Rpc_error.invalid_request "no method")
     | _ -> None)
  | Some _, None -> None (* notifications: initialized, cancelled, ... *)
  | Some method_, Some id ->
    Some
      (match method_ with
       | "initialize" -> response ~id (initialize params)
       | "ping" -> response ~id (`Object [])
       | "tools/list" ->
         response
           ~id
           (`Object [ "tools", `Array (List.map t.tools ~f:tool_json) ])
       | "tools/call" -> call t ~id params
       | other ->
         error ~id Rpc_error.method_not_found ("method not found: " ^ other))
;;

let handle_line t line =
  match Json.parse line with
  | Ok json -> Option.map (handle t json) ~f:Json.to_string
  | Error err ->
    Some
      (Json.to_string
         (error
            ~id:`Null
            Rpc_error.parse
            ("parse error: " ^ Error.to_string_hum err)))
;;

let claude_reply t (request : Claude_codec.Mcp_request.t) =
  let reply =
    if String.equal request.server server_name
    then handle t request.message
    else (
      match Json.member "id" request.message with
      | None -> None
      | Some id ->
        Some
          (error
             ~id
             Rpc_error.method_not_found
             (sprintf
                "no in-band MCP server %S here: the only one is %S"
                request.server
                server_name)))
  in
  Claude_codec.mcp_reply ~request_id:request.request_id reply
;;

module Bridge = struct
  type t =
    { command : string
    ; args : string list
    ; env : (string * string) list
    }
  [@@deriving sexp_of]

  let create ~prigh ~socket =
    { command = prigh
    ; args = [ "mcp-bridge" ]
    ; env = [ Mcp_bridge.socket_env, socket ]
    }
  ;;

  let server_json t =
    `Object
      [ "command", `String t.command
      ; "args", `Array (List.map t.args ~f:(fun a -> `String a))
      ; "env", `Object (List.map t.env ~f:(fun (k, v) -> k, `String v))
      ]
  ;;

  let codex_config t = "mcp_servers." ^ server_name, server_json t

  let acp_server t =
    `Object
      [ "name", `String server_name
      ; "command", `String t.command
      ; "args", `Array (List.map t.args ~f:(fun a -> `String a))
      ; ( "env"
        , `Array
            (List.map t.env ~f:(fun (name, value) ->
               `Object [ "name", `String name; "value", `String value ])) )
      ]
  ;;
end
