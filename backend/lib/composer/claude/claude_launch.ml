open! Core
open! Import

module Resume = struct
  type t =
    | New of { session_id : string option }
    | Resume of
        { session_id : string
        ; fork : bool
        ; at : string option
        }
  [@@deriving sexp_of]
end

type t =
  { executable : string
  ; model : string option
  ; permission_mode : Claude_permission_mode.t option
  ; resume : Resume.t
  ; mcp_config : Json.t option
  ; max_thinking_tokens : int option
  ; effort : string option
  ; setting_sources : string list
  ; strict_mcp_config : bool
  }
[@@deriving sexp_of]

let default =
  { executable = "claude"
  ; model = None
  ; permission_mode = None
  ; resume = New { session_id = None }
  ; mcp_config = None
  ; max_thinking_tokens = None
  ; effort = None
  ; setting_sources = [ "user" ]
  ; strict_mcp_config = true
  }
;;

let argv t =
  (* As the SDK does: a value that could be read as a flag goes after an
     [=]. *)
  let opt flag = function
    | None -> []
    | Some v when String.is_prefix v ~prefix:"-" -> [ flag ^ "=" ^ v ]
    | Some v -> [ flag; v ]
  in
  let eq flag = function
    | None -> []
    | Some v -> [ flag ^ "=" ^ v ]
  in
  let resume =
    match t.resume with
    | New { session_id } -> eq "--session-id" session_id
    | Resume { session_id; fork; at } ->
      [ "--resume=" ^ session_id ]
      @ (if fork then [ "--fork-session" ] else [])
      @ eq "--resume-session-at" at
  in
  let bypass =
    match t.permission_mode with
    | Some Bypass_permissions -> [ "--allow-dangerously-skip-permissions" ]
    | Some _ | None -> []
  in
  List.concat
    [ [ t.executable; "--output-format"; "stream-json"; "--verbose" ]
    ; opt "--model" t.model
    ; [ "--permission-prompt-tool"; "stdio" ]
    ; opt
        "--permission-mode"
        (Option.map t.permission_mode ~f:Claude_permission_mode.to_string)
    ; bypass
    ; resume
    ; opt "--mcp-config" (Option.map t.mcp_config ~f:Json.to_string)
    ; (if t.strict_mcp_config then [ "--strict-mcp-config" ] else [])
    ; [ "--setting-sources=" ^ String.concat ~sep:"," t.setting_sources ]
    ; [ "--include-partial-messages" ]
    ; opt
        "--max-thinking-tokens"
        (Option.map t.max_thinking_tokens ~f:Int.to_string)
    ; eq "--effort" t.effort
    ; [ "--input-format"; "stream-json" ]
    ]
;;

let env_unset = [ "CLAUDECODE" ]

let env_set =
  [ "CLAUDE_CODE_ENTRYPOINT", "sdk-ts"
  ; "CLAUDE_CODE_SDK_READS_SESSION_STATE", "1"
  ; "CLAUDE_CODE_AUTO_CONNECT_IDE", "0"
  ; "CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL", "1"
  ]
;;

let sdk_mcp_config names =
  `Object
    [ ( "mcpServers"
      , `Object
          (List.map names ~f:(fun name ->
             name, `Object [ "type", `String "sdk"; "name", `String name ])) )
    ]
;;
