open! Core

type t =
  { id : int
  ; method_ : string
  ; params : (string * Json.t) list
  }
[@@deriving sexp_of]

module Method = struct
  type t =
    | List_skills
    | List_mcp of { reconnect : bool }
    | Mcp_approve of
        { source : string
        ; server : string
        }
    | Set_archived of
        { path : string
        ; archived : bool
        }
    | Delete_sessions of string list
    | Delete_archived
    | Get_messages_page of
        { session : string
        ; before : string option
        ; from : string option
        ; limit : int
        }
    | Get_tool_output of
        { call_id : string
        ; agent_id : string option
        }
    | Change_dir of
        { target : Dir_change.Target.t
        ; path : string
        ; if_missing : Dir_change.If_missing.t
        }
  [@@deriving sexp_of, equal]

  let name = function
    | List_skills -> "list_skills"
    | List_mcp _ -> "list_mcp"
    | Mcp_approve _ -> "mcp_approve"
    | Set_archived _ -> "set_archived"
    | Delete_sessions _ -> "delete_sessions"
    | Delete_archived -> "delete_archived"
    | Get_messages_page _ -> "get_messages_page"
    | Get_tool_output _ -> "get_tool_output"
    | Change_dir { target = Cwd; _ } -> "set_cwd"
    | Change_dir { target = Host _; _ } -> "set_active_host"
  ;;

  let params = function
    | List_skills | List_mcp { reconnect = false } | Delete_archived -> []
    | List_mcp { reconnect = true } -> [ "reconnect", Json.bool true ]
    | Mcp_approve { source; server } ->
      [ "source", Json.str source; "server", Json.str server ]
    | Set_archived { path; archived } ->
      [ "path", Json.str path; "archived", Json.bool archived ]
    | Delete_sessions paths -> [ "paths", `Array (List.map paths ~f:Json.str) ]
    | Get_messages_page { session; before; from; limit } ->
      [ "session", Json.str session ]
      @ Option.value_map before ~default:[] ~f:(fun b ->
        [ "before", Json.str b ])
      @ Option.value_map from ~default:[] ~f:(fun f -> [ "from", Json.str f ])
      @ [ "limit", Json.int limit ]
    | Get_tool_output { call_id; agent_id } ->
      [ "call_id", Json.str call_id ]
      @ Option.value_map agent_id ~default:[] ~f:(fun a ->
        [ "agent_id", Json.str a ])
    | Change_dir { target; path; if_missing } ->
      let if_missing =
        match if_missing with
        | Fail -> []
        | Ask | Create ->
          [ "if_missing", Dir_change.If_missing.to_json if_missing ]
      in
      (match target with
       | Cwd -> [ "path", Json.str path ]
       | Host { id; name = _ } -> [ "host", Json.str id; "cwd", Json.str path ])
      @ if_missing
  ;;
end

let create ~id method_ =
  { id; method_ = Method.name method_; params = Method.params method_ }
;;

let to_json { id; method_; params } =
  Json.obj
    [ "id", Json.int id; "method", Json.str method_; "params", Json.obj params ]
;;

let to_line t = Json.to_string (to_json t)
