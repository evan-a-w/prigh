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
    | Get_outline of { session : string }
    | Get_tool_output of
        { call_id : string
        ; agent_id : string option
        }
    | Change_dir of
        { target : Dir_change.Target.t
        ; path : string
        ; if_missing : Dir_change.If_missing.t
        }
    | Forge_status
    | Forge_login of
        { host : string
        ; kind : string option
        ; method_ : string option
        }
    | Forge_logout of string
    | Pr_list of { state : string option }
    | Pr_view of int
    | Pr_defaults
    | Pr_create of
        { title : string
        ; body : string
        ; base : string option
        ; head : string option
        ; draft : bool
        ; push : bool
        }
    | Pr_checkout of int
  [@@deriving sexp_of, equal]

  let name = function
    | List_skills -> "list_skills"
    | List_mcp _ -> "list_mcp"
    | Mcp_approve _ -> "mcp_approve"
    | Set_archived _ -> "set_archived"
    | Delete_sessions _ -> "delete_sessions"
    | Delete_archived -> "delete_archived"
    | Get_messages_page _ -> "get_messages_page"
    | Get_outline _ -> "get_outline"
    | Get_tool_output _ -> "get_tool_output"
    | Change_dir { target = Cwd; _ } -> "set_cwd"
    | Change_dir { target = Host _; _ } -> "set_active_host"
    | Forge_status -> "forge_status"
    | Forge_login _ -> "forge_login"
    | Forge_logout _ -> "forge_logout"
    | Pr_list _ -> "pr_list"
    | Pr_view _ -> "pr_view"
    | Pr_defaults -> "pr_defaults"
    | Pr_create _ -> "pr_create"
    | Pr_checkout _ -> "pr_checkout"
  ;;

  let params = function
    | List_skills
    | List_mcp { reconnect = false }
    | Delete_archived | Forge_status | Pr_defaults -> []
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
    | Get_outline { session } -> [ "session", Json.str session ]
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
    | Forge_login { host; kind; method_ } ->
      [ "host", Json.str host ]
      @ Option.value_map kind ~default:[] ~f:(fun k -> [ "kind", Json.str k ])
      @ Option.value_map method_ ~default:[] ~f:(fun m ->
        [ "method", Json.str m ])
    | Forge_logout host -> [ "host", Json.str host ]
    | Pr_list { state } ->
      Option.value_map state ~default:[] ~f:(fun s -> [ "state", Json.str s ])
    | Pr_view number | Pr_checkout number -> [ "number", Json.int number ]
    | Pr_create { title; body; base; head; draft; push } ->
      [ "title", Json.str title; "body", Json.str body ]
      @ Option.value_map base ~default:[] ~f:(fun b -> [ "base", Json.str b ])
      @ Option.value_map head ~default:[] ~f:(fun h -> [ "head", Json.str h ])
      @ [ "draft", Json.bool draft; "push", Json.bool push ]
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
