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
  [@@deriving sexp_of, equal]

  let name = function
    | List_skills -> "list_skills"
    | List_mcp _ -> "list_mcp"
    | Mcp_approve _ -> "mcp_approve"
    | Set_archived _ -> "set_archived"
    | Delete_sessions _ -> "delete_sessions"
    | Delete_archived -> "delete_archived"
  ;;

  let params = function
    | List_skills | List_mcp { reconnect = false } | Delete_archived -> []
    | List_mcp { reconnect = true } -> [ "reconnect", Json.bool true ]
    | Mcp_approve { source; server } ->
      [ "source", Json.str source; "server", Json.str server ]
    | Set_archived { path; archived } ->
      [ "path", Json.str path; "archived", Json.bool archived ]
    | Delete_sessions paths -> [ "paths", `Array (List.map paths ~f:Json.str) ]
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
