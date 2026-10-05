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
  [@@deriving sexp_of, equal]

  let name = function
    | List_skills -> "list_skills"
    | List_mcp _ -> "list_mcp"
    | Mcp_approve _ -> "mcp_approve"
  ;;

  let params = function
    | List_skills | List_mcp { reconnect = false } -> []
    | List_mcp { reconnect = true } -> [ "reconnect", Json.bool true ]
    | Mcp_approve { source; server } ->
      [ "source", Json.str source; "server", Json.str server ]
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
