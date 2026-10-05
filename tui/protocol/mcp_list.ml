open! Core

type t =
  { servers : Mcp_server.t list
  ; problems : string list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind servers = Json.list_field j "servers" ~f:Mcp_server.of_json in
  let%map problems =
    Json.optional_list_field j "problems" ~f:Json.to_string_or_error
  in
  { servers; problems }
;;
