open! Core

module Status = struct
  type t =
    | Ready
    | Failed
    | Needs_approval
  [@@deriving sexp_of, equal]

  let of_string = function
    | "ready" -> Ok Ready
    | "failed" -> Ok Failed
    | "needs_approval" -> Ok Needs_approval
    | other -> Or_error.errorf "unknown MCP server status %S" other
  ;;
end

module Tool = struct
  type t =
    { name : string
    ; description : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind name = Json.string_field j "name" in
    let%map description = Json.string_field j "description" in
    { name; description }
  ;;
end

type t =
  { name : string
  ; source : string
  ; project : bool
  ; status : Status.t
  ; error : string option
  ; tools : Tool.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind name = Json.string_field j "name" in
  let%bind source = Json.string_field j "source" in
  let%bind project = Json.bool_field j "project" in
  let%bind status =
    Json.string_field j "status"
    >>= Status.of_string
    |> Or_error.tag ~tag:"field \"status\""
  in
  let%bind error = Json.string_opt_field j "error" in
  let%map tools = Json.optional_list_field j "tools" ~f:Tool.of_json in
  { name; source; project; status; error; tools }
;;
