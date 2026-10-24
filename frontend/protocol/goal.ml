open! Core

module Status = struct
  type t =
    | Active
    | Done
    | Stopped
  [@@deriving sexp_of, equal]

  let to_string = function
    | Active -> "active"
    | Done -> "done"
    | Stopped -> "stopped"
  ;;

  let of_string = function
    | "active" -> Ok Active
    | "done" -> Ok Done
    | "stopped" -> Ok Stopped
    | other -> Or_error.errorf "unknown goal status %S" other
  ;;
end

type t =
  { text : string
  ; status : Status.t
  ; continuations : int
  ; max_continuations : int
  ; summary : string option
  ; reason : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind text = Json.string_field j "text" in
  let%bind status = Json.string_field j "status" >>= Status.of_string in
  let%bind continuations = Json.int_field j "continuations" in
  let%bind max_continuations = Json.int_field j "max_continuations" in
  let%bind summary = Json.string_opt_field j "summary" in
  let%map reason = Json.string_opt_field j "reason" in
  { text; status; continuations; max_continuations; summary; reason }
;;
