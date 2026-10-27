open! Core

type t =
  { before : string
  ; messages : int
  ; tokens : int
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind before = Json.string_field j "before" in
  let%bind messages = Json.int_field j "messages" in
  let%bind tokens = Json.int_field j "tokens" in
  let%map at = Json.time_ms_opt_field j "at" in
  { before; messages; tokens; at }
;;

let field j =
  match Json.field j "trim" with
  | None | Some `Null -> Ok None
  | Some trim -> Or_error.map (of_json trim) ~f:Option.some
;;

module Freed = struct
  type t =
    { messages : int
    ; tokens : int
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind messages = Json.int_field j "messages" in
    let%map tokens = Json.int_field j "tokens" in
    { messages; tokens }
  ;;
end

module Point = struct
  type t =
    { before : string
    ; index : int
    ; reply : bool [@sexp.bool]
    ; text : string
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    ; messages : int
    ; tokens : int
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind before = Json.string_field j "before" in
    let%bind index = Json.int_field j "index" in
    let%bind reply =
      match Json.field j "role" with
      | None | Some `Null -> Ok false
      | Some _ ->
        (match%bind Json.string_field j "role" with
         | "user" -> Ok false
         | "assistant" -> Ok true
         | role -> Or_error.error_s [%message "unknown role" role])
    in
    let%bind text = Json.string_field j "text" in
    let%bind at = Json.time_ms_opt_field j "at" in
    let%bind messages = Json.int_field j "messages" in
    let%map tokens = Json.int_field j "tokens" in
    { before; index; reply; text; at; messages; tokens }
  ;;

  let list_of_json j = Json.list_field j "points" ~f:of_json
end

let tokens_label tokens =
  if tokens >= 1000
  then (
    let k = Float.of_int tokens /. 1000. in
    if Float.( < ) k 10.
    then sprintf "~%.1fk tokens" k
    else sprintf "~%dk tokens" (Float.iround_nearest_exn k))
  else sprintf "~%d tokens" tokens
;;

let label ~messages ~tokens =
  sprintf
    "%d message%s trimmed · %s"
    messages
    (if messages = 1 then "" else "s")
    (tokens_label tokens)
;;

let trimmed_of_json j = Json.list_field j "messages" ~f:Message.of_json
