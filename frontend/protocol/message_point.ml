open! Core

module Role = struct
  type t =
    | User
    | Assistant
    | Tool_result
  [@@deriving sexp_of, equal]
end

type t =
  { message : string
  ; index : int
  ; role : Role.t
  ; text : string
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
  ; delete : Trim.Freed.t option [@sexp.option]
  ; trim_after : Trim.Freed.t option [@sexp.option]
  }
[@@deriving sexp_of, equal]

let effect j name =
  match Json.field j name with
  | None | Some `Null -> Ok None
  | Some e -> Or_error.map (Trim.Freed.of_json e) ~f:Option.some
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind message = Json.string_field j "message" in
  let%bind index = Json.int_field j "index" in
  let%bind role =
    match%bind Json.string_field j "role" with
    | "user" -> Ok Role.User
    | "assistant" -> Ok Role.Assistant
    | "tool_result" -> Ok Role.Tool_result
    | role -> Or_error.error_s [%message "unknown role" role]
  in
  let%bind text = Json.string_field j "text" in
  let%bind at = Json.time_ms_opt_field j "at" in
  let%bind delete = effect j "delete" in
  let%map trim_after = effect j "trim_after" in
  { message; index; role; text; at; delete; trim_after }
;;

let list_of_json j = Json.list_field j "points" ~f:of_json
