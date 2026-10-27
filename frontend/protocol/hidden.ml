open! Core

module Kind = struct
  type t =
    | Trim_before
    | Trim_after of { through : string }
    | Deleted
  [@@deriving sexp_of, equal]
end

type t =
  { entry : string
  ; kind : Kind.t
  ; message : string
  ; messages : int
  ; tokens : int
  ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
  ; divider : int option [@sexp.option]
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind entry = Json.string_field j "entry" in
  let%bind kind =
    match%bind Json.string_field j "kind" with
    | "trim_before" -> Ok Kind.Trim_before
    | "trim_after" ->
      let%map through = Json.string_field j "through" in
      Kind.Trim_after { through }
    | "deleted" -> Ok Kind.Deleted
    | kind -> Or_error.error_s [%message "unknown hidden range" kind]
  in
  let%bind message = Json.string_field j "message" in
  let%bind messages = Json.int_field j "messages" in
  let%bind tokens = Json.int_field j "tokens" in
  let%bind at = Json.time_ms_opt_field j "at" in
  let%map divider =
    match Json.field j "divider" with
    | None | Some `Null -> Ok None
    | Some _ -> Or_error.map (Json.int_field j "divider") ~f:Option.some
  in
  { entry; kind; message; messages; tokens; at; divider }
;;

let field j = Json.optional_list_field j "hidden" ~f:of_json

let is_trim t =
  match t.kind with
  | Trim_before | Trim_after _ -> true
  | Deleted -> false
;;

let trims_after =
  List.filter ~f:(fun t ->
    match t.kind with
    | Trim_after _ -> true
    | Trim_before | Deleted -> false)
;;

let deleted_label t =
  if t.messages <= 1 then "Message deleted" else sprintf "%d messages deleted" t.messages
;;
