open! Core

module Kind = struct
  type t =
    | Message of Message.t
    | Model of
        { model : string
        ; thinking : string
        }
    | Compaction of
        { summary : string
        ; kept_from : string
        }
    | Name of { name : string }
    | Description of { text : string }
    | Cwd of { cwd : string }
    | System_prompt
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; parent : string option
  ; kind : Kind.t
  }
[@@deriving sexp_of, equal]

let kind_of_json j =
  let open Or_error.Let_syntax in
  match%bind Json.string_field j "kind" with
  | "message" ->
    Or_error.map
      (Json.object_field j "message" >>= Message.of_json)
      ~f:(fun m -> Kind.Message m)
  | "model" ->
    let%bind model = Json.string_field j "model" in
    let%map thinking = Json.string_field j "thinking" in
    Kind.Model { model; thinking }
  | "compaction" ->
    let%bind summary = Json.string_field j "summary" in
    let%map kept_from = Json.string_field j "kept_from" in
    Kind.Compaction { summary; kept_from }
  | "name" ->
    Or_error.map (Json.string_field j "name") ~f:(fun name ->
      Kind.Name { name })
  | "description" ->
    Or_error.map (Json.string_field j "text") ~f:(fun text ->
      Kind.Description { text })
  | "cwd" ->
    Or_error.map (Json.string_field j "cwd") ~f:(fun cwd -> Kind.Cwd { cwd })
  | "system_prompt" -> Ok Kind.System_prompt
  | other -> Or_error.errorf "unknown entry kind %S" other
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind parent = Json.string_opt_field j "parent" in
  let%map kind = kind_of_json j in
  { id; parent; kind }
;;
