open! Core

module File = struct
  type t =
    { path : string
    ; index : int
    ; snapshot : Review.Snapshot.t
    ; current : Review.Snapshot.Content.t
    ; changed : bool
    ; added : int
    ; removed : int
    ; diff : string
    ; note : string option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind path = Json.string_field j "path" in
    let%bind index = Json.int_field j "index" in
    let%bind snapshot =
      Or_error.bind (Json.object_field j "snapshot") ~f:Review.Snapshot.of_json
    in
    let%bind current =
      Or_error.bind
        (Json.object_field j "current")
        ~f:Review.Snapshot.Content.of_json
    in
    let%bind changed = Json.bool_field j "changed" in
    let%bind added = Json.int_field j "added" in
    let%bind removed = Json.int_field j "removed" in
    let%bind diff = Json.string_field j "diff" in
    let%map note = Json.string_opt_field j "note" in
    { path; index; snapshot; current; changed; added; removed; diff; note }
  ;;
end

type t = File.t list [@@deriving sexp_of, equal]

let of_json j = Json.list_field j "files" ~f:File.of_json
