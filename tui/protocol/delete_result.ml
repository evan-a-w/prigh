open! Core

module Deleted = struct
  type t =
    { path : string
    ; id : string
    ; bytes : int
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind path = Json.string_field j "path" in
    let%bind id = Json.string_field j "id" in
    let%map bytes = Json.int_field j "bytes" in
    { path; id; bytes }
  ;;
end

module Refused = struct
  type t =
    { path : string
    ; error : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind path = Json.string_field j "path" in
    let%map error = Json.string_field j "error" in
    { path; error }
  ;;
end

type t =
  { deleted : Deleted.t list
  ; refused : Refused.t list
  }
[@@deriving sexp_of, equal]

let list j name ~f =
  match Json.field j name with
  | None | Some `Null -> Ok []
  | Some (`Array items) -> Or_error.all (List.map items ~f)
  | Some _ -> Or_error.errorf "%s must be an array" name
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind deleted = list j "deleted" ~f:Deleted.of_json in
  let%map refused = list j "refused" ~f:Refused.of_json in
  { deleted; refused }
;;

let summary t =
  let deleted =
    match t.deleted with
    | [] -> "Deleted nothing"
    | deleted ->
      sprintf
        "Deleted %d session%s (%s)"
        (List.length deleted)
        (if List.length deleted = 1 then "" else "s")
        (Session_summary.size_string
           (List.sum (module Int) deleted ~f:(fun d -> d.bytes)))
  in
  String.concat
    ~sep:"\n"
    (deleted :: List.map t.refused ~f:(fun r -> "Not deleted: " ^ r.error))
;;
