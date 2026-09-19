open! Core

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind scoped_models =
    match Json.field j "scoped_models" with
    | None -> Ok []
    | Some (`Array items) ->
      Or_error.all (List.map items ~f:Json.to_string_or_error)
    | Some _ ->
      Or_error.error_string "scoped_models must be an array of strings"
  in
  let%map confirm_tools =
    match Json.field j "confirm_tools" with
    | None -> Ok false
    | Some `True -> Ok true
    | Some `False -> Ok false
    | Some _ -> Or_error.error_string "confirm_tools must be a boolean"
  in
  { scoped_models; confirm_tools }
;;

let to_json t =
  `Object
    [ "scoped_models", `Array (List.map t.scoped_models ~f:(fun s -> `String s))
    ; ("confirm_tools", if t.confirm_tools then `True else `False)
    ]
;;
