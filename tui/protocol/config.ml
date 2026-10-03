open! Core

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : string option
  }
[@@deriving sexp_of, equal]

let default =
  { scoped_models = []
  ; confirm_tools = false
  ; default_model = None
  ; default_thinking = None
  }
;;

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
  let%bind confirm_tools =
    match Json.field j "confirm_tools" with
    | None -> Ok false
    | Some `True -> Ok true
    | Some `False -> Ok false
    | Some _ -> Or_error.error_string "confirm_tools must be a boolean"
  in
  let optional_string name =
    match Json.field j name with
    | None | Some `Null -> Ok None
    | Some (`String s) -> Ok (Some s)
    | Some _ -> Or_error.errorf "%s must be a string" name
  in
  let%bind default_model = optional_string "default_model" in
  let%map default_thinking = optional_string "default_thinking" in
  { scoped_models; confirm_tools; default_model; default_thinking }
;;

let to_json t =
  let optional = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  `Object
    [ "scoped_models", `Array (List.map t.scoped_models ~f:(fun s -> `String s))
    ; ("confirm_tools", if t.confirm_tools then `True else `False)
    ; "default_model", optional t.default_model
    ; "default_thinking", optional t.default_thinking
    ]
;;
