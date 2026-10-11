open! Core

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
  ; default_cwd : string option
  ; archive_after_days : float
  }
[@@deriving sexp_of, equal]

let default =
  { scoped_models = []
  ; confirm_tools = false
  ; default_model = None
  ; default_thinking = None
  ; fallback_models = []
  ; default_cwd = None
  ; archive_after_days = 7.
  }
;;

let of_json j =
  let open Or_error.Let_syntax in
  let strings name =
    match Json.field j name with
    | None -> Ok []
    | Some (`Array items) ->
      Or_error.all (List.map items ~f:Json.to_string_or_error)
    | Some _ -> Or_error.errorf "%s must be an array of strings" name
  in
  let%bind scoped_models = strings "scoped_models" in
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
  let%bind default_thinking = optional_string "default_thinking" in
  let%bind fallback_models = strings "fallback_models" in
  let%bind default_cwd = optional_string "default_cwd" in
  let%map archive_after_days =
    match Json.field j "archive_after_days" with
    | None | Some `Null -> Ok default.archive_after_days
    | Some (`Number n) ->
      Or_error.try_with (fun () -> Float.of_string n)
      |> Or_error.tag ~tag:"archive_after_days must be a number"
    | Some _ -> Or_error.error_string "archive_after_days must be a number"
  in
  { scoped_models
  ; confirm_tools
  ; default_model
  ; default_thinking
  ; fallback_models
  ; default_cwd
  ; archive_after_days
  }
;;

let to_json t =
  let optional = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  let strings l = `Array (List.map l ~f:(fun s -> `String s)) in
  `Object
    [ "scoped_models", strings t.scoped_models
    ; ("confirm_tools", if t.confirm_tools then `True else `False)
    ; "default_model", optional t.default_model
    ; "default_thinking", optional t.default_thinking
    ; "fallback_models", strings t.fallback_models
    ; "default_cwd", optional t.default_cwd
    ; "archive_after_days", `Number (sprintf "%g" t.archive_after_days)
    ]
;;
