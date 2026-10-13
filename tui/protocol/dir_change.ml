open! Core

module If_missing = struct
  type t =
    | Fail
    | Ask
    | Create
  [@@deriving sexp_of, equal]

  let to_json = function
    | Fail -> Json.str "fail"
    | Ask -> Json.str "ask"
    | Create -> Json.str "create"
  ;;
end

module Target = struct
  type t =
    | Cwd
    | Host of
        { id : string
        ; name : string
        }
  [@@deriving sexp_of, equal]
end

module Outcome = struct
  type t =
    | Changed of
        { cwd : string option
        ; created : bool
        }
    | Missing of string
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    match%bind Json.string_opt_field j "missing" with
    | Some path -> Ok (Missing path)
    | None ->
      let%bind cwd = Json.string_opt_field j "cwd" in
      let%map created = Json.bool_default_field j "created" ~default:false in
      Changed { cwd; created }
  ;;
end
