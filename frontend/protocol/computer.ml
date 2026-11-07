open! Core

type t =
  { backend : string
  ; detail : string option
  ; platform : string option
  ; width : int
  ; height : int
  ; scaled_width : int
  ; scaled_height : int
  ; scale : float
  }
[@@deriving sexp_of, equal]

let of_json j =
  let int name =
    match Json.field j name with
    | Some (`Number n) ->
      Option.value
        (Option.first_some
           (Int.of_string_opt n)
           (Option.map (Float.of_string_opt n) ~f:Float.iround_nearest_exn))
        ~default:0
    | _ -> 0
  in
  let str name =
    match Json.field j name with
    | Some (`String s) -> Some s
    | _ -> None
  in
  match j with
  | `Object _ ->
    Ok
      { backend = Option.value (str "backend") ~default:"unknown"
      ; detail = str "detail"
      ; platform = str "platform"
      ; width = int "width"
      ; height = int "height"
      ; scaled_width = int "scaled_width"
      ; scaled_height = int "scaled_height"
      ; scale =
          (match Json.field j "scale" with
           | Some (`Number n) -> Option.value (Float.of_string_opt n) ~default:1.
           | _ -> 1.)
      }
  | _ -> Or_error.error_string "computer must be an object"
;;
