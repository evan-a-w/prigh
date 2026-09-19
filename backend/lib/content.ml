open! Core
open! Import

module Tool_call = struct
  type t =
    { id : string
    ; name : string
    ; arguments : string
    }
  [@@deriving sexp, jsonaf, equal]

  let parse_arguments t =
    match Json.parse t.arguments with
    | Ok (`Object _ as json) -> Ok json
    | Ok `Null when String.is_empty (String.strip t.arguments) ->
      Ok (`Object [])
    | Ok other ->
      Or_error.error_s
        [%message "tool arguments must be a JSON object" (other : Json.t)]
    | Error _ when String.is_empty (String.strip t.arguments) -> Ok (`Object [])
    | Error e -> Error e
  ;;
end

type t =
  | Text of string
  | Thinking of string
  | Tool_call of Tool_call.t
[@@deriving sexp, jsonaf, equal]
