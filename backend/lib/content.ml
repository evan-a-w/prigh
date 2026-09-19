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

module Thinking = struct
  (* [signature] is provider-opaque data that must be echoed back with the
     block (Anthropic signatures, OpenAI encrypted reasoning items). *)
  type t =
    { text : string
    ; signature : string option [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  | Text of string
  | Thinking of Thinking.t
  | Tool_call of Tool_call.t
[@@deriving sexp, jsonaf, equal]

(* Sessions written before signatures existed store [["Thinking", "<text>"]]. *)
let t_of_jsonaf (json : Json.t) =
  match json with
  | `Array [ `String "Thinking"; `String text ] ->
    Thinking { text; signature = None }
  | _ -> t_of_jsonaf json
;;

let thinking ?signature text = Thinking { text; signature }
