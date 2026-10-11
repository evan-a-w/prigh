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

module Server_tool = struct
  (* A tool the provider runs itself (web search): [input] is the JSON text
     of its arguments, [output] a readable summary of its result once it has
     one. [raw] is provider-opaque wire data echoed back only to the provider
     that wrote it. *)
  type t =
    { id : string
    ; name : string
    ; input : string
    ; output : string option [@jsonaf.option]
    ; is_error : bool [@jsonaf.default false]
    ; raw : string option [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]

  (* What a model that did not run it sees. *)
  let as_text t =
    sprintf
      "[%s %s]%s"
      t.name
      t.input
      (match t.output with
       | None -> ""
       | Some output -> "\n" ^ output)
  ;;
end

type t =
  | Text of string
  | Thinking of Thinking.t
  | Tool_call of Tool_call.t
  | Server_tool of Server_tool.t
[@@deriving sexp, jsonaf, equal]

(* Sessions written before signatures existed store [["Thinking", "<text>"]]. *)
let t_of_jsonaf (json : Json.t) =
  match json with
  | `Array [ `String "Thinking"; `String text ] ->
    Thinking { text; signature = None }
  | _ -> t_of_jsonaf json
;;

let thinking ?signature text = Thinking { text; signature }
