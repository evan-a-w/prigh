open! Core

type t =
  | Agent_start
  | Agent_end of Message.t list
  | Turn_start
  | Turn_end of
      { assistant : Message.Assistant.t
      ; tool_results : Message.Tool_result.t list
      }
  | Message_start of Message.t
  | Message_update of
      { partial : Message.Assistant.t
      ; delta : Delta.t
      }
  | Message_end of Message.t
  | Tool_start of Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_end of
      { call : Tool_call.t
      ; result : Message.Tool_result.t
      }
  | State of State.t
  | Compacted of string
  | Notice of string
  | Auth of Auth_event.t
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
