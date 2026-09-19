open! Core
open! Import

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
      ; delta : Assistant_event.t
      }
  | Message_end of Message.t
  | Tool_start of Content.Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_end of
      { call : Content.Tool_call.t
      ; result : Message.Tool_result.t
      }
[@@deriving sexp_of]
