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
  | Subagent_start of
      { call_id : string
      ; agent_id : string
      ; task : string
      ; model : string
      ; tools : string list
      }
  | Subagent of
      { call_id : string
      ; agent_id : string
      ; event : t
      }
  | Subagent_end of
      { call_id : string
      ; agent_id : string
      ; usage : Usage.t
      ; turns : int
      ; cost_usd : float
      ; result : Tool_result.t
      }
[@@deriving sexp_of]
