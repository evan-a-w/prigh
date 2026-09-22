open! Core

module Subagent_result : sig
  type t =
    { text : string
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

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
  | Tool_confirm of
      { call_id : string
      ; name : string
      ; summary : string
      }
  | State of State.t
  | Compacted of string
  | Config_changed of Config.t
  | Notice of string
  | Queue_update of
      { steer : int
      ; follow_up : int
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
      ; result : Subagent_result.t
      }
  | Auth of Auth_event.t
  | Tool_exec of
      { exec_id : string
      ; call_id : string
      ; name : string
      ; arguments : Json.t
      ; cwd : string
      } (** run this tool on our machine (we are the active host) *)
  | Tool_exec_cancel of string
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
