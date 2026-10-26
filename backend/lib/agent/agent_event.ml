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
  | Retry of
      { attempt : int (** the attempt about to be made, from 2 *)
      ; max_attempts : int
      ; error : string (** what failed the attempt before *)
      ; delay : float (** seconds until it is made *)
      }
  (** The provider failed transiently: the reply is requested again after
      [delay]; a [Message_start] begins the new attempt. *)
  | Tool_start of Content.Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_confirm of
      { call_id : string
      ; agent_id : string option (** a subagent's call *)
      ; name : string
      ; summary : string
      ; rules : string list
        (** what "don't ask again" adds; none when it is not offered *)
      }
  | Tool_confirm_done of
      { call_id : string
      ; agent_id : string option
      } (** answered (by any frontend), let through or withdrawn *)
  | Ask_user of
      { call_id : string
      ; questions : User_question.t list
      ; deadline : float option
        (** when it times out (seconds since the epoch), if it does *)
      }
  | Ask_user_done of
      { call_id : string
      ; timed_out : bool
      } (** answered (by any frontend), timed out or withdrawn *)
  | Tool_end of
      { call : Content.Tool_call.t
      ; result : Message.Tool_result.t
      }
  | Compacted of
      { summary : string
      ; usage : Usage.t (** of the summarising request *)
      ; cost_usd : float
      }
  | Subagent_start of
      { call_id : string
      ; agent_id : string
      ; task : string
      ; model : string
      ; tools : string list
      ; worktree : Agent_worktree.t option
        (** the worktree it works in, made for it ([subagent]'s
            [worktree]) *)
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
  | Worktrees_changed of
      { added : Session_worktree.t list
      ; removed : string list (** their paths *)
      }
  (** A tool made or removed git worktrees ([worktree], or [bash] running
      [git worktree]): {!Agent} records them on the session, a subagent's as
      that subagent's; frontends do not see this event. *)
[@@deriving sexp_of]
