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
      { delta : Assistant_event.t
      ; block : int (** the index in the content of the block [delta] changed *)
      ; stream : int (** the streamed message's id, unique in the process *)
      ; seq : int (** [delta]'s number in the stream, from 1 *)
      ; partial : unit -> Message.Assistant.t [@sexp.opaque]
        (** the whole message so far (through [seq] only while the event is
            emitted), for a frontend that has to catch up *)
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
  | Agent_waiting of
      { agent : string (** who: "Codex", "Claude Code" *)
      ; starting : bool (** its process is starting: not asked yet *)
      }
  (** The composer's agent CLI is starting, or was asked and has said
      nothing since: frontends show they wait for it, as they do for the
      model after the harness's [Message_start]. *)
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
      ; kind : Confirm_kind.t
      }
  | Tool_confirm_done of
      { call_id : string
      ; agent_id : string option
      } (** answered (by any frontend), let through or withdrawn *)
  | Tool_allowed of
      { call_id : string
      ; agent_id : string option
      ; model : string
        (** the approval model's key, or [you] ({!Allowed_call.you}) when
            the user let the same call through earlier *)
      ; reason : string
      }
  (** The approval model ([smart] mode, {!Approval_judge}) let the call run
      without asking the user, for [reason], or the exact call was let
      through earlier in the session ({!Allowed_call}); frontends note it
      on the call. *)
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
  | Masked of
      { masked : int (** how many messages *)
      ; tokens_saved : int (** estimated *)
      } (** Old tool outputs were masked ({!Observation_mask}) to make room. *)
  | Subagent_start of
      { call_id : string
      ; agent_id : string
      ; task : string
      ; name : string (** {!Subagent_name} *)
      ; model : string
      ; tools : string list
      ; host : string option
        (** the tool host its tools run on (its name), when not the
            session's active one ([subagent]'s [host]) *)
      ; worktree : Agent_worktree.t option
        (** the worktree it works in, made for it ([subagent]'s
            [worktree]) *)
      ; forked : bool
        (** it started from its caller's conversation ([subagent]'s
            [fork]) *)
      ; continued : bool
        (** an agent that ran out of turns runs again ([subagent_continue]),
            under the same ids: it ends again with totals over all its runs *)
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
  | Custom_instruction_set of
      { id : string
      ; instruction : Custom_instruction.t option (** [None]: removed *)
      }
  (** The [custom_instructions] tool changed one: {!Agent} records that the
      model knows, so it is not told back to it; frontends do not see this
      event (they hear [custom_instructions_changed]). *)
[@@deriving sexp_of]
