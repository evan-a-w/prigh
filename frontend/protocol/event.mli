open! Core

module Subagent_result : sig
  type t =
    { text : string
    ; is_error : bool
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Frame_kind : sig
  type t =
    | Binary (** [data] is base64 *)
    | Text
  [@@deriving sexp_of, equal]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
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
  | Retry of Retry.t
  | Tool_start of Tool_call.t
  | Tool_output of
      { call_id : string
      ; chunk : string
      }
  | Tool_end of
      { call : Tool_call.t
      ; result : Message.Tool_result.t
      }
  | Tool_confirm of Pending.Confirm.t
  | Tool_confirm_done of
      { call_id : string
      ; agent_id : string option
      } (** answered by any frontend, let through or withdrawn: close it *)
  | Ask_user of Pending.Ask.t
  | Ask_user_done of
      { call_id : string
      ; timed_out : bool
        (** nobody answered in time: the agent carries on (a [Notice] says
            so) *)
      } (** answered (by any frontend), timed out or withdrawn *)
  | State of State.t
  | Compacted of string
  | Masked of
      { masked : int (** how many messages *)
      ; tokens_saved : int (** estimated *)
      }
  (** old tool outputs were replaced with placeholders to make room in the
      context (observation masking) *)
  | Config_changed of Config.t
  | Review of Review.t (** the session's review changed *)
  | Notice of string
  | Queue_update of
      { steer : string list (** the queued texts, oldest first *)
      ; follow_up : string list
      }
  | Agent_queues of Agent_queue.t list
  (** the background subagents' queues ({!Agent_queue}), those not empty;
      after every change of one *)
  | Subagent_start of
      { call_id : string
      ; agent_id : string
      ; task : string
      ; model : string
      ; tools : string list
      ; worktree : Agent_worktree.t option
        (** a worktree made for it, which it works in *)
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
      ; stdin : bool (** keep a stdin pipe open: a background job *)
      } (** run this tool on our machine (we are the active host) *)
  | Tool_exec_cancel of string
  | Tool_exec_input of
      { exec_id : string
      ; data : string
      ; close : bool
      } (** text for a running exec's stdin, then maybe end it *)
  | Terminal_open of
      { term_id : string
      ; key : string
      ; cwd : string
      ; cols : int
      ; rows : int
      } (** relayed to our tool host worker (we are the active host) *)
  | Terminal_frame of
      { term_id : string
      ; kind : Frame_kind.t
      ; data : string
      }
  | Terminal_close of string
  | Terminal_kill of string
  (** a session's key: it was deleted, so its terminal on our worker (if
      any) is killed; sent because our [hello] has the [terminal_kill]
      feature *)
  | Btw_delta of
      { btw_id : string
      ; delta : string
      } (** streamed answer to our own [btw] request *)
  | Generate_delta of
      { gen_id : string
      ; delta : string
      } (** streamed text of our own [generate_text] request *)
  | Sessions_changed
  (** something [list_sessions] shows changed, in any session *)
  | Prefs_changed of Prefs.Change.t list
  (** another frontend of the user changed their preferences *)
  | Nix_gc_progress of Nix_gc_progress.t
  (** how far the [nix_store_gc] we asked for got *)
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** What a frontend says about [Masked]. *)
val masked_notice : masked:int -> tokens_saved:int -> string
