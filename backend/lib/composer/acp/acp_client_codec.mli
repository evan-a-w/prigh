open! Core
open! Import

(** The Agent Client Protocol (ACP, protocol version 1) from the client's
    side, for driving any ACP agent (OpenCode, Gemini CLI, Antigravity, ...)
    as a composer adapter: JSON-RPC 2.0, one JSON object per line on the
    agent's stdio. A state machine with no I/O: the builders return the
    lines to write to the agent's stdin and remember what each request was
    for; {!handle_line} takes a line the agent wrote and says what it means,
    as {!Adapter_event.t}s, lines to send back and the answers to our
    requests.

    It follows the spec strictly, with no agent-specific cases: what an
    agent lacks shows in its {!Agent_capabilities.t} and the session's
    {!Session_config.t}. We advertise no file system or terminal
    capabilities (the agent uses its own), but take [notice] and
    [compaction] updates.

    One codec follows one ACP session. ACP has no turn ids: a turn is one
    [session/prompt] request, named ["turn-N"]; it ends with the request's
    response ([stopReason]). Updates replayed by [session/load] (prigh keeps
    its own log) and updates for other sessions are dropped. *)

module Auth_method : sig
  type t =
    { id : string
    ; name : string
    ; description : string option
    ; terminal : bool
      (** [type: "terminal"]: the user logs in by running the agent's program
        interactively, not through [authenticate] *)
    }
  [@@deriving sexp_of, equal]
end

module Agent_capabilities : sig
  type t =
    { load_session : bool
    ; resume_session : bool (** [sessionCapabilities.resume]: no replay *)
    ; close_session : bool
    ; images : bool
    ; audio : bool
    ; embedded_context : bool
    ; mcp_http : bool
    ; mcp_sse : bool
    }
  [@@deriving sexp_of, equal]
end

module Agent_info : sig
  type t =
    { name : string
    ; title : string option
    ; version : string option
    }
  [@@deriving sexp_of, equal]
end

module Mode : sig
  type t =
    { id : string
    ; name : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]
end

module Config_option : sig
  module Choice : sig
    type t =
      { value : string
      ; name : string
      ; description : string option
      ; group : string option (** the name of its group, if grouped *)
      }
    [@@deriving sexp_of, equal]
  end

  module Value : sig
    type t =
      | Select of
          { current : string
          ; choices : Choice.t list
          }
      | Boolean of bool
    [@@deriving sexp_of, equal]
  end

  type t =
    { id : string
    ; name : string
    ; description : string option
    ; category : string option
      (** ["mode"], ["model"], ["model_config"], ["thought_level"] or another *)
    ; value : Value.t
    }
  [@@deriving sexp_of, equal]
end

(** What the agent says the session runs with, kept up to date by
    [current_mode_update] and [config_option_update]. *)
module Session_config : sig
  type t =
    { current_mode : string option
    ; modes : Mode.t list
    ; config_options : Config_option.t list
    }
  [@@deriving sexp_of, equal]

  val empty : t

  (** The first option of [category] (e.g. ["model"], ["thought_level"]). *)
  val option : t -> category:string -> Config_option.t option
end

(** The id of the [model] option that stands for the session model state
    of ACP's unstable API ([models] in the session's setup,
    [current_model_update]), when the agent has no [model] option of its
    own: {!set_config_option} on it sends [session/set_model]. *)
val session_model_option : string

module Command : sig
  type t =
    { name : string
    ; description : string
    ; input_hint : string option
    }
  [@@deriving sexp_of, equal]
end

module Mcp_server : sig
  type t =
    | Stdio of
        { name : string
        ; command : string
        ; args : string list
        ; env : (string * string) list
        }
    | Http of
        { name : string
        ; url : string
        ; headers : (string * string) list
        }
  [@@deriving sexp_of]
end

module Input : sig
  type t =
    | Text of string
    | Image of Image.t
end

(** What one of our requests was. *)
module Request_kind : sig
  type t =
    | Initialize
    | New_session
    | Load_session
    | Resume_session
    | Prompt
    | Set_mode
    | Set_config_option
  [@@deriving sexp_of, equal]
end

module Response : sig
  type t =
    | Initialized of
        { protocol_version : int
        ; agent : Agent_info.t option
        ; capabilities : Agent_capabilities.t
        ; auth_methods : Auth_method.t list
        }
    | Session_opened of
        { kind : Request_kind.t (** new, load or resume *)
        ; session_id : string
        ; config : Session_config.t
        }
    | Mode_set of string
    | Config_set of Config_option.t list
    | Failed of
        { kind : Request_kind.t
        ; code : int (** -32000: authentication required *)
        ; message : string
        }
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    | Event of Adapter_event.t
    | Send of string (** a line for the agent's stdin *)
    | Response of Response.t
    | Config_changed of Session_config.t
    | Commands of Command.t list (** the agent's slash commands *)
    | Title of string option (** the agent's title for the session *)
  [@@deriving sexp_of]
end

type t

(** [cost_total]: the running cost total ([usage_update.cost]) the session
    to resume reported last: the first live update counts only what came
    after it. *)
val create : ?cost_total:float -> unit -> t

(** The session's running cost total, as the agent reported it last. *)
val cost_total : t -> float option

(** [initialize] with protocol version 1. *)
val initialize : t -> client_name:string -> client_version:string -> string

val new_session : t -> cwd:string -> mcp_servers:Mcp_server.t list -> string

(** [session/load]: the agent replays the session's history as updates,
    which are dropped until the response. *)
val load_session
  :  t
  -> session_id:string
  -> cwd:string
  -> mcp_servers:Mcp_server.t list
  -> string

(** [session/resume] (when {!Agent_capabilities.t.resume_session}): no
    replay. *)
val resume_session
  :  t
  -> session_id:string
  -> cwd:string
  -> mcp_servers:Mcp_server.t list
  -> string

(** Starts a turn: the request's line, then [Turn_started]. Errors without a
    session, while a turn runs, or with images the agent does not take. *)
val prompt : t -> Input.t list -> Output.t list Or_error.t

(** [session/cancel] for the running turn, and [cancelled] answers to the
    permission requests it waits on (as the spec asks); the turn ends with
    its prompt's response ([Turn_end] [Interrupted]). Nothing when idle. *)
val cancel : t -> Output.t list

val set_mode : t -> mode_id:string -> string Or_error.t

(** Errors for an option the agent did not offer, or a value of the wrong
    type or not among its choices. *)
val set_config_option
  :  t
  -> config_id:string
  -> [ `Id of string | `Bool of bool ]
  -> string Or_error.t

(** The answer to a [session/request_permission], picking the option of the
    matching kind (the agent's own option ids are sent back unchanged):
    [Allow] allow_once, [Allow_always] allow_always (else allow_once),
    [Deny] reject_once (else reject_always; else [cancelled]), [Cancel]
    [cancelled]. [None] when no such request is waiting. *)
val reply_approval
  :  t
  -> request_id:string
  -> Adapter_event.Approval_reply.t
  -> string option

val handle_line : t -> string -> Output.t list

(** The agent's process ended: a running turn fails ([Crashed]) and waiting
    requests are withdrawn. [name]: how its message names the agent (default "the
    ACP agent"). *)
val process_exited
  :  ?name:string
  -> t
  -> code:int option
  -> stderr_tail:string
  -> Output.t list

val session_id : t -> string option
val turn_id : t -> string option
val in_turn : t -> bool
val agent_capabilities : t -> Agent_capabilities.t option
val session_config : t -> Session_config.t

(** Some agents never answer [session/load] once they have replayed the
    history: after a quiet spell the client takes the session as loaded
    (its id is [session_id]); a late answer is then ignored. *)
val assume_loaded : t -> session_id:string -> unit
