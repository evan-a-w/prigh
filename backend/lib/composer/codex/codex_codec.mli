open! Core
open! Import

(** Codex's app-server protocol (`codex app-server`: JSON-RPC without the
    ["jsonrpc"] field, one JSON object per line on stdio), as a state
    machine with no I/O: the builders return the lines to write to Codex's
    stdin and remember what each request was for; {!handle_line} takes a
    line Codex wrote and says what it means, as {!Adapter_event.t}s, lines
    to send back and the answers to our requests.

    One codec follows one main thread (the session's), plus the threads of
    the subagents Codex spawns from it, whose calls are reported under the
    subagent's call ([parent]). Only the main thread's [turn/completed]
    ends a turn. *)

(** What a thread and its turns run with: sent on [thread/start],
    [thread/resume] and [thread/fork], and again on every [turn/start]
    (Codex keeps a turn's overrides for the turns after it, and forgets the
    effort on resume, so every turn says them all). *)
module Settings : sig
  type t =
    { cwd : string
    ; model : string option (** [None]: Codex's default *)
    ; thinking : Thinking.t
    ; approval : Approval_mode.t
    ; plan : bool (** Codex's plan collaboration mode *)
    ; efforts : string list option
      (** the model's reasoning efforts, when Codex listed them: the
            effort sent is the one asked for if it is among them, else the
            nearest (the lowest for thinking off) *)
    ; sandbox : bool
      (** Codex's sandbox for the mode; [false] where it cannot run: none,
          Codex still asking before what it does not know to be read-only *)
    ; writable_roots : string list
      (** extra absolute directories writable in [workspaceWrite] mode
          alongside [cwd] (e.g. session worktrees under [worktrees_dir]) *)
    }
  [@@deriving sexp_of]
end

(** A tool of ours the thread can call ([dynamicTools]): its calls come as
    {!Output.Tool_call}s. *)
module Dynamic_tool : sig
  type t =
    { name : string
    ; description : string
    ; input_schema : Json.t
    }
end

(** Given only when a thread starts, resumes or forks. *)
module Thread_options : sig
  type t =
    { developer_instructions : string option
    ; mcp_servers : (string * Json.t) list
      (** name, and Codex's server config: [{command, args, env}] or [{url,
        http_headers}] *)
    ; dynamic_tools : Dynamic_tool.t list
    ; config : (string * Json.t) list (** more dotted config overrides *)
    ; ephemeral : bool
      (** [true] for one-shot completions: passes [ephemeral: true] on
          [thread/start] and omits [tools.update_plan.enabled] *)
    }

  val empty : t
end

module Input : sig
  type t =
    | Text of string
    | Image of Image.t
    | Local_image of string (** a path on Codex's machine *)
end

module Account : sig
  type t =
    | Logged_out
    | Not_needed (** a provider that needs no OpenAI login *)
    | Api_key
    | Chatgpt of
        { email : string option
        ; plan : string option
        }
    | Other of string (** ["amazonBedrock"], ["gcp"], ... *)
  [@@deriving sexp_of, equal]
end

(** A ChatGPT login's access token, handed to Codex for the process
    ([account/login/start {type: "chatgptAuthTokens"}]): Codex keeps it in
    memory and asks for a new one when refused ({!Output.Token_refresh}).
    Printed without the token. *)
module Chatgpt_tokens : sig
  type t =
    { access_token : string
    ; account_id : string (** [chatgpt_account_id] *)
    ; plan : string option (** [None]: Codex reads it from the token *)
    }
  [@@deriving sexp_of]
end

module Model_info : sig
  type t =
    { id : string
    ; display_name : string
    ; description : string
    ; is_default : bool
    ; hidden : bool
    ; efforts : string list (** Codex's names: "low", "medium", ... *)
    ; default_effort : string option
    ; images : bool
    }
  [@@deriving sexp_of]
end

(** What one of our requests was. *)
module Request_kind : sig
  type t =
    | Initialize
    | Thread_start
    | Thread_resume
    | Thread_fork
    | Turn_start
    | Turn_steer
    | Turn_interrupt
    | Compact
    | Revert
    | Model_list
    | Account_read
    | Rate_limits_read
    | Login
  [@@deriving sexp_of, equal]
end

module Response : sig
  type t =
    | Initialized of
        { user_agent : string
        ; version : string option
        }
    | Thread of
        { kind : Request_kind.t (** start, resume or fork *)
        ; thread_id : string
        ; model : string option
        }
    | Turn_accepted of { turn_id : string }
    | Steered of { turn_id : string }
    | Models of
        { models : Model_info.t list
        ; next_cursor : string option
        }
    | Account of Account.t
    | Rate_limits of Subscription_usage.Status.t
    (** [account/rateLimits/read]: the login's usage limits *)
    | Logged_in (** Codex took the tokens of {!login_tokens} *)
    | Done of Request_kind.t
    (** interrupt, compact, revert: nothing more to say *)
    | Failed of
        { kind : Request_kind.t
        ; code : int
        ; message : string
        }
  [@@deriving sexp_of]
end

(** A call of one of the tools we gave the thread ([dynamicTools]): answer it
    with {!reply_tool_call}. *)
module Tool_call : sig
  type t =
    { request_id : string
    ; call_id : string
    ; tool : string
    ; arguments : Json.t
    }
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    | Event of Adapter_event.t
    | Send of string (** a line for Codex's stdin *)
    | Response of Response.t
    | Tool_call of Tool_call.t
    | Token_refresh of
        { request_id : string
        ; previous_account_id : string option
        }
    (** Codex was refused with the tokens it has (a 401) and waits, 10s at
        most, for new ones: answer with {!reply_token_refresh} *)
  [@@deriving sexp_of]
end

type t

val create : unit -> t

(** The version in [initialize]'s [userAgent] (["prigh/0.166.0 (Linux;
    ...)"] is ["0.166.0"]). *)
val version_of_user_agent : string -> string option

(** [initialize]; its response makes {!handle_line} send [initialized]. *)
val initialize : t -> client_name:string -> client_version:string -> string

val start_thread : t -> Settings.t -> Thread_options.t -> string

val resume_thread
  :  t
  -> thread_id:string
  -> Settings.t
  -> Thread_options.t
  -> string

(** A new thread with [thread_id]'s history up to [last_turn_id] (all of it
    when [None]). The fork becomes this codec's thread when it starts. *)
val fork_thread
  :  t
  -> thread_id:string
  -> last_turn_id:string option
  -> Settings.t
  -> Thread_options.t
  -> string

(** Errors without a thread, or while a turn runs. *)
val start_turn : t -> Settings.t -> Input.t list -> string Or_error.t

(** Adds input to the running turn ([turn/steer]); errors when no turn has
    started (send the input as the next turn instead). *)
val steer : t -> Input.t list -> string Or_error.t

(** Stops the running turn. A turn Codex has not started yet cannot be
    interrupted: the request goes out (from {!handle_line}) once it starts.
    The turn ends with its [turn/completed] ([Turn_end] [Interrupted]), not
    with the response. *)
val interrupt : t -> [ `Send of string | `Deferred | `Idle ]

(** Cancels any pending approval or user-question requests from Codex,
    returning their protocol cancel replies and [Request_withdrawn] events. *)
val cancel_pending : t -> Output.t list

(** [thread/compact/start]: Codex compacts in a turn of its own (events as
    for {!start_turn}'s, ending with [Turn_end]). *)
val compact : t -> string Or_error.t

(** Drops the turn [before_turn_id] and those after it from the thread's
    history (not from the files). *)
val revert : t -> before_turn_id:string -> string Or_error.t

val list_models : t -> cursor:string option -> string

(** [account/read] without refreshing the token: passive. *)
val read_account : t -> string

(** [account/login/start {type: "chatgptAuthTokens"}]: the process runs on
    these tokens (in memory: Codex writes no [auth.json]) until it is given
    others; answered [Logged_in], or [Failed] by a Codex without the mode
    (it is experimental) or one that forces another login. *)
val login_tokens : t -> Chatgpt_tokens.t -> string

(** [account/rateLimits/read]: the ChatGPT login's usage limits. *)
val read_rate_limits : t -> string

(** The login's usage limits as Codex last said ([account/rateLimits/read]
    or [account/rateLimits/updated]). *)
val limits : t -> Subscription_usage.Status.t option

(** [None] when no such request is waiting (answered, or withdrawn). *)
val reply_approval
  :  t
  -> request_id:string
  -> Adapter_event.Approval_reply.t
  -> string option

val answer_question
  :  t
  -> request_id:string
  -> User_question.Reply.t
  -> string option

val reply_tool_call
  :  t
  -> request_id:string
  -> output:string
  -> success:bool
  -> string option

(** Answers a {!Output.Token_refresh}; [Error] tells Codex why there are
    none. [None] when no such request is waiting. *)
val reply_token_refresh
  :  t
  -> request_id:string
  -> Chatgpt_tokens.t Or_error.t
  -> string option

val handle_line : t -> string -> Output.t list
val thread_id : t -> string option

(** The running turn's id, once Codex has said it. *)
val turn_id : t -> string option
