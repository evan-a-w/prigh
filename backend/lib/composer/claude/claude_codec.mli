open! Core
open! Import

(** Claude Code's stream-json and control protocol, as a state machine with
    no I/O: one per [claude] process ({!Claude_launch}). Lines from the
    process's stdout go to {!handle_line}, which yields neutral
    {!Adapter_event.t}s, lines to write back to its stdin (answers the
    protocol requires at once), the answers to our requests, and in-band
    MCP requests for prigh's injected tools. The builders return the lines
    that send our requests and answers.

    Claude Code has no turn ids: a turn starts with the first output after
    the previous one ended (a prompt, or a background task waking the
    session) and ends with its [result], or when the process exits
    ({!process_exited} settles whatever is open). Cost is reported per turn
    though Claude Code's is cumulative per process. *)

type t

val create : unit -> t

(** The session id Claude Code reported last ([system/init]); persist it to
    [--resume]. It changes on a fork and on [/clear]. *)
val session_id : t -> string option

(** Whether a turn is running (started and not yet ended). *)
val in_turn : t -> bool

module Account : sig
  type t =
    { email : string option
    ; organization : string option
    ; subscription_type : string option
    ; token_source : string option
    ; api_key_source : string option
    ; api_provider : string option
    }
  [@@deriving sexp_of]

  (** Not logged in only when nothing says so: no token, no API key, no
      email, no subscription, and Anthropic itself as the provider (Bedrock
      and Vertex sign in elsewhere). *)
  val logged_in : t -> bool
end

module Model_info : sig
  type t =
    { value : string (** what [--model] / [set_model] take *)
    ; display_name : string
    ; description : string option
    }
  [@@deriving sexp_of]
end

module Command_info : sig
  type t =
    { name : string
    ; description : string option
    }
  [@@deriving sexp_of]
end

module Init_info : sig
  type t =
    { version : string option
    ; account : Account.t option
    ; models : Model_info.t list
    ; commands : Command_info.t list
    }
  [@@deriving sexp_of]
end

(** The answer to one of our control requests. *)
module Response : sig
  module Command : sig
    type t =
      | Initialize
      | Interrupt
      | Set_permission_mode
      | Set_model
      | Set_max_thinking_tokens
    [@@deriving sexp_of, equal]
  end

  type t =
    | Initialized of Init_info.t
    | Done of Command.t
    | Failed of
        { command : Command.t
        ; message : string
        }
  [@@deriving sexp_of]
end

(** An MCP JSON-RPC message for one of the [sdk] servers we declared; answer
    with {!mcp_reply}. *)
module Mcp_request : sig
  type t =
    { request_id : string
    ; server : string
    ; message : Json.t
    }
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    | Event of Adapter_event.t
    | Send of string (** a line to write to the process's stdin now *)
    | Response of Response.t
    | Mcp of Mcp_request.t
  [@@deriving sexp_of]
end

(** One line of the process's stdout. Unknown messages are ignored;
    unparseable lines and unknown control requests are answered or
    reported, never left hanging. *)
val handle_line : t -> string -> Output.t list

(** The process exited (or its stream ended): ends a running turn,
    [Interrupted] if we asked for that, else [Failed] with [Crashed], and
    settles open tool calls and requests. *)
val process_exited : t -> code:int option -> stderr_tail:string -> Output.t list

(** {2 Our requests} Each returns the line to write. *)

(** The first line to send: registers the in-band MCP servers by name. *)
val initialize : t -> sdk_mcp_servers:string list -> string

(** A user turn (queued by Claude Code if one is running). Slash commands
    are text: see {!compact}. *)
val user_message
  :  t
  -> ?uuid:string
  -> text:string
  -> images:Image.t list
  -> unit
  -> string

val compact : t -> instructions:string option -> string
val interrupt : t -> string
val set_permission_mode : t -> Claude_permission_mode.t -> string

(** [None]: Claude Code's default model. *)
val set_model : t -> string option -> string

(** [None]: thinking off. *)
val set_max_thinking_tokens : t -> int option -> string

(** {2 Answers to Claude Code's requests} *)

(** Answer an [Approval_request]. [Allow_always] adds Claude Code's own
    suggested rules for the rest of the session; [Cancel] denies and stops
    the turn. Errors if the request is not pending (answered, withdrawn or
    unknown). *)
val respond_approval
  :  t
  -> request_id:string
  -> Adapter_event.Approval_reply.t
  -> string Or_error.t

(** Answer a [User_question] ([AskUserQuestion]): the answers go back
    keyed by question text, several picks joined with [", "]; anything
    but [Answered] denies the call ([Cancelled] also stops the turn). *)
val answer
  :  t
  -> request_id:string
  -> User_question.Reply.t
  -> string Or_error.t

(** Answer an {!Mcp_request} with the MCP JSON-RPC response (or [None] for
    a notification's empty acknowledgement). *)
val mcp_reply : request_id:string -> Json.t option -> string
