open! Core
open! Import

(** How the web, job-input and worktree tools and the provider's own tools (web search)
    read in a transcript; shared by [Tool_render] and prigh-web's [Tool_view]. *)

(** [text] on one line in double quotes, escaped like a JSON string ([\n], [\t], [\x07]),
    cut to [width] columns (default 40) with an ellipsis. *)
val quoted : ?width:int -> string -> string

(** [job_input]'s argument: the job id and its input, quoted. *)
val job_input : Tool_args.t -> string option

(** [worktree]'s argument: the action and its branch (and base) or path,
    e.g. [create feat/x from main], [remove ../wt (force)], [list]. *)
val worktree : Tool_args.t -> string option

(** Whether a [job_input] call ends the job's stdin. *)
val closes_stdin : Tool_args.t -> bool

(** A server tool as a tool call (its input as the arguments) and, once it
    has one, its output as the result, so that it renders like one. *)
val as_call
  :  P.Content.Server_tool.t
  -> P.Tool_call.t * P.Message.Tool_result.t option

(** A server tool's argument: [web_search]'s query, quoted; otherwise the raw
    input on one line. *)
val server_tool_arg : P.Content.Server_tool.t -> string option

(** [name arg], on one line: a server tool in a list of steps or entries. *)
val server_tool_line : P.Content.Server_tool.t -> string

module Search_result : sig
  type t =
    { title : string
    ; url : string option
    }
  [@@deriving sexp_of]

  (** The numbered lines ([1. Title — URL]) of a web search's output. *)
  val parse : string -> t list
end

module Fetched : sig
  type t =
    { url : string
    ; details : string (** [HTTP 200, text/html, 12.3 KB] *)
    ; body : string
    }
  [@@deriving sexp_of]

  (** A [web_fetch] result: its header line, [URL (details)], and the page. *)
  val parse : string -> t option
end
