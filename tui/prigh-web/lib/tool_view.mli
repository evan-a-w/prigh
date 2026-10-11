open! Core
open! Import

(** A tool call as a compact card: status, what it acts on, and its output
    presented per tool (bash, read, write, edit, ls/grep/find, subagent, jobs,
    web_fetch, job_input).
    [streaming]: the call's arguments are still arriving. [running]: the agent
    is working, so a call without a result is still to get one. [nested]
    renders a subagent's transcript. *)
val view
  :  nested:(Chat.t -> Node.t)
  -> streaming:bool
  -> running:bool
  -> Tool_call.t
  -> Chat.Tool.t option
  -> Node.t

(** A tool the provider ran (web search) as a card like a tool call's.
    [running]: its message is still streaming, so a missing output is still
    to come. *)
val server_tool : running:bool -> Content.Server_tool.t -> Node.t

(** A subagent's latest step: its current tool call, or the last line of its
    text. *)
val last_activity : Chat.t -> string option
