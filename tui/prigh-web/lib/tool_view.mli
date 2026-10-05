open! Core
open! Import

(** A tool call as a compact card: status, what it acts on, and its output
    presented per tool (bash, read, write, edit, ls/grep/find, subagent, jobs).
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
