open! Core
open! Import

(** A transcript as DOM: messages, thinking, tool calls with their output and
    images, and subagents' nested transcripts. *)
val view : Chat.t -> Node.t

(** A [data:] URL. *)
val image_src : Image.t -> string

(** One line about a tool call: its command, path, pattern or task. *)
val tool_summary : Tool_call.t -> string
