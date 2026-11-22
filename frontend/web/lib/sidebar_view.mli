open! Core
open! Import

(** Sessions: search, new, the sessions (each its project in a word over its
    title; at the right what its agent is doing, else how long ago it
    worked), context menus (right-click, Shift+F10, a long press, ⋯),
    selecting several to delete; at the foot the archived ones (folded into
    the list), the settings, pull requests, help and the signed-in user. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** What the current session's agent is doing, for its row: ["thinking…"]
    (or waiting for the model), ["retrying…"] or ["working…"] (also while
    only its subagents run); [None] when nothing runs. *)
val current_activity : App.Model.t -> string option

(** The gear opening the settings ([/settings]). *)
val settings : inject:(App.Action.t -> unit Effect.t) -> Node.t

(** {!Tilde.of_path}: [HOME/x] as [~/x], [host_home] being the home directory
    of the tool host the path is on. *)
val home_relative : ?host_home:string -> string -> string

(** The home directory of the model's session's active tool host, when it
    said. *)
val state_host_home : App.Model.t -> string option

(** The home directory of the tool host where [s] works, when it is
    connected. *)
val session_host_home : App.Model.t -> Session_summary.t -> string option

(** Where a session works: its repository and branch, else its directory
    ([~] being [host_home]). *)
val place
  :  ?cls:string
  -> ?host_home:string
  -> Session_summary.t
  -> cwd:string
  -> Node.t
