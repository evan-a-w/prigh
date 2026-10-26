open! Core
open! Import

(** A path as the UI shows it: under [host_home], the home directory of the
    tool host it is on, as [~]; with that unknown (an older backend or host),
    any [/home/USER] is taken for it. *)
val of_path : ?host_home:string -> string -> string

(** The home directory [host] reported, when it is connected. *)
val host_home : State.t -> host:string -> string option

(** The home directory of the session's active tool host. *)
val active_host_home : State.t -> string option

(** The home directory of the tool host [s]'s cwd is on. *)
val session_host_home : State.t option -> Session_summary.t -> string option
