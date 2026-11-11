open! Core
open! Import

(** [on_host]: runs one of the agent's file and shell tools ([bash], [read],
    [write], the editor, [ls], [grep], [find]) on another connected tool
    host for one call, leaving the session on its active host; without a
    host, lists the hosts. The inner call goes through the context's
    approver and executor as a normal call would, with the context's [host]
    set and [cwd] on that host (default: its home); [bash] runs in the
    foreground there (jobs belong to the active host). *)

module Host : sig
  (** A connected tool host, as the session sees it. *)
  type t =
    { id : string
    ; name : string
    ; home : string option
    ; active : bool (** the session's active host *)
    ; computer : bool (** it runs the [computer] tool *)
    }
  [@@deriving sexp_of]
end

val name : string

(** The tools [on_host] runs, when the agent has them. *)
val runs : string list

(** [hosts]: the session's connected tool hosts. *)
val create : hosts:(unit -> Host.t list) -> unit -> Tool.t

(** The host, inner tool and its arguments (an object) an [on_host] call
    names. *)
val inner : Json.t -> (string * string * Json.t) option
