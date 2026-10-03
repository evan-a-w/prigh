open! Core
open! Import

(** The output side of tmux control mode ([tmux -C attach]): raw pane output,
    one reply block per command this client sent (in order), and the client's
    exit. *)

module Event : sig
  type t =
    | Output of string (** pane bytes, unescaped *)
    | Reply of (string list, string list) Result.t
    (** the lines of a [%begin] ... [%end] block, or of a [%error] one *)
    | Exit
  [@@deriving sexp_of, equal]
end

type t

val create : unit -> t

(** Feeds bytes read from the control client and returns the events completed
    by them. Blocks for commands from other clients and unknown notifications
    are dropped. *)
val feed : t -> string -> Event.t list

(** Command lines typing [data] into [target] as raw bytes ([send-keys -H]),
    a bounded number of bytes per line. *)
val send_keys : target:string -> string -> string list
