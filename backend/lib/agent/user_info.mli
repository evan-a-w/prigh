open! Core
open! Import

(** What the user wrote about themselves for the model (who they are, how
    they like to work): unlike {!Memory}'s notes, which the model looks up
    when it needs them, this is in the context of every session
    ([System_prompt.build] for a new one, a note on the next user message
    for one that knows another text: {!Told_environment}). Only the user
    changes it. [~/.prigh/user_info.md] under the world's home, so one per
    namespace with [-tokens]: plain text, read on every {!get}, so that an
    edit by hand counts. Make one per world. *)

type t

(** [~/.prigh/user_info.md] under [home]. *)
val path : home:string -> string

val create : home:string -> t

(** [f] runs after every change made through this value. *)
val subscribe : t -> f:(unit -> unit) -> unit

(** The longest text, in bytes. *)
val max_bytes : int

(** The text, stripped; empty when there is none. A file edited by hand
    beyond {!max_bytes} is cut there. *)
val get : t -> string

(** [get], [None] when there is none. *)
val text : t -> string option

(** Replaces the text (stripped), atomically; an empty one removes the
    file. Refused beyond {!max_bytes}. *)
val set : t -> string -> unit Or_error.t
