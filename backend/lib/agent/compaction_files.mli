open! Core
open! Import

(** The files a compacted conversation read and changed, recorded from its
    tool calls ([read]; [write], [edit], [apply_patch]) and appended to the
    summary as a section of its own, so the list does not depend on the
    model. Each compaction starts from the previous summary's list. *)

type t =
  { read : string list (** not also modified *)
  ; modified : string list
  }
[@@deriving sexp_of]

val empty : t

(** [t] with the files of the tool calls in [messages] after its own, each
    once, in the order first seen. *)
val add_calls : t -> Message.t list -> t

(** [summary] with the section listing [t]'s files ([Files read: a, b],
    [Files modified: c]); [summary] alone when there are none. *)
val append : string -> t -> string

(** A summary split into the model's text and the files of its section. *)
val of_summary : string -> string * t
