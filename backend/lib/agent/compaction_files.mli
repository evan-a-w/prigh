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

(** [t] with the files of the tool calls in [messages] that succeeded (their
    results are not errors), each once, in the order last seen (its own
    first). Relative paths are resolved against [cwd i], the cwd the [i]th
    of [messages] was made in, when known. *)
val add_calls : ?cwd:(int -> string option) -> t -> Message.t list -> t

(** [summary] with the section listing [t]'s files ([Files read: a, b],
    [Files modified: c]); [summary] alone when there are none. Each line
    lists the 100 most recent, then how many more there are
    ([(and 3 more)]). *)
val append : string -> t -> string

(** A summary split into the model's text and the files of its section (those
    listed: not the ones a line says there are more of). *)
val of_summary : string -> string * t
