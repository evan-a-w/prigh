open! Core
open! Import

(** Short notices over the bottom of the chat: at most [max_shown], newest
    last. Errors stay until dismissed; acknowledgements go after a time that
    grows with their length. *)

type t =
  { id : int
  ; text : string
  ; error : bool
  ; repeats : int (** how many more times it was said while showing *)
  ; undo : Hidden.t option
    (** what it acknowledges, with an Undo that restores it ([restore]) *)
  }
[@@deriving sexp_of, equal]

val max_shown : int

(** How long a non-error toast stays (long enough to undo with): [None] for
    errors. *)
val lifetime_ms : t -> int option

(** [push toasts ~id ?replaces ~error text] adds a toast, collapsing one
    saying the same into it (counting the repeat; not those with an Undo,
    which undo each their own), and dropping those starting
    with [replaces]: cycling a setting shows only where it ended up. *)
val push
  :  t list
  -> id:int
  -> ?replaces:string
  -> ?undo:Hidden.t
  -> error:bool
  -> string
  -> t list * t

(** The text to show: long paths elided in the middle (the whole text goes in
    the title). *)
val short : string -> string
