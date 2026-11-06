open! Core

(** A [tree] component's paths as the rows of a file tree: every directory a
    path implies gets a row, and each directory's entries follow it in the
    order they first appear. *)

module Row : sig
  type 'a t =
    { depth : int
    ; name : string (** a directory's ends in [/] *)
    ; path : string (** from the root, as given (implied directories end in [/]) *)
    ; dir : bool
    ; info : 'a option (** the entry's, [None] for an implied directory *)
    }
  [@@deriving sexp_of]
end

val rows : (string * 'a) list -> 'a Row.t list
