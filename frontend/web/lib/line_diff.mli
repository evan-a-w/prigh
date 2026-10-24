open! Core

(** A line diff of two texts (longest common subsequence), for showing an
    edit's [old_text] → [new_text]. *)

module Line : sig
  type t =
    | Same of string
    | Removed of string
    | Added of string
  [@@deriving sexp_of, equal]
end

(** Inputs over a few hundred lines each are shown as all removed then all
    added rather than diffed. *)
val diff : old:string -> new_:string -> Line.t list
