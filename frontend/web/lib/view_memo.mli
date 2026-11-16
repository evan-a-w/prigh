open! Core
open! Import

(** A part of the page kept from one render to the next while what it shows
    is the same (its [deps] are [equal]): the same [Node.lazy_] node, which
    virtual_dom neither renders nor diffs again (see {!View_cache}). Each [t]
    is one place in the page and keeps only its last node. *)

type 'deps t

(** [name] is what {!made} reports. *)
val create : string -> 'deps t

val find
  :  'deps t
  -> deps:'deps
  -> equal:('deps -> 'deps -> bool)
  -> f:(unit -> Node.t)
  -> Node.t

(** The parts made again (rendered, not reused) since the last call, by
    name, in the order they were made: for tests. *)
val made : unit -> string list
