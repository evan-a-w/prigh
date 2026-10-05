open! Core
open! Import

(** Views cached per value, by physical identity, as [Node.lazy_] nodes:
    virtual_dom neither recomputes nor diffs a lazy node that is reused, so
    a long transcript costs nothing to re-render when only its end (or
    nothing in it) changed. Keys must be heap blocks (records, non-constant
    variants); entries go when their key is garbage collected. *)

type ('key, 'deps) t

val create : unit -> ('key, 'deps) t

(** The node last made for [key], if it was made with [deps] equal to these,
    otherwise a new one made by [f] (lazily: only when rendered). *)
val find
  :  ('key, 'deps) t
  -> 'key
  -> deps:'deps
  -> equal:('deps -> 'deps -> bool)
  -> f:(unit -> Node.t)
  -> Node.t
