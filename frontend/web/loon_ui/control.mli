(** What a form control's Bonsai [component] gives: its current value, a way
    to set it, and its view. *)

open! Import

type 'a t =
  { value : 'a
  ; set : 'a -> unit Effect.t
  ; view : Node.t
  }

(** Keeps a value in Bonsai state, and views it with the controlled [view]. *)
val make
  :  ?equal:('a -> 'a -> bool)
  -> 'a
  -> view:('a -> ('a -> unit Effect.t) -> Node.t)
  -> local_ Bonsai.graph
  -> 'a t Bonsai.t

(** Like [make], with a view that also gets a name unique to this component
    (for the elements' ids). *)
val make_named
  :  ?equal:('a -> 'a -> bool)
  -> 'a
  -> view:(name:string -> 'a -> ('a -> unit Effect.t) -> Node.t)
  -> local_ Bonsai.graph
  -> 'a t Bonsai.t
