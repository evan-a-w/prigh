(** Effects run after a delay (a hover card's open and close delays), one
    pending per key: scheduling again, or cancelling, drops the pending one.
    Outside a browser (in tests) the effect runs at once. *)

open! Import

val schedule : key:string -> ms:float -> unit Effect.t -> unit Effect.t
val cancel : key:string -> unit Effect.t
