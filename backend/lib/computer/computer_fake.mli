open! Core
open! Import

(** An in-process stand-in for [prigh-computer --backend fake], speaking
    its protocol ({!Computer_helper.Transport}): a screen of [width] x
    [height] pixels whose screenshots are grey PNGs of the scaled size with
    the pointer drawn, recording the input it is sent in real pixels. For
    tests. *)

type t

val create
  :  ?width:int (** default 2560 *)
  -> ?height:int (** default 1600 *)
  -> ?max_long_side:int (** default 1280 *)
  -> ?max_pixels:int (** default 1_150_000 *)
  -> unit
  -> t

val info : t -> Computer_info.t

(** A connection to it; [kill] makes the next [send] fail, as with a dead
    helper. *)
val transport : t -> Computer_helper.Transport.t

(** The input received, oldest first, e.g. ["move 1280,624"], ["click
    left×2"], ["type hello"]. *)
val log : t -> string list

(** The user moves the pointer: the next action is refused as interrupted
    (the kill switch). *)
val user_move : t -> x:int -> y:int -> unit

(** A grey PNG of that size: what the fake's screenshots are. *)
val png : width:int -> height:int -> string
