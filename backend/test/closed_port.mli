open! Core

(** A loopback TCP port that refuses connections at once, for tests of what
    happens when nothing listens there. A fixed low port such as 1 will not
    do: some hosts drop connections to it instead (WSL2 with mirrored
    networking does, outside the ephemeral range), so they time out. This
    one is bound but never listening for as long as it is held, so the
    kernel refuses connections to it and nothing else can take it. *)

(** Runs [f] with the port held. *)
val with_ : (int -> 'a) -> 'a

(** [s] with [:PORT] of every port held replaced by [:CLOSED]. *)
val mask : string -> string
