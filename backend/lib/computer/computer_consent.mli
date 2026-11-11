open! Core
open! Import

(** The user's consent to computer use in a session: the first [computer]
    action asks the user (a [Tool_confirm] of kind
    {!Confirm_kind.Computer_use}, in every approval mode), and while the
    grant lasts the next ones run without asking. It is given for one tool
    host and ends when control does: the user takes over the screen, the run
    ends or is aborted, or the session moves to another host ({!Agent}
    revokes it). *)

type t

(** Not granted. *)
val create : unit -> t

(** The host (its id) whose screen may be controlled, if any. *)
val granted : t -> string option

val is_granted : t -> host:string -> bool
val grant : t -> host:string -> unit

(** Ends the grant; [true] when there was one. *)
val revoke : t -> bool

(** {2 What the model is told when computer use is not allowed} *)

(** The user said no, maybe saying why. *)
val declined : host:string -> reason:string option -> string

(** Nobody answered within [after] seconds. *)
val timed_out : host:string -> after:float -> string

(** Nobody can answer, for the reason given (e.g. no frontend is
    connected). *)
val unavailable : host:string -> string -> string

(** The notice when the question timed out. *)
val timed_out_notice : host:string -> after:float -> string
