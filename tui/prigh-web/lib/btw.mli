open! Core

(** A side question ([/btw]): answered from the conversation so far while the
    agent keeps working, streamed into a panel, never added to the session. *)

module Status : sig
  type t =
    | Streaming
    | Done
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; question : string
  ; answer : string
  ; status : Status.t
  }
[@@deriving sexp_of, equal]

val create : id:string -> question:string -> t

(** A streamed piece of the answer. *)
val append : t -> string -> t

(** The final answer replaces the streamed one. *)
val finish : t -> answer:string -> t

val fail : t -> string -> t
