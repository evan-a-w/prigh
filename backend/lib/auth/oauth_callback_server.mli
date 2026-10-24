open! Core
open! Import

(** A one-shot loopback HTTP server that receives the OAuth redirect
    ([?code=...&state=...]) and shows a small confirmation page. Requests
    with the wrong path, a missing code or a mismatched state are answered
    with an error page and the server keeps waiting. *)

module Result_ : sig
  type t =
    { code : string
    ; state : string
    }
  [@@deriving sexp_of]
end

type t

(** Fails if the port is busy. The server lives as long as [sw]. *)
val start
  :  sw:Switch.t
  -> env:Env.t
  -> ?host:string
  -> port:int
  -> path:string
  -> expected_state:string
  -> unit
  -> t Or_error.t

val port : t -> int

(** Blocks until a valid callback arrives. *)
val wait : t -> Result_.t

module For_testing : sig
  module Outcome : sig
    type t =
      | Success of Result_.t
      | Failure of
          { status : int
          ; message : string
          }
  end

  val classify : path:string -> expected_state:string -> string -> Outcome.t
end
