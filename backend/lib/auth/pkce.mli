open! Core

(** PKCE (RFC 7636) helpers: an S256 verifier/challenge pair plus the base64url
    and random helpers the OAuth flows share. *)

module Pair : sig
  type t =
    { verifier : string
    ; challenge : string
    }
  [@@deriving sexp_of]
end

val generate : unit -> Pair.t
val challenge_of_verifier : string -> string
val base64url : string -> string
val random_hex : int -> string
