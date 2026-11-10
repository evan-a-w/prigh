open! Core

(** The paste-back code of a browser sign-in ({!Browser_signin}) for a
    program the browser cannot reach (it runs over SSH): the program makes a
    one-time X25519 key pair and puts the public key in the link
    ([&paste=...]); after Allow the page seals the grant to it and shows the
    code, which the user pastes into the program's terminal.

    The format ([prigh1:], the page's [paste_seal.js] makes it):
    [base64url(ephemeral public key (32) || nonce (12) || AES-256-GCM
    ciphertext and tag)], the key HKDF-SHA256 of the X25519 shared secret
    (salt: the ephemeral then the program's public key; info
    ["prigh1 cli-signin"]), the state as associated data. The plaintext is
    JSON: [{"cookie", "token", "user"}].

    A code opens only with this key pair's private key (which never leaves
    memory) and this sign-in's state, and only once, before it expires. *)

type t

(** A fresh key pair for the sign-in with [state], whose code is taken until
    [expires]. [secret]: the private key's 32 bytes (for tests). *)
val create : ?secret:string -> state:string -> expires:Time_ns.t -> unit -> t

(** The public key, base64url without padding (as in the link). *)
val public_key : t -> string

module Error : sig
  type t =
    | Not_a_code (** no [prigh1:], not base64url, or too short *)
    | Not_for_this_signin
    (** it does not open: another sign-in's code, or changed on the way *)
    | Bad_contents (** it opens, but holds no proxy cookie *)
    | Expired
    | Used (** this sign-in already took a code *)
  [@@deriving sexp_of]

  (** What went wrong and what to do next. *)
  val to_string_hum : t -> string
end

(** The grant in [code] (surrounding and inner whitespace, from a wrapped
    paste, is ignored). Takes it only once. *)
val open_
  :  t
  -> now:Time_ns.t
  -> string
  -> (Browser_signin_grant.t, Error.t) Result.t
