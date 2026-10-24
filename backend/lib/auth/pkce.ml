open! Core
open! Import

let base64url s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
;;

let random_bytes n =
  Mirage_crypto_rng_unix.use_default ();
  Mirage_crypto_rng.generate n
;;

let challenge_of_verifier verifier =
  base64url
    (Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string verifier))
;;

module Pair = struct
  type t =
    { verifier : string
    ; challenge : string
    }
  [@@deriving sexp_of]
end

let generate () =
  let verifier = base64url (random_bytes 32) in
  { Pair.verifier; challenge = challenge_of_verifier verifier }
;;

let random_hex n = Ohex.encode (random_bytes n)
