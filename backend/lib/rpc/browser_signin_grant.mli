open! Core

(** What a browser sign-in ({!Browser_signin}) brings back: the proxy's
    cookie and the prigh credentials the page is signed in with. Printed
    without the secrets. *)
type t =
  { proxy_cookie : string
  ; token : string option
  ; user : string option
  }
[@@deriving sexp_of]
