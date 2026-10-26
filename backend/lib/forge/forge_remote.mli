open! Core

(** Where a git remote's URL points: [https://HOST/OWNER/NAME(.git)],
    [git@HOST:OWNER/NAME.git] or [ssh://git@HOST[:PORT]/OWNER/NAME.git].
    An HTTP URL's port is kept (it is the web port); an SSH one's is not. *)

type t =
  { host : string
  ; owner : string
  ; name : string
  }
[@@deriving sexp_of, equal]

val parse : string -> t option
