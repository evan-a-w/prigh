open! Core

module Color : sig
  type t =
    | Default
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | Gray
    | White
  [@@deriving sexp_of, equal, compare]
end

type t =
  { fg : Color.t
  ; bold : bool
  ; dim : bool
  ; italic : bool
  ; underline : bool
  ; invert : bool
  ; strike : bool
  ; link : string option
  }
[@@deriving sexp_of, equal, compare]

val plain : t
val fg : Color.t -> t
val bold : t -> t
val dim : t -> t
val italic : t -> t
val underline : t -> t
val invert : t -> t
val strike : t -> t
val link : t -> string -> t
