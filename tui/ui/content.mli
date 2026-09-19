open! Core

(** Styled text, the platform-neutral rendering target. The terminal maps spans
    to notty attributes; the web maps them to DOM nodes. *)

module Span : sig
  type t =
    { text : string
    ; style : Style.t
    }
  [@@deriving sexp_of, equal]
end

module Line : sig
  type t = Span.t list [@@deriving sexp_of, equal]

  val of_string : ?style:Style.t -> string -> t
  val width : t -> int
  val to_plain : t -> string

  (** Wraps at display width, breaking at spaces when possible. Always returns
      at least one line. *)
  val wrap : t -> width:int -> t list

  val truncate : t -> width:int -> t

  (** Case-insensitive; matching substrings take [Style.invert]. *)
  val highlight : t -> needle:string -> t
end

type t = Line.t list [@@deriving sexp_of, equal]

val text : ?style:Style.t -> string -> t

(** One line per [\n]-separated piece, all in [style]. *)
val lines : ?style:Style.t -> string -> t

val to_plain : t -> string

(** Debug dump: style runs become [[red]…[/]], [[bold]…[/]], [[link=url]…[/]]. *)
val to_styled : t -> string

val wrap : t -> width:int -> t
