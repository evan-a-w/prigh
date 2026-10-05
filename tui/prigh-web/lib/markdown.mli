open! Core

(** Markdown as a tree: the subset language models write (CommonMark-ish
    blocks, GitHub tables, task lists and strikethrough). Total: any input
    parses. Single newlines inside a paragraph are line breaks.

    [partial]: the text is a prefix still being streamed, so spans left open
    at its end (emphasis, code, links) are shown as if closed and a lone
    marker at the very end is hidden. *)

module Inline : sig
  type t =
    | Text of string
    | Code of string
    | Strong of t list
    | Emph of t list
    | Strike of t list
    | Link of
        { href : string (** http, https or mailto only *)
        ; children : t list
        }
    | Break
  [@@deriving sexp_of, equal]

  val parse : ?partial:bool -> string -> t list
  val to_plain : t list -> string
end

module Align : sig
  type t =
    | Default
    | Left
    | Center
    | Right
  [@@deriving sexp_of, equal]
end

module Block : sig
  type t =
    | Paragraph of Inline.t list
    | Heading of int * Inline.t list
    | Code of
        { lang : string
        ; text : string
        ; closed : bool (** false while the closing fence is still to come *)
        }
    | Quote of t list
    | List of
        { start : int option (** [Some n] for an ordered list *)
        ; tight : bool (** no blank lines between items *)
        ; items : item list
        }
    | Rule
    | Table of
        { aligns : Align.t list
        ; header : Inline.t list list
        ; rows : Inline.t list list list
        }

  and item =
    { checked : bool option (** a task list item *)
    ; blocks : t list
    }
  [@@deriving sexp_of, equal]
end

val parse : ?partial:bool -> string -> Block.t list

(** The first non-blank line as plain text, without markup: for one-line
    previews. *)
val preview : string -> string

(** Whether [href] is safe to link to: http, https or mailto. *)
val safe_href : string -> bool
