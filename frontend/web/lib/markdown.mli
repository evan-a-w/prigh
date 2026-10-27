open! Core

(** Markdown as a tree: the subset language models write (CommonMark-ish
    blocks, GitHub tables, task lists and strikethrough) and TeX math. Total:
    any input parses. Single newlines inside a paragraph are line breaks.

    Math is [$..$] or [\(..\)] inline, [$$..$$] or [\[..\]] for display
    (inline, or on lines of their own), outside code. A [$] opens math only
    before a non-space and not after a letter or digit, and closes it only
    after a non-space and not before a letter or digit (pandoc's rule,
    stricter), so that prices ([$5 and $10], [US$3]) and shell variables
    ([$HOME/$PATH]) stay text; [\$] is a dollar. Math left open is text, so
    a reply streaming its math shows it as written until it closes.

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
    | Math of
        { tex : string
        ; display : bool (** [$$..$$] or [\[..\]], not [$..$] or [\(..\)] *)
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
    | Math of string (** display math on lines of its own *)
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

(** The first line of text, without markup: for one-line previews. *)
val preview : string -> string

(** Whether [href] is safe to link to: http, https or mailto. *)
val safe_href : string -> bool

(** Where [text] can be cut into parts that parse alone as they do together,
    however it goes on: the starts of its top-level blocks after a blank
    line, outside code, that cannot continue a list. Ascending. While a
    reply streams, the parts before the last one are complete. *)
val block_starts : string -> int list
