open! Core

(** HTML to readable, markdown-ish text for [web_fetch]: headings as [#],
    lists as [-]/[1.], links as [[text](url)] (resolved against [base]),
    [pre] as fenced code, tables as [|]-separated rows, block quotes with
    [>]. Scripts, styles, forms' controls, [svg] and the like are dropped,
    and so are [nav] menus; when the page has a [main] (else an [article])
    element only that is kept. Tolerates any input: unclosed tags, stray
    [<], unknown entities. *)

module Page : sig
  type t =
    { title : string option
    ; text : string
    }
  [@@deriving sexp_of]
end

val convert : ?base:Uri.t -> string -> Page.t

(** Decodes character references ([&amp;], [&#233;], [&#x2014;], the
    common named ones); unknown ones are kept as written. *)
val decode_entities : string -> string
