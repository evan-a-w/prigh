open! Core

(** The YAML frontmatter of a markdown file: the block between a first line
    [---] and the next [---] (or [...]). Only what skill files use is
    understood: top-level [key: value] pairs whose values are plain,
    single- or double-quoted scalars, possibly continued on indented lines,
    or [|]/[>] block scalars. Nested values are kept as their raw text. *)

type t = (string * string) list [@@deriving sexp_of]

(** The fields and the body after the frontmatter; a file without
    frontmatter is all body. *)
val split : string -> t * string

val find : t -> string -> string option
