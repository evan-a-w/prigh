open! Core
open! Import

(** How large code shows in diffs (the review page, the chat's tool calls,
    [/diff], a pull request's changes): a preference
    ([Pref_sync.Key.code_size]) that follows the user, the page's
    [--code-size]. *)
type t =
  | Small
  | Medium
  | Large
  | Huge
[@@deriving sexp_of, equal, enumerate]

val default : t

(** "small", "medium", "large", "huge": the preference's value. *)
val name : t -> string

val of_name : string -> t option

(** The font size, in pixels. *)
val px : t -> float

(** One size bigger (1) or smaller (-1), staying within the sizes. *)
val step : t -> int -> t
