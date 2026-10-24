open! Core

(** Whether the user is looking at us, as far as the terminal or the page
    says: terminals that do not report focus stay [Unknown]. *)
type t =
  | Unknown
  | Focused
  | Unfocused
[@@deriving sexp_of, equal]
