open! Core
open! Import

(** A named piece of text the frontends insert into the prompt editor
    ([/snippet]), kept under [snippets] in [~/.prigh/config.json]. *)

type t =
  { name : string (** non-empty, no whitespace, unique in the list *)
  ; text : string (** non-empty, may span lines *)
  }
[@@deriving sexp_of, equal]

val to_json : t -> Json.t

(** The [snippets] array; errors name the entry and say how to fix it. *)
val list_of_json : Json.t -> t list Or_error.t
