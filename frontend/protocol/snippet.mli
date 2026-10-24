open! Core

(** A named text the user inserts into the prompt editor ([/snippet]), from
    [snippets] in the backend's config. *)
type t =
  { name : string (** one word, unique *)
  ; text : string
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t

(** The [snippets] field of [set_config]. *)
val list_to_json : t list -> Json.t

(** [None] when [name] can name a snippet, else why not. *)
val name_problem : string -> string option

val find : t list -> string -> t option

(** Replaces the snippet of the same name in place, else appends. *)
val add : t list -> t -> t list

val remove : t list -> string -> t list

(** The text's first non-blank line, for lists. *)
val first_line : t -> string
