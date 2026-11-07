open! Core
open! Import

(** What kind of frontend the user is on: [hello]'s [client]. Only prigh-web
    displays [render_ui]'s components. *)

type t =
  | Web (** prigh-web *)
  | Tui (** prigh-tui *)
  | Acp (** an editor over [prigh acp] *)
  | Other (** a client that did not say, or one we do not know *)
[@@deriving sexp, jsonaf, equal, enumerate]

(** [web], [tui], [acp], [other]. *)
val to_string : t -> string

(** [hello]'s [client]: absent or unknown is [Other]. *)
val of_hello : string option -> t

(** How the model and the user are told of it: [prigh-web], [prigh-tui],
    [an editor (ACP)], [another client]. *)
val describe : t -> string

val shows_components : t -> bool
