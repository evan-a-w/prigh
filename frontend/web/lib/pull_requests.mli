open! Core
open! Import

(** The session's repository's pull requests ([/pr]): a list to filter by
    state and fuzzily by text, while it loads, when it could not be listed,
    and once listed. *)

module Filter : sig
  type t =
    | Open
    | Closed
    | All
  [@@deriving sexp_of, equal, enumerate]

  (** [open], [closed], [all]: [pr_list]'s [state]. *)
  val to_string : t -> string

  val of_string : string -> t option
end

module Status : sig
  type t =
    | Loading
    | Failed of string (** the backend's words: they say what to do *)
    | Loaded of Forge_repo.t
  [@@deriving sexp_of, equal]
end

type t =
  { filter : Filter.t
  ; status : Status.t
  ; prs : Pull_request.t list
  ; picker : Picker.t (** the [prs] as items, ids their numbers *)
  }
[@@deriving sexp_of, equal]

(** Loading, keeping [query]. *)
val create : ?query:string -> Filter.t -> t

val loaded : t -> Pr_list.t -> t
val failed : t -> string -> t

(** The highlighted pull request. *)
val selected : t -> Pull_request.t option

val find : t -> int -> Pull_request.t option

(** [#24 Git hosting · lloyd · feat/x → main · draft], as the TUI lists it. *)
val describe : Pull_request.t -> string

(** [Open], [Draft], [Merged] or [Closed]. *)
val state_label : Pull_request.t -> string

module Command : sig
  (** [/pr]'s arguments. *)
  type t =
    | List of Filter.t
    | View of int
    | New of string (** the title, or [""] *)
    | Checkout of int
  [@@deriving sexp_of, equal]

  (** The error is the usage to show. *)
  val parse : string -> (t, string) Result.t
end
