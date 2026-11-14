open! Core
open! Import

(** Links to sessions, as the backend's session tools and the model write
    them: [\[title\](?session=ID)]. Relative, so they work on whatever
    address the user reaches prigh-web at; [ID] may be a unique prefix of
    the session's id (the 8 characters the UI shows). *)

(** [?session=ID] *)
val href : string -> string

(** The session a link goes to: [?session=ID] (with other parameters too),
    [/?session=ID] or [prigh://session/ID]; not an absolute http URL (only
    prigh-web knows its own origin). *)
val of_href : string -> string option

(** How a link's or a session list's sessions are shown: the name, else the
    description or first prompt. *)
val title : P.Session_summary.t -> string

(** The sessions the user can see (the session list), to resolve ids and
    their prefixes against. *)
module Known : sig
  type t

  val empty : t
  val create : P.Session_summary.t list -> t

  (** Changes when the sessions' ids or titles do: for caches of what was
      drawn with them. *)
  val stamp : t -> int

  module Lookup : sig
    type t =
      | Found of P.Session_summary.t (** the id, or the only one it starts *)
      | Ambiguous of P.Session_summary.t list
      | Missing of { closest : P.Session_summary.t option }
      (** deleted, or not the user's; [closest]: an id one or two
          characters away, a likely typo *)
    [@@deriving sexp_of]
  end

  val find : t -> string -> Lookup.t
end

(** Where [text] names known sessions by id: words of 8 to 16 lowercase hex
    digits that are an id or the unique prefix of one, as
    [(pos, len, session)]. Other hex (a commit's hash, an unknown id) is
    left alone. *)
val ids : Known.t -> string -> (int * int * P.Session_summary.t) list
