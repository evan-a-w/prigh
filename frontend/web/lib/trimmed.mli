open! Core
open! Import

(** What the page shows of the messages trimmed from the session's context
    ([State.trim]): folded into the divider at the transcript's start, or
    shown above it, read-only ([get_trimmed]); and the trims and undos it
    waits on the backend for. *)

module Shown : sig
  type t =
    | Folded
    | Loading
    | Shown of Chat.t (** oldest first, as [get_trimmed] gave them *)
  [@@deriving sexp_of]
end

type t =
  { shown : Shown.t
  ; restoring : bool (** an [untrim] is on its way *)
  ; reveal : string option
    (** this page trimmed before this message (an entry id): once the
      transcript is reloaded starting there, it shows the divider *)
  }
[@@deriving sexp_of]

val empty : t
