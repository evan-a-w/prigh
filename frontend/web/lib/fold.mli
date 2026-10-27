open! Core
open! Import

(** Which tool outputs show in full. Normal verbosity folds every output to a
    few lines, verbose shows them all; the user toggles one (its call id, or
    a delivered job's id) from that default, and [scope] tells apart the
    same call ids in subagents' transcripts. Also which kinds of the
    transcript show at all ([/filter]), in subagents' transcripts too. *)

type t [@@deriving sexp_of]

(** [toggled]: keys ([key]) the user unfolded, or in verbose folded.
    [filter]: everything, by default. *)
val create
  :  ?filter:Transcript_filter.t
  -> verbosity:Prigh_ui.Verbosity.t
  -> toggled:String.Set.t
  -> unit
  -> t

val filter : t -> Transcript_filter.t
val shows : t -> Transcript_filter.Kind.t -> bool

(** Normal verbosity, nothing toggled. *)
val default : t

(** For the transcript of the subagent [agent_id] (at any depth: its id is
    unique). *)
val scoped : t -> string -> t

(** [id]'s key in this scope, as [Toggle_fold] takes it. *)
val key : t -> string -> string

val expanded : t -> string -> bool
val equal : t -> t -> bool

(** The attribute holding the key a [button] toggles: the page listens for
    clicks on it. *)
val attribute : string

(** The button that toggles [id]: saying how to show all [total] lines, of
    which [shown] show while folded, or how to fold them again. *)
val button : t -> string -> total:int -> shown:int -> Node.t
