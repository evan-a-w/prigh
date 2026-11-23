open! Core

(** What a backend deploys, from its [hello] reply: absent (a backend with
    the harness alone, or from before engines) is the harness alone. *)
type t =
  { engines : string list (** the first is new sessions' default *)
  ; adapters : string list (** the composer's, the first its default *)
  }
[@@deriving sexp_of, equal]

val harness_only : t

(** The composer is one of them: new sessions may choose an engine and an
    adapter ([new_session]'s [engine] and [adapter]). *)
val has_choice : t -> bool

val of_hello_json : Json.t -> t Or_error.t

(** What a new session can run on: the harness, or the composer with one of
    its adapters. *)
module Choice : sig
  type t =
    { engine : string
    ; adapter : string option
    }
  [@@deriving sexp_of, equal]

  (** What [/new] and [/engine] take: the adapter ([codex], [claude],
      [acp:NAME]), else the engine ([harness]). *)
  val key : t -> string

  (** "Codex", "Claude Code", "prigh harness". *)
  val label : t -> string

  (** One line on what runs the session. *)
  val describe : t -> string

  val of_session : Session_engine.t -> t

  (** [new_session]'s [engine] and [adapter]. *)
  val params : t -> (string * Json.t) list
end

(** Every choice, in the order the backend lists its engines and adapters. *)
val choices : t -> Choice.t list

(** What a session can switch to ([set_adapter]): the composer's adapters,
    and the harness engine when it runs too (none without adapters). *)
val switch_targets : t -> Choice.t list

(** The choice [arg] names (its key or label, or a prefix naming one); else
    an error listing them. *)
val find : t -> string -> Choice.t Or_error.t

val find_in : Choice.t list -> string -> Choice.t Or_error.t
