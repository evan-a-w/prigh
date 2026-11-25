open! Core

(** Which login an agent CLI runs the user's composer sessions on
    ([list_adapters]' and [set_adapter_login]'s [adapter_logins]): its own
    on the session's tool host, or one of prigh's (theirs, or one shared
    with them), which prigh hands it for the session. Only the adapters that
    can take one of prigh's are listed ([codex]: ChatGPT logins). *)

module Choice : sig
  type t =
    { ref_ : string (** ["work"], ["~alice/work"] *)
    ; label : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { adapter : string
  ; provider : string (** whose logins it can take: ["openai-codex"] *)
  ; login : Choice.t option (** [None]: the CLI's own *)
  ; choices : Choice.t list
  }
[@@deriving sexp_of, equal]

(** From a reply's [adapter_logins] ([\[\]] when absent: an older backend). *)
val list_of_json : Json.t -> t list Or_error.t

(** For a provider whose terms frown on prigh handing its logins to the CLI
    (Anthropic): a line to show where the user chooses one. *)
val warning : t -> string option

(** ["ChatGPT"], ["Claude Pro/Max"]: whose logins it takes. *)
val provider_name : t -> string

(** ["Codex's own login"], ["ChatGPT (prigh) · Work"]. *)
val describe : t -> string

(** A login named as the user typed it: a ref or label of [choices]
    (case-insensitive), or ["own"]. [Ok None]: the CLI's own. The error
    lists the choices. *)
val parse_choice : t -> string -> Choice.t option Or_error.t
