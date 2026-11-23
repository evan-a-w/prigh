open! Core
open! Import

(** An ACP agent the composer can drive ([acp:<name>]): the command that
    starts it speaking ACP on its stdio. *)
type t =
  { name : string (** as in [acp:<name>] *)
  ; display_name : string
  ; command : string list (** program and arguments, never empty *)
  }
[@@deriving sexp_of, equal]

(** The agents prigh knows how to start: [opencode] ([opencode acp]),
    [gemini] ([gemini --experimental-acp]) and [goose] ([goose acp]). *)
val known : t list

val find_known : string -> t option

(** A [name=command args] spec (from [-adapters acp:name=command args]):
    the command is split on whitespace. *)
val of_spec : string -> t Or_error.t
