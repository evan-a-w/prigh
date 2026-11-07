open! Core
open! Import

(** The tool host's side of computer use: [prigh-computer], the helper
    that captures the screen and sends input (desktop/computer), driven
    with JSON lines over its stdin and stdout. A host started with
    [-computer] probes it once ([--probe]: is there a screen it can control,
    how large) to say so in [hello], and spawns it on the first action,
    keeping it for the next ones (and spawning it again if it died). *)

(** One running helper: a request line in, its reply line out. *)
module Transport : sig
  type t =
    { send : string -> unit (** raises when the helper is gone *)
    ; receive : unit -> string option (** [None]: the helper exited *)
    ; kill : unit -> unit
    }
end

type t

(** [$PRIGH_COMPUTER], else [prigh-computer] (on [$PATH]). *)
val program : unit -> string

(** [$PRIGH_COMPUTER_ARGS], split on spaces. *)
val extra_args : unit -> string list

(** Runs the helper with [--probe]. The error says what to do (install it,
    or which desktops are not supported yet). *)
val probe : env:Env.t -> Computer_info.t Or_error.t

(** A helper started by [start] when first needed. *)
val create : Computer_info.t -> start:(unit -> Transport.t) -> t

(** {!probe}, then {!create} with the real helper (on [sw]). *)
val enable : env:Env.t -> sw:Switch.t -> t Or_error.t

val info : t -> Computer_info.t

(** Runs the [computer] tool's call (its arguments, {!Computer_action}): one
    action at a time. The result's text says what was done and where on the
    screen, then the cursor and, last, ["screenshot W×H"]; the screenshot is
    its image. When the helper saw the user's own input it is an error
    starting with {!interrupted_prefix}. Cancelling kills the helper (the
    next call starts another). *)
val run : t -> cancel:Cancellation.t -> Json.t -> Tool_result.t

(** ["[interrupted]"] *)
val interrupted_prefix : string

(** Whether a [computer] result is the user taking over (the kill switch). *)
val interrupted : Tool_result.t -> bool

(** The tool result for an action and the helper's reply to it. *)
val result_of_reply : Computer_action.t -> Json.t -> Tool_result.t
