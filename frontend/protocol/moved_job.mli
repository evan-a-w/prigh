open! Core

(** A foreground [bash] call that was still running after the backend's
    [auto_background_seconds] and became a background job, read from the
    call's result ("still running after 30s, so it was moved to the
    background as job j3 ..."). *)
type t =
  { id : string (** the job id, [j<n>] *)
  ; after : string (** how long it had run, e.g. [30s] *)
  ; output : string (** its output when it moved (at most its last lines) *)
  }
[@@deriving sexp_of, equal]

val parse : string -> t option
