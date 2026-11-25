open! Core
open! Import

(** [/adapters]: whether the composer engine's agent CLIs (Codex, Claude
    Code, ACP agents) are installed and logged in on each connected tool
    host ([list_adapters]); [/adapters refresh] checks again. prigh only
    checks: what is missing says what to run on that host.

    [/adapters login ADAPTER [LOGIN|own]]: which login the CLI runs the
    user's sessions on, its own or one of prigh's ({!P.Adapter_login});
    without [LOGIN], a picker. *)

val usage : string

module Action : sig
  type t =
    | List of { refresh : bool }
    | Login of
        { adapter : string option (** [None]: the only one there is *)
        ; login : string option (** [None]: pick; ["own"]: its own *)
        }
  [@@deriving sexp_of, equal]
end

val parse : string -> Action.t Or_error.t

(** A heading per host, then a line per adapter and, under one that cannot
    run, what to do; then the login each CLI that can take one of prigh's
    runs on. *)
val lines : P.Adapter_status.t list -> P.Adapter_login.t list -> string list

(** [None] for the only adapter: there is one. The error names those there
    are. *)
val find_login
  :  P.Adapter_login.t list
  -> adapter:string option
  -> P.Adapter_login.t Or_error.t

(** What the user is told once the CLI's login changed. *)
val changed : P.Adapter_login.t -> string
