open! Core
open! Import

(** Whether Claude Code is installed and logged in on a host, from the
    output of commands that change nothing: [claude --version] and
    [claude auth status] (JSON; exits 1 when logged out). *)

module Auth : sig
  type t =
    | Logged_in of
        { method_ : string option (** ["claude.ai"], ["api_key"], ... *)
        ; email : string option
        }
    | Logged_out
    | Unknown of string (** the output could not be read *)
  [@@deriving sexp_of, equal]
end

(** ["2.1.330 (Claude Code)"] -> ["2.1.330"]. *)
val parse_version : string -> string option

(** [claude auth status]'s stdout and exit code. *)
val parse_auth_status : stdout:string -> exit_code:int -> Auth.t

(** What to tell the user, or [None] when it is ready to use. *)
val problem
  :  host:string
  -> version:string option
  -> auth:Auth.t
  -> string option
