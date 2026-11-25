open! Core
open! Import

(** [$probe_programs]: what a tool host has of some programs, without
    changing anything: where each is on [PATH] (or only in a login shell),
    the output of commands like [--version] and a login-status command,
    whether files exist (never their contents) and whether environment
    variables are set (never their values). For saying "codex is not
    installed on laptop, please install it" or "not signed in". *)

val op : string

module Program : sig
  type t =
    { name : string
    ; version : string list (** arguments, e.g. [["--version"]]; [[]]: none *)
    ; status : string list (** e.g. [["login"; "status"]]; [[]]: none *)
    }
  [@@deriving sexp_of]
end

module Request : sig
  type t =
    { programs : Program.t list
    ; paths : string list (** [~] is the host's home *)
    ; env : string list
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

module Command_output : sig
  type t =
    { exit : Process.Exit.t
    ; stdout : string
    ; stderr : string
    }
  [@@deriving sexp_of]
end

module Found : sig
  type t =
    { name : string
    ; path : string option
    ; login_shell_path : string option (** only when [path] is [None] *)
    ; version : Command_output.t option
    ; status : Command_output.t option
    }
  [@@deriving sexp_of]
end

module File : sig
  type t =
    { path : string (** with [~] expanded *)
    ; exists : bool
    ; size : int option
    ; mode : int option
    }
  [@@deriving sexp_of]
end

module Reply : sig
  type t =
    { programs : Found.t list
    ; paths : File.t list
    ; env : (string * bool) list
    }
  [@@deriving sexp_of]

  val of_result : Tool.Result.t -> t Or_error.t
end

(** How long each command may run. *)
val command_timeout : Time_ns.Span.t

(** Runs [$probe_programs] on this machine, with the commands in parallel. *)
val execute
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Json.t
  -> unit
  -> Tool.Result.t
