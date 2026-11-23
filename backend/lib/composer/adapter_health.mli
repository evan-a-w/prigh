open! Core
open! Import

(** Whether a composer adapter's CLI can run on a tool host: installed
    (which version) and logged in, found out passively with
    [$probe_programs] ({!Host_probe}): the CLI's [--version] and its own
    login-status command ([codex login status], [claude auth status]),
    never a login, a session, an MCP server or a turn. We never install
    anything or log anyone in: a problem says what to run on that host. *)

module Install : sig
  type t =
    | Installed of
        { version : string option
        ; path : string
        }
    | Not_installed of { login_shell_path : string option }
    (** a login shell finds it there, but not the tool host's [PATH] *)
    | Unknown of string (** the host could not say, and why *)
  [@@deriving sexp_of, equal]
end

module Auth : sig
  type t =
    | Logged_in of { account : string option (** e.g. ["ChatGPT"], an email *) }
    | Logged_out
    | Not_needed (** prigh's own harness *)
    | Unknown (** the CLI has no status command we know, or it said nothing *)
  [@@deriving sexp_of, equal]
end

module Status : sig
  type t =
    { adapter : Adapter_id.t
    ; host : string (** the host's id *)
    ; host_name : string
    ; install : Install.t
    ; auth : Auth.t
    ; checked_at : float (** Unix seconds *)
    }
  [@@deriving sexp_of, equal]

  (** What stops the adapter from running on that host and what to do, e.g.
      ["Codex is not installed on laptop, please install it"]; [None] when
      nothing we know of does. *)
  val problem : t -> string option

  (** [{adapter, label, host, host_name, installed, version?, path?,
      login_shell_path?, auth ("logged_in" | "logged_out" | "not_needed" |
      "unknown"), account?, problem?, checked_at}] *)
  val to_json : t -> Json.t
end

(** The program an adapter runs ([codex], [claude], an ACP agent's
    command); [None] for [Prigh], which needs none. *)
val program : Adapter_id.t -> string option

(** The [$probe_programs] request for [adapters] (one call per host). *)
val request : Adapter_id.t list -> Host_probe.Request.t

(** The statuses from a [$probe_programs] result on [host] (named
    [host_name]): one per adapter, [Unknown] for them all when the host
    could not run it (an older prigh, gone). *)
val of_result
  :  Adapter_id.t list
  -> host:string
  -> host_name:string
  -> checked_at:float
  -> Tool.Result.t
  -> Status.t list

(** Probes [adapters] on one host: [exec] runs a host operation there. *)
val probe
  :  exec:(name:string -> Json.t -> Tool.Result.t)
  -> Adapter_id.t list
  -> host:string
  -> host_name:string
  -> now:float
  -> Status.t list

(** The latest statuses, by host and adapter. *)
module Cache : sig
  type t

  (** Statuses older than [ttl] seconds are probed again when asked for. *)
  val create : ttl:float -> t

  val find : t -> host:string -> Adapter_id.t -> Status.t option
  val fresh : t -> now:float -> host:string -> Adapter_id.t -> bool
  val set : t -> Status.t list -> unit

  (** On a host's (re)connection: its CLIs may have changed. *)
  val forget_host : t -> host:string -> unit

  (** After a CLI failed to start there. *)
  val forget : t -> host:string -> Adapter_id.t -> unit
end
