open! Core
open! Import

(** Whether a composer adapter's CLI can run on a tool host: installed
    (which version) and logged in, found out passively with
    [$probe_programs] ({!Host_probe}): the CLI's [--version] and its own
    login-status command ([codex login status], [claude auth status]),
    never a login, a session, an MCP server or a turn. An ACP agent has no
    login-status command: its login shows when it is started ({!Seen}: the
    model-list check, a session), or when it runs on one of prigh's logins.
    We never install anything or log anyone in: a problem says what to run
    on that host. *)

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

(** How we know it is logged in. *)
module Via : sig
  type t =
    | Status_command (** its own login-status command said so *)
    | Prigh_login of { label : string }
    (** it runs on one of prigh's logins, chosen in Settings → Engines *)
    | Session of { at : float }
    (** it opened a session on its own login there at [at] (Unix seconds):
        a model-list check or a session's start *)
    | Login_file of { path : string }
    (** its token file is there (Antigravity's, kept when the keyring is
        out of reach); its contents are never read *)
  [@@deriving sexp_of, equal]
end

module Auth : sig
  type t =
    | Logged_in of
        { account : string option (** e.g. ["ChatGPT"], an email *)
        ; via : Via.t
        }
    | Logged_out
    | Not_needed (** prigh's own harness *)
    | Unknown (** the CLI has no status command we know, or it said nothing *)
  [@@deriving sexp_of, equal]
end

(** What starting an ACP agent on its own login showed: [session/new]
    succeeding means it is logged in, [-32000 Authentication required] that
    it is not. *)
module Seen : sig
  type t =
    | Opened of { at : float }
    | Refused of { at : float }
    | Check_failed of
        { at : float
        ; why : string
        } (** it could not be started or did not answer: no evidence *)
  [@@deriving sexp_of, equal]

  (** The latest, by host and adapter. A [Check_failed] does not replace an
      [Opened] or [Refused]. *)
  module Table : sig
    type seen := t
    type t

    val create : unit -> t
    val record : t -> host:string -> Adapter_id.t -> seen -> unit
    val find : t -> host:string -> Adapter_id.t -> seen option
  end
end

module Status : sig
  type t =
    { adapter : Adapter_id.t
    ; host : string (** the host's id *)
    ; host_name : string
    ; install : Install.t
    ; auth : Auth.t
    ; checked_at : float (** Unix seconds *)
    ; quirks : Acp_agent.Quirks.t
      (** an ACP agent's: its login command and minimum version *)
    ; checking : bool (** its login is being checked (by starting it) *)
    ; check_failed : string option
      (** why the last check of its login could not tell *)
    }
  [@@deriving sexp_of, equal]

  (** What stops the adapter from running on that host and what to do, e.g.
      ["Codex is not installed on laptop, please install it"]; [None] when
      nothing we know of does. *)
  val problem : t -> string option

  (** Advice short of a problem: that an agent's login was not checked yet
      (or why checking failed), or how to log in to a CLI that cannot say. *)
  val hint : t -> string option

  (** An ACP agent installed there whose login nothing has shown: checking
      it (starting it briefly, {!Seen}) would tell. *)
  val checkable : t -> bool

  (** [t] with what is known beyond the probe: [prigh_login], the label of
      the prigh login it runs on (its own login on the host is then beside
      the point); [seen], what starting it showed (ACP agents only);
      [checking], whether that is under way. *)
  val with_evidence
    :  t
    -> prigh_login:string option
    -> seen:Seen.t option
    -> checking:bool
    -> t

  (** [{adapter, label, host, host_name, installed, version?, path?,
      login_shell_path?, auth ("logged_in" | "logged_out" | "not_needed" |
      "unknown"), account?, via? ("status" | "prigh_login" | "session" |
      "login_file"), prigh_login?, confirmed_at?, confirmed? (["5m ago"],
      against [now]), checking?, checkable?, problem?, hint?, checked_at}] *)
  val to_json : ?now:float -> t -> Json.t
end

(** The program an adapter runs ([codex], [claude], an ACP agent's
    command); [None] for [Prigh], which needs none. *)
val program
  :  ?acp:(string -> Acp_agent.t option)
  -> Adapter_id.t
  -> string option

(** The [$probe_programs] request for [adapters] (one call per host).
    [acp]: the ACP agent of a name (default: {!Acp_agent.find_known}; an
    unknown one runs the program of its name). *)
val request
  :  ?acp:(string -> Acp_agent.t option)
  -> Adapter_id.t list
  -> Host_probe.Request.t

(** The statuses from a [$probe_programs] result on [host] (named
    [host_name]): one per adapter, [Unknown] for them all when the host
    could not run it (an older prigh, gone). *)
val of_result
  :  ?acp:(string -> Acp_agent.t option)
  -> Adapter_id.t list
  -> host:string
  -> host_name:string
  -> checked_at:float
  -> Tool.Result.t
  -> Status.t list

(** Probes [adapters] on one host: [exec] runs a host operation there. *)
val probe
  :  ?acp:(string -> Acp_agent.t option)
  -> exec:(name:string -> Json.t -> Tool.Result.t)
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
