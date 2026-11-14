open! Core
open! Import

(** MCP servers to start, in Claude Code's [mcpServers] format: the user's
    [~/.prigh/mcp.json] and each project's [.mcp.json] (in the cwd or an
    ancestor). Strings may use [${VAR}] and [${VAR:-default}].

    {v
    { "mcpServers": {
        "github": { "type": "http", "url": "https://...", "headers": {...} },
        "fs": { "command": "npx", "args": ["-y", "pkg"], "env": {...} } } }
    v}

    A project's servers run commands that came with the project, so they
    start only once the user approves them ({!approve}, [/mcp]); approvals
    are kept in [~/.prigh/mcp-approvals.json] and lapse when the server's
    definition changes.

    The servers the user added in prigh's settings ({!Mcp_settings}) come
    from the backend with every request, as an [mcpServers] object
    ([~settings]); like the user's file they need no approval, and they
    rank below it: a project's [.mcp.json], then [~/.prigh/mcp.json], then
    the settings. *)

module Transport : sig
  type t =
    | Stdio of
        { command : string
        ; args : string list
        ; env : (string * string) list
        }
    | Http of
        { url : string
        ; headers : (string * string) list
        }
  [@@deriving compare]

  (** With the values of [env] and [headers] hidden: they hold secrets. *)
  val sexp_of_t : t -> Sexp.t
end

module Server : sig
  type t =
    { name : string
    ; source : string (** the file that defines it *)
    ; project : bool (** from a project's [.mcp.json] *)
    ; dir : string (** where it runs: the project, or the home directory *)
    ; transport : Transport.t
    ; approval : string
      (** identifies the definition as written, before variables are
          expanded, for approvals *)
    }
  [@@deriving sexp_of]

  (** Changes whenever the server would have to be started differently. *)
  val key : t -> string
end

module Discovered : sig
  type t =
    { servers : Server.t list
    ; problems : string list (** each says what to fix, and where *)
    }
  [@@deriving sexp_of]
end

val user_file : home:string -> string

(** The {!Server.source} of the servers from the settings. *)
val settings_source : string

(** The servers of an [mcpServers] object from the settings, run in
    [home]. *)
val settings_servers
  :  ?getenv:(string -> string option)
  -> home:string
  -> Json.t
  -> Discovered.t

(** The servers for a session in [cwd]. The closest definition of a name
    wins: the cwd's [.mcp.json], then its ancestors', then the user's, then
    [settings] (an [mcpServers] object). *)
val discover
  :  ?getenv:(string -> string option)
  -> ?settings:Json.t
  -> cwd:string
  -> home:string
  -> unit
  -> Discovered.t

(** The server [name] as [source] defines it now ([settings] for
    {!settings_source}). *)
val find
  :  ?getenv:(string -> string option)
  -> ?settings:Json.t
  -> home:string
  -> source:string
  -> string
  -> Server.t Or_error.t

(** Always [true] for the user's own servers. *)
val is_approved : home:string -> Server.t -> bool

val approve : home:string -> Server.t -> unit Or_error.t

(** Whether [name] can name a server: letters, digits, [-] and [_] (no
    [__]), at most 32 characters, so that [mcp__NAME__TOOL] reads back. *)
val check_name : string -> unit Or_error.t

(** Whether [spec] is a server definition, without expanding its
    variables (that happens where it runs); the error says what to fix. *)
val check : Json.t -> unit Or_error.t
