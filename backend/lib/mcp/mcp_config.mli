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
    definition changes. *)

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
  [@@deriving sexp_of, compare]
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

(** The servers for a session in [cwd]. The closest definition of a name
    wins: the cwd's [.mcp.json], then its ancestors', then the user's. *)
val discover
  :  ?getenv:(string -> string option)
  -> cwd:string
  -> home:string
  -> unit
  -> Discovered.t

(** The server [name] as [source] defines it now. *)
val find
  :  ?getenv:(string -> string option)
  -> home:string
  -> source:string
  -> string
  -> Server.t Or_error.t

(** Always [true] for the user's own servers. *)
val is_approved : home:string -> Server.t -> bool

val approve : home:string -> Server.t -> unit Or_error.t
