open! Core
open! Import

(** [/mcp]'s arguments, shared by the terminal and browser frontends:
    [/mcp] lists the servers, [/mcp reconnect] restarts failed ones, and
    these change the servers in the user's settings ({!P.Mcp_settings}):
    [/mcp add NAME URL] (HTTP), [/mcp add NAME COMMAND [ARGS...]] (stdio;
    the command line is split like a shell's: quote arguments with spaces),
    [/mcp add {"NAME": {...}}] (Claude Code [mcpServers] JSON, e.g. to give
    [env] or [headers]), [/mcp remove NAME], [/mcp enable NAME] and
    [/mcp disable NAME]. *)

module Action : sig
  type t =
    | List
    | Reconnect
    | Change of
        { request : P.Request.Method.t
        ; done_ : string (** the notice once it succeeded *)
        }
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string

(** [rest] is what follows [/mcp]. *)
val parse : string -> Action.t

(** Splits a command line into words: whitespace separates them, single
    quotes keep text as is, double quotes and backslashes as in a shell. *)
val split_words : string -> string list Or_error.t

(** Where a server's definition is: "Settings", "~/.prigh/mcp.json"
    (abbreviated) or the project's file. *)
val source_label : home:string option -> string -> string

(** Where to change a server from [source]. *)
val edit_hint : string -> string
