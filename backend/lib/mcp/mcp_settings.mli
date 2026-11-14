open! Core
open! Import

(** The MCP servers the user added in prigh's settings: kept by the backend
    under the world's home ([~/.prigh/mcp-servers.json], so one list per
    namespace with [-tokens]), written only through this module, and sent
    to every tool host the user's sessions use, which starts them as it
    starts those of its own [~/.prigh/mcp.json] ({!Mcp_config}).

    The file is Claude Code's [mcpServers] format with the names of the
    disabled servers beside it, and is readable by its owner only: the
    definitions may hold secrets (tokens in [env] and [headers]), which the
    sexps here hide. Every read goes to the file, so an edit by hand counts. *)

type t

(** [~/.prigh/mcp-servers.json] under [home]. *)
val path : home:string -> string

val create : home:string -> t

(** [f] runs after every change made through this value. *)
val subscribe : t -> f:(unit -> unit) -> unit

module Entry : sig
  type t =
    { name : string
    ; enabled : bool
    ; config : Json.t (** as written, variables unexpanded *)
    }

  (** With the values of [env] and [headers] hidden. *)
  val sexp_of_t : t -> Sexp.t

  val to_json : t -> Json.t
end

(** The servers, in the order they were added. *)
val list : t -> Entry.t list Or_error.t

(** The enabled servers as an [mcpServers] object, for the tool hosts
    (none when the file cannot be read). *)
val enabled : t -> Json.t

(** The servers in a pasted text: an [mcpServers] object, a whole config
    holding one, or its entries without the outer braces (["name": {...}]).
    Each is checked ({!Mcp_config.check_name}, {!Mcp_config.check}). *)
val parse : string -> (string * Json.t) list Or_error.t

(** Adds the servers (all or none), enabled. Refused when a name is taken,
    by another of them or one already there. *)
val add : t -> (string * Json.t) list -> Entry.t list Or_error.t

(** Replaces the definition of [name], renaming it to [rename] if given;
    it stays enabled or disabled. *)
val update
  :  t
  -> name:string
  -> ?rename:string
  -> Json.t
  -> Entry.t list Or_error.t

val remove : t -> name:string -> Entry.t list Or_error.t
val set_enabled : t -> name:string -> bool -> Entry.t list Or_error.t
