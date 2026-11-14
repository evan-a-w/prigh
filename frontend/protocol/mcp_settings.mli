open! Core

(** The MCP servers in the user's settings ([get_mcp_settings], the replies
    of the methods that change them, and the [mcp_settings_changed] event):
    kept by the backend and started on every tool host the user's sessions
    use. Their definitions may hold secrets (in [env] and [headers]): the
    sexps hide those values. *)

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
  [@@deriving equal]

  (** With the values of [env] and [headers] hidden. *)
  val sexp_of_t : t -> Sexp.t

  (** In Claude Code's [mcpServers] format. *)
  val to_json : t -> Json.t

  val of_json : Json.t -> t Or_error.t

  (** The command line or the URL, in a line. *)
  val summary : t -> string
end

module Server : sig
  type t =
    { name : string
    ; enabled : bool
    ; transport : Transport.t
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

type t = Server.t list [@@deriving sexp_of, equal]

(** [{servers: [...]}]. *)
val of_json : Json.t -> t Or_error.t

val find : t -> string -> Server.t option

(** The {!Mcp_server.source} of the servers from the settings. *)
val source : string
