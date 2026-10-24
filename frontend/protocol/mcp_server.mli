open! Core

(** A configured MCP server, as [list_mcp] lists it. *)

module Status : sig
  type t =
    | Ready
    | Failed
    | Needs_approval (** a project server the user has not approved *)
  [@@deriving sexp_of, equal]
end

module Tool : sig
  type t =
    { name : string (** as the model sees it: [mcp__<server>__<tool>] *)
    ; description : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { name : string
  ; source : string (** the config file that defines it *)
  ; project : bool (** from a project's [.mcp.json] *)
  ; status : Status.t
  ; error : string option (** when [Failed] *)
  ; tools : Tool.t list (** empty unless [Ready] *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
