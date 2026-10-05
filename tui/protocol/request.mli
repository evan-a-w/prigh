open! Core

type t =
  { id : int
  ; method_ : string
  ; params : (string * Json.t) list
  }
[@@deriving sexp_of]

(** Typed constructors for methods whose parameters are worth spelling out. *)
module Method : sig
  type t =
    | List_skills
    | List_mcp of { reconnect : bool } (** restart failed/dead servers *)
    | Mcp_approve of
        { source : string (** the [.mcp.json] that defines the server *)
        ; server : string
        }
  [@@deriving sexp_of, equal]

  val name : t -> string
  val params : t -> (string * Json.t) list
end

val create : id:int -> Method.t -> t
val to_json : t -> Json.t
val to_line : t -> string
