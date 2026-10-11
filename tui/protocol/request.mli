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
    | Set_archived of
        { path : string (** a session's path or id *)
        ; archived : bool
        }
    | Delete_sessions of string list (** paths; see {!Delete_result} *)
    | Delete_archived (** every session shown as archived *)
    | Get_messages_page of
        { session : string (** a path or id; need not be the client's *)
        ; before : string option
          (** {!Message_page.before}; [None]: the newest page *)
        ; limit : int
        } (** see {!Message_page} *)
    | Get_tool_output of
        { call_id : string
        ; agent_id : string option (** in this subagent's transcript *)
        } (** a whole output ([{text, is_error}]) that was {!Elided} *)
  [@@deriving sexp_of, equal]

  val name : t -> string
  val params : t -> (string * Json.t) list
end

val create : id:int -> Method.t -> t
val to_json : t -> Json.t
val to_line : t -> string
