open! Core
open! Import

(** The settings page's MCP servers section: the servers in the user's
    settings ({!Mcp_settings}, from [get_mcp_settings] and
    [mcp_settings_changed]), how they and the host's own servers are doing on
    the session's host ([list_mcp]), the server being added or edited, the
    pasted [mcpServers] JSON, and the server whose removal waits for a yes.
    They belong to the user (switching starts over). *)

module Status : sig
  type t =
    | Unknown (** not asked *)
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

(** An environment variable or a header, as typed: its value is masked until
    shown. *)
module Pair : sig
  type t =
    { key : string
    ; value : string
    ; shown : bool
    }
  [@@deriving sexp_of, equal]
end

module Kind : sig
  type t =
    | Stdio (** a command the host runs *)
    | Http (** a URL *)
  [@@deriving sexp_of, equal]
end

module Draft : sig
  type t =
    { editing : string option (** the server edited; [None]: a new one *)
    ; name : string
    ; kind : Kind.t
    ; command : string
    ; args : string (** one per line *)
    ; url : string
    ; pairs : Pair.t list (** [env] (stdio) or [headers] (HTTP) *)
    ; saving : bool (** sent, no reply yet *)
    ; error : string option (** why it was not sent or was refused *)
    }
  [@@deriving sexp_of, equal]
end

module Paste : sig
  type t =
    { text : string
    ; saving : bool
    ; error : string option
    }
  [@@deriving sexp_of, equal]
end

(** A field of the draft, as typed. *)
module Field : sig
  type t =
    | Name of string
    | Kind of Kind.t
    | Command of string
    | Args of string
    | Url of string
    | Pair_key of int * string
    | Pair_value of int * string
    | Pair_shown of int
    | Pair_add
    | Pair_remove of int
  [@@deriving sexp_of, equal]
end

(** How a server in the settings is doing on the session's host. *)
module Health : sig
  type t =
    | Off (** disabled *)
    | Checking (** the host's list is not in yet *)
    | Ready of Mcp_server.Tool.t list
    | Failed of string
    | Overridden of string (** by the definition in this file *)
    | Not_listed (** the host does not list it (an older host) *)
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; servers : Mcp_settings.t
  ; host : Mcp_list.t option (** [list_mcp] on the session's host *)
  ; host_error : string option
  ; draft : Draft.t option
  ; paste : Paste.t option
  ; removing : string option (** the server asked about *)
  ; expanded : String.Set.t (** whose tools are shown *)
  ; busy : String.Set.t (** being switched, approved or restarted *)
  ; error : string option (** the last change that failed *)
  }
[@@deriving sexp_of, equal]

val empty : t

(** Asking for the settings and the host's list again. *)
val loading : t -> t

val loaded : t -> (Mcp_settings.t, string) Result.t -> t

(** [mcp_settings_changed]: a draft of a server that is gone is dropped. *)
val changed : t -> Mcp_settings.t -> t

val host_listed : t -> (Mcp_list.t, string) Result.t -> t
val health : t -> Mcp_settings.Server.t -> Health.t

(** The host's own servers (its [~/.prigh/mcp.json] and the project's
    [.mcp.json]): not in the settings, or overriding one there. *)
val host_servers : t -> Mcp_server.t list

(** Adds a server ([None]) or edits one. *)
val edit : t -> string option -> t

val set : t -> Field.t -> t
(** Leaves the draft and the paste. *)
val cancel : t -> t

(** The draft to send ([Add_mcp_servers] or [Update_mcp_server]), or why not
    (said under it). *)
val save : t -> t * Request.Method.t option

val saved : t -> (unit, string) Result.t -> t
val open_paste : t -> t
val set_paste : t -> string -> t

(** [Add_mcp_servers_text] of the pasted text, or why not. *)
val save_paste : t -> t * Request.Method.t option

val pasted : t -> (unit, string) Result.t -> t
val ask_remove : t -> string -> t
val cancel_remove : t -> t

(** [Remove_mcp_server] of the server asked about. *)
val remove : t -> t * Request.Method.t option

val toggle_tools : t -> string -> t

(** [Set_mcp_server_enabled] flipping server [name]. *)
val toggle_enabled : t -> string -> t * Request.Method.t option

val set_busy : t -> string -> bool -> t
val failed : t -> string -> t

(** Esc: closes the confirmation, the paste or the draft, the innermost
    first; [None] when there is none. *)
val escape : t -> t option
