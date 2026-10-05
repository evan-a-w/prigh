open! Core
open! Import

(** The popup under the editor: slash commands, their arguments, and [@] paths.
*)

module Source : sig
  type t =
    | Command
    | Argument of Slash.Argument.t
    | Path (** [@path], listed by the backend ([list_paths]) *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

val source : t -> Source.t
val prefix : t -> string
val items : t -> Picker.Item.t list
val selected : t -> int
val selected_item : t -> Picker.Item.t option

(** What the editor's [text] with the caret at byte [cursor] completes, if
    anything. Paths and directories start without items: see [request]. *)
val compute
  :  text:string
  -> cursor:int
  -> models:Llm.t list
  -> auth:Auth_status.t list
  -> current_model:string option
  -> t option

(** Whether two completions are for the same thing, so the newer can keep the
    older's items and highlight. *)
val same : t -> t -> bool

(** The RPC that lists the items ([list_paths] or [list_dirs]) and its
    [prefix]. *)
val request : t -> (string * string) option

(** The backend's listing for [prefix]; stale ones are ignored. *)
val set_results : t -> prefix:string -> string list -> t

val move : t -> int -> t

(** The text with the highlighted item in place of [prefix], and the caret
    after it. Commands get a trailing space and files a space (directories
    keep completing). *)
val accept : t -> text:string -> string * int
