open! Core
module P = Prigh_protocol

(** Conversation history plus the live streaming tails. Pure. *)

module Item : sig
  type t =
    | User of string
    | Assistant of string
    | Thinking of string
    | Tool_call of P.Tool_call.t
    | Tool_result of P.Message.Tool_result.t
    | Notice of Severity.t * string
    | Block of Content.t
  [@@deriving sexp_of, equal]
end

module Stream_kind : sig
  type t =
    | Text
    | Thinking
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val empty : t
val items : t -> Item.t list
val add : t -> Item.t -> t
val add_message : t -> P.Message.t -> t
val notice : ?severity:Severity.t -> t -> string -> t
val clear : t -> t

(** Streaming: [append] accumulates; a kind change or [flush] turns the
    accumulated text into an item. *)
val append : t -> Stream_kind.t -> string -> t

val flush : t -> t
val set_tool_tail : t -> string option -> t
val tool_tail : t -> string option
val append_tool_output : t -> string -> t

(** Renders wrapped lines, newest last. Only the last [rows + skip] lines are
    produced; [skip] is how many bottom lines to hide (scrolling). *)
val render_tail
  :  t
  -> width:int
  -> rows:int
  -> skip:int
  -> expand_tools:bool
  -> Content.t

(** Total wrapped line count, for scroll clamping. *)
val line_count : t -> width:int -> expand_tools:bool -> int

val render_item : Item.t -> expand_tools:bool -> Content.t
