open! Core

(** The [/btw] panel above the editor: a side question and its streamed answer,
    never part of the transcript. *)

module Status : sig
  type t =
    | Streaming
    | Done
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string (** the [btw_id] sent to the backend *)
  ; question : string
  ; answer : string
  ; status : Status.t
  }
[@@deriving sexp_of, equal]

val create : id:string -> question:string -> t
val is_streaming : t -> bool
val add_delta : t -> string -> t

(** The full answer replaces the streamed one. *)
val finish : t -> text:string -> t

val fail : t -> string -> t

(** At most [max_rows] rows (including the frame); a long answer shows its tail. *)
val render : t -> width:int -> max_rows:int -> Content.t

(** The [max_rows] the screen gives the box at [height]. *)
val max_rows : height:int -> int
