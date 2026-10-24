open! Core
open! Import

(** The transcripts of the sessions shown last (and of those the pointer
    rested on in the sidebar, fetched ahead), by session path: switching to
    one shows it at once, then refreshes it from the backend, keeping it
    (and its views) when its newest page is unchanged. *)

module Entry : sig
  type t =
    { chat : Chat.t
    ; paging : Paging.t
    ; newest : Message.t list (** its newest page as the backend last sent it *)
    }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val empty : t

(** How many sessions are kept. *)
val capacity : int

val find : t -> string -> Entry.t option

(** Most recently used first; the least recent goes beyond [capacity]. *)
val set : t -> string -> Entry.t -> t

(** A newest page from the backend: the cache keeping it, and the entry to
    show: the cached one when the page is the same as last time
    ([`Unchanged]), else a new one, of a session that was cached
    ([`Changed]) or not ([`New]). *)
val add_page
  :  t
  -> string
  -> Message_page.t
  -> running:bool
  -> t * Entry.t * [ `Unchanged | `Changed | `New ]

val remove : t -> string -> t

(** Only the sessions the backend still has (deleted ones go). *)
val retain : t -> paths:String.Set.t -> t

(** Newest pages being fetched, by path, and whether one is. *)
val loading : t -> string -> bool

val set_loading : t -> string -> bool -> t

(** Fetching ahead: one session at a time, the last one asked for next. *)
val prefetch : t -> string -> t * string option

(** After a page arrived: the session to fetch ahead next, if any. *)
val next_prefetch : t -> t * string option
