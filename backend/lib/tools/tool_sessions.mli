open! Core
open! Import

(** The [sessions] tool: the model looks into the other saved sessions of
    its namespace ([sessions_dir]), cheaply: [search] finds sessions by what
    was said in them and their names ({!Session_search}, the index of the
    clients' search) or, in one session, its messages with the words;
    [outline] lists one's landmarks ({!Outline}) and [read] shows a few of
    its messages, cut short. Message indices are positions in
    [Session.identified_messages] throughout. It reads session files and
    changes nothing. *)

val name : string

(** The saved session whose id is [key] or the only one whose id starts
    with it; never a path. Errors say what to do next. *)
val find : dir:string -> string -> Session.Summary.t Or_error.t

module Search : sig
  (** The best [limit] sessions for [query], best first. *)
  type t = query:string -> limit:int -> Session_search.Hit.t list
end

(** A search over the sessions saved in [dir ()] with an index of its own,
    for a backend without a server's. *)
val local_search : dir:(unit -> string) -> Search.t

val create
  :  search:Search.t
  -> sessions_dir:(unit -> string)
  -> current:(unit -> string option) (** the calling session's id *)
  -> unit
  -> Tool.t
