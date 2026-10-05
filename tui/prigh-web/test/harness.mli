open! Core
open Prigh_web

(** Drives [App.update] like the page does and shows what the user sees. *)

type t

(** Started and connected, with [state] (a fresh session) answered. *)
val create : unit -> t

val model : t -> App.Model.t

(** Applies an action and prints the commands it issued. *)
val act : t -> App.Action.t -> unit

(** An event from the backend, decoded from its JSON. *)
val event : t -> string -> unit

(** Answers the oldest unanswered [Rpc] command whose method is [method_]. *)
val reply : t -> string -> string -> unit

(** The page as indented HTML; [selector] picks part of it. *)
val show : ?selector:string -> t -> unit

(** The page's visible text. *)
val text : ?selector:string -> t -> unit

(** A state as JSON, with [fields] overriding the defaults. *)
val state_json : ?fields:(string * Jsonaf.t) list -> unit -> string
