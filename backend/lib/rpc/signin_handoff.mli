open! Core
open! Import

(** How [prigh web] signs the page it opens in without the user retyping
    its [-token] and [-user]: the page's URL carries a one-time code in its
    fragment ([#signin=CODE], which browsers never send, so it is in no
    request line or log), and the page trades it for the credentials with a
    [POST] of the code to {!path}. Only the first trade succeeds, so the URL
    printed or left in the browser's history is worth nothing after it. *)

type t

val create
  :  user:string option
  -> token:string option
  -> prefer_host:string option
       (** the host id of the tool host [prigh web] runs, for [hello] *)
  -> t

(** 32 hex digits. *)
val code : t -> string

(** [/signin-handoff] *)
val path : string

(** The first [POST] of {!code} to {!path} gets
    [{"user", "token", "prefer_host"}] (each a string or null); any other
    [POST] there gets 403 and a [GET] 405. *)
val route : t -> Web_server.Request.t -> Web_server.on_http option
