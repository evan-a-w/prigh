open! Core
open! Prigh

(** A sign-in proxy as authentik's forward auth behaves, for tests: in front
    of a prigh site on [upstream], it passes a request on (WebSocket upgrades
    too) only with the cookie [proxy_session=<session>], redirects anything
    else to a login page elsewhere, and [GET /login] "signs in": it sets the
    cookie (HttpOnly) and redirects to [/]. *)

val session : string

(** The cookie a signed-in browser sends. *)
val cookie : string

(** Listens on 127.0.0.1 until [sw] ends; the port. With [ws_exempt],
    [/ws] and [/terminal] pass without the cookie (prigh checks its own
    token there); with [fresh_state], each redirect to the login has a new
    [state] in its query, as a real one does. *)
val start
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> ?ws_exempt:bool
  -> ?fresh_state:bool
  -> upstream:int
  -> unit
  -> int

(** One HTTP/1.1 request to 127.0.0.1:[port] ([Connection: close]); the
    whole response. *)
val request
  :  env:Eio_unix.Stdenv.base
  -> port:int
  -> ?headers:(string * string) list
  -> ?body:string
  -> meth:string
  -> string
  -> string
