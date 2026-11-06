open! Core
open! Async

(** Signing in through the browser to a sign-in proxy in front of a
    backend's site, by running [prigh browser-signin -json] (the backend
    binary does the work: see its [Browser_signin]). *)

module Grant : sig
  type t =
    { proxy_cookie : string
    ; token : string option (** the prigh credentials the page signed in with *)
    ; user : string option
    }
  [@@deriving sexp_of, equal]
end

module Line : sig
  (** A line [prigh browser-signin -json] prints. *)
  type t =
    | Url of
        { url : string
        ; port : int
        }
    | Granted of Grant.t
    | Failed of string
  [@@deriving sexp_of]

  val parse : string -> t option
end

(** Runs [backend browser-signin -json -no-browser -program tui -connect
    connect], calls [on_url] with the sign-in link and the listener's port
    (to show and open), and waits for the sign-in. *)
val run
  :  backend:string
  -> connect:string
  -> on_url:(url:string -> port:int -> unit)
  -> (Grant.t, string) Result.t Deferred.t
