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

(** Where the TUI sends a code pasted into its sign-in dialog: a line on the
    program's stdin, which opens it (the backend's [Paste_code]). *)
module Code_input : sig
  type t

  val create : send:(string -> unit) -> cancel:(unit -> unit) -> t
  val send : t -> string -> unit

  (** Stops the sign-in: it fails with ["cancelled"]. *)
  val cancel : t -> unit
end

(** Whether to offer pasting the code: over SSH (the browser is elsewhere and
    cannot reach our listener), unless [PRIGH_SIGNIN_PASTE] says (1 or 0). *)
val paste_wanted : getenv:(string -> string option) -> bool

module Line : sig
  (** A line [prigh browser-signin -json] prints. *)
  type t =
    | Url of
        { url : string
        ; port : int
        ; paste : bool (** a pasted code is read on stdin *)
        }
    | Granted of Grant.t
    | Failed of string
    | Paste_refused of string (** a pasted code that does not open: why *)
  [@@deriving sexp_of]

  val parse : string -> t option
end

(** Runs [backend browser-signin -json -no-browser -program tui -connect
    connect] (with [-paste] when [paste]), calls [on_url] with the sign-in
    link and the listener's port (to show and open) and, in paste mode,
    where to send a pasted code, and waits for the sign-in. *)
val run
  :  backend:string
  -> connect:string
  -> paste:bool
  -> on_url:(url:string -> port:int -> paste:Code_input.t option -> unit)
  -> on_paste_refused:(string -> unit)
  -> (Grant.t, string) Result.t Deferred.t
