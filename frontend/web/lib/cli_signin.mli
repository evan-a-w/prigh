open! Core

(** The page's half of signing a program on the user's computer in through
    the browser when a sign-in proxy (authentik...) guards the backend's
    site (the backend's [Browser_signin]): the program opens the site at
    [#cli-signin=PORT.STATE&program=P]; the page asks the user to allow [P]
    to connect as them, then asks the server for the proxy's cookie (only
    the server sees it: it is HttpOnly) at {!grant_path} and posts it, with
    the page's prigh credentials and the state, to the program's listener
    at [http://127.0.0.1:PORT/callback] in a top-level form submission. *)

module Program : sig
  type t =
    | Tui
    | Tool_host
    | Web
    | Desktop
    | Other (** a name the page does not know: shown as "A program" *)
  [@@deriving sexp_of, equal]

  (** [prigh-tui], [prigh tool-host], ..., [A program]. *)
  val name : t -> string
end

module Request : sig
  type t =
    { port : int
    ; state : string
    ; program : Program.t
    }
  [@@deriving sexp_of, equal]

  (** From a [location.hash]: the port must be 1-65535, the state 16 to 128
      hex digits; anything else is [None]. *)
  val of_hash : string -> t option

  (** As kept in sessionStorage while the user signs in to prigh first. *)
  val to_string : t -> string

  val of_string : string -> t option
end

(** Where a request waits while the user signs in to prigh (sessionStorage:
    this tab only). *)
val pending_key : string

(** [cli-signin/grant], relative to the page, so that a site under a path
    asks its own server. *)
val grant_path : string

(** The grant request's JSON body: the page's prigh credentials. *)
val grant_body : user:string option -> token:string option -> string

(** The cookie in the grant's answer. *)
val cookie_of_grant : string -> string option

(** [http://127.0.0.1:PORT/callback] *)
val callback_url : Request.t -> string

module Answer : sig
  type t =
    | Allow of
        { cookie : string
        ; token : string option
        ; user : string option
        }
    | Cancel
end

(** The form's fields for the listener. *)
val callback_fields : Request.t -> Answer.t -> (string * string) list

(** The confirmation's text: its heading, and the lines under it. [user]:
    the user the page is signed in as, if known. *)
val heading : Request.t -> string

val explanation : Request.t -> host:string -> user:string option -> string list

(** What a failed grant (its HTTP status and body) tells the user, and
    whether signing in to prigh again would help (a 401). *)
val grant_error : status:int -> body:string -> string * [ `Sign_in | `Stop ]
