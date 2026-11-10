open! Core

(** The page's half of signing a program on the user's computer in through
    the browser when a sign-in proxy (authentik...) guards the backend's
    site (the backend's [Browser_signin]): the program opens the site at
    [#cli-signin=PORT.STATE&program=P]; the page asks the user to allow [P]
    to connect as them, then asks the server for the proxy's cookie (only
    the server sees it: it is HttpOnly) at {!grant_path} and posts it, with
    the page's prigh credentials and the state, to the program's listener
    at [http://127.0.0.1:PORT/callback] in a top-level form submission.

    With [&paste=KEY] (the program runs where this browser cannot reach its
    listener, e.g. over SSH) the page instead seals the grant to the
    program's one-time key (the app's [paste_seal.js]) and shows the code to
    paste into its terminal; the form post stays a button, for a forwarded
    port. It is not tried on its own: a [no-cors] fetch to the loopback
    would meet the browsers' private network checks (a preflight, a
    permission prompt) and its opaque answer could not say whether it got
    through. *)

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

module Paste : sig
  (** The program also takes the grant as a pasted code (it runs where the
      browser cannot reach its listener, over SSH): its one-time public key
      ([&paste=], 43 base64url characters) and the computer's name
      ([&from=], a host name, else [None]). *)
  type t =
    { key : string
    ; from : string option
    }
  [@@deriving sexp_of, equal]
end

module Request : sig
  type t =
    { port : int
    ; state : string
    ; program : Program.t
    ; paste : Paste.t option
    }
  [@@deriving sexp_of, equal]

  (** From a [location.hash]: the port must be 1-65535, the state 16 to 128
      hex digits; anything else is [None]. A malformed [paste] is ignored. *)
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

(** In paste mode: the JSON the page seals into the code (the backend's
    [Paste_code] opens it). *)
val paste_plaintext
  :  cookie:string
  -> token:string option
  -> user:string option
  -> string

(** Why the page could not seal the code ([secure]: the page is a secure
    context; [name] and [message]: the WebCrypto error's), and what to do. *)
val seal_error
  :  Request.t
  -> secure:bool
  -> name:string
  -> message:string
  -> string

module View : sig
  type t =
    | Ask of
        { working : bool (** the grant is on its way *)
        ; error : string option
        } (** Allow or Cancel *)
    | Code of
        { code : string
        ; copied : [ `Not_yet | `Copied | `Failed ]
        } (** paste mode, allowed: the code to paste, Copy *)
    | No_code of string (** paste mode, allowed, but no code: why *)
    | Cancelled (** paste mode: nothing was sent *)
  [@@deriving sexp_of]
end

(** The page for [view]: the buttons are [#cli-signin-allow],
    [#cli-signin-cancel], [#cli-signin-copy] and [#cli-signin-send] (the
    form post to the listener, for a forwarded port), the code is in
    [#cli-signin-code]. *)
val html : Request.t -> host:string -> user:string option -> View.t -> string
