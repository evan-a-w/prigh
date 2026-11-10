open! Core
open! Import

(** Signing a program (prigh-tui, [prigh tool-host], [prigh web], [prigh
    desktop]) in through the browser when a sign-in proxy guards the
    backend's site, as [gh auth login] does:

    + a listener on [127.0.0.1] (a random port) waits for one sign-in, with a
      one-time [state] (128 bits from the OS's RNG);
    + the browser opens the site at [#cli-signin=PORT.STATE&program=P] (a
      fragment: never sent, so in no proxy's log); the proxy signs the user
      in, prigh-web loads, signs in to prigh if needed and asks the user to
      allow [P] to connect as them;
    + on Allow the page gets the proxy's cookie from the server
      ({!Cli_signin_grant}) and posts it, its prigh token and user and the
      state to [http://127.0.0.1:PORT/callback] in a top-level form
      submission (a navigation: no CORS or private network preflight);
    + the listener takes the first [POST] with the right state (compared in
      constant time), answers a page saying so and closes. Cancel posts the
      state with [cancel]; a wrong state, another path or method get an
      error page and the listener keeps waiting. *)

module Program : sig
  (** Who asks: the page names it ("prigh-tui on this computer wants...") *)
  type t =
    | Tui
    | Tool_host
    | Web
    | Desktop
  [@@deriving sexp_of, enumerate]

  (** As in the URL: [tui], [tool-host], [web], [desktop]. *)
  val to_string : t -> string

  val of_string : string -> t option

  (** [prigh-tui], [prigh tool-host]... *)
  val name : t -> string
end

module Grant = Browser_signin_grant

module Paste : sig
  (** Taking the sign-in as a code pasted into the program as well: for a
      program the browser cannot reach (over SSH, its listener is on another
      computer's loopback), the link carries a one-time public key
      ([&paste=KEY&from=HOST]); after Allow the page shows the grant sealed
      to it ({!Paste_code}), and the user pastes it back. *)
  type t

  (** Codes come from [read_line], one a line ([None]: the input ended);
      [from] names this computer on the page ("paste this into the terminal
      on [from]"). [secret]: the key pair's private key (for tests). *)
  val create
    :  ?secret:string
    -> from:string
    -> read_line:(unit -> string option)
    -> unit
    -> t

  (** Lines of the process's standard input, from this host. *)
  val stdin : env:Env.t -> t

  (** Whether to offer it: [flag] ([-paste]), else [PRIGH_SIGNIN_PASTE] ([1]
      or [0]), else whether the program runs over SSH ([SSH_CONNECTION] or
      [SSH_TTY]). *)
  val wanted : flag:bool -> getenv:(string -> string option) -> bool
end

(** The page's URL for a listener on [port] with [state]; [paste]: the paste
    key and the host's name. *)
val url
  :  ?paste:Paste_code.t * string
  -> Backend_address.t
  -> program:Program.t
  -> port:int
  -> state:string
  -> string

(** Listens on [127.0.0.1] (any port), calls [on_url] with {!url} and the
    port, and serves until the page posts the sign-in (or cancels it:
    [Error]). [state] defaults to 32 random hex digits.

    With [paste], also takes a pasted code: the first of the listener and a
    code that opens wins. A code that does not open goes to [on_refused]
    (what to do next) and the next line is read; the codes expire after 30
    minutes ([Error]). *)
val run
  :  env:Env.t
  -> ?state:string
  -> ?paste:Paste.t
  -> ?on_refused:(string -> unit)
  -> program:Program.t
  -> on_url:(url:string -> port:int -> unit)
  -> Backend_address.t
  -> (Grant.t, string) Result.t

(** {!run} for a program on a terminal: says that [why] (the backend wants
    a browser sign-in), prints the URL with [say] (to open by hand, e.g.
    over SSH) and the [ssh -L] that forwards the listener's port, opens it
    with [open_url] (if given) and waits; with [paste], it also asks for the
    page's code (and says what is wrong with one that does not open). *)
val run_on_terminal
  :  env:Env.t
  -> ?state:string
  -> program:Program.t
  -> say:(string -> unit)
  -> ?open_url:(string -> unit)
  -> ?paste:Paste.t
  -> why:string
  -> Backend_address.t
  -> (Grant.t, string) Result.t
