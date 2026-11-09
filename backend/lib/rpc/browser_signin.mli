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

(** The page's URL for a listener on [port] with [state]. *)
val url
  :  Backend_address.t
  -> program:Program.t
  -> port:int
  -> state:string
  -> string

(** Listens on [127.0.0.1] (any port), calls [on_url] with {!url} and the port, and
    serves until the page posts the sign-in (or cancels it: [Error]).
    [state] defaults to 32 random hex digits. *)
val run
  :  env:Env.t
  -> ?state:string
  -> program:Program.t
  -> on_url:(url:string -> port:int -> unit)
  -> Backend_address.t
  -> (Grant.t, string) Result.t

(** {!run} for a program on a terminal: says that [why] (the backend wants
    a browser sign-in), prints the URL with [say] (to open by hand, e.g.
    over SSH), opens it with [open_url] (if given) and waits. *)
val run_on_terminal
  :  env:Env.t
  -> program:Program.t
  -> say:(string -> unit)
  -> ?open_url:(string -> unit)
  -> why:string
  -> Backend_address.t
  -> (Grant.t, string) Result.t
