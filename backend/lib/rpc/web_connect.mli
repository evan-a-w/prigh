open! Core
open! Import

(** [prigh web]: prigh-web on this machine for a backend elsewhere. Nothing
    of a backend runs here (no sessions, no provider credentials): the site
    is served locally ({!Static_site}) with the backend relayed behind it
    ({!Web_proxy}), the page is signed in with the credentials given here
    ({!Signin_handoff}), and by default a {!Tool_host} connects to the
    backend so that sessions run their tools on this machine. *)

module Tools : sig
  (** Where the page's sessions run their tools: here (the default, as for
      a TUI connected to a remote backend) or on the backend. *)
  type t =
    | Local
    | Remote
  [@@deriving sexp_of, equal]

  val of_string : string -> t Or_error.t
end

module Credentials : sig
  type t =
    { token : string option
    ; user : string option
    }
  [@@deriving sexp_of]
end

(** Says [hello] to the backend once, as a frontend, so that an unreachable
    server or refused credentials are told apart (and reported) before
    anything is served. *)
module Check : sig
  module Error : sig
    type t =
      | Unreachable of string
      | Refused of string (** the backend's [unauthorised: ...] *)
      | Not_prigh_web of string (** no WebSocket at the URL *)
      | Signin_required of string
      (** a sign-in proxy's login instead of the WebSocket
          ({!Backend_address.Signin_required}) *)
      | Failed of string
    [@@deriving sexp_of]
  end

  (** Gives up after [timeout] (default 15s) as [Unreachable]. *)
  val run
    :  env:Env.t
    -> ?timeout:Time_ns.Span.t
    -> Backend_address.t
    -> Credentials.t
    -> (unit, Error.t) Result.t

  (** Where the address came from, for what to do about it. *)
  module Source : sig
    type t =
      | Flag
      | Env (** [$PRIGH_CONNECT] *)
      | Saved of string (** the saved connection's file *)
      | Form (** typed into [prigh desktop]'s connect form *)
    [@@deriving sexp_of]
  end

  (** What went wrong and what to do next. *)
  val explain
    :  Error.t
    -> Backend_address.t
    -> source:Source.t
    -> Credentials.t
    -> string
end

module Listen : sig
  type t =
    | At of Eio.Net.Ipaddr.v4v6 * int (** exactly there (port 0: any) *)
    | From of Eio.Net.Ipaddr.v4v6 * int
    (** the first free port of the ten from there *)
    | From_or_any of Eio.Net.Ipaddr.v4v6 * int
    (** the same, else any free port *)

  (** Calls [serve] (which binds and returns the bound port) at the address
      [t] says, trying the next port while one is in use. *)
  val bind
    :  t
    -> (addr:Eio.Net.Ipaddr.v4v6 -> port:int -> int)
    -> Eio.Net.Ipaddr.v4v6 * int
end

(** The host to put in a page's URL for a site listening on [addr]. *)
val browser_host : Eio.Net.Ipaddr.v4v6 -> string

(** What a site serving prigh-web for a backend elsewhere answers a
    JSON-lines client: it is [what]'s site, connect to [connect_to]. *)
val not_a_backend : what:string -> connect_to:string -> Web_server.on_lines

module Tool_host_config : sig
  type t =
    { remote : Backend_address.t
    ; credentials : Credentials.t
    ; name : string
    ; host_id : string
    ; cwd : string
    ; computer : Computer_helper.t option
    }
end

(** Connects a {!Tool_host} to the backend in a daemon fiber of [sw] (until
    it refuses the credentials) and says how it went once it connected or
    [wait] passed. *)
val start_tool_host
  :  env:Env.t
  -> sw:Switch.t
  -> log:(string -> unit)
  -> wait:Time_ns.Span.t
  -> Tool_host_config.t
  -> [ `Connected | `Connecting | `Refused of string ]

module Config : sig
  type t =
    { remote : Backend_address.t
    ; credentials : Credentials.t
    ; tools : Tools.t
    ; name : string (** the tool host's *)
    ; host_id : string
    ; cwd : string (** the tool host's *)
    ; computer : Computer_helper.t option
      (** the tool host runs the [computer] tool with it (-computer) *)
    ; listen : Listen.t
    ; root : string (** the built site *)
    }
end

module Started : sig
  type t =
    { url : string (** with the one-time sign-in code *)
    ; port : int
    ; tool_host : [ `Connected | `Connecting | `Refused of string | `Off ]
      (** as the page opens *)
    }
end

(** Serves the site until [sw] ends, with the tool host, once it connected
    or [tool_host_wait] (default 10s) passed. Raises what listening raises
    (an address in use, for [From] when all ten are). *)
val start
  :  env:Env.t
  -> sw:Switch.t
  -> ?tool_host_wait:Time_ns.Span.t
  -> log:(string -> unit)
  -> Config.t
  -> Started.t
