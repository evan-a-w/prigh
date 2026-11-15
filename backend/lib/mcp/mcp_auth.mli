open! Core
open! Import

(** The user's OAuth sign-ins to remote (streamable HTTP) MCP servers, kept
    by the backend: one per server URL, in the user's credential file
    ({!Auth_store}, entries [mcp:URL], 0600 and written under its lock),
    with the client and endpoints needed to refresh and revoke it.

    The backend sends the access tokens to the tool hosts with every MCP
    request ({!tokens}, {!Mcp_hub}); a host reports a server that answers
    401 and advertises OAuth as needing a sign-in.

    Signing in ({!sign_in}) discovers the server's authorization server
    ({!Mcp_oauth.discover}), registers a client (or uses the server's
    configured [oauth]) and returns the authorization URL; the browser comes
    back to [REDIRECT_BASE/mcp/oauth/callback] (the backend's web server, a
    proxy in front of it, or a one-shot listener on [127.0.0.1]), whose
    handler ({!callback}, {!Mcp_callback}) checks the state, exchanges the code and stores the
    token. *)

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> store:Auth_store.t
  -> ?listen:(sw:Switch.t -> int)
       (** starts a listener on [127.0.0.1] that answers {!callback_path}
           with {!callback} in [sw]: its port *)
  -> ?now_ms:(unit -> int)
  -> unit
  -> t

(** [f] runs after a sign-in completes or a token is removed. *)
val subscribe : t -> f:(unit -> unit) -> unit

(** Whether there is a token for the server at [url]. *)
val signed_in : t -> url:string -> bool

(** The access tokens by server URL (a JSON object, for the hosts),
    refreshing those about to expire; one that cannot be refreshed is
    dropped (the server then needs a sign-in again). *)
val tokens : t -> Json.t

(** The server at [url] refused [rejected] (the token sent): refreshes it,
    unless it has changed since. [true] when there is a new token to try;
    otherwise the token is gone. *)
val renew : t -> url:string -> rejected:string -> bool

(** Starts a sign-in to the server at [url], returning the URL to open in a
    browser. The redirect goes to [redirect_base] (prigh-web's own origin)
    if given, else to the backend's web server ({!set_web_base}), else to a
    listener on [127.0.0.1] that lasts until the sign-in ends (10 minutes
    at most). *)
val sign_in
  :  t
  -> url:string
  -> oauth:Mcp_config.Oauth.t option
  -> redirect_base:string option
  -> string Or_error.t

(** Forgets the token (revoking it, when the server can). *)
val sign_out : t -> url:string -> unit Or_error.t

(** [/mcp/oauth/callback] *)
val callback_path : string

(** The backend's web server, [http://HOST:PORT], for sign-ins started
    without a [redirect_base] (from a terminal). *)
val set_web_base : string -> unit

(** The page for the redirect to {!callback_path} with [query] (any
    pending sign-in's, found by its [state]), once the sign-in is done. *)
val callback : (string * string) list -> Static_site.Response.t
