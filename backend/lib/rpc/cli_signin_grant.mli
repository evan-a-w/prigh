open! Core
open! Import

(** The server's half of signing a program in through the browser
    ({!Browser_signin}) when a sign-in proxy (authentik, oauth2-proxy...)
    guards [prigh serve -web]: prigh-web's page, which the proxy let through,
    asks for the [Cookie] header its request carried, which holds the
    proxy's session cookie (HttpOnly: the page cannot read it), and hands it
    to the program's loopback listener.

    [POST {!path}] with a JSON body [{"token", "user"}] (the page's prigh
    credentials) is answered [{"cookie": COOKIE}] only when:

    - the credentials sign in ([authenticate]), else 401;
    - it is the page's own request: [Content-Type: application/json] (so
      another site's page cannot send it without a CORS preflight, which is
      never granted), an [Origin] whose host is the request's [Host] or
      [X-Forwarded-Host] (a proxy that rewrites [Host] should set either),
      or else [Sec-Fetch-Site: same-origin]; any other [Sec-Fetch-Site] is
      refused. Else 403;
    - the request carried cookies, else 422.

    Only the [Cookie] header is returned, never other headers, and it is
    never logged. *)

(** [/cli-signin/grant] *)
val path : string

(** Whether [token] (and [user]) sign in; the error is shown to the page. *)
type authenticate =
  token:string option -> user:string option -> (unit, string) Result.t

(** The answer to a request to {!path} with [body]. *)
val respond
  :  authenticate:authenticate
  -> Web_server.Request.t
  -> body:string
  -> Static_site.Response.t

val route
  :  authenticate:authenticate
  -> Web_server.Request.t
  -> Web_server.on_http option
