open! Core
open! Import

(** Requests to a site that only the page it served may use: [prigh web]'s
    and [prigh desktop]'s, which relay to a backend and run this machine's
    tool host, so a request from another page could run commands here.

    - [Host] must name the site by an IP address or [localhost] (with the
      site's [port], when it has one): a page under another name reached
      it by DNS rebinding, and is answered 421.
    - [Origin], when sent, must be the site's own ([http://] and the
      [Host]): browsers let any page open a WebSocket to any address, and
      send simple [POST]s without asking. Others are answered 403. Clients
      that are not browsers send no [Origin]. *)

val check
  :  port:int
  -> headers:(string * string) list
  -> (unit, Static_site.Response.t) Result.t
