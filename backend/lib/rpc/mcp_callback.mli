open! Core
open! Import

(** The web server's half of MCP sign-ins ({!Mcp_auth}): the redirect's
    route, and the one-shot loopback listener for sign-ins started where
    there is no web server. *)

(** Answers [GET {!Mcp_auth.callback_path}]. *)
val route : Web_server.Request.t -> Web_server.on_http option

(** A listener on [127.0.0.1] (any port) serving only {!route}, until [sw]
    ends: its port. *)
val listen : env:Env.t -> sw:Switch.t -> int
