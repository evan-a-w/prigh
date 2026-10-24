open! Core
open! Import

(** The web server's file transfers, authorised by {!File_tickets}:

    - [GET /files/download?ticket=T]: the file (with its length; [Inline]
      tickets as their image type, inline), or the directory as a .tar.gz,
      streamed from the tool host chunk by chunk. Archives and files that
      announce no size (/proc's) are sent chunked, so a transfer that fails
      midway lacks the last chunk and does not look complete. A ticket used
      at the wrong endpoint is refused and kept.
    - [PUT /files/upload?ticket=T] (or [POST]) with a [Content-Length]
      body: streamed to the tool host; answers [{path, bytes}], or
      [{error}] with 409 when the file exists, 413 when too large, 410 for
      an expired ticket.

    Responses allow any origin (the ticket is the authority, not a cookie),
    so a page served elsewhere can use them; [OPTIONS] answers the
    preflight. Downloads are never rendered by the browser as a page of
    this origin ([Content-Security-Policy: sandbox], [nosniff],
    [attachment] for everything but images). *)

val handles : path:string -> bool

val serve
  :  meth:string
  -> path:string
  -> query:(string * string) list
  -> headers:(string * string) list
  -> reader:Eio.Buf_read.t
  -> _ Eio.Flow.sink
  -> unit

module For_testing : sig
  val content_disposition : inline:bool -> string -> string
  val content_type : string -> string
end
