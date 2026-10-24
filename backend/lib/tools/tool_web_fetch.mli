open! Core
open! Import

(** [web_fetch]: a URL's content as readable text. It runs on the session's
    active tool host ([on_host]), like [bash]'s [curl] would, so it reaches
    what that machine reaches (a dev server on its [localhost], an
    intranet). HTML becomes markdown-ish text ({!Html_text}), other text as
    it is, images are attached like [read]'s; redirects are followed (up to
    10), the body is read up to [max_bytes] and the text truncated like other
    tools' output, with the full text saved to a file. Errors (HTTP status,
    connection, timeout, a binary type) say what to try next. *)

val tool : Tool.t

(** How much of a body is read. *)
val max_bytes : int

val default_timeout_seconds : int
val max_timeout_seconds : int
