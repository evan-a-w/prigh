open! Core
open! Import

(** The web root's files over HTTP: [GET] and [HEAD] of plain relative paths
    ([/] is [/index.html]; no [..], no hidden files).

    Every file has an [ETag] (a content hash, cached per file version) and
    [Last-Modified] (none for Nix's mtime 1), so a reload revalidates with
    [If-None-Match] / [If-Modified-Since] and gets [304 Not Modified]. They
    are [Cache-Control: no-cache] (the names are the same in every build),
    except a versioned name, [NAME?v=VERSION] with VERSION the first 16 hex
    digits of the file's MD5, which the frontend's build gives what
    [index.html] loads: [immutable] for a year.

    A precompressed sibling ([file.br], [file.gz]) is served instead, with
    [Content-Encoding], when the request's [Accept-Encoding] allows it and the
    sibling is not older than the file; responses for files with such a
    sibling carry [Vary: Accept-Encoding]. *)

module Response : sig
  type t =
    { status : int
    ; reason : string
    ; headers : (string * string) list
    ; content_length : int option
      (** [None] for [304]; the length a [GET] would send for [HEAD]. *)
    ; body : string
    }
  [@@deriving sexp_of]

  (** A plain-text reply. *)
  val text : status:int -> reason:string -> string -> t
end

val respond
  :  ?query:(string * string) list
  -> root:string
  -> meth:string
  -> path:string
  -> headers:(string * string) list
  -> unit
  -> Response.t

module For_testing : sig
  val safe_relative : string -> string option
  val content_type : string -> string
  val http_date : float -> string
  val parse_http_date : string -> float option
end
