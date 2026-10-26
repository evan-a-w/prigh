open! Core

(** The page [prigh web] opens ([prigh web -connect HOST:PORT], a backend
    elsewhere behind a site served on this machine) is signed in without
    retyping: its URL's fragment is [#signin=CODE], a one-time code the page
    [POST]s to {!path} for the credentials [prigh web] was given. *)

(** The code in a [location.hash] such as ["#signin=CODE"]. *)
val code_of_hash : string -> string option

(** [/signin-handoff] *)
val path : string

module Credentials : sig
  type t =
    { user : string option
    ; token : string option
    ; prefer_host : string option
      (** the tool host [prigh web] runs on this machine, for [hello]'s
          [prefer_host] *)
    }
  [@@deriving sexp_of]

  (** The answer to the [POST]. *)
  val of_json : string -> t option
end

(** Where the page keeps [prefer_host] between loads. *)
val prefer_host_key : string
