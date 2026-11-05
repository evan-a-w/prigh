open! Core

(** The page [prigh desktop] (the desktop app) shows. Its URL's fragment is
    [#desktop=KEY], a per-run key the page keeps for the tab and sends with
    every [POST] to the sidecar's [/desktop/] endpoints. The sidecar answers
    with either a connect form to show (no working connection yet) or a
    {!Signin_link} code to sign in with. *)

(** The key in a [location.hash] such as ["#desktop=KEY"]. *)
val key_of_hash : string -> string option

(** Where the tab keeps the key (sessionStorage), so a reload stays in
    desktop mode. *)
val storage_key : string

(** The backend's own prigh-web opened by the desktop app ([prigh desktop
    -ui remote]): its URL's fragment is [#app=desktop], with [&host=ID],
    this machine's tool host, when its tools run here. *)
module Hint : sig
  type t = { host : string option } [@@deriving sexp_of]

  val of_hash : string -> t option
end

(** Where the tab keeps that it is in the desktop app (sessionStorage). *)
val hint_storage_key : string

(** Where the backend's page goes to quit the app: the window catches it (the
    page cannot reach [prigh desktop] from another origin). *)
val quit_url : string

(** [/desktop/status], [/desktop/connect], [/desktop/quit]. *)
val status_path : string

val connect_path : string
val quit_path : string

(** The [POST]s' bodies. *)
val status_body : key:string -> string

val quit_body : key:string -> string

val connect_body
  :  key:string
  -> address:string
  -> user:string option
  -> token:string option
  -> plaintext:bool (** connect even though it is not encrypted *)
  -> string

module Status : sig
  type t =
    | Connect of
        { address : string option
        ; user : string option
        ; error : string option
        } (** show the connect form, prefilled, with what went wrong *)
    | Connected of { code : string option }
    (** sign in with [code]; [None]: already signed in (a reload) *)
    | Remote of string (** the window goes to the backend's own prigh-web *)
  [@@deriving sexp_of]

  val of_json : string -> t option
end

module Connect_reply : sig
  type t =
    | Connected of string option (** as {!Status.Connected} *)
    | Refused of string (** what is wrong and what to do *)
    | Plaintext of string
    (** the address is not encrypted and is another machine: the risk and
        the alternatives; connecting needs [~plaintext:true] *)
    | Remote of string (** connected: as {!Status.Remote} *)
  [@@deriving sexp_of]

  val of_json : string -> t option
end
