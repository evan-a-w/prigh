open! Core

(** The user name and password the connect form remembers in localStorage. With
    [prigh serve -tokens] the user name is a namespace's name and the password
    its token; otherwise the password is the [-token] and the user name is
    optional (the backend ignores it). *)

module Storage : sig
  type t =
    { get : string -> string option
    ; set : string -> string -> unit
    ; remove : string -> unit
    }

  (** The page's localStorage. *)
  val browser : t
end

type t =
  { user : string option
  ; password : string option
  }
[@@deriving sexp_of]

(** [prigh.user]. *)
val user_key : string

(** [prigh.token], where the token was kept before user names existed. *)
val password_key : string

val load : Storage.t -> t

(** Stores the stripped values; an empty one is removed. *)
val save : Storage.t -> user:string -> password:string -> unit

(** Signing out: removes both, and notes it for [take_signed_out]. *)
val forget : Storage.t -> unit

(** Whether [forget] ran since the last call, so the page that reloads after
    signing out shows the connect form instead of connecting anonymously. *)
val take_signed_out : Storage.t -> bool

(** [user] and [token], when present. *)
val hello_fields : t -> (string * Jsonaf.t) list
