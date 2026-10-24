open! Core

(** The [/signin] dialog: a user name and a token (or password) for one
    server. Typing goes to the focused field; the token is only ever shown
    masked. *)

type t [@@deriving sexp_of, equal]

module Field : sig
  type t =
    | User
    | Token
  [@@deriving sexp_of, equal]
end

(** Focused on the token when the user is known. [error] is shown under the
    fields (why we are asking). *)
val create : address:string -> user:string option -> error:string option -> t

val address : t -> string
val user : t -> string
val focus : t -> Field.t
val error : t -> string option

(** Whether the credentials were sent and the dialog waits for the answer. *)
val submitting : t -> bool

(** The token as shown: one [*] per character. *)
val masked_token : t -> string

val insert : t -> string -> t
val backspace : t -> t

(** Empties the focused field. *)
val clear : t -> t

(** Tab, Up and Down: the other field. *)
val toggle : t -> t

(** Enter: on the user field, moves to the token; with a token, the
    credentials to sign in with (the form now [submitting]). *)
val submit : t -> [ `Wait of t | `Sign_in of t * Server_link.Change.t ]

(** The server refused, or could not be reached: shown, the token emptied
    and focused for another try. *)
val failed : t -> error:string -> t
