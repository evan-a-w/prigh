open! Core
open! Import

(** A model provider's stored logins (accounts, as [auth_status] lists them),
    shared by the terminal and browser frontends: [/accounts] and its
    subcommands, [/account], [/login PROVIDER [METHOD] as LABEL],
    [/logout PROVIDER@LABEL] and the session's login beside its model. *)

module Login : sig
  type t =
    { provider : P.Auth_status.t
    ; account : P.Auth_status.Account.t
    }
  [@@deriving sexp_of]
end

(** Every provider's, in [auth_status]'s order (each provider's in fallback
    order). *)
val all : P.Auth_status.t list -> Login.t list

(** ["ChatGPT: Work"]. *)
val title : Login.t -> string

(** ["subscription, default, takes over at usage limits"]. *)
val detail : Login.t -> string

(** The label, or the id when the label is long: beside a model name. *)
val short : P.Auth_status.Account.t -> string

(** The login named by [arg]: its ref ([provider/id]), [provider@label],
    [provider@id], the title, or (unambiguous) its label or id; among
    [provider]'s only when given. The error says what to type instead. *)
val find
  :  ?provider:string
  -> P.Auth_status.t list
  -> string
  -> (Login.t, string) Result.t

(** Whether a [/logout] or [/account] argument names a login rather than a
    provider: [provider@label] or a ref. *)
val names_login : string -> bool

(** The session's login, to show beside its model: the pinned one, else the
    provider's default when it has several. *)
val of_session : P.Auth_status.t list -> P.State.t -> Login.t option

(** [/login]'s words split at [as]: [(before, Some label)]. *)
val split_as : string list -> string list * string option

(** [login]'s parameters: [label] names an existing login of the provider
    (re-logs it in, [account]) or a new one ([label]). *)
val login_params
  :  P.Auth_status.t list
  -> provider:string
  -> method_:string option
  -> label:string option
  -> (string * P.Json.t) list

(** A request that changes logins, and the notice once it succeeded. *)
module Request : sig
  type t =
    { method_ : string
    ; params : (string * P.Json.t) list
    ; done_ : string
    }
  [@@deriving sexp_of]
end

(** [logout]'s request for a login. *)
val logout : Login.t -> Request.t

(** [set_model]'s parameters for [model_key] on [login] ([None]: the
    provider's default). *)
val set_model_params
  :  model_key:string
  -> Login.t option
  -> (string * P.Json.t) list

(** Switching to [model] from [state]: the pinned login carries over when
    [model] has the same provider. *)
val keep_pin : P.State.t option -> provider:string -> (string * P.Json.t) list

module Command : sig
  type t =
    | Pick of string (** [/accounts [QUERY]]: the picker *)
    | Menu of string (** [/accounts menu ACCOUNT]: what can be done to one *)
    | Default of string
    | Rename of string (** [ACCOUNT to LABEL], resolved by {!request} *)
    | Fallback of
        { account : string
        ; on : bool
        }
    | Up of string
    | Down of string
    | Remove of string
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]

  val usage : string

  (** [rest] is what follows [/accounts]. *)
  val parse : string -> t
end

(** What a command other than [Pick], [Menu] and [Error] asks the backend. *)
val request : P.Auth_status.t list -> Command.t -> (Request.t, string) Result.t

(** A picker row: [id] is a slash command; one ending in a space wants the
    rest typed ({!asks}). *)
module Row : sig
  type t =
    { id : string
    ; label : string
    ; detail : string
    ; marked : bool
    }
  [@@deriving sexp_of]
end

val asks : string -> bool

(** [/accounts]'s rows: every login, ids [/accounts menu REF]; and a row to
    add one per signed-in provider. *)
val rows : P.Auth_status.t list -> session:P.State.t option -> Row.t list

(** A login's menu: use it here, make it the default, rename, fallback on
    or off, move up or down, log in again, log out. *)
val menu : Login.t -> session:P.State.t option -> Row.t list

(** A provider's name ("ChatGPT"), by its id. *)
val provider_name : P.Auth_status.t list -> string -> string

(** [/account ARG]: [default] goes back to the provider's default login,
    anything else names one of the session's provider's. *)
val session_request
  :  P.Auth_status.t list
  -> P.State.t
  -> string
  -> (Request.t, string) Result.t

(** [/account]'s rows: the session's provider's logins and its default. *)
val session_rows : P.Auth_status.t list -> P.State.t -> Row.t list
