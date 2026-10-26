open! Core
open! Import

(** The git hosts (GitHub, Forgejo/Gitea) the user signs in to, as
    [forge_status] lists them: [/login] and [/logout]'s arguments and
    pickers, and [/auth]'s listing. *)

module Login : sig
  type t =
    | Host of
        { host : string
        ; kind : string option (** [None]: the backend guesses *)
        ; method_ : string option (** [token]; [None]: OAuth when available *)
        }
    | Ask_host of
        { kind : string
        ; method_ : string option
        } (** [/login forgejo]: which host *)
  [@@deriving sexp_of, equal]

  (** [/login]'s arguments when they name a git host: [github [HOST]
      [token]], [forgejo HOST [token]], or a host ([forge_status]'s, or any
      name with a dot) and [[token]]. [None]: a model provider's login. *)
  val parse
    :  P.Forge_account.t list
    -> string list
    -> (t, string) Result.t option

  (** The question [Ask_host] puts. *)
  val host_question : kind:string -> string
end

(** [/logout]'s argument as a git host: [github] (github.com), a listed
    host, or any name with a dot; [None]: a model provider. *)
val logout_host : P.Forge_account.t list -> string -> string option

val find : P.Forge_account.t list -> string -> P.Forge_account.t option

(** [GitHub (github.com)]. *)
val name : P.Forge_account.t -> string

(** The [/login] picker's git-host rows: each host (and its token login
    when OAuth is available), then [Forgejo: another host…]. *)
val login_items : P.Forge_account.t list -> Picker.Item.t list

(** What a [login_items] row's id stands for; [None] for other rows. *)
val login_of_item_id : P.Forge_account.t list -> string -> Login.t option

(** The [/logout] picker's rows: the signed-in hosts, with ids that
    {!logout_of_item_id} reads back. *)
val logout_items : P.Forge_account.t list -> Picker.Item.t list

val logout_of_item_id : string -> string option

(** [/login] and [/logout]'s completions, as [(id, detail)]. *)
val login_completions : P.Forge_account.t list -> (string * string) list

val logout_completions : P.Forge_account.t list -> (string * string) list

(** [/auth]'s rows: host, kind, user, method, and why OAuth is unavailable. *)
val describe : P.Forge_account.t list -> Content.t
