open! Core
open! Import

(** One stored credential per provider, in the same shape as pi's auth.json:
    [{"type":"api_key","key":...}] or
    [{"type":"oauth","access":...,"refresh":...,"expires":<ms>,"accountId"?:...}]. *)

module Oauth : sig
  type t =
    { access : string
    ; refresh : string
    ; expires_ms : int
    ; account_id : string option
    }
  [@@deriving sexp, equal]
end

type t =
  | Api_key of string
  | Oauth of Oauth.t
[@@deriving sexp, equal]

val kind : t -> string
val to_json : t -> Json.t

(** Also accepts a bare string (the pre-oauth ["provider": "<key>"] form). *)
val of_json : Json.t -> t Or_error.t

val now_ms : unit -> int
val refresh_margin_ms : int
val oauth_needs_refresh : ?now_ms:int -> Oauth.t -> bool
