open! Core
open! Import

(** Whether the backend's site (its prigh-web pages, not [/ws]) is behind a
    sign-in proxy: a plain [GET] of {!Backend_address.web_url}, with the
    address's proxy cookie. A sign-in is a redirect to another site (not
    the same host, nor [http] to [https] on it; redirects on the same host
    are followed, a few), or a 401, 403 or 407. Anything else, a 200 or a
    404, is not. *)

module Result : sig
  type t =
    | Open (** no sign-in in the way (or a [HOST:PORT], never probed) *)
    | Signin_required of
        { why : string
        ; login : string option (** the login site's host, from [Location] *)
        }
    | Unreachable of string (** not a sign-in: the check reports it *)
  [@@deriving sexp_of]
end

(** Gives up after [timeout] (default 15s) as [Unreachable]. *)
val run : env:Env.t -> ?timeout:Time_ns.Span.t -> Backend_address.t -> Result.t
