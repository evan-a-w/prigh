open! Core
open! Import

(** Where a composer session's harness is with listing its models, while
    the session's model is still a placeholder: being asked, or why asking
    failed and what to do. The model chip and picker say it, so a harness
    that cannot run never looks like it is still loading. *)

module Problem : sig
  type t =
    | Not_installed
    | Not_logged_in (** on its own login on the host *)
    | Login_refused (** prigh's login it runs on was refused, or gave no token *)
    | Unreachable (** the tool host is not connected *)
    | Failed
  [@@deriving sexp_of, equal]
end

(** What the user can do about it. *)
module Fix : sig
  type t =
    | Login of { provider : string } (** [/login PROVIDER] *)
    | Engines (** Settings → Engines: install hints, importing or checking its login *)
    | Retry (** ask again *)
  [@@deriving sexp_of, equal]
end

type t =
  | Loading
  | Failed of
      { problem : Problem.t
      ; summary : string (** for the chip: ["not logged in on desktop"] *)
      ; message : string (** why, and what to do *)
      ; fixes : Fix.t list
      }
[@@deriving sexp_of, equal]

(** Why [label] ([Antigravity]) on [host_name] could not list its models:
    [why], what asking said; [install], what the host's probe found;
    [prigh_login], the provider of prigh's login it runs on, if one. *)
val failed
  :  label:string
  -> host_name:string
  -> prigh_login:string option
  -> install:Adapter_health.Install.t option
  -> Error.t
  -> t

(** [{state: "loading"}] or [{state: "failed", problem, summary, message,
    fixes: [{kind: "login", provider} | {kind: "engines"} | {kind:
    "retry"}]}]. *)
val to_json : t -> Json.t
