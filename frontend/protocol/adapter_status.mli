open! Core

(** Whether one of the composer's agent CLIs can run on a tool host
    ([list_adapters], [adapters_changed]): installed (which version) and
    logged in. The backend only checks; installing and logging in are the
    user's, on that host, as [problem] says. *)

module Installed : sig
  type t =
    | Yes
    | No
    | Unknown of string (** the host could not say, and why *)
  [@@deriving sexp_of, equal]
end

module Auth : sig
  type t =
    | Logged_in
    | Logged_out
    | Not_needed
    | Unknown
  [@@deriving sexp_of, equal]
end

(** How the backend knows it is logged in. *)
module Via : sig
  type t =
    | Status_command (** its own login-status command said so *)
    | Prigh_login of string (** it runs on this login of prigh's *)
    | Session of
        { at : float (** Unix seconds *)
        ; ago : string option (** ["5m ago"], when the backend answered *)
        }
    (** an ACP agent opened a session on its own login then *)
    | Login_file of string (** its token file is there (Antigravity) *)
  [@@deriving sexp_of, equal]
end

type t =
  { adapter : string (** e.g. ["codex"], ["acp:gemini"] *)
  ; label : string (** e.g. ["Claude Code"] *)
  ; host : string (** the host's id *)
  ; host_name : string
  ; installed : Installed.t
  ; version : string option
  ; path : string option
  ; login_shell_path : string option
    (** where a login shell finds it when the tool host's [PATH] does not *)
  ; auth : Auth.t
  ; account : string option
  ; via : Via.t option
  ; checking : bool (** its login is being checked, by starting it *)
  ; checkable : bool
    (** an ACP agent whose login nothing has shown yet: Refresh checks it *)
  ; problem : string option
    (** what stops it and what to run, e.g. ["Codex is not installed on
        laptop, please install it"] *)
  ; hint : string option (** advice short of a problem: how to log in *)
  ; checked_at : float (** Unix seconds *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [{adapters}], [list_adapters]'s reply and [adapters_changed]'s event. *)
val list_of_json : Json.t -> t list Or_error.t

(** One line: ["Codex 0.166.0 · logged in (ChatGPT)"], ["not installed"]. *)
val summary : t -> string
