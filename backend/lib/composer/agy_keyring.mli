open! Core
open! Import

(** Antigravity's Google login in the Linux keyring (go-keyring: Secret
    Service item [service=gemini username=antigravity]), seen from bash on a
    tool host. A tool host started outside the desktop session may lack
    [DBUS_SESSION_BUS_ADDRESS]: the scripts derive it from
    [/run/user/UID/bus]. The secret is read with [secret-tool] (libsecret),
    else with it through [nix shell nixpkgs#libsecret]; [busctl] (systemd)
    only finds the item: a Secret Service session lives as long as its D-Bus
    connection, so a secret cannot be read with one [busctl] call per
    method. *)

module Unreachable : sig
  type t =
    | No_bus (** no session bus: not set, and no [/run/user/UID/bus] *)
    | No_tool (** neither [secret-tool], [nix] nor [busctl] *)
    | Busctl of string (** busctl's error *)
    | Secret_tool of string (** secret-tool's error *)
  [@@deriving sexp_of, equal]

  (** Why, e.g. ["secret-tool isn't installed on desktop and busctl couldn't
      reach the Secret Service (...)"]. *)
  val to_string : t -> host:string -> string

  (** What to do about it, e.g. ["install libsecret (secret-tool) there and
      Import again"]. *)
  val advice : t -> string
end

module Probe : sig
  type t =
    | Found of { readable : bool (** [secret-tool] or [nix] is there *) }
    | Empty
    | Locked
    | Unreachable of Unreachable.t
  [@@deriving sexp_of, equal]

  (** The line {!script} prints. *)
  val of_line : string -> t option
end

(** Bash printing one line that {!Probe.of_line} reads. *)
val script : string

(** Bash printing the secret. *)
val lookup : string

(** Bash deleting it. *)
val clear : string

(** What the user runs to clear it by hand. *)
val clear_by_hand : string
