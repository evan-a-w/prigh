open! Core
open! Import

(** [/default-host], shared by the terminal and browser frontends:
    [/default-host] shows the tool host prigh-web's new sessions start on,
    [/default-host NAME|off] sets the config's [default_host] (the backend
    resolves a connected host's name or id and keeps its id; [off]: where
    the session you are on is). A terminal's own new sessions always start
    on its machine. *)

module Action : sig
  type t =
    | Show
    | Set of string option
    | Error of string
  [@@deriving sexp_of, equal]
end

val usage : string

(** [""], [off], a host's name or id. *)
val parse : string -> Action.t

(** [set_config]'s fields for [Set]. *)
val fields : Action.t -> (string * P.Json.t) list

(** The host's name, given the connected ones; its id when it is not
    connected. *)
val name : hosts:P.Host.t list -> string -> string

(** ["new sessions start on laptop ..."] *)
val summary : hosts:P.Host.t list -> P.Config.t -> string
