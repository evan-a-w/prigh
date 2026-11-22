(** Autocomplete input and command palette with a list of suggestions.
    Port of shadcn/ui's [Combobox] demo, built from a {!Popover} and a
    {!Command}: an outline button showing the chosen option, opening a
    searchable list; picking the chosen option again clears it. Compose
    [Popover] and [Command] yourself for other shapes (see the docs). *)

(* Core has a Command too: name ours before Import opens Core. *)
module Cmdk := Command

open! Import

module Option_ : sig
  type t =
    { value : string
    ; label : string
    }
  [@@deriving sexp_of, equal]
end

(** Controlled: [value], [on_value_change], whether it is [open_], and the
    command's state. [id] (unique on the page) prefixes the parts' ids. *)
val view
  :  ?class_:string
  -> ?content_class:string
  -> ?id:string
  -> ?placeholder:string
  -> ?search_placeholder:string
  -> ?empty:string
  -> value:string option
  -> on_value_change:(string option -> unit Effect.t)
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> command:Cmdk.State.t
  -> on_command_change:(Cmdk.State.t -> unit Effect.t)
  -> Option_.t list
  -> Node.t

(** Keeps the value (initially [default_value]) and the rest; returns the
    value and the view. *)
val component
  :  ?class_:string
  -> ?content_class:string
  -> ?placeholder:string
  -> ?search_placeholder:string
  -> ?empty:string
  -> ?default_value:string
  -> Option_.t list Bonsai.t
  -> local_ Bonsai.graph
  -> (string option * Node.t) Bonsai.t
