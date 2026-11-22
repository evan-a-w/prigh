(** The settings page around a section: its navigation on the left (on
    phones, a list of sections the section replaces, with a way back) and
    the section's title and body, plus the parts sections are made of. *)

open! Core
open Bonsai_web

module Section : sig
  type t =
    | General
    | Appearance
    | Models
    | Hosts
    | Git_hosting
    | Mcp
    | Memory
    | About_you
    | Instructions
    | Keys
    | Account
  [@@deriving equal]
end

(** [start_on_list]: on phones, open on the list of sections rather than
    on [section]. [actions] go beside the section's title. *)
val component
  :  ?start_on_list:bool
  -> ?actions:Vdom.Node.t list
  -> section:Section.t
  -> description:string
  -> body:Vdom.Node.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** A titled group of rows in a bordered card. *)
val group
  :  ?title:string
  -> ?description:string
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A row of a group: its title and description, and its control on the
    right ([stack]: below them, on phones too). *)
val row
  :  ?stack:bool
  -> ?for_:string
  -> title:string
  -> ?description:string
  -> Vdom.Node.t
  -> Vdom.Node.t
