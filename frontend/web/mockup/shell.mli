(** The page around every mockup screen: the session sidebar, the top bar
    (breadcrumb and panel toggles) and the main area, with an optional
    right panel, bottom panel and an overlay (dialogs, sheets) on top. [more]
    wraps the top bar's ⋯ button (e.g. in a menu) and shows it on every width. *)

open! Core
open Bonsai_web

(** [right_size]: the right panel is in a resizable split, that percentage
    wide (else it sizes itself). [bottom]: a resizable panel under the main
    area ([bottom_size] percent tall), desktop only. [pressed]: the top bar's
    toggles shown on ("terminal", "review"). On phones the right
    panel is a full-screen sheet titled [right_title], with a way back. *)
val component
  :  ?right:Vdom.Node.t Bonsai.t
  -> ?right_size:float
  -> ?right_title:string
  -> ?bottom:Vdom.Node.t Bonsai.t
  -> ?bottom_size:float
  -> ?overlay:Vdom.Node.t Bonsai.t
  -> ?pressed:string list
  -> ?more:(Vdom.Node.t -> Vdom.Node.t) Bonsai.t
  -> tip:Tip.t Bonsai.t
  -> main:Vdom.Node.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** A project's coloured two-letter tag. *)
val project_tag : Fake.Project.t -> Vdom.Node.t

(** A session's status: a spinner, a dot, or nothing when idle. *)
val status_icon : Fake.Status.t -> Vdom.Node.t
