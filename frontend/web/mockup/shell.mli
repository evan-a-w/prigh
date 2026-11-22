(** The page around every mockup screen: the session sidebar, the top bar
    (breadcrumb and panel toggles) and the main area, with an optional
    right panel and an overlay (dialogs, sheets) on top. *)

open! Core
open Bonsai_web

val component
  :  ?right:Vdom.Node.t Bonsai.t
  -> ?overlay:Vdom.Node.t Bonsai.t
  -> tip:Tip.t Bonsai.t
  -> main:Vdom.Node.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** A project's coloured two-letter tag. *)
val project_tag : Fake.Project.t -> Vdom.Node.t

(** A session's status: a spinner, a dot, or nothing when idle. *)
val status_icon : Fake.Status.t -> Vdom.Node.t
