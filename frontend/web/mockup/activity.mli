(** What a session or subagent is doing, as the sidebar and the session
    panel show it: "thinking…" or "working…" (shimmering) while active, else
    how long ago it last worked. *)

open! Core
open Bonsai_web

val view : ?class_:string -> Fake.Status.t -> age:string -> Vdom.Node.t
