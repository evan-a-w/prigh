open! Core
open! Import

(** Reports of finished background work (subagents, shell jobs) as compact
    cards: a head line with the id, status and task, the report or output
    folded under it. For deliveries and for [job_wait]-style results. *)
val view : Prigh_ui.Delivery.Section.t list -> Node.t
