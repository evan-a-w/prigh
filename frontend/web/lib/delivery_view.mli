open! Core
open! Import

(** Reports of finished background work (subagents, shell jobs) as compact
    cards: a head line with the id, status and task, then the report folded
    under it, or a job's last lines, folded under [key] (default [job_key]).
    [name] gives a subagent's short name from its id: shown for its task,
    and its line opens it in the agents panel. For deliveries and for [job_kill]/[subagent_cancel] results. *)
val view
  :  ?name:(string -> string option)
  -> fold:Fold.t
  -> ?key:(Prigh_ui.Delivery.Section.t -> string)
  -> Prigh_ui.Delivery.Section.t list
  -> Node.t

(** [job:<id>] *)
val job_key : Prigh_ui.Delivery.Section.t -> string
