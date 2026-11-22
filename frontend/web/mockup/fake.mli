(** The mockups' made-up data. *)

module Project : sig
  type t =
    { name : string
    ; tag : string
    ; tag_class : string (** the tag's colours, as Tailwind classes *)
    }

  val prigh : t
  val loon : t
  val co : t
end

module Status : sig
  type t =
    | Running
    | Waiting (** waiting for the user: a confirmation or a question *)
    | Unread
    | Idle
end

module Session : sig
  type t =
    { project : Project.t
    ; title : string
    ; age : string
    ; status : Status.t
    }

  val current : t
  val recent : t list

  (** The worktree group's branch and its sessions. *)
  val worktree : string * t list

  val settled : int
end

module Tool_call : sig
  type t =
    { tool : string
    ; summary : string
    ; duration : string
    ; ok : bool
    ; output : string option
    }

  val first_turn : t list
  val second_turn : t list
end

module User : sig
  val name : string
  val initials : string
  val host : string
  val model : string
  val thinking : string
  val mode : string
  val cwd : string
  val branch : string
end
