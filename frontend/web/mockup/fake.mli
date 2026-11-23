(** The mockups' made-up data. *)

module Project : sig
  (** [name]: one word, from the git repository or the directory. *)
  type t = { name : string }

  val prigh : t
  val loon : t
  val co : t
end

module Status : sig
  type t =
    | Thinking
    | Working (** running tools, or only a subagent is *)
    | Idle
end

module Session : sig
  type t =
    { project : Project.t
    ; title : string
    ; age : string (** since it last worked: "now", "12m" *)
    ; status : Status.t
    }

  val current : t
  val recent : t list
  val settled : int
end

module Subagent : sig
  type t =
    { name : string
    ; task : string
    ; status : Status.t
    ; age : string
    }

  val all : t list
end

module Worktree : sig
  type t =
    { branch : string
    ; path : string
    ; sessions : int
    ; added : int
    ; removed : int
    }

  val all : t list
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
