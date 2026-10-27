open! Core

(** Which kinds of the transcript's items are on show ([/filter]): a view
    preference only, the model's context is unchanged. *)

module Kind : sig
  type t =
    | User (** your messages, and the skills you ran *)
    | Assistant (** the model's replies *)
    | Thinking
    | Tools (** tool calls with their output, and the model's web searches *)
    | Subagents (** [subagent] calls with their transcripts *)
    | Notes
    (** what the harness says: notices, errors and how a reply stopped,
        compactions, hand-overs and other notes, reports of background work *)
  [@@deriving sexp_of, equal, compare, enumerate]

  (** One word, as [/filter] takes it and the status line shows it. *)
  val name : t -> string

  val label : t -> string
  val detail : t -> string

  (** A [name], or a word for it ([tool], [agents], [notices]...). *)
  val of_name : string -> t option

  (** A tool call's kind, by the tool's name. *)
  val of_tool : string -> t
end

type t [@@deriving sexp_of, equal]

(** Everything on show. *)
val all : t

val is_all : t -> bool
val shows : t -> Kind.t -> bool

(** In [Kind.all]'s order. *)
val hidden : t -> Kind.t list

val of_hidden : Kind.t list -> t
val toggle : t -> Kind.t -> t

(** The hidden kinds' names, comma-separated (empty for [all]): how a
    preference keeps it. *)
val to_string : t -> string

(** Unknown names are left out. *)
val of_string : string -> t

(** For the status line: [−thinking −tools]; [None] for [all]. *)
val summary : t -> string option

(** [/filter]'s arguments, applied to [t]: [all] shows everything; bare kinds
    show only those, then [-KIND] hides one and [+KIND] shows it again. An
    unknown kind is an error that names the closest one and the kinds. *)
val parse : t -> string -> (t, string) Result.t

(** The words [/filter] completes: [all], then each kind. *)
val words : string list
