open! Core
open! Import

(** A list of the user's notes in a JSON file under their home, shared by
    {!Memory} and {!Custom_instructions}: [{"next": n, "notes": [...]}],
    oldest first, each note with an id ([<prefix><n>], never reused), an
    optional title, its text and when it was saved.

    Every change re-reads the file and replaces it atomically (a temporary
    file renamed over it) under the file's {!Lock_dir} and this value's
    mutex, so sessions and backends sharing a home do not lose each other's
    notes. An enabled switch is kept in the user's {!User_prefs} (absent:
    on). Make one per world. *)

module Note : sig
  type t =
    { id : string
    ; title : string option
    ; text : string
    ; saved : Time_float.t (** shown in UTC *)
    }
  [@@deriving sexp_of]

  (** [{"id", "title"?, "text", "saved"}], [saved] in UTC as
      [YYYY-MM-DD HH:MM:SS.ffffffZ]. *)
  val to_json : t -> Json.t

  (** Whether its title is [title] (stripped, compared without case). *)
  val has_title : t -> string -> bool
end

module Config : sig
  type t =
    { file : string (** under the home, e.g. [.prigh/memory.json] *)
    ; id_prefix : string (** [m]: [m1], [m2], ... *)
    ; enabled_key : string (** in {!User_prefs} *)
    ; max_text : int (** bytes *)
    ; max_title : int (** bytes *)
    ; noun : string (** [note]: "the note's text is empty" *)
    ; a_noun : string (** [a note]: "a note is limited to" *)
    ; too_long : string (** what to do about a text over [max_text] *)
    }
end

module File : sig
  type t =
    { next : int (** the number of the next id *)
    ; notes : Note.t list (** oldest first *)
    }
end

type t

val create : Config.t -> home:string -> t
val path : t -> string
val config : t -> Config.t

(** [f] runs after every change made through this value: by {!update} or
    {!set_enabled}. *)
val subscribe : t -> f:(unit -> unit) -> unit

val enabled : t -> bool

(** Records the switch (on: the key is removed); subscribers run when it
    changed. *)
val set_enabled : t -> bool -> unit Or_error.t

(** Every note, oldest first. *)
val notes : t -> Note.t list Or_error.t

(** Reads the file, applies [f] and writes what it returns; nothing is
    written when [f] fails. *)
val update : t -> f:(File.t -> (File.t * 'a) Or_error.t) -> 'a Or_error.t

(** The title (stripped; [None] when empty) and text (stripped), or why
    they are refused: an empty text, or one longer than [max_text], a title
    longer than [max_title]. *)
val checked
  :  t
  -> ?title:string
  -> string
  -> (string option * string) Or_error.t

(** Appends a new note with the next id. *)
val append
  :  t
  -> File.t
  -> title:string option
  -> now:Time_float.t
  -> string
  -> File.t * Note.t
