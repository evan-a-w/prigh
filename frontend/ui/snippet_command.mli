open! Core
open! Import

(** [/snippet]'s arguments, shared by the terminal and browser frontends:
    [/snippet] picks, [/snippet NAME] inserts, [/snippet add NAME [TEXT]] and
    [/snippet rm NAME] change the [snippets] of the config. *)

module Action : sig
  type t =
    | Pick of P.Snippet.t list (** non-empty *)
    | Insert of P.Snippet.t (** at the cursor, without sending *)
    | Save of
        { snippets : P.Snippet.t list (** the whole new list *)
        ; notice : string (** once [set_config] succeeds *)
        }
    | Info of string
    | Error of string
  [@@deriving sexp_of]
end

(** [/snippet]'s usage line. *)
val usage : string

(** [snippets] is [None] until the config has loaded; [last_prompt] is the
    text [/snippet add NAME] saves when no TEXT follows it. *)
val run
  :  snippets:P.Snippet.t list option
  -> last_prompt:string option
  -> string (** what follows [/snippet] *)
  -> Action.t

(** [Pick], or what to do when there is nothing to pick. *)
val pick : P.Snippet.t list option -> Action.t

(** The [set_config] fields that save [snippets]. *)
val config_fields : P.Snippet.t list -> (string * P.Json.t) list
