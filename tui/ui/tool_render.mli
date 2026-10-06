open! Core
open! Import

(** A tool call as a step of the work log: its outcome mark, its name and the
    one argument that says what it did (a path, [$ command], a task), chips
    (line counts, [+4 −2], [exit code 1]), then output or a diff as much as the
    verbosity shows. *)

module Mark : sig
  type t =
    | Running
    | Done
    | Failed
    | Interrupted

  val span : t -> Content.Span.t
end

val chip : ?style:Style.t -> string -> Content.Span.t

(** The header line. [arg] is cut to fit [width] with the chips. *)
val header
  :  width:int
  -> mark:Mark.t
  -> name:string
  -> ?arg:string
  -> Content.Span.t list
  -> Log_line.t

(** [name arg], on one line: what a subagent is doing. *)
val summary : P.Tool_call.t -> string

(** Output under a step: the first [head] lines, a count of the hidden ones,
    and the last [tail]. *)
val output
  :  ?style:Style.t
  -> head:int
  -> ?tail:int
  -> string
  -> Log_line.t list

(** How much of a running tool's output is kept. *)
val live_tail_lines : int

val render
  :  verbosity:Verbosity.t
  -> width:int
  -> P.Tool_call.t
  -> P.Message.Tool_result.t option
  -> live_tail:string option
  -> Log_line.t list
