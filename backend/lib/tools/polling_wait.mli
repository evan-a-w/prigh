open! Core
open! Import

(** Spots commands that wait by polling, which [bash] refuses (they block
    the agent, and often spin until their timeout):

    - [pgrep -f]/[pkill -f] whose pattern matches the command's own text:
      the shell running it ([bash -c COMMAND]) always matches, so [pgrep]
      always finds a process and [pkill] kills the shell itself;
    - a [while]/[until] loop that sleeps (in its condition or its body, for
      1 second or more, or an unknown time), unless a [timeout] of at most
      {!max_bounded_seconds} comes before it or the call's own timeout is
      that short;
    - a [for] loop over a known number of items ([seq N], [{A..B}] or a
      list of words) whose sleeps add up to more than {!max_bounded_seconds}.

    The check is lexical (words, with quotes removed so that loops inside
    [bash -c '...'] count): it is a guard against a habit, not a parser. *)

val max_bounded_seconds : float

(** The refusal, saying what to do instead, if [command] waits by
    polling. [timeout] is the call's own, in seconds. *)
val check : ?timeout:int -> string -> string option
