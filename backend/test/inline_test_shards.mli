(** The runner of [prigh_test]'s inline tests, which partitions them into a few
    shards of files rather than a process per file (each process pays the
    runner's start, about 25 ms).

    [-list-partitions] lists the shards, each named by its files joined with
    [","] (from the files' partitions, listed by running the runner again).
    [-partition] with such a name runs the runner again with an [-only-test]
    per file; anything else runs as ppx_inline_test's runner does. *)

val run : unit -> unit
