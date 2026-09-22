Only have one dune process running at a time, because otherwise it hangs. Eg. if
you are running tests, don't also dune exec.

You may need to run eval $(opam env) before dune commands, eg.
[eval $(opam env) && dune build]. You can try a single normal [dune build] initially, and if it fails with no explanation (eg. no diffs and I didn't say build was broken) that's probably the reason.

You should prefer defining types as a [type t] inside a module with the name of
the type. Eg. [type undo_entry = ...] should be [module Undo_entry = struct type
t = ... end].

Always run ocamlformat after you finish (eg. ocamlformat --inplace */*.ml; ocamlformat */*.mli).

Prefer to write explicit interfaces for new files in the mli, rather than just
creating an ml file. If the mli and the ml file duplicate definitions of module
types (eg. for functors), use a \*_intf.ml file where you define the module types
and the resulting mli module type, include that from the ml, and make the mli simply [include X_intf.X] for X.mli

Prefer to write tests in expect test style (let%expect_test ..., prefer printing sexps to demonstrate state rather than eg. asserting things are equal to some manually defined value, look at the output of dune build @runtest, and if diffs are acceptable run dune promote to accept the changes)

Prefer to modularize code where possible. For instance, if there is functionality that is self contained, you should create a file module.ml with a [type t] defined inside, and expose only the minimal necessary functionality in the .mli file.

You can run the benchmarks, using the same commands as the files under
bench/results/, which are split by platform and benchmark:
bench/results/{mac,windows}/{sat_dimacs,smt}.txt. You can add entries in a
similar format, to the file matching the platform you are on. Be brief in
descriptions of changes. Add entries to the TOP of the file, that's where things
are added.

See bench/README.md for the benchmark comparison and profiling workflow
(filtering with -only, saving/comparing results, flamegraph and memtrace
profiling).

Eagerly remove dead code, unless you think it has a good chance of being useful
in future, and it doesn't add complexity / performance losses / prevent optimisations.

For common modules in Core, like [Table], [Set] etc., you should refer the re-exported/functor applied modules, like [Int.Table.t]. [Int.Table] (and equivalent fo [Set], [Map], etc.) have functions specific to the [Int] table, like [create], [of_list], etc., but not the functions that are the same for all tables (eg. find, where instead you need [Hashtbl.find], [Map.find] etc.). This applies everywhere these generic [Core] modules exist, and you should use them this way, rather than doing stuff like [Hashtbl.create (module Int)] or referring to the type as [(int, int) Hashtbl.t].

Types should typically be in a module like [Type] and be [Type.t], rather than [type type_].

Use the import.ml module to import things.

Only write comments for things that are truly hard to understand without them.
Don't just restate things that can be intuited based on context, names, code, etc.

If you see that I have changed a module, do not revert those changes, but apply
the ideas elsewhere. For instance, I might change the API slightly to make it
cleaner, or remove/reword comments, and you should respect that.

Don't write useless comments.

Please limit usage of claude models. If I have explicitly chosen you to be
claude, that's fine, but for subagents prefer to use non claude models.

Test comprehensively, such that we can be fully confident in correctness, and
don't consider work done until tests are present and working. Prefer
having human readable test output (in expect test style) rather than checking
(eg. don't just [assert_true] some condition, print something demonstrating it).
