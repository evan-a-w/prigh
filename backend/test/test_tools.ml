open! Core
open! Prigh
open Tool_test_helpers

let%expect_test "registry" =
  print_s [%sexp (List.map Tools.all ~f:Tool.name : string list)];
  print_s
    [%sexp
      (Option.is_some (Tools.find "bash") : bool)
    , (Option.is_some (Tools.find "nope") : bool)];
  [%expect
    {|
    (bash read write edit ls grep find)
    (true false)
    |}]
;;

let%expect_test "bash: output, exit codes, cwd, streaming" =
  with_sandbox
  @@ fun t ->
  run t Tool_bash.tool {|{"command": "echo hello; echo err >&2"}|};
  [%expect
    {|
    hello
    err
    |}];
  run t Tool_bash.tool {|{"command": "echo partial; exit 4"}|};
  [%expect
    {|
    ERROR: partial
    [exit code 4]
    |}];
  run t Tool_bash.tool {|{"command": "pwd"}|};
  [%expect {| $DIR |}];
  let chunks = ref [] in
  run
    t
    ~on_output:(fun s -> chunks := s :: !chunks)
    Tool_bash.tool
    {|{"command": "printf a; sleep 0.05; printf b"}|};
  print_s [%sexp (List.rev !chunks : string list)];
  [%expect
    {|
    ab
    (a b)
    |}];
  run t Tool_bash.tool {|{"command": "echo x", "timeout": "soon"}|};
  [%expect
    {| ERROR: invalid arguments: argument "timeout" must be an integer |}];
  run t Tool_bash.tool {|{}|};
  [%expect {| ERROR: invalid arguments: missing required argument "command" |}]
;;

let%expect_test "bash: timeout, cancellation, truncation" =
  with_sandbox
  @@ fun t ->
  run
    t
    Tool_bash.tool
    {|{"command": "echo start; sleep 5; echo never", "timeout": 1}|};
  [%expect
    {|
    ERROR: start
    [timed out after 1s]
    |}];
  let cancel = Cancellation.create () in
  Eio.Fiber.both
    (fun () -> run t ~cancel Tool_bash.tool {|{"command": "echo go; sleep 5"}|})
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.1;
       Cancellation.cancel cancel);
  [%expect
    {|
    ERROR: go
    [cancelled]
    |}];
  run t Tool_bash.tool {|{"command": "seq 1 3000"}|};
  let output = [%expect.output] in
  let lines = String.split_lines output in
  print_s
    [%sexp
      { first = (List.hd_exn lines : string)
      ; second = (List.nth_exn lines 1 : string)
      ; last = (List.last_exn lines : string)
      ; count = (List.length lines : int)
      }];
  [%expect
    {|
    ((first
      "[output truncated: showing the last part of 3000 lines / 13893 bytes]")
     (second 1001) (last 3000) (count 2001))
    |}]
;;

let%expect_test "read" =
  with_sandbox
  @@ fun t ->
  write t "a.txt" "line1\nline2\nline3\nline4\n";
  run t Tool_read.tool {|{"path": "a.txt"}|};
  [%expect
    {|
    line1
    line2
    line3
    line4
    |}];
  run t Tool_read.tool {|{"path": "a.txt", "offset": 2, "limit": 2}|};
  [%expect
    {|
    line2
    line3

    [showing lines 2-3 of 4; use offset=4 to continue]
    |}];
  run t Tool_read.tool {|{"path": "a.txt", "offset": 4}|};
  [%expect {| line4 |}];
  run t Tool_read.tool {|{"path": "a.txt", "offset": 0}|};
  [%expect {| ERROR: invalid arguments: offset must be >= 1 |}];
  run t Tool_read.tool {|{"path": "missing.txt"}|};
  [%expect {| ERROR: file not found: $DIR/missing.txt |}];
  run t Tool_read.tool {|{"path": "."}|};
  [%expect {| ERROR: $DIR is a directory; use ls |}];
  write t "bin.dat" "abc\000def";
  run t Tool_read.tool {|{"path": "bin.dat"}|};
  [%expect {| ERROR: $DIR/bin.dat looks like a binary file |}];
  write t "big.txt" (String.concat_lines (List.init 3000 ~f:Int.to_string));
  run t Tool_read.tool {|{"path": "big.txt"}|};
  let output = [%expect.output] in
  let lines = String.split_lines output in
  print_s [%sexp (List.length lines : int), (List.last_exn lines : string)];
  [%expect
    {| (2002 "[showing lines 1-2000 of 3000; use offset=2001 to continue]") |}];
  run t Tool_read.tool (sprintf {|{"path": "%s/a.txt", "limit": 1}|} t.dir);
  [%expect
    {|
    line1

    [showing lines 1-1 of 4; use offset=2 to continue]
    |}]
;;

let%expect_test "read_for_context: shared truncation and errors" =
  with_sandbox
  @@ fun t ->
  write t "a.txt" "line1\nline2\n";
  let show = function
    | Ok s -> print_endline (mask t ("Ok:\n" ^ s))
    | Error e -> print_endline (mask t ("Error: " ^ Error.to_string_hum e))
  in
  show (Tool_read.read_for_context ~cwd:t.dir "a.txt");
  show (Tool_read.read_for_context ~cwd:t.dir "missing.txt");
  show (Tool_read.read_for_context ~cwd:t.dir ".");
  write t "big.txt" (String.concat_lines (List.init 3000 ~f:Int.to_string));
  (match Tool_read.read_for_context ~cwd:t.dir "big.txt" with
   | Error _ -> print_endline "unexpected error"
   | Ok s ->
     let lines = String.split_lines s in
     print_s [%sexp (List.length lines : int), (List.last_exn lines : string)]);
  [%expect
    {|
    Ok:
    line1
    line2

    Error: file not found: $DIR/missing.txt
    Error: $DIR is a directory; use ls
    (2002 "[showing lines 1-2000 of 3000; use offset=2001 to continue]")
    |}]
;;

let%expect_test "write" =
  with_sandbox
  @@ fun t ->
  run
    t
    Tool_write.tool
    {|{"path": "new/deep/file.txt", "content": "one\ntwo\nthree\n"}|};
  [%expect {| wrote 3 lines to $DIR/new/deep/file.txt |}];
  print_string (read t "new/deep/file.txt");
  [%expect
    {|
    one
    two
    three
    |}];
  run t Tool_write.tool {|{"path": "new/deep/file.txt", "content": "bye"}|};
  [%expect {| overwrote 1 line to $DIR/new/deep/file.txt |}];
  print_string (read t "new/deep/file.txt");
  [%expect {| bye |}]
;;

let%expect_test "edit" =
  with_sandbox
  @@ fun t ->
  write
    t
    "f.ml"
    (String.concat_lines
       (List.init 10 ~f:(fun i -> sprintf "line%02d" (i + 1))));
  run
    t
    Tool_edit.tool
    {|{"path": "f.ml", "edits": [{"old_text": "line05", "new_text": "line05 changed"}]}|};
  [%expect
    {|
    --- a/f.ml
    +++ b/f.ml
    @@ -2,7 +2,7 @@
     line02
     line03
     line04
    -line05
    +line05 changed
     line06
     line07
     line08
    |}];
  print_string (read t "f.ml");
  [%expect
    {|
    line01
    line02
    line03
    line04
    line05 changed
    line06
    line07
    line08
    line09
    line10
    |}];
  run
    t
    Tool_edit.tool
    {|{"path": "f.ml", "edits": [{"old_text": "line", "new_text": "x"}]}|};
  [%expect
    {| ERROR: edit 1: old_text occurs 10 times; add context to make it unique |}];
  run
    t
    Tool_edit.tool
    {|{"path": "f.ml", "edits": [{"old_text": "nope", "new_text": "x"}]}|};
  [%expect {| ERROR: edit 1: old_text not found in file |}];
  run
    t
    Tool_edit.tool
    {|{"path": "f.ml", "edits": [{"old_text": "line05", "new_text": "X"}, {"old_text": "ne05", "new_text": "Y"}]}|};
  [%expect {| ERROR: edits overlap |}];
  run
    t
    Tool_edit.tool
    {|{"path": "f.ml", "edits": [{"old_text": "", "new_text": "x"}]}|};
  [%expect {| ERROR: edit 1: old_text must not be empty |}];
  run t Tool_edit.tool {|{"path": "f.ml", "edits": []}|};
  [%expect {| ERROR: invalid arguments: edits must be a non-empty array |}];
  run t Tool_edit.tool {|{"path": "f.ml", "edits": [{"old_text": "a"}]}|};
  [%expect {| ERROR: invalid arguments: missing required argument "new_text" |}];
  run
    t
    Tool_edit.tool
    {|{"path": "missing.ml", "edits": [{"old_text": "a", "new_text": "b"}]}|};
  [%expect {| ERROR: file not found: $DIR/missing.ml |}];
  print_string (read t "f.ml");
  [%expect
    {|
    line01
    line02
    line03
    line04
    line05 changed
    line06
    line07
    line08
    line09
    line10
    |}]
;;

let%expect_test "ls" =
  with_sandbox
  @@ fun t ->
  run t Tool_ls.tool {|{}|};
  [%expect {| (empty directory) |}];
  write t "b.txt" "";
  write t "a.txt" "";
  write t "sub/x" "";
  run t Tool_ls.tool {|{}|};
  [%expect
    {|
    a.txt
    b.txt
    sub/
    |}];
  run t Tool_ls.tool {|{"path": "sub"}|};
  [%expect {| x |}];
  run t Tool_ls.tool {|{"limit": 2}|};
  [%expect
    {|
    a.txt
    b.txt
    [2 of 3 entries shown]
    |}];
  run t Tool_ls.tool {|{"path": "a.txt"}|};
  [%expect {| ERROR: not a directory: $DIR/a.txt |}]
;;

let%expect_test "grep" =
  with_sandbox
  @@ fun t ->
  write t "a.ml" "let foo = 1\nlet bar = 2\n";
  write t "b.txt" "Foo here\nnothing\n";
  write t "sub/c.ml" "foo again\n";
  run t Tool_grep.tool {|{"pattern": "foo"}|};
  [%expect
    {|
    a.ml:1:let foo = 1
    sub/c.ml:1:foo again
    |}];
  run
    t
    Tool_grep.tool
    {|{"pattern": "foo", "ignore_case": true, "glob": "*.txt"}|};
  [%expect {| b.txt:1:Foo here |}];
  run t Tool_grep.tool {|{"pattern": "foo", "path": "sub"}|};
  [%expect {| sub/c.ml:1:foo again |}];
  run t Tool_grep.tool {|{"pattern": "zzz"}|};
  [%expect {| No matches found. |}];
  run t Tool_grep.tool {|{"pattern": "a", "limit": 1}|};
  [%expect
    {|
    a.ml:2:let bar = 2
    [results truncated; showing first 1 lines]
    |}];
  run t Tool_grep.tool {|{"pattern": "("}|};
  let output = [%expect.output] in
  print_s
    [%sexp
      (String.is_prefix output ~prefix:"ERROR: " : bool)
    , (String.is_substring output ~substring:"regex parse error" : bool)];
  [%expect {| (true true) |}]
;;

let%expect_test "find" =
  with_sandbox
  @@ fun t ->
  write t "a.ml" "";
  write t "src/b.ml" "";
  write t "src/deep/c.ml" "";
  write t "src/d.txt" "";
  write t ".gitignore" "ignored/\n";
  write t "ignored/e.ml" "";
  run t Tool_find.tool {|{"pattern": "*.ml"}|};
  [%expect
    {|
    a.ml
    src/b.ml
    src/deep/c.ml
    |}];
  run t Tool_find.tool {|{"pattern": "src/*.ml"}|};
  [%expect {| src/b.ml |}];
  run t Tool_find.tool {|{"pattern": "*.ml", "path": "src", "limit": 1}|};
  [%expect
    {|
    b.ml
    [1 of 2 results shown]
    |}];
  run t Tool_find.tool {|{"pattern": "*.rs"}|};
  [%expect {| No files found. |}]
;;

let%expect_test "resolve_path" =
  with_sandbox
  @@ fun t ->
  let context = Tool.Context.create ~env:t.env ~cwd:"/work" () in
  let home = Option.value_exn (Sys.getenv "HOME") in
  List.iter [ "rel/x"; "/abs/y"; "~/z"; "~" ] ~f:(fun p ->
    print_endline
      (String.substr_replace_all
         (Tool.resolve_path context p)
         ~pattern:home
         ~with_:"$HOME"));
  [%expect
    {|
    /work/rel/x
    /abs/y
    $HOME/z
    $HOME
    |}]
;;

let%expect_test "path listing: fd and the readdir fallback agree" =
  with_sandbox
  @@ fun t ->
  List.iter [ "src/sub"; "docs"; ".git/objects"; "_build/default" ] ~f:(fun d ->
    Core_unix.mkdir_p (Filename.concat t.dir d));
  List.iter
    [ "src/app.ml"
    ; "src/sub/deep.ml"
    ; "docs/README.md"
    ; ".git/HEAD"
    ; "_build/x"
    ]
    ~f:(fun f -> write t f "");
  let show paths = print_s [%sexp (paths : string list)] in
  show (Path_listing.list ~env:t.env ~root:t.dir ~prefix:"");
  show (Path_listing.readdir ~root:t.dir ~prefix:"");
  show (Path_listing.readdir ~root:t.dir ~prefix:"APP");
  show (Path_listing.readdir ~root:(Filename.concat t.dir "docs") ~prefix:"");
  [%expect
    {|
    (docs/ docs/README.md src/ src/app.ml src/sub/ src/sub/deep.ml)
    (docs/ docs/README.md src/ src/app.ml src/sub/ src/sub/deep.ml)
    (src/app.ml)
    (README.md)
    |}]
;;
