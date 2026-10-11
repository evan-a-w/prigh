open! Core
open! Prigh

let show_parse patch =
  match Apply_patch.parse patch with
  | Ok operations -> print_s [%sexp (operations : Apply_patch.Operation.t list)]
  | Error error -> print_endline ("ERROR: " ^ Error.to_string_hum error)
;;

let show_apply ?(files = []) patch =
  let files = String.Map.of_alist_exn files in
  match
    Or_error.bind (Apply_patch.parse patch) ~f:(fun operations ->
      Apply_patch.apply operations ~read:(Map.find files))
  with
  | Ok changes ->
    List.iter changes ~f:(fun (change : Apply_patch.Change.t) ->
      match change with
      | Added { path; contents } -> printf "=== added %s\n%s" path contents
      | Deleted { path; before = _ } -> printf "=== deleted %s\n" path
      | Updated { path; move_to; before = _; after } ->
        printf
          "=== updated %s%s\n%s"
          path
          (match move_to with
           | None -> ""
           | Some to_ -> " -> " ^ to_)
          after)
  | Error error -> print_endline ("ERROR: " ^ Error.to_string_hum error)
;;

let%expect_test "parse: every operation" =
  show_parse
    {|*** Begin Patch
*** Add File: new.txt
+hello
+
+world
*** Delete File: old.txt
*** Update File: src/a.ml
*** Move to: src/b.ml
@@ let f x =
 context
-removed
+added
@@
 more
+tail
*** End of File
*** End Patch|};
  [%expect
    {|
    ((Add (path new.txt) (contents  "hello\
                                   \n\
                                   \nworld\
                                   \n"))
     (Delete (path old.txt))
     (Update (path src/a.ml) (move_to (src/b.ml))
      (chunks
       (((context ("let f x =")) (old_lines (context removed))
         (new_lines (context added)) (end_of_file false))
        ((context ()) (old_lines (more)) (new_lines (more tail))
         (end_of_file true))))))
    |}]
;;

let%expect_test "parse: leniencies" =
  (* Surrounding whitespace, CRLF, a heredoc wrapper, no @@ on the first
     hunk, an empty line as empty context. *)
  show_parse
    "\n\n\
    \  apply_patch <<'EOF'\r\n\
     *** Begin Patch\r\n\
     *** Update File: a\r\n\
    \ x\r\n\
     \r\n\
     -y\r\n\
     +z\r\n\
     *** End Patch\r\n\
     EOF  \n\n";
  [%expect
    {|
    ((Update (path a) (move_to ())
      (chunks
       (((context ()) (old_lines (x "" y)) (new_lines (x "" z))
         (end_of_file false))))))
    |}];
  show_parse "<<EOF\n*** Begin Patch\n*** Delete File: a\n*** End Patch\nEOF";
  [%expect {| ((Delete (path a))) |}];
  (* Blank lines between operations, several @@ lines narrowing the location. *)
  show_parse
    {|*** Begin Patch
*** Delete File: a

*** Update File: b
@@ class C
@@     def m():
-x
+y
*** End Patch
|};
  [%expect
    {|
    ((Delete (path a))
     (Update (path b) (move_to ())
      (chunks
       (((context ("class C")) (old_lines ()) (new_lines ()) (end_of_file false))
        ((context ("    def m():")) (old_lines (x)) (new_lines (y))
         (end_of_file false))))))
    |}]
;;

let%expect_test "parse errors" =
  show_parse "";
  show_parse "hello\n*** End Patch";
  show_parse "*** Begin Patch\n*** Delete File: a";
  show_parse "*** Begin Patch\n*** End Patch";
  show_parse "*** Begin Patch\n*** Frobnicate File: a\n*** End Patch";
  show_parse "*** Begin Patch\n*** Delete File: a\n+stray\n*** End Patch";
  show_parse "*** Begin Patch\n*** Update File: a\n*** End Patch";
  show_parse
    "*** Begin Patch\n*** Update File: a\n*** Delete File: b\n*** End Patch";
  show_parse "*** Begin Patch\n*** Update File: a\nfoo\n*** End Patch";
  show_parse "*** Begin Patch\n*** Update File: a\n@@\n*** End Patch";
  show_parse
    "*** Begin Patch\n*** Update File: a\n@@\n*** End of File\n*** End Patch";
  show_parse
    "*** Begin Patch\n\
     *** Update File: a\n\
     @@\n\
     -x\n\
     *** End of File\n\
     foo\n\
     *** End Patch";
  show_parse "*** Begin Patch\n*** Add File:\n+x\n*** End Patch";
  show_parse "*** Begin Patch\n*** Add File: \n+x\n*** End Patch";
  (* Line numbers count from the start of the text, wrapper included. *)
  show_parse "\n\n<<'EOF'\n*** Begin Patch\n*** Nope\n*** End Patch\nEOF\n";
  [%expect
    {|
    ERROR: the patch is empty: expected '*** Begin Patch'
    ERROR: line 1: expected '*** Begin Patch', got 'hello'
    ERROR: line 2: expected '*** End Patch' as the last line of the patch, got '*** Delete File: a'
    ERROR: line 2: the patch contains no file operations: expected '*** Add File: {path}', '*** Delete File: {path}' or '*** Update File: {path}' before '*** End Patch'
    ERROR: line 2: invalid hunk header '*** Frobnicate File: a': valid headers are '*** Add File: {path}', '*** Delete File: {path}', '*** Update File: {path}'
    ERROR: line 3: invalid hunk header '+stray': valid headers are '*** Add File: {path}', '*** Delete File: {path}', '*** Update File: {path}'
    ERROR: line 2: Update File hunk for path 'a' is empty
    ERROR: line 2: Update File hunk for path 'a' is empty
    ERROR: line 3: unexpected line found in update hunk: 'foo'. Every line should start with ' ' (context line), '+' (added line), or '-' (removed line)
    ERROR: line 4: update hunk does not contain any lines
    ERROR: line 4: update hunk does not contain any lines
    ERROR: line 6: expected update hunk to start with a @@ context marker, got: 'foo'
    ERROR: line 2: invalid hunk header '*** Add File:': valid headers are '*** Add File: {path}', '*** Delete File: {path}', '*** Update File: {path}'
    ERROR: line 2: invalid hunk header '*** Add File:': valid headers are '*** Add File: {path}', '*** Delete File: {path}', '*** Update File: {path}'
    ERROR: line 5: invalid hunk header '*** Nope': valid headers are '*** Add File: {path}', '*** Delete File: {path}', '*** Update File: {path}'
    |}]
;;

let%expect_test "apply: add, delete, update, move" =
  show_apply
    ~files:[ "old.txt", "bye\n"; "a.ml", "let x = 1\nlet y = 2\nlet z = 3\n" ]
    {|*** Begin Patch
*** Add File: new.txt
+hello
+world
*** Delete File: old.txt
*** Update File: a.ml
*** Move to: b.ml
@@
 let x = 1
-let y = 2
+let y = 20
+let w = 4
 let z = 3
*** End Patch|};
  [%expect
    {|
    === added new.txt
    hello
    world
    === deleted old.txt
    === updated a.ml -> b.ml
    let x = 1
    let y = 20
    let w = 4
    let z = 3
    |}];
  (* Adding an existing file replaces it; a missing trailing newline is
     added. *)
  show_apply
    ~files:[ "a", "old\n"; "b", "one\ntwo" ]
    "*** Begin Patch\n\
     *** Add File: a\n\
     +new\n\
     *** Update File: b\n\
     -two\n\
     +2\n\
     *** End Patch";
  [%expect
    {|
    === updated a
    new
    === updated b
    one
    2
    |}]
;;

let%expect_test "apply: several hunks, in order" =
  show_apply
    ~files:[ "f", "a\nb\nc\nd\ne\nf\ng\n" ]
    {|*** Begin Patch
*** Update File: f
@@
 a
-b
+B
 c
@@
 e
-f
+F
*** End Patch|};
  [%expect
    {|
    === updated f
    a
    B
    c
    d
    e
    F
    g
    |}];
  (* Hunks out of order: the second search starts after the first. *)
  show_apply
    ~files:[ "f", "a\nb\nc\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n-c\n+C\n@@\n-a\n+A\n*** End Patch";
  [%expect
    {|
    ERROR: Failed to find expected lines in f:
    a
    |}]
;;

let%expect_test "apply: @@ context picks among duplicate lines" =
  let files =
    [ ( "m.py"
      , "class A:\n\
        \    def f():\n\
        \        return 1\n\
         class B:\n\
        \    def f():\n\
        \        return 1\n" )
    ]
  in
  show_apply
    ~files
    {|*** Begin Patch
*** Update File: m.py
@@ class B:
     def f():
-        return 1
+        return 2
*** End Patch|};
  [%expect
    {|
    === updated m.py
    class A:
        def f():
            return 1
    class B:
        def f():
            return 2
    |}];
  (* Several @@ lines narrow it down. *)
  show_apply
    ~files
    {|*** Begin Patch
*** Update File: m.py
@@ class B:
@@     def f():
-        return 1
+        return 3
*** End Patch|};
  [%expect
    {|
    === updated m.py
    class A:
        def f():
            return 1
    class B:
        def f():
            return 3
    |}];
  show_apply
    ~files
    "*** Begin Patch\n*** Update File: m.py\n@@ class C:\n-x\n+y\n*** End Patch";
  [%expect {| ERROR: Failed to find context 'class C:' in m.py |}]
;;

let%expect_test "apply: end of file" =
  show_apply
    ~files:[ "f", "x\ny\nx\ny\n" ]
    "*** Begin Patch\n\
     *** Update File: f\n\
     @@\n\
    \ x\n\
     -y\n\
     +Y\n\
     *** End of File\n\
     *** End Patch";
  [%expect
    {|
    === updated f
    x
    y
    x
    Y
    |}];
  (* Not at the end after all: falls back to searching from the start. *)
  show_apply
    ~files:[ "f", "x\ny\nz\n" ]
    "*** Begin Patch\n\
     *** Update File: f\n\
     @@\n\
    \ x\n\
     -y\n\
     +Y\n\
     *** End of File\n\
     *** End Patch";
  [%expect
    {|
    === updated f
    x
    Y
    z
    |}]
;;

let%expect_test "apply: fuzzy matching" =
  (* Trailing whitespace in the file. *)
  show_apply
    ~files:[ "f", "a  \nb\t\nc\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n a\n-b\n+B\n*** End Patch";
  [%expect
    {|
    === updated f
    a
    B
    c
    |}];
  (* Different indentation. *)
  show_apply
    ~files:[ "f", "    if x:\n        y()\n" ]
    "*** Begin Patch\n\
     *** Update File: f\n\
     @@\n\
    \ if x:\n\
     -  y()\n\
     +        z()\n\
     *** End Patch";
  [%expect
    {|
    === updated f
    if x:
            z()
    |}];
  (* Typographic punctuation in the file, ASCII in the patch. *)
  show_apply
    ~files:
      [ "f", "say \u{201C}hi\u{201D} \u{2014} it\u{2019}s\u{00A0}fine\nend\n" ]
    "*** Begin Patch\n\
     *** Update File: f\n\
     @@\n\
     -say \"hi\" - it's fine\n\
     +said\n\
    \ end\n\
     *** End Patch";
  [%expect
    {|
    === updated f
    said
    end
    |}];
  (* An exact match later in the file beats a fuzzy one earlier. *)
  show_apply
    ~files:[ "f", "a \nb\na\nc\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n-a\n+A\n*** End Patch";
  [%expect
    {|
    === updated f
    a
    b
    A
    c
    |}]
;;

let%expect_test "apply: a trailing empty context line that is not in the file" =
  show_apply
    ~files:[ "f", "a\nb\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n a\n-b\n+B\n\n*** End Patch";
  [%expect
    {|
    === updated f
    a
    B
    |}]
;;

let%expect_test "apply: pure insertion" =
  show_apply
    ~files:[ "f", "a\nb\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n+c\n+d\n*** End Patch";
  [%expect
    {|
    === updated f
    a
    b
    c
    d
    |}];
  show_apply
    ~files:[ "f", "a\nb\n" ]
    "*** Begin Patch\n*** Update File: f\n@@ a\n+after a\n*** End Patch";
  [%expect
    {|
    === updated f
    a
    after a
    b
    |}];
  show_apply
    ~files:[ "empty", "" ]
    "*** Begin Patch\n*** Update File: empty\n+first\n*** End Patch";
  [%expect
    {|
    === updated empty
    first
    |}]
;;

let%expect_test "apply: several operations on one file" =
  show_apply
    ~files:[ "f", "1\n2\n" ]
    {|*** Begin Patch
*** Update File: f
-1
+one
*** Update File: f
*** Move to: g
-2
+two
*** Update File: g
+three
*** Delete File: g
*** Add File: f
+again
*** End Patch|};
  [%expect
    {|
    === updated f
    one
    2
    === updated f -> g
    one
    two
    === updated g
    one
    two
    three
    === deleted g
    === added f
    again
    |}];
  show_apply
    ~files:[ "f", "1\n" ]
    "*** Begin Patch\n*** Delete File: f\n*** Update File: f\n-1\n*** End Patch";
  [%expect
    {| ERROR: Failed to update f: the file does not exist (use '*** Add File: f' to create it) |}]
;;

let%expect_test "apply errors" =
  show_apply
    "*** Begin Patch\n*** Update File: missing.ml\n-x\n+y\n*** End Patch";
  show_apply "*** Begin Patch\n*** Delete File: missing.ml\n*** End Patch";
  show_apply
    ~files:[ "f", "a\nb\n" ]
    "*** Begin Patch\n*** Update File: f\n@@\n a\n-c\n+C\n*** End Patch";
  [%expect
    {|
    ERROR: Failed to update missing.ml: the file does not exist (use '*** Add File: missing.ml' to create it)
    ERROR: Failed to delete missing.ml: the file does not exist
    ERROR: Failed to find expected lines in f:
    a
    c
    |}]
;;

let%expect_test "apply: Changed carries contents before" =
  let operations =
    Apply_patch.parse
      "*** Begin Patch\n\
       *** Delete File: d\n\
       *** Update File: u\n\
       -x\n\
       +y\n\
       *** End Patch"
    |> ok_exn
  in
  print_s
    [%sexp
      (Apply_patch.apply operations ~read:(fun path ->
         Some ("x of " ^ path ^ "\nx\n"))
       : Apply_patch.Change.t list Or_error.t)];
  [%expect
    {|
    (Ok
     ((Deleted (path d) (before  "x of d\
                                \nx\
                                \n"))
      (Updated (path u) (move_to ()) (before  "x of u\
                                             \nx\
                                             \n")
       (after  "x of u\
              \ny\
              \n"))))
    |}];
  print_s
    [%sexp (List.map operations ~f:Apply_patch.Operation.path : string list)];
  [%expect {| (d u) |}]
;;

let%expect_test "paths" =
  let patch =
    {|*** Begin Patch
*** Add File: new.txt
+x
*** Update File: a.ml
*** Move to: b.ml
@@
-x
*** Delete File: old.txt
*** Update File: a.ml
-y
*** End Patch|}
  in
  print_s [%sexp (Apply_patch.paths patch : string list)];
  [%expect {| (new.txt a.ml b.ml old.txt) |}];
  let truncated = String.prefix patch 70 in
  print_endline truncated;
  print_s [%sexp (Apply_patch.paths truncated : string list)];
  [%expect
    {|
    *** Begin Patch
    *** Add File: new.txt
    +x
    *** Update File: a.ml
    *** Mov
    (new.txt a.ml)
    |}];
  print_s
    [%sexp
      (Apply_patch.paths "*** Begin Patch\n*** Update File:" : string list)];
  [%expect {| () |}]
;;

let%expect_test "grammar and description" =
  print_endline Apply_patch.grammar;
  [%expect
    {|
    start: begin_patch hunk+ end_patch
    begin_patch: "*** Begin Patch" LF
    end_patch: "*** End Patch" LF?

    hunk: add_hunk | delete_hunk | update_hunk
    add_hunk: "*** Add File: " filename LF add_line+
    delete_hunk: "*** Delete File: " filename LF
    update_hunk: "*** Update File: " filename LF change_move? change?

    filename: /(.+)/
    add_line: "+" /(.*)/ LF -> line

    change_move: "*** Move to: " filename LF
    change: (change_context | change_line)+ eof_line?
    change_context: ("@@" | "@@ " /(.+)/) LF
    change_line: ("+" | "-" | " ") /(.*)/ LF
    eof_line: "*** End of File" LF

    %import common.LF
    |}];
  print_endline Apply_patch.description;
  [%expect
    {|
    Use the `apply_patch` tool to edit files.
    Your patch language is a stripped-down, file-oriented diff format designed to be easy to parse and safe to apply. You can think of it as a high-level envelope:

    *** Begin Patch
    [ one or more file sections ]
    *** End Patch

    Within that envelope, you get a sequence of file operations.
    You MUST include a header to specify the action you are taking.
    Each operation starts with one of three headers:

    *** Add File: <path> - create a new file. Every following line is a + line (the initial contents).
    *** Delete File: <path> - remove an existing file. Nothing follows.
    *** Update File: <path> - patch an existing file in place (optionally with a rename).

    May be immediately followed by *** Move to: <new path> if you want to rename the file.
    Then one or more "hunks", each introduced by @@ (optionally followed by a hunk header).
    Within a hunk each line starts with:
    - " " (a space) for a context line, unchanged
    - "-" for a line to remove
    - "+" for a line to add

    For instructions on [context_before] and [context_after]:
    - By default, show 3 lines of code immediately above and 3 lines immediately below each change. If a change is within 3 lines of a previous change, do NOT duplicate the first change's [context_after] lines in the second change's [context_before] lines.
    - If 3 lines of context is insufficient to uniquely identify the snippet of code within the file, use the @@ operator to indicate the class or function to which the snippet belongs. For instance, we might have:
    @@ class BaseClass
    [3 lines of pre-context]
    -[old_code]
    +[new_code]
    [3 lines of post-context]

    - If a code block is repeated so many times in a class or function such that even a single @@ statement and 3 lines of context cannot uniquely identify the snippet of code, you can use multiple @@ statements to jump to the right context. For instance:

    @@ class BaseClass
    @@     def method():
    [3 lines of pre-context]
    -[old_code]
    +[new_code]
    [3 lines of post-context]

    - Hunks must appear in the order their changes occur in the file.
    - If a hunk changes the last lines of the file, end it with *** End of File.

    The full grammar definition is below:
    Patch := Begin { FileOp } End
    Begin := "*** Begin Patch" NEWLINE
    End := "*** End Patch" NEWLINE
    FileOp := AddFile | DeleteFile | UpdateFile
    AddFile := "*** Add File: " path NEWLINE { "+" line NEWLINE }
    DeleteFile := "*** Delete File: " path NEWLINE
    UpdateFile := "*** Update File: " path NEWLINE [ MoveTo ] { Hunk }
    MoveTo := "*** Move to: " newPath NEWLINE
    Hunk := "@@" [ header ] NEWLINE { HunkLine } [ "*** End of File" NEWLINE ]
    HunkLine := (" " | "-" | "+") text NEWLINE

    A full patch can combine several operations:

    *** Begin Patch
    *** Add File: hello.txt
    +Hello world
    *** Update File: src/app.py
    *** Move to: src/main.py
    @@ def greet():
    -print("Hi")
    +print("Hello, world!")
    *** Delete File: obsolete.txt
    *** End Patch

    It is important to remember:

    - You must include a header with your intended action (Add/Delete/Update)
    - You must prefix new lines with `+` even when creating a new file
    - Use paths relative to the working directory (absolute paths also work, but prefer relative ones)
    |}]
;;
