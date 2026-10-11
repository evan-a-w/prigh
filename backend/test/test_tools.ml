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

let%expect_test "the file editor suits the model" =
  let names tools = List.map tools ~f:Tool.name in
  List.iter
    [ "openai-codex/gpt-5.5"
    ; "openai/gpt-5"
    ; "openai/gpt-6.1-sol"
    ; "openai/gpt-5-chat-latest"
    ; "openai/gpt-4.1"
    ; "anthropic/claude-opus-4-6"
    ; "deepseek/deepseek-flash"
    ]
    ~f:(fun key ->
      let model = Option.value_exn (Model.find key) in
      print_s
        [%sexp
          (key : string)
        , (Tools.uses_apply_patch model : bool)
        , (names (Tools.for_model model Tools.all) : string list)]);
  [%expect
    {|
    (openai-codex/gpt-5.5 true (bash read write apply_patch ls grep find))
    (openai/gpt-5 true (bash read write apply_patch ls grep find))
    (openai/gpt-6.1-sol true (bash read write apply_patch ls grep find))
    (openai/gpt-5-chat-latest false (bash read write edit ls grep find))
    (openai/gpt-4.1 false (bash read write edit ls grep find))
    (anthropic/claude-opus-4-6 false (bash read write edit ls grep find))
    (deepseek/deepseek-flash false (bash read write edit ls grep find))
    |}];
  let gpt = Option.value_exn (Model.find "openai/gpt-5") in
  let claude = Option.value_exn (Model.find "anthropic/claude-opus-4-6") in
  (* Back again, and lists without an editor stay as they are. *)
  print_s
    [%sexp
      (names (Tools.for_model claude (Tools.for_model gpt Tools.all))
       : string list)
    , (names (Tools.for_model gpt [ Tool_bash.tool ]) : string list)
    , (Option.map (Tools.editor (Tools.for_model gpt Tools.all)) ~f:Tool.name
       : string option)
    , (Option.map (Tools.find "apply_patch") ~f:Tool.name : string option)];
  [%expect
    {| ((bash read write edit ls grep find) (bash) (apply_patch) (apply_patch)) |}]
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

let%expect_test
    "bash: invalid utf-8 is repaired, multibyte chars split across chunks \
     survive"
  =
  with_sandbox
  @@ fun t ->
  let chunks = ref [] in
  run
    t
    ~on_output:(fun s -> chunks := s :: !chunks)
    Tool_bash.tool
    {|{"command": "printf 'a\\xe2\\x88'; sleep 0.05; printf '\\xa8b'; sleep 0.05; printf 'x\\xc2\\n'"}|};
  print_s [%sexp (List.rev !chunks : string list)];
  [%expect
    {|
    a∨bx�
    (a "\226\136\168b" "x\239\191\189\n")
    |}]
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
  let read = Tool_read.read_for_context ~env:t.env ~cwd:t.dir in
  let show = function
    | { Tool.Result.is_error = false; text = s; _ } ->
      print_endline (mask t ("Ok:\n" ^ s))
    | { is_error = true; text = e; _ } -> print_endline (mask t ("Error: " ^ e))
  in
  show (read "a.txt");
  show (read "missing.txt");
  show (read ".");
  write t "big.txt" (String.concat_lines (List.init 3000 ~f:Int.to_string));
  (match read "big.txt" with
   | { is_error = true; _ } -> print_endline "unexpected error"
   | { text = s; _ } ->
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

let%expect_test "read: images are attached for the model" =
  with_sandbox
  @@ fun t ->
  let module F = Image_fixtures in
  write t "shot.png" (F.bytes F.png_3x2);
  (* Recognised by content, whatever the name. *)
  write t "photo.dat" (F.bytes F.jpeg_7x6);
  write t "anim.gif" (F.bytes F.gif_5x4);
  write t "x.bmp" (F.bytes F.bmp_2x2);
  run t Tool_read.tool {|{"path": "shot.png"}|};
  run t Tool_read.tool {|{"path": "photo.dat", "offset": 3}|};
  run t Tool_read.tool {|{"path": "anim.gif"}|};
  run t Tool_read.tool {|{"path": "x.bmp"}|};
  [%expect
    {|
    Read image file [image/png, 3x2]
    <image image/png, 84 bytes>
    Read image file [image/jpeg, 7x6]
    <image image/jpeg, 286 bytes>
    Read image file [image/gif, 5x4]
    <image image/gif, 45 bytes>
    ERROR: $DIR/x.bmp looks like a binary file
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
  [%expect
    {| ERROR: edit 1: old_text not found in file; read the file to get its current text |}];
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

let%expect_test "directory completion keeps the typed notation" =
  with_sandbox
  @@ fun t ->
  List.iter
    [ "src/sub"; "src/Support"; "docs"; ".hidden"; "other/proj/deep" ]
    ~f:(fun d -> Core_unix.mkdir_p (Filename.concat t.dir d));
  write t "src/app.ml" "";
  write t "notes.txt" "";
  let show ?(cwd = t.dir) prefix =
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~cwd
        ~name:Host_ops.list_dirs_op
        ~arguments:(`Object [ "prefix", `String prefix ])
    in
    printf "%S -> %s\n" (mask t prefix) (mask t result.text)
  in
  show "";
  show ".";
  show "s";
  show "src/";
  show "src/SU";
  show "src/sub/";
  show ~cwd:(Filename.concat t.dir "src") "../ot";
  show (t.dir ^ "/oth");
  show (t.dir ^ "/other/proj/");
  show "nowhere/";
  show "notes.txt/";
  [%expect
    {|
    "" -> ["docs/","other/","src/"]
    "." -> [".hidden/"]
    "s" -> ["src/"]
    "src/" -> ["src/Support/","src/sub/"]
    "src/SU" -> ["src/Support/","src/sub/"]
    "src/sub/" -> []
    "../ot" -> ["../other/"]
    "$DIR/oth" -> ["$DIR/other/"]
    "$DIR/other/proj/" -> ["$DIR/other/proj/deep/"]
    "nowhere/" -> []
    "notes.txt/" -> []
    |}]
;;

let%expect_test "apply_patch: writes every file, or none" =
  with_sandbox
  @@ fun t ->
  let patch lines =
    Jsonaf.to_string (`Object [ "input", `String (String.concat_lines lines) ])
  in
  let tree () =
    let files =
      Core_unix.open_process_in
        (sprintf "cd %s && find . -type f | sort" (Filename.quote t.dir))
      |> In_channel.input_all
    in
    print_string files
  in
  write t "src/a.ml" (String.concat_lines (List.init 8 ~f:(sprintf "a%d")));
  write t "old.txt" "old\n";
  write t "gone.txt" "bye\n";
  run
    t
    Tool_apply_patch.tool
    (patch
       [ "*** Begin Patch"
       ; "*** Add File: new/dir/n.txt"
       ; "+hello"
       ; "*** Update File: src/a.ml"
       ; "@@"
       ; " a2"
       ; " a3"
       ; "-a4"
       ; "+A4"
       ; " a5"
       ; "*** Update File: old.txt"
       ; "*** Move to: moved/new.txt"
       ; "-old"
       ; "+new"
       ; "*** Delete File: gone.txt"
       ; "*** End Patch"
       ]);
  tree ();
  print_string (read t "moved/new.txt");
  [%expect
    {|
    Success. Updated the following files:
    A new/dir/n.txt
    M src/a.ml
    M moved/new.txt
    D gone.txt

    --- a/src/a.ml
    +++ b/src/a.ml
    @@ -2,7 +2,7 @@
     a1
     a2
     a3
    -a4
    +A4
     a5
     a6
     a7
    --- a/moved/new.txt
    +++ b/moved/new.txt
    @@ -1,1 +1,1 @@
    -old
    +new
    ./moved/new.txt
    ./new/dir/n.txt
    ./src/a.ml
    new
    |}];
  (* The second operation fails: the first one is not written either. *)
  run
    t
    Tool_apply_patch.tool
    (patch
       [ "*** Begin Patch"
       ; "*** Update File: src/a.ml"
       ; "-A4"
       ; "+B4"
       ; "*** Update File: src/a.ml"
       ; "-nope"
       ; "+x"
       ; "*** End Patch"
       ]);
  print_string (read t "src/a.ml");
  [%expect
    {|
    ERROR: Failed to find expected lines in src/a.ml:
    nope
    a0
    a1
    a2
    a3
    A4
    a5
    a6
    a7
    |}];
  run t Tool_apply_patch.tool (patch [ "*** Update File: x" ]);
  run
    t
    Tool_apply_patch.tool
    (patch
       [ "*** Begin Patch"; "*** Delete File: missing.txt"; "*** End Patch" ]);
  (* Absolute paths work and are shown relative to the cwd. *)
  run
    t
    Tool_apply_patch.tool
    (patch
       [ "*** Begin Patch"
       ; "*** Update File: " ^ Filename.concat t.dir "moved/new.txt"
       ; "-new"
       ; "+newer"
       ; "*** End Patch"
       ]);
  [%expect
    {|
    ERROR: line 1: expected '*** Begin Patch', got '*** Update File: x'
    ERROR: Failed to delete missing.txt: the file does not exist
    Success. Updated the following files:
    M moved/new.txt

    --- a/moved/new.txt
    +++ b/moved/new.txt
    @@ -1,1 +1,1 @@
    -new
    +newer
    |}]
;;

let%expect_test "apply_patch asks for confirmation naming its files" =
  with_sandbox
  @@ fun t ->
  let summaries = ref [] in
  let provider =
    Faux_provider.create
      [ Faux_provider.Reply.tool_call
          ~id:"c1"
          ~name:"apply_patch"
          ~arguments:
            (Jsonaf.to_string
               (`Object
                   [ ( "input"
                     , `String
                         "*** Begin Patch\n\
                          *** Add File: a.txt\n\
                          +x\n\
                          *** Update File: b.txt\n\
                          *** Move to: c.txt\n\
                          *** End Patch" )
                   ]))
          ()
      ; Faux_provider.Reply.text "done"
      ]
  in
  let (_ : Message.t list) =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:
        { Agent_loop.Config.model = Model.default
        ; thinking = Off
        ; system = None
        ; tools = [ Tool_apply_patch.tool ]
        ; max_turns = None
        ; max_tokens = None
        ; retries = 0
        }
      ~cwd:t.dir
      ~approve:(fun _ tool args ->
        summaries
        := Agent_loop.tool_summary ~name:(Tool.name tool) args :: !summaries;
        Some (Tool.Result.error "[denied by user]"))
      ~context:[]
      ~prompts:[ Message.user "go" ]
      ()
  in
  print_s [%sexp (!summaries : string list)];
  [%expect {| ("a.txt, b.txt, c.txt") |}]
;;

let%expect_test
    "edit: lenient matching when old_text is not in the file as written"
  =
  with_sandbox
  @@ fun t ->
  let edit path edits =
    run
      t
      Tool_edit.tool
      (Jsonaf.to_string
         (`Object
             [ "path", `String path
             ; ( "edits"
               , `Array
                   (List.map edits ~f:(fun (o, n) ->
                      `Object [ "old_text", `String o; "new_text", `String n ]))
               )
             ]))
  in
  (* Curly quotes and an en dash in the file, typed as ASCII; trailing
     spaces in the file. *)
  write t "q.txt" "say \u{201C}hi\u{201D} \u{2013} ok   \nnext\n";
  edit "q.txt" [ "say \"hi\" - ok\nnext", "said hi\nnext" ];
  print_string (read t "q.txt");
  (* CRLF file, LF old_text and new_text: the replacement keeps CRLF. *)
  write t "crlf.txt" "a\r\nb\r\nc\r\n";
  edit "crlf.txt" [ "a\nb\n", "A\nB\n" ];
  print_s [%sexp (read t "crlf.txt" : string)];
  (* Exact matches win; a lenient one must be unique too. *)
  write t "two.txt" "x \nx\t\n";
  edit "two.txt" [ "x\n", "y\n" ];
  edit "two.txt" [ "x \nx", "z" ];
  write t "amb.txt" "\u{2018}a\u{2019}\n'a' \n";
  edit "amb.txt" [ "'a'\n", "b\n" ];
  [%expect
    {|
    edit 1 matched only after normalising whitespace and punctuation; the diff shows the result
    --- a/q.txt
    +++ b/q.txt
    @@ -1,2 +1,2 @@
    -say “hi” – ok
    +said hi
     next
    said hi
    next
    edit 1 matched only after normalising whitespace and punctuation; the diff shows the result
    --- a/crlf.txt
    +++ b/crlf.txt
    @@ -1,3 +1,3 @@
    -a
    -b
    +A
    +B
     c
     "A\r\
    \nB\r\
    \nc\r\
    \n"
    ERROR: edit 1: old_text is not in the file as written, and matches 2 places when whitespace and punctuation are normalised; copy it exactly from the file
    --- a/two.txt
    +++ b/two.txt
    @@ -1,2 +1,1 @@
    -x
    -x
    +z
    ERROR: edit 1: old_text is not in the file as written, and matches 2 places when whitespace and punctuation are normalised; copy it exactly from the file
    |}]
;;
