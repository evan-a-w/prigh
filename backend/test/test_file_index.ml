open! Core
open! Prigh
open Tool_test_helpers

let tree t =
  List.iter
    [ "src/sub"; "docs"; ".git-like/objects"; "_build/default"; "node_modules/x" ]
    ~f:(fun d -> Core_unix.mkdir_p (Filename.concat t.dir d));
  List.iter
    [ "src/app.ml"
    ; "src/sub/deep.ml"
    ; "docs/README.md"
    ; ".git-like/objects/a"
    ; ".hidden"
    ; "_build/default/x.ml"
    ; "node_modules/x/index.js"
    ; "top.txt"
    ]
    ~f:(fun f -> write t f "")
;;

let show (r : File_index.t) = print_s [%sexp (r : File_index.t)]

let git' t args =
  let r = Process.run_collect ~env:t.env ~cwd:t.dir ~prog:"git" ~args () in
  if not (Process.Exit.is_success r.exit) then print_endline r.stderr
;;

let%expect_test "outside git: a walk that skips dot and build directories" =
  with_sandbox
  @@ fun t ->
  tree t;
  show (File_index.list ~env:t.env ~root:t.dir ());
  [%expect
    {|
    ((files (docs/README.md src/app.ml src/sub/deep.ml top.txt))
     (truncated false))
    |}];
  show (File_index.walk ~limit:2 ~root:t.dir ());
  [%expect {| ((files (docs/README.md src/app.ml)) (truncated true)) |}];
  print_endline
    (Jsonaf.to_string
       (File_index.to_json ~root:"/r" (File_index.walk ~limit:2 ~root:t.dir ())));
  [%expect
    {| {"root":"/r","files":"docs/README.md\nsrc/app.ml","truncated":true} |}]
;;

let%expect_test "in git: tracked and untracked files, not ignored or deleted ones" =
  with_sandbox
  @@ fun t ->
  tree t;
  git' t [ "init"; "-q" ];
  write t ".gitignore" "_build/\nnode_modules/\n*.log\n";
  write t "debug.log" "";
  write t "gone.ml" "";
  git' t [ "add"; "-A" ];
  git'
    t
    [ "-c"
    ; "user.name=t"
    ; "-c"
    ; "user.email=t@t"
    ; "commit"
    ; "-q"
    ; "-m"
    ; "x"
    ];
  Core_unix.unlink (Filename.concat t.dir "gone.ml");
  write t "new file é.ml" "";
  show (File_index.list ~env:t.env ~root:t.dir ());
  [%expect
    {|
    ((files
      (.git-like/objects/a .gitignore .hidden docs/README.md
       "new file \195\169.ml" src/app.ml src/sub/deep.ml top.txt))
     (truncated false))
    |}];
  (* A subdirectory lists its own files, relative to it. *)
  show (File_index.list ~env:t.env ~root:(Filename.concat t.dir "src") ());
  [%expect {| ((files (app.ml sub/deep.ml)) (truncated false)) |}]
;;

let%expect_test "a large tree: every file, quickly" =
  with_sandbox
  @@ fun t ->
  for d = 0 to 99 do
    let dir = sprintf "d%02d" d in
    Core_unix.mkdir_p (Filename.concat t.dir dir);
    for f = 0 to 199 do
      Out_channel.write_all (Filename.concat t.dir (sprintf "%s/f%03d.ml" dir f)) ~data:""
    done
  done;
  let r = File_index.list ~env:t.env ~root:t.dir () in
  printf
    "%d files, truncated %b, first %s, last %s\n"
    (List.length r.files)
    r.truncated
    (List.hd_exn r.files)
    (List.last_exn r.files);
  [%expect {| 20000 files, truncated false, first d00/f000.ml, last d99/f199.ml |}];
  git' t [ "init"; "-q" ];
  let r = File_index.list ~env:t.env ~root:t.dir () in
  printf "in git: %d files\n" (List.length r.files);
  [%expect {| in git: 20000 files |}];
  let r = File_index.list ~limit:500 ~env:t.env ~root:t.dir () in
  printf "limited: %d files, truncated %b\n" (List.length r.files) r.truncated;
  [%expect {| limited: 500 files, truncated true |}]
;;

let%expect_test "$list_files: the host operation" =
  with_sandbox
  @@ fun t ->
  tree t;
  let r =
    Host_ops.execute
      ~mcp:None
      ~env:t.env
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~cwd:t.dir
      ~name:Host_ops.list_files_op
      ~arguments:(`Object [])
      ()
  in
  printf "error %b: %s\n" r.is_error (mask t r.text);
  [%expect
    {| error false: {"root":"$DIR","files":"docs/README.md\nsrc/app.ml\nsrc/sub/deep.ml\ntop.txt","truncated":false} |}]
;;
