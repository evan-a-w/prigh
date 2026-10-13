open! Core
open! Prigh
open Tool_test_helpers

let instructions t arguments =
  Host_ops.execute
    ~mcp:None
    ~env:t.env
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~cwd:t.dir
    ~name:Host_ops.instructions_op
    ~arguments
    ()
;;

let show t (result : Tool.Result.t) =
  printf "reply: %s\n" (mask t result.text);
  let i = Host_ops.instructions_of_result result in
  let files = List.map i.files ~f:(fun (path, text) -> mask t path, text) in
  print_s [%sexp ({ i with files } : Host_ops.Instructions.t)]
;;

let%expect_test
    "$instructions: the files, and whether nix is on the host's PATH"
  =
  with_sandbox
  @@ fun t ->
  write t "AGENTS.md" "rules\n";
  let args = Host_ops.instructions_args ~home:t.dir in
  with_nix_on_path t ~nix:true (fun () -> show t (instructions t args));
  [%expect
    {|
    reply: {"files":[{"path":"$DIR/AGENTS.md","text":"rules"}],"nix":true,"skills":[]}
    ((files (($DIR/AGENTS.md rules))) (nix true) (skills ()))
    |}];
  with_nix_on_path t ~nix:false (fun () -> show t (instructions t args));
  [%expect
    {|
    reply: {"files":[{"path":"$DIR/AGENTS.md","text":"rules"}],"nix":false,"skills":[]}
    ((files (($DIR/AGENTS.md rules))) (nix false) (skills ()))
    |}];
  (* Backends that predate [with_nix] get the bare array they expect. *)
  with_nix_on_path t ~nix:true (fun () ->
    show t (instructions t (`Object [ "home", `String t.dir ])));
  [%expect
    {|
    reply: [{"path":"$DIR/AGENTS.md","text":"rules"}]
    ((files (($DIR/AGENTS.md rules))) (nix false) (skills ()))
    |}]
;;

let%expect_test "$instructions: only an executable file named nix counts" =
  with_sandbox
  @@ fun t ->
  let args = Host_ops.instructions_args ~home:t.dir in
  let nix_in dir =
    let bin = Filename.concat t.dir dir in
    Core_unix.mkdir_p bin;
    Core_unix.putenv ~key:"PATH" ~data:(":" ^ bin ^ ":/nonexistent");
    (Host_ops.instructions_of_result (instructions t args)).nix
  in
  with_nix_on_path t ~nix:false (fun () ->
    write t "plain/nix" "not executable";
    Core_unix.mkdir_p (Filename.concat t.dir "dir/nix");
    print_s
      [%sexp
        { not_executable = (nix_in "plain" : bool)
        ; directory = (nix_in "dir" : bool)
        ; empty = (nix_in "empty" : bool)
        }]);
  [%expect {| ((not_executable false) (directory false) (empty false)) |}]
;;

let%expect_test "instructions_of_result: old and new hosts, errors" =
  with_sandbox
  @@ fun t ->
  let reply ?(is_error = false) text =
    show t (if is_error then Tool.Result.error text else Tool.Result.ok text)
  in
  reply {|[{"path":"/p/AGENTS.md","text":"old host"}]|};
  reply {|{"files":[{"path":"/p/AGENTS.md","text":"new host"}],"nix":true}|};
  reply {|{"files":[],"nix":false}|};
  reply {|{"nix":true}|};
  reply {|[{"path":"/p/AGENTS.md"},{"path":"/q/AGENTS.md","text":"kept"}]|};
  reply ~is_error:true "unknown host tool";
  reply "not json";
  [%expect
    {|
    reply: [{"path":"/p/AGENTS.md","text":"old host"}]
    ((files ((/p/AGENTS.md "old host"))) (nix false) (skills ()))
    reply: {"files":[{"path":"/p/AGENTS.md","text":"new host"}],"nix":true}
    ((files ((/p/AGENTS.md "new host"))) (nix true) (skills ()))
    reply: {"files":[],"nix":false}
    ((files ()) (nix false) (skills ()))
    reply: {"nix":true}
    ((files ()) (nix true) (skills ()))
    reply: [{"path":"/p/AGENTS.md"},{"path":"/q/AGENTS.md","text":"kept"}]
    ((files ((/q/AGENTS.md kept))) (nix false) (skills ()))
    reply: unknown host tool
    ((files ()) (nix false) (skills ()))
    reply: not json
    ((files ()) (nix false) (skills ()))
    |}]
;;

let%expect_test "$make_dir: checks, creates with parents, says why not" =
  with_sandbox
  @@ fun t ->
  let make ?(check = false) path =
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~cwd:t.dir
        ~name:Dir_change.op
        ~arguments:(Dir_change.args ~path ~check)
        ()
    in
    printf
      "%s %s -> %s%s\n"
      (if check then "check" else "make ")
      (mask t path)
      (if result.is_error then "error: " else "")
      (mask t result.text)
  in
  let exists path =
    printf
      "  %s exists: %b\n"
      path
      (Sys_unix.is_directory_exn (Filename.concat t.dir path))
  in
  write t "file" "x";
  make ~check:true "a/b/../c";
  exists "a";
  make "a/b/../c";
  exists "a/c";
  make (Filename.concat t.dir "a/c");
  make ~check:true ".";
  make ~check:true "file";
  make "file/sub";
  make ~check:true "file/sub/deeper";
  let locked = Filename.concat t.dir "locked" in
  Core_unix.mkdir locked ~perm:0o555;
  make ~check:true "locked/new";
  make "locked/new/deeper";
  Core_unix.chmod locked ~perm:0o755;
  make "locked/new/deeper";
  make "";
  [%expect
    {|
    check a/b/../c -> $DIR/a/c
      a exists: false
    make  a/b/../c -> $DIR/a/c
      a/c exists: true
    make  $DIR/a/c -> $DIR/a/c
    check . -> $DIR
    check file -> error: $DIR/file is a file, not a directory: choose another path
    make  file/sub -> error: $DIR/file is a file, so $DIR/file/sub cannot be created: choose another path
    check file/sub/deeper -> error: $DIR/file is a file, so $DIR/file/sub/deeper cannot be created: choose another path
    check locked/new -> error: permission denied: cannot create $DIR/locked/new in $DIR/locked; choose a directory you can write
    make  locked/new/deeper -> error: permission denied: cannot create $DIR/locked/new/deeper in $DIR/locked; choose a directory you can write
    make  locked/new/deeper -> $DIR/locked/new/deeper
    make   -> $DIR
    |}]
;;

let%expect_test "Dir_change: parameters, outcomes, an older host's answer" =
  List.iter
    [ None; Some "fail"; Some "ask"; Some "create"; Some "yes" ]
    ~f:(fun p ->
      print_s
        [%sexp
          (p : string option)
        , (Dir_change.If_missing.of_param p
           : Dir_change.If_missing.t Or_error.t)]);
  List.iter
    [ Dir_change.Outcome.Changed { cwd = "/a"; created = true }
    ; Changed { cwd = "/a"; created = false }
    ; Missing "/b"
    ]
    ~f:(fun o ->
      print_endline (Jsonaf.to_string (Dir_change.Outcome.to_json o)));
  let older text ~is_error =
    print_s
      [%sexp
        (Dir_change.older_host
           (if is_error then Tool.Result.error text else Tool.Result.ok text)
           ~host:"laptop"
           ~path:"/x"
         : string option)]
  in
  older
    ~is_error:true
    {|unknown host tool "$make_dir": tool host "laptop" runs an older prigh|};
  older ~is_error:true "permission denied";
  older ~is_error:false {|unknown host tool "$make_dir"|};
  [%expect
    {|
    (() (Ok Fail))
    ((fail) (Ok Fail))
    ((ask) (Ok Ask))
    ((create) (Ok Create))
    ((yes) (Error "if_missing must be fail, ask or create, got \"yes\""))
    {"cwd":"/a","created":true}
    {"cwd":"/a","created":false}
    {"missing":"/b"}
    ("tool host laptop runs an older prigh that cannot create directories: update prigh there (and restart its TUI or tool-host), or create /x yourself")
    ()
    ()
    |}]
;;
