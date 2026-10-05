open! Core
open! Prigh
open Tool_test_helpers

let instructions t arguments =
  Host_ops.execute
    ~env:t.env
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~cwd:t.dir
    ~name:Host_ops.instructions_op
    ~arguments
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
    reply: {"files":[{"path":"$DIR/AGENTS.md","text":"rules"}],"nix":true}
    ((files (($DIR/AGENTS.md rules))) (nix true))
    |}];
  with_nix_on_path t ~nix:false (fun () -> show t (instructions t args));
  [%expect
    {|
    reply: {"files":[{"path":"$DIR/AGENTS.md","text":"rules"}],"nix":false}
    ((files (($DIR/AGENTS.md rules))) (nix false))
    |}];
  (* Backends that predate [with_nix] get the bare array they expect. *)
  with_nix_on_path t ~nix:true (fun () ->
    show t (instructions t (`Object [ "home", `String t.dir ])));
  [%expect
    {|
    reply: [{"path":"$DIR/AGENTS.md","text":"rules"}]
    ((files (($DIR/AGENTS.md rules))) (nix false))
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
    ((files ((/p/AGENTS.md "old host"))) (nix false))
    reply: {"files":[{"path":"/p/AGENTS.md","text":"new host"}],"nix":true}
    ((files ((/p/AGENTS.md "new host"))) (nix true))
    reply: {"files":[],"nix":false}
    ((files ()) (nix false))
    reply: {"nix":true}
    ((files ()) (nix true))
    reply: [{"path":"/p/AGENTS.md"},{"path":"/q/AGENTS.md","text":"kept"}]
    ((files ((/q/AGENTS.md kept))) (nix false))
    reply: unknown host tool
    ((files ()) (nix false))
    reply: not json
    ((files ()) (nix false))
    |}]
;;
