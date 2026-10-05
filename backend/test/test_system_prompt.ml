open! Core
open! Prigh

let%expect_test "instruction discovery and prompt" =
  let root = Filename_unix.temp_dir "prigh-sp" "" in
  let home = Filename.concat root "home" in
  let proj = Filename.concat root "proj" in
  let sub = Filename.concat proj "sub" in
  Core_unix.mkdir_p (Filename.concat home ".prigh");
  Core_unix.mkdir_p sub;
  let mask s = String.substr_replace_all s ~pattern:root ~with_:"$ROOT" in
  let show () =
    print_s
      [%sexp
        (List.map (System_prompt.instruction_files ~cwd:sub ~home) ~f:mask
         : string list)]
  in
  show ();
  [%expect {| () |}];
  Out_channel.write_all
    (Filename.concat proj "CLAUDE.md")
    ~data:"project rules\n";
  Out_channel.write_all (Filename.concat sub "AGENTS.md") ~data:"sub rules";
  Out_channel.write_all
    (Filename.concat home ".prigh/AGENTS.md")
    ~data:"global rules";
  show ();
  [%expect
    {| ($ROOT/home/.prigh/AGENTS.md $ROOT/proj/CLAUDE.md $ROOT/proj/sub/AGENTS.md) |}];
  Out_channel.write_all
    (Filename.concat proj "AGENTS.md")
    ~data:"preferred over CLAUDE.md";
  show ();
  [%expect
    {| ($ROOT/home/.prigh/AGENTS.md $ROOT/proj/AGENTS.md $ROOT/proj/sub/AGENTS.md) |}];
  print_endline
    (mask
       (System_prompt.build
          ~date:"2026-01-02"
          ~cwd:sub
          ~home
          ~tools:(Tools.specs [ Tool_read.tool; Tool_ls.tool ])
          ()));
  [%expect
    {|
    You are prigh, a coding agent working in the user's project from the command line.

    Guidelines:
    - Use the tools to inspect and change the project; do not guess file contents.
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - After making changes, verify them (build, tests) when a way to do so exists.
    - Be concise. Explain non-trivial decisions briefly. No filler.
    - Ask before destructive or irreversible actions.

    Available tools:
    - read: Read a text file. Returns the content; large files are truncated and can be read
    - ls: List a directory. Directories are shown with a trailing slash.

    Environment:
    - Working directory: $ROOT/proj/sub
    - Date: 2026-01-02
    - OS: Linux

    Instructions from $ROOT/home/.prigh/AGENTS.md:
    global rules

    Instructions from $ROOT/proj/AGENTS.md:
    preferred over CLAUDE.md

    Instructions from $ROOT/proj/sub/AGENTS.md:
    sub rules
    |}]
;;

let%expect_test "background guidance follows the tools" =
  let base tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs tools)
      ()
    |> String.split ~on:'\n'
    |> List.take_while ~f:(fun line ->
      not (String.equal line "Available tools:"))
    |> String.concat ~sep:"\n"
  in
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create [])
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:"/home"
  in
  print_endline
    (base
       (Tools.all @ (subagent :: Tool_subagent.control_tools) @ Tool_jobs.tools));
  [%expect
    {|
    You are prigh, a coding agent working in the user's project from the command line.

    Guidelines:
    - Use the tools to inspect and change the project; do not guess file contents.
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - After making changes, verify them (build, tests) when a way to do so exists.
    - Be concise. Explain non-trivial decisions briefly. No filler.
    - Ask before destructive or irreversible actions.

    Subagents run in the background: subagent returns at once, and each one's final report arrives later as a message starting with "[subagent <id> finished]" (or failed), sent automatically, not typed by the user. While they run, keep working or end your turn; call subagent_wait only when you cannot continue without a result.

    Commands that may take more than about a minute (builds, test suites, image builds, deployments, servers, watchers) belong in the background: run them with bash background: true, then continue with other useful work or end your turn. When one exits, its status and output tail arrive automatically in a message starting with "[job <id> exited <code>]" (or killed, failed). Never poll with sleep loops; if you truly need the result before continuing, use job_wait. Run long-running servers as background jobs and stop them with job_kill when done.
    |}];
  print_endline (base Tools.all);
  [%expect
    {|
    You are prigh, a coding agent working in the user's project from the command line.

    Guidelines:
    - Use the tools to inspect and change the project; do not guess file contents.
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - After making changes, verify them (build, tests) when a way to do so exists.
    - Be concise. Explain non-trivial decisions briefly. No filler.
    - Ask before destructive or irreversible actions.
    |}]
;;
