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
    - read: Read a text or image file. Returns the content; large text files are truncated a
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
  let base ?auto_background tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ?auto_background
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
      ()
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
    - You are the orchestrator: for substantial work (implementing a feature, a multi-file change, a long investigation), spawn subagents with self-contained tasks rather than doing it all yourself, and run independent tasks in parallel. Keep your own context for planning, coordinating, reviewing their reports and talking to the user; do small, quick things directly.
    - Delegate wide exploration (searching a large codebase, reading many files, long investigations) to subagents too, asking for a concise report: only the report enters your context, which stays small and sharp.
    - Fork (subagent with fork: true) for work that builds on what you have explored, such as implementing what you investigated: the subagent starts from your conversation so far, so it needs little exploration and a short task. Do not fork when your conversation holds nothing useful for it, or when it should be independent of your thinking (a reviewer, a second opinion, a fresh investigation): then it sees only the task, so include everything it needs.

    Background work: never block on it.
    - Commands that may take long (builds, test suites, installs, image builds, deployments) or that keep running (servers, watchers) go in the background: bash with background: true. Stop servers with job_kill when done.
    - Subagents always run in the background: subagent returns at once with the agent id.
    - Each finished job or subagent reports automatically, in a message starting with "[job <id> exited <code>]" (or killed, failed) or "[subagent <id> finished]" (or failed, or out of turns: subagent_continue gives it more turns), sent by prigh, not typed by the user. You are woken by it even after you end your turn.
    - So while they run, keep working on whatever does not depend on them, or end your turn. Never sleep or poll to pass the time. To look at a running job's output without blocking, use job_output; job_kill stops it.
    |}];
  (* The main agent's prompt, with the default threshold; a subagent's
     tools have no background controls. *)
  print_endline
    (base
       ~auto_background:30.
       (Tools.all @ (subagent :: Tool_subagent.control_tools) @ Tool_jobs.tools)
     |> String.split_lines
     |> List.filter ~f:(String.is_prefix ~prefix:"- A ")
     |> String.concat ~sep:"\n");
  print_endline
    (base
       ~auto_background:0.5
       (Tools.all @ (subagent :: Tool_subagent.control_tools) @ Tool_jobs.tools)
     |> String.split_lines
     |> List.filter ~f:(String.is_substring ~substring:"0.5s")
     |> String.concat ~sep:"\n");
  [%expect
    {|
    - A foreground bash command still running after 30s is moved to the background automatically (it keeps running as a job, not restarted), so a long command never stalls you: you get its job id and output so far at once.
    - A foreground bash command still running after 0.5s is moved to the background automatically (it keeps running as a job, not restarted), so a long command never stalls you: you get its job id and output so far at once.
    |}];
  print_endline (base ~auto_background:30. Tools.all);
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

let%expect_test "how to get tools, only when the host has nix" =
  let prompt ~nix =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~nix
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs Tools.all)
      ()
  in
  let with_nix = prompt ~nix:true in
  let without = prompt ~nix:false in
  print_endline
    (String.split with_nix ~on:'\n'
     |> List.take_while ~f:(fun line ->
       not (String.equal line "Available tools:"))
     |> String.concat ~sep:"\n");
  [%expect
    {|
    You are prigh, a coding agent working in the user's project from the command line.

    Guidelines:
    - Use the tools to inspect and change the project; do not guess file contents.
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - After making changes, verify them (build, tests) when a way to do so exists.
    - Be concise. Explain non-trivial decisions briefly. No filler.
    - Ask before destructive or irreversible actions.

    Get tools that are not installed from nixpkgs, not with apt or sudo (you may have no root): `nix shell nixpkgs#python3 nixpkgs#nodejs -c <cmd>` runs a command with those packages (any number of them), `nix run nixpkgs#<pkg> -- <args>` runs a package's program, `nix search nixpkgs <term>` finds package names (slow the first time), and `nix profile install nixpkgs#<pkg>` keeps a tool on PATH across sessions. Packages are cached and shared, so repeating these is cheap.
    |}];
  let lines s = String.split s ~on:'\n' in
  let only_in a b =
    List.filter (lines a) ~f:(fun l ->
      not (List.mem (lines b) l ~equal:String.equal))
  in
  print_s
    [%sexp
      { only_with_nix =
          (only_in with_nix without |> List.map ~f:(fun l -> String.prefix l 40)
           : string list)
      ; only_without = (only_in without with_nix : string list)
      }];
  [%expect
    {|
    ((only_with_nix ("Get tools that are not installed from ni"))
     (only_without ()))
    |}]
;;

let%expect_test "the guidelines name the model's file editor" =
  List.iter [ "anthropic/claude-opus-4-6"; "openai/gpt-5" ] ~f:(fun key ->
    let model = Option.value_exn (Model.find key) in
    let prompt =
      System_prompt.build
        ~instructions:[]
        ~cwd:"/"
        ~home:"/nonexistent"
        ~tools:(Tools.specs (Tools.for_model model Tools.all))
        ()
    in
    String.split_lines prompt
    |> List.filter ~f:(String.is_prefix ~prefix:"- Prefer")
    |> List.iter ~f:print_endline);
  [%expect
    {|
    - Prefer edit over write for existing files. Keep changes minimal and focused.
    - Prefer apply_patch over write for existing files. Keep changes minimal and focused.
    |}]
;;

let%expect_test "the main agent is told to orchestrate, subagents are not" =
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create [])
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:"/home"
      ()
  in
  let show ~role tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~role
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs tools)
      ()
    |> String.split_lines
    |> List.filter ~f:(String.is_substring ~substring:"orchestrator")
    |> function
    | [] -> print_endline "(no orchestrator guidance)"
    | lines -> List.iter lines ~f:print_endline
  in
  show ~role:Main (Tools.all @ [ subagent ]);
  (* Without the subagent tool there is nobody to delegate to. *)
  show ~role:Main Tools.all;
  (* A subagent (nested ones keep the subagent tool) does the work itself. *)
  show ~role:Subagent (Tools.all @ [ subagent ]);
  [%expect
    {|
    - You are the orchestrator: for substantial work (implementing a feature, a multi-file change, a long investigation), spawn subagents with self-contained tasks rather than doing it all yourself, and run independent tasks in parallel. Keep your own context for planning, coordinating, reviewing their reports and talking to the user; do small, quick things directly.
    (no orchestrator guidance)
    (no orchestrator guidance)
    |}]
;;

let%expect_test "a subagent is told to block on commands and jobs, not sleep" =
  let section tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~role:Subagent
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs tools)
      ()
    |> String.split_lines
    |> List.drop_while ~f:(fun l -> not (String.is_prefix l ~prefix:"Waiting"))
    |> List.take_while ~f:(fun l -> not (String.is_empty l))
    |> String.concat ~sep:"\n"
    |> print_endline
  in
  let sub =
    Or_error.ok_exn
      (Tools.for_context ~parent:(Tools.all @ Tool_jobs.tools) ~depth:1 ())
  in
  section sub;
  [%expect
    {|
    Waiting: never sleep or poll.
    - Never run sleep to pass the time or to poll in separate calls (sleep, then check again).
    - When you need a command's result, run it in the foreground: the call blocks until it finishes (give it a timeout long enough; the default is 10 minutes). To wait for something to become ready (a server, a file), use one foreground command that blocks until it is, with a timeout.
    - To run commands side by side or keep a server running while you work, start them with bash background: true. Each job's report arrives in a message starting with "[job <id> exited <code>]" when it exits, even after you end your turn: keep working or end your turn meanwhile. To look at a running job's output without blocking, use job_output; job_kill stops it.
    - You finish when you end your turn with no jobs running: kill the ones you no longer need (servers) before your final report.
    |}];
  (* Without job tools, blocking means foreground commands. *)
  section (Or_error.ok_exn (Tools.for_context ~parent:Tools.all ~depth:1 ()));
  [%expect
    {|
    Waiting: never sleep or poll.
    - Never run sleep to pass the time or to poll in separate calls (sleep, then check again).
    - When you need a command's result, run it in the foreground: the call blocks until it finishes (give it a timeout long enough; the default is 10 minutes). To wait for something to become ready (a server, a file), use one foreground command that blocks until it is, with a timeout.
    |}];
  (* Nothing to wait on without bash. *)
  section [];
  [%expect {||}]
;;

let%expect_test "agents with the worktree tool are told to use it" =
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create [])
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:"/home"
      ()
  in
  let worktree = Tool_worktree.create () in
  let show ~role tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~role
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs tools)
      ()
    |> String.split_lines
    |> List.filter ~f:(String.is_substring ~substring:"git worktree")
    |> function
    | [] -> print_endline "(no worktree guidance)"
    | lines -> List.iter lines ~f:print_endline
  in
  show ~role:Main (Tools.all @ [ subagent; worktree ]);
  show ~role:Subagent (Tools.all @ [ subagent; worktree ]);
  (* A nested subagent has no subagent tool. *)
  show ~role:Subagent (Tools.all @ [ worktree ]);
  show ~role:Main (Tools.all @ [ subagent ]);
  [%expect
    {|
    - To work on a separate branch, use the worktree tool (or the subagent tool's worktree option for delegated work); never run git worktree add yourself. Work in the worktree by its path: cd there in bash, use paths under it in file tools, pass it as a subagent's cwd.
    - To work on a separate branch, use the worktree tool (or the subagent tool's worktree option for delegated work); never run git worktree add yourself. Work in the worktree by its path: cd there in bash, use paths under it in file tools, pass it as a subagent's cwd.
    - To work on a separate branch, use the worktree tool; never run git worktree add yourself. Work in the worktree by its path: cd there in bash, use paths under it in file tools.
    (no worktree guidance)
    |}]
;;

let%expect_test "agents with the pull_request tool are told to use it, not gh" =
  let pull_request =
    Tool_pull_request.create
      ~forges:
        (Forges.create
           ~http:(fun ?cancel:_ _ -> Or_error.error_string "offline")
           ~sleep:ignore
           ~store:(Auth_store.create ~path:"/nonexistent/auth.json")
           ())
      ()
  in
  let show tools =
    System_prompt.build
      ~date:"2026-01-02"
      ~instructions:[]
      ~role:Main
      ~cwd:"/proj"
      ~home:"/home"
      ~tools:(Tools.specs tools)
      ()
    |> String.split_lines
    |> List.filter ~f:(String.is_substring ~substring:"pull request")
    |> function
    | [] -> print_endline "(no pull request guidance)"
    | lines -> List.iter lines ~f:print_endline
  in
  show (Tools.all @ [ pull_request ]);
  show Tools.all;
  [%expect {||}]
;;
