open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let sh (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote t.dir)
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let with_agent ?on_request replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = Faux_provider.create ?on_request replies in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir ".sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f t agent
;;

let print_diff t (r : Git_diff.Result.t Or_error.t) =
  match r with
  | Error e -> printf "error: %s\n" (mask t (Error.to_string_hum e))
  | Ok r ->
    let files, added, removed = Git_diff.Result.stats r in
    printf
      "root=%s against=%s truncated=%b files=%d +%d -%d\n%s"
      (mask t r.root)
      (if String.length r.against = 40 then "<sha>" else r.against)
      r.truncated
      files
      added
      removed
      (Re.replace_string
         (Re.compile (Re.Perl.re {|index [0-9a-f]+\.\.[0-9a-f]+|}))
         ~by:"index <ids>"
         r.diff)
;;

let user_messages t agent =
  List.iter
    (Session.messages (Agent.session agent))
    ~f:(fun (m : Message.t) ->
      match m with
      | User { text; origin; _ } ->
        print_s [%sexp (origin : Injected.t option)];
        print_endline
          (mask t text
           |> Re.replace_string
                (Re.compile (Re.Perl.re {|[0-9a-f]{40}|}))
                ~by:"<sha>"
           |> Re.replace_string
                (Re.compile (Re.Perl.re {|index [0-9a-f]+\.\.[0-9a-f]+|}))
                ~by:"index <ids>")
      | Assistant a -> printf "assistant: %s\n" (Message.Assistant.text a)
      | Tool_result _ -> ())
;;

let result t r =
  match (r : unit Or_error.t) with
  | Ok () -> print_endline "ok"
  | Error e -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

let%expect_test "/review and /init wait for a running turn like a follow-up" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2"}|}
        ()
    ; Reply.text "slept"
    ; Reply.text "wrote AGENTS.md"
    ]
  @@ fun t agent ->
  Or_error.ok_exn (Agent.prompt agent "sleep");
  result t (Agent.init agent);
  print_s [%sexp (Agent.queued_texts agent : string list * string list)];
  Agent.wait_idle agent;
  user_messages t agent;
  [%expect
    {|
    ok
    (() (/init))
    (((notes
       (((kind (Client (client Other)))
         (text
          "[Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]"))))))
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    sleep
    assistant:
    assistant: slept
    (((source Init)))
    [prigh init] Write an AGENTS.md for the project in $DIR, or improve the one there.

    First explore the repository: the README and other docs, the build files (package manifests, Makefile, dune, flake.nix, CI workflows), the source layout, the tests, and any AGENTS.md, CLAUDE.md, .cursorrules, .github/copilot-instructions.md or similar instruction files.

    Then write AGENTS.md at the root of the repository (in $DIR if it is not in one), for coding agents working on this project. Keep it concise, at most about 150 lines, and specific to this project:
    - how to build, run the tests (all of them and a single one), lint and format, with the exact commands;
    - the layout: what lives where, in a few lines;
    - conventions that are not obvious from the code: style, naming, error handling, how tests are written, commit messages;
    - anything surprising: required tools or environment, generated files not to edit, slow or flaky steps.

    Only write what you have verified in the repository; leave out generic advice. If AGENTS.md exists, keep what is still accurate, correct what is wrong and add what is missing rather than starting over; if only CLAUDE.md exists, build on it. When done, summarise what you wrote or changed.
    assistant: wrote AGENTS.md
    |}]
;;
