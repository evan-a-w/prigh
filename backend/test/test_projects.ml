open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* Sessions as projects: where a cwd is in git ([Git_location]), recorded in
   the session and listed by [list_sessions]; subagents in worktrees of
   their own. *)

let sh = Test_worktree.sh
let init_repo = Test_worktree.init_repo
let show = Test_worktree.show

let print_location t cwd =
  match Git_location.find ~cwd:(Filename.concat t.dir cwd) with
  | None -> printf "%-14s -> none\n" cwd
  | Some l ->
    printf
      "%-14s -> root=%s worktree=%s branch=%s%s\n"
      cwd
      (show t l.root)
      (show t l.worktree)
      (match l.branch with
       | Some b
         when String.length b = 8 && String.for_all b ~f:Char.is_hex_digit ->
         "<sha8>"
       | Some b -> b
       | None -> "-")
      (if l.main then " main" else "")
;;

let%expect_test
    "Git_location: main checkout, worktrees, subdirectories, detached, outside"
  =
  with_sandbox
  @@ fun t ->
  init_repo t;
  sh
    t
    "mkdir -p src/deep && git worktree add -q -b feat/x ../wt-x && git \
     worktree add -q --detach ../wt-d && mkdir -p ../wt-x/sub";
  Core_unix.mkdir_p (Filename.concat t.dir "plain/dir");
  List.iter
    [ "repo"
    ; "repo/src/deep"
    ; "wt-x"
    ; "wt-x/sub"
    ; "wt-d"
    ; "plain/dir"
    ; "missing"
    ]
    ~f:(print_location t);
  [%expect
    {|
    repo           -> root=$DIR/repo worktree=$DIR/repo branch=main main
    repo/src/deep  -> root=$DIR/repo worktree=$DIR/repo branch=main main
    wt-x           -> root=$DIR/repo worktree=$DIR/wt-x branch=feat/x
    wt-x/sub       -> root=$DIR/repo worktree=$DIR/wt-x branch=feat/x
    wt-d           -> root=$DIR/repo worktree=$DIR/wt-d branch=<sha8>
    plain/dir      -> none
    missing        -> none
    |}];
  (* On a tool host: [$git_location] answers JSON. *)
  List.iter [ "wt-x/sub"; "plain" ] ~f:(fun cwd ->
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~cwd:(Filename.concat t.dir cwd)
        ~name:Git_location.op
        ~arguments:(`Object [])
        ()
    in
    print_endline (show t result.text);
    print_s
      [%sexp
        (Or_error.map (Git_location.of_result result) ~f:Option.is_some
         : bool Or_error.t)]);
  [%expect
    {|
    {"root":"$DIR/repo","worktree":"$DIR/wt-x","branch":"feat/x","main":false}
    (Ok true)
    null
    (Ok false)
    |}]
;;

let%expect_test "a session records its repository as its cwd changes" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  init_repo t;
  sh t "git worktree add -q -b feat/y ../wt-y";
  Core_unix.mkdir_p (Filename.concat t.dir "plain");
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [ Reply.text "hi" ])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:(Filename.concat t.dir "repo")
      ()
  in
  let print () =
    let session = Agent.session agent in
    printf
      "%s: %s\n"
      (show t (Session.cwd session))
      (match Session.repo session with
       | None -> "not recorded"
       | Some None -> "not in a repository"
       | Some (Some l) -> sprintf "%s in %s" (show t l.root) (show t l.worktree))
  in
  print ();
  (* A run records it; so does each change of directory. *)
  Or_error.ok_exn (Agent.prompt agent "hello");
  Agent.wait_idle agent;
  print ();
  List.iter [ "../wt-y"; "../plain"; "../repo" ] ~f:(fun path ->
    ignore (Or_error.ok_exn (Agent.set_cwd agent ~path) : Dir_change.Outcome.t);
    print ());
  (* Only changes are written: the same place again adds nothing. *)
  let repos () =
    List.count
      (Session.entries (Agent.session agent))
      ~f:(fun e ->
        match e.payload with
        | Repo _ -> true
        | _ -> false)
  in
  let before = repos () in
  ignore
    (Or_error.ok_exn (Agent.set_cwd agent ~path:".") : Dir_change.Outcome.t);
  printf "new entries: %d\n" (repos () - before);
  (* The summary carries it, from the file too. *)
  let summary =
    List.hd_exn (Session.list ~dir:(Filename.concat t.dir "sessions"))
  in
  print_endline
    (show
       t
       (Sexp.to_string [%sexp (summary.repo : Git_location.t option option)]));
  [%expect
    {|
    $DIR/repo: not recorded
    $DIR/repo: $DIR/repo in $DIR/repo
    $DIR/wt-y: $DIR/repo in $DIR/wt-y
    $DIR/plain: not in a repository
    $DIR/repo: $DIR/repo in $DIR/repo
    new entries: 0
    ((((root $DIR/repo)(worktree $DIR/repo)(branch(main))(main true))))
    |}]
;;

let%expect_test
    "a session's worktrees: entered, made, a pull request's, its subagents'; \
     removed; forked"
  =
  with_sandbox
  @@ fun t ->
  let session = Session.create ~dir:(Filename.concat t.dir "s") ~cwd:"/r" () in
  ignore (Session.set_name session ~name:"n" : Session.Entry.t);
  let add payload = ignore (payload : Session.Entry.t) in
  let location ?(main = false) worktree branch =
    Some { Git_location.root = "/r"; worktree; branch = Some branch; main }
  in
  let print s =
    List.iter (Session.worktrees s) ~f:(fun (w : Session_worktree.t) ->
      print_s [%sexp (w : Session_worktree.t)])
  in
  (* The main checkout is not one of them; moving into a worktree is, once
     however often (its branch kept up to date). *)
  add (Session.set_repo session ~cwd:"/r" (location ~main:true "/r" "main"));
  add (Session.set_repo session ~cwd:"/t/a" (location "/t/a" "feat/a"));
  add (Session.set_repo session ~cwd:"/t/a/sub" (location "/t/a" "feat/a2"));
  add
    (Session.add_worktree
       session
       { path = "/t/b"; branch = Some "feat/b"; root = "/r"; source = Created });
  add
    (Session.add_worktree
       session
       { path = "/t/pr-7"
       ; branch = Some "pr-7"
       ; root = "/r"
       ; source = Pull_request { number = 7 }
       });
  add
    (Session.add_agent_worktree
       session
       { agent = "1"
       ; task = "t"
       ; path = "/t/c"
       ; branch = "agent/c"
       ; root = "/r"
       ; host = None
       });
  (* Made, then moved into: still made. *)
  add (Session.set_repo session ~cwd:"/t/b" (location "/t/b" "feat/b"));
  print session;
  [%expect
    {|
    ((path /t/a) (branch (feat/a2)) (root /r) (source Entered))
    ((path /t/b) (branch (feat/b)) (root /r) (source Created))
    ((path /t/pr-7) (branch (pr-7)) (root /r) (source (Pull_request (number 7))))
    ((path /t/c) (branch (agent/c)) (root /r)
     (source (Agent (agent 1) (task t))))
    |}];
  (* Removed: gone, until it is entered again. *)
  add (Session.worktree_removed session ~path:"/t/a");
  add (Session.worktree_removed session ~path:"/t/pr-7");
  let paths s =
    List.map (Session.worktrees s) ~f:(fun (w : Session_worktree.t) -> w.path)
  in
  let loaded = Or_error.ok_exn (Session.load (Session.path session)) in
  let forked =
    Or_error.ok_exn (Session.fork session ~dir:(Filename.concat t.dir "s"))
  in
  add (Session.set_repo session ~cwd:"/t/a" (location "/t/a" "feat/a"));
  print_s
    [%sexp
      { live = (paths session : string list)
      ; loaded = (paths loaded : string list)
      ; forked = (paths forked : string list)
      ; summary =
          (List.find_map_exn
             (Session.list ~dir:(Filename.concat t.dir "s"))
             ~f:(fun (s : Session.Summary.t) ->
               Option.some_if
                 (String.equal s.id (Session.id session))
                 (List.map s.worktrees ~f:(fun (w : Session_worktree.t) ->
                    w.path)))
           : string list)
      }];
  [%expect
    {|
    ((live (/t/b /t/c /t/a)) (loaded (/t/b /t/c)) (forked (/t/b))
     (summary (/t/b /t/c /t/a)))
    |}]
;;

let subagent ~provider ~trees =
  Tool_subagent.create
    ~provider
    ~worktrees_dir:(fun () -> trees)
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
    ()
;;

let%expect_test "a fork in a worktree is told its paths moved" =
  with_sandbox
  @@ fun t ->
  init_repo t;
  Core_unix.mkdir_p (Filename.concat t.dir "repo/src");
  sh t "echo x > src/f && git add -A && git commit -q -m src";
  let trees = Filename.concat t.dir "trees" in
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> requests := r :: !requests)
      [ Reply.text "fixed it" ]
  in
  let subagent = subagent ~provider ~trees in
  let arguments = {|{"task":"Fix f","fork":true,"worktree":"auto"}|} in
  let conversation =
    { Conversation.model = Model.default
    ; thinking = Off
    ; system = Some "the caller's system prompt"
    ; messages =
        [ Message.user "look at src/f"
        ; Assistant
            { content =
                [ Tool_call { id = "b1"; name = "subagent"; arguments } ]
            ; stop_reason = Tool_use
            ; usage = Usage.zero
            ; model = Model.default.id
            ; account = None
            }
        ]
    }
  in
  let context =
    Tool.Context.create
      ~env:t.env
      ~call_id:"b1"
      ~cwd:(Filename.concat t.dir "repo/src")
      ~tools:(Tools.all @ [ subagent ])
      ~conversation
      ()
  in
  let result = Tool.execute subagent context (Jsonaf.of_string arguments) in
  print_endline (show t result.text);
  List.iter !requests ~f:(fun (r : Provider.Request.t) ->
    print_endline (show t (Option.value r.system ~default:""));
    List.iter r.messages ~f:(function
      | User { text; _ } -> print_endline ("user: " ^ show t text)
      | Assistant _ -> print_endline "assistant: (its call)"
      | Tool_result r -> printf "tool_result %s: %s\n" r.tool_call_id r.text));
  [%expect
    {|
    fixed it
    [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    [worktree: the work is on branch agent/fix-f, checked out in $DIR/trees/repo/agent-fix-f (a worktree of $DIR/repo); review it there, then merge the branch or remove the worktree (/worktree rm $DIR/trees/repo/agent-fix-f)]
    the caller's system prompt
    user: look at src/f
    assistant: (its call)
    tool_result b1: This call started you: you are the subagent it forked from this conversation. Your task follows.
    user: [You are now a subagent forked from the conversation above. Your caller carries on separately: you do not see what it does next, and it sees only your final report. Do only the task below (not the rest of the conversation's work), then end with a concise report of what you found or changed and anything your caller must know.]

    You work in a git worktree made for you, $DIR/trees/repo/agent-fix-f, on branch agent/fix-f: paths in the conversation above refer to your caller's working directory, $DIR/repo/src; work on their counterparts in yours, $DIR/trees/repo/agent-fix-f/src.

    Your own subagents run synchronously: subagent blocks until its subagent finishes and returns its report, and they cannot delegate further.

    Your task:
    Fix f
    |}]
;;

(* [list_sessions]' [repo] and [worktrees] fields, as a client sees
   them, by name: sessions made within the same second have no order. *)
let print_sessions t json =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal "result" with
     | Some (`Array sessions) ->
       let field s name =
         Option.value_map (Jsonaf.member name s) ~default:"absent" ~f:(fun j ->
           show t (Jsonaf.to_string j))
       in
       List.sort sessions ~compare:(fun a b ->
         String.compare (field a "name") (field b "name"))
       |> List.iter ~f:(fun s ->
         let field = field s in
         printf
           "%s cwd=%s\n  repo=%s\n  worktrees=%s\n"
           (field "name")
           (field "cwd")
           (field "repo")
           (field "worktrees"))
     | _ -> print_endline (show t (Jsonaf.to_string json)))
  | _ -> print_endline (show t (Jsonaf.to_string json))
;;
