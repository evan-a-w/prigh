open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* [subagent] with [host]: a subagent that works on another connected tool
   host. The session runs on the backend; a client host, "laptop", is
   connected (served here, in this process, by [Host_ops.execute] as [prigh
   tool-host] would), and every call that reaches it is printed with where
   it ran. *)

let laptop t =
  { Agent.Host.id = "host-laptop"
  ; name = "laptop"
  ; cwd = Filename.concat t.dir "laptop"
  ; session_id = None
  ; session_name = None
  ; home = Some (Filename.concat t.dir "laptop")
  ; exec_input = true
  ; git_location = false
  ; computer = None
  }
;;

(* [subagent_status]'s elapsed time depends on the machine. *)
let elapsed_re = Re.compile (Re.Perl.re "  [0-9]+s  ")

let show t s =
  let s = Test_worktree.show t s in
  let s = Re.replace_string elapsed_re ~by:"  <elapsed>  " s in
  match Sys.getenv "HOME" with
  | Some home when String.length home > 1 ->
    String.substr_replace_all s ~pattern:home ~with_:"$HOME"
  | _ -> s
;;

let call ?(id = "s") name args = Reply.tool_call ~id ~name ~arguments:args ()

(* The environment and instruction lines of a subagent's system prompt. *)
let print_prompt t (request : Provider.Request.t) =
  Option.iter request.system ~f:(fun system ->
    String.split_lines system
    |> List.filter ~f:(fun line ->
      List.exists
        [ "- Tool host"; "- Working directory"; "- OS"; "Instructions from" ]
        ~f:(fun prefix -> String.is_prefix line ~prefix))
    |> List.iter ~f:(fun line -> printf "  [prompt] %s\n" (show t line)))
;;

let with_agent
      ?(mode = Approval_mode.Auto)
      ?(answer = fun (_ : string) -> Agent.Confirm_answer.Allow)
      ?(prompts = false)
      ~main
      children
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "laptop/sub");
  write t "laptop/AGENTS.md" "The laptop's rules.\n";
  write t "laptop/notes.txt" "on the laptop\n";
  let agent_ref = ref None in
  let hosts () = Option.value_map !agent_ref ~default:[] ~f:Agent.tool_hosts in
  let trees = Filename.concat t.dir "laptop/trees" in
  let worktrees_dir () = trees in
  let seen = String.Hash_set.create () in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key request ->
        if prompts
           && (not (String.equal key "main"))
           && not (Hash_set.mem seen key)
        then (
          Hash_set.add seen key;
          print_prompt t request))
      ~main
      children
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ (Tool_subagent.create
              ~hosts
              ~worktrees_dir
              ~provider
              ~current_model:(fun () -> Model.default)
              ~current_thinking:(fun () -> Off)
              ~home:t.dir
              ()
            :: Tool_subagent.control_tools)
         @ Tool_jobs.tools
         @ [ Tool_on_host.create ~hosts ()
           ; Tool_worktree.create
               ~worktrees_dir
               ~hosts
               ~session:(fun () -> Option.map !agent_ref ~f:Agent.session)
               ()
           ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  Agent.set_mode agent ~approval:mode;
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Tool_exec { exec_id; name; arguments; cwd; host; _ } ->
      printf "%s runs %s in %s\n" host name (show t cwd);
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Fiber.yield ();
        let result =
          Host_ops.execute
            ~mcp:None
            ~env:t.env
            ~cancel:(Cancellation.create ())
            ~on_output:ignore
            ~cwd
            ~name
            ~arguments
            ()
        in
        ignore (Agent.tool_exec_result agent ~exec_id result : unit Or_error.t))
    | Loop (Tool_confirm { call_id; agent_id; name; summary; _ }) ->
      printf "confirm %s: %s\n" name (show t summary);
      Or_error.ok_exn
        (Agent.respond_confirm ?agent_id agent ~call_id (answer summary))
    | Loop (Subagent_start { agent_id; host; worktree; continued; _ }) ->
      printf
        "subagent %s %s on %s%s\n"
        agent_id
        (if continued then "continues" else "starts")
        (Option.value host ~default:"the session's host")
        (Option.value_map worktree ~default:"" ~f:(fun w ->
           sprintf
             ", worktree %s on %s"
             (show t w.path)
             (Option.value w.host ~default:"the session's host")))
    | Loop (Subagent_end { agent_id; result; _ }) ->
      printf "subagent %s ends: %s\n" agent_id (show t result.text)
    | Loop (Tool_end { call; result }) ->
      printf
        "%s -> %s%s\n"
        call.name
        (if result.is_error then "ERROR " else "")
        (show t (String.strip result.text))
    | Loop (Subagent { event = Tool_end { call; result }; _ }) ->
      printf
        "  [subagent] %s -> %s%s\n"
        call.name
        (if result.is_error then "ERROR " else "")
        (show t (String.strip result.text))
    | _ -> ());
  Agent.set_hosts agent [ laptop t ];
  f t agent
;;

let run ?(prompt = "go") agent =
  Or_error.ok_exn (Agent.prompt agent prompt);
  Agent.wait_idle agent
;;

let%expect_test "a subagent on another host runs its tools there" =
  with_agent
    ~prompts:true
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop"}|}
      ; Reply.text "started"
      ; Reply.text "noted"
      ]
    [ ( "look"
      , [ call ~id:"b" "bash" {|{"command":"pwd"}|}
        ; call ~id:"r" "read" {|{"path":"notes.txt"}|}
        ; call ~id:"l" "ls" {|{"path":"."}|}
        ; call
            ~id:"o"
            "on_host"
            {|{"host":"laptop","tool":"bash","args":{"command":"pwd"}}|}
        ; Reply.text "looked"
        ] )
    ]
  @@ fun _t agent ->
  run agent ~prompt:"look around on the laptop";
  [%expect {|
    host-laptop runs $instructions in $DIR/laptop
    subagent -> started agent a1 on laptop (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop
      [prompt] - Tool host: laptop, another machine than the session's own: all your tools run there, and paths are on it
      [prompt] - Working directory: $DIR/laptop (on laptop)
      [prompt] Instructions from $DIR/laptop/AGENTS.md:
    host-laptop runs bash in $DIR/laptop
      [subagent] bash -> $DIR/laptop
    host-laptop runs read in $DIR/laptop
      [subagent] read -> on the laptop
    host-laptop runs ls in $DIR/laptop
      [subagent] ls -> AGENTS.md
    notes.txt
    sub/
      [subagent] on_host -> ERROR laptop is the host your tools run on: call bash directly
    subagent a1 ends: looked
    [subagent: 5 turns, 90 in / 37 out tokens, $0.0001]
    |}]
;;

let%expect_test "an unknown host is an error listing the connected ones" =
  with_agent
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptp"}|}; Reply.text "oops" ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"look around on the laptop";
  [%expect {|
    subagent -> ERROR invalid arguments: no connected tool host "laptp" (it may have disconnected: the user can connect it with `prigh tool-host -connect` or a TUI there). Connected:
    - laptop (id host-laptop): home $DIR/laptop
    - <host> (id backend): home $HOME, the session's active host (call tools directly)
    |}]
;;

let%expect_test "a host the user has not named is asked about; denied, it fails" =
  with_agent
    ~answer:(fun _ -> Deny None)
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop"}|}; Reply.text "ok" ]
    []
  @@ fun _t agent ->
  run agent ~prompt:"look around";
  [%expect {|
    confirm on_host: on laptop: subagent (runs on laptop, which you haven't named in this session)
    subagent -> ERROR [denied by user]
    |}]
;;

let%expect_test
    "approving the start permits the host: its calls follow the mode"
  =
  with_agent
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop","cwd":"sub"}|}
      ; Reply.text "started"
      ; Reply.text "noted"
      ]
    [ ( "look"
      , [ call ~id:"b" "bash" {|{"command":"pwd"}|}
        ; call ~id:"w" "write" {|{"path":"x.txt","content":"x"}|}
        ; Reply.text "looked"
        ] )
    ]
  @@ fun _t agent ->
  run agent ~prompt:"look around";
  [%expect {|
    confirm on_host: on laptop: subagent (runs on laptop, which you haven't named in this session)
    host-laptop runs $instructions in $DIR/laptop/sub
    subagent -> started agent a1 on laptop (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop
    host-laptop runs bash in $DIR/laptop/sub
      [subagent] bash -> $DIR/laptop/sub
    host-laptop runs write in $DIR/laptop/sub
      [subagent] write -> wrote 1 line to $DIR/laptop/sub/x.txt
    subagent a1 ends: looked
    [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    |}]
;;

let%expect_test "in ask mode, its calls there ask as the session's would" =
  with_agent
    ~mode:Ask
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop"}|}
      ; Reply.text "started"
      ; Reply.text "noted"
      ]
    [ ( "look"
      , [ call ~id:"r" "read" {|{"path":"notes.txt"}|}
        ; call ~id:"b" "bash" {|{"command":"echo hi"}|}
        ; Reply.text "looked"
        ] )
    ]
  @@ fun _t agent ->
  run agent ~prompt:"look around on laptop";
  [%expect {|
    host-laptop runs $instructions in $DIR/laptop
    subagent -> started agent a1 on laptop (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop
    host-laptop runs read in $DIR/laptop
      [subagent] read -> on the laptop
    confirm on_host: on laptop: bash echo hi
    host-laptop runs bash in $DIR/laptop
      [subagent] bash -> hi
    subagent a1 ends: looked
    [subagent: 3 turns, 50 in / 21 out tokens, $0.0000]
    |}]
;;

let%expect_test "subagent_continue keeps its host" =
  with_agent
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop","max_turns":1}|}
      ; Reply.text "started"
      ; call ~id:"c" "subagent_continue" {|{"id":"a1"}|}
      ; Reply.text "continuing"
      ; Reply.text "noted"
      ]
    [ ( "look"
      , [ call ~id:"b" "bash" {|{"command":"pwd"}|}
        ; call ~id:"b2" "bash" {|{"command":"echo again"}|}
        ; Reply.text "looked"
        ] )
    ]
  @@ fun _t agent ->
  run agent ~prompt:"look around on the laptop";
  [%expect {|
    host-laptop runs $instructions in $DIR/laptop
    subagent -> started agent a1 on laptop (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop
    host-laptop runs bash in $DIR/laptop
      [subagent] bash -> $DIR/laptop
    subagent a1 ends: subagent hit the 1-turn limit without finishing
    [subagent: 1 turns, 20 in / 8 out tokens, $0.0000]
    [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    subagent a1 continues on laptop
    subagent_continue -> continuing a1; its report will be delivered to you when it finishes: keep working or end your turn meanwhile
    host-laptop runs bash in $DIR/laptop
      [subagent] bash -> again
    subagent a1 ends: subagent hit the 1-turn limit without finishing
    [subagent: 2 turns, 40 in / 16 out tokens, $0.0000]
    [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]
    |}]
;;

let%expect_test "its worktree is made on its host" =
  with_agent
    ~main:
      [ call
          "subagent"
          {|{"task":"fix","host":"laptop","cwd":"repo","worktree":"fix-it"}|}
      ; Reply.text "started"
      ; call
          ~id:"l"
          "worktree"
          {|{"action":"list","host":"laptop","cwd":"repo"}|}
      ; Reply.text "noted"
      ]
    [ "fix", [ call ~id:"b" "bash" {|{"command":"pwd"}|}; Reply.text "fixed" ] ]
  @@ fun t agent ->
  Test_worktree.init_repo ~dir:"laptop/repo" t;
  run agent ~prompt:"fix it on the laptop";
  List.iter (Session.messages (Agent.session agent)) ~f:(function
    | Message.User u ->
      List.iter
        (String.split_lines (Message.User.body u))
        ~f:(fun line ->
          if String.is_prefix line ~prefix:"[worktree:"
          then print_endline (show t line))
    | Assistant _ | Tool_result _ -> ());
  print_s
    [%sexp
      (List.map (Session.worktrees (Agent.session agent)) ~f:(fun w -> w.path)
       : string list)];
  [%expect {|
    host-laptop runs $worktree_create in $DIR/laptop/repo
    host-laptop runs $instructions in $DIR/laptop/trees/repo/fix-it
    subagent -> started agent a1 on laptop (fix); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop, worktree $DIR/laptop/trees/repo/fix-it on laptop
    host-laptop runs bash in $DIR/laptop/trees/repo/fix-it
      [subagent] bash -> $DIR/laptop/trees/repo/fix-it
    subagent a1 ends: fixed
    [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    [worktree: the work is on branch fix-it, checked out in $DIR/laptop/trees/repo/fix-it (a worktree of $DIR/laptop/repo) on the tool host laptop; review it there, then merge the branch or remove the worktree (the worktree tool with host laptop and action remove)]
    host-laptop runs $worktree_list in $DIR/laptop/repo
    worktree -> worktrees of $DIR/laptop/repo (new ones go in $DIR/laptop/trees/repo):
    - $DIR/laptop/repo  main  clean  [main checkout]
    - $DIR/laptop/trees/repo/fix-it  fix-it  clean
    [worktree: the work is on branch fix-it, checked out in $DIR/laptop/trees/repo/fix-it (a worktree of $DIR/laptop/repo) on the tool host laptop; review it there, then merge the branch or remove the worktree (the worktree tool with host laptop and action remove)]
    ()
    |}]
;;

let%expect_test "subagent_status says which host it is on" =
  with_agent
    ~main:
      [ call "subagent" {|{"task":"look","host":"laptop"}|}
      ; call ~id:"st" "subagent_status" {|{}|}
      ; Reply.text "started"
      ; Reply.text "noted"
      ]
    [ "look", [ Reply.text "looked" ] ]
  @@ fun _t agent ->
  run agent ~prompt:"look around on the laptop";
  [%expect {|
    host-laptop runs $instructions in $DIR/laptop
    subagent -> started agent a1 on laptop (look); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    subagent a1 starts on laptop
    subagent_status -> a1  running, on laptop  <elapsed>  look  (last: waiting for the model)
    subagent a1 ends: looked
    [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
    |}]
;;
