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

let show t s =
  let s = Test_worktree.show t s in
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
        ; call ~id:"l" "ls" {|{"path":"~"}|}
        ; call
            ~id:"o"
            "on_host"
            {|{"host":"laptop","tool":"bash","args":{"command":"pwd"}}|}
        ; Reply.text "looked"
        ] )
    ]
  @@ fun _t agent ->
  run agent ~prompt:"look around on the laptop";
  [%expect {| |}]
;;
