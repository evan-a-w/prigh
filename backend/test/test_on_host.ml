open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* [on_host]: one tool call on another connected tool host. The session runs
   on the backend; a client host, "laptop", is connected (served here, in
   this process, by [Host_ops.execute] as [prigh tool-host] would). *)

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

let desktop t =
  { (laptop t) with
    id = "host-desktop"
  ; name = "desktop"
  ; cwd = Filename.concat t.dir "desktop"
  ; home = Some (Filename.concat t.dir "desktop")
  }
;;

(* The session's replies, and [judge]'s for the approval model ([smart]
   mode), whose prompts are printed. *)
let provider ~show ~judge replies =
  let session = Faux_provider.create replies in
  let judge = Faux_provider.create judge in
  { Provider.name = "test"
  ; stream =
      (fun (request : Provider.Request.t) ~cancel ~on_event ->
        if
          Option.equal
            String.equal
            request.system
            (Some Approval_judge.system_prompt)
        then (
          List.iter request.messages ~f:(function
            | User u -> printf "judge sees:\n%s\n" (show u.text)
            | Assistant _ | Tool_result _ -> ());
          judge.stream request ~cancel ~on_event)
        else session.stream request ~cancel ~on_event)
  }
;;

let verdict decision reason =
  Reply.tool_call
    ~id:"v"
    ~name:"verdict"
    ~arguments:
      (Json.to_string
         (`Object [ "decision", `String decision; "reason", `String reason ]))
    ()
;;

let show t s =
  let s = mask t s in
  match Sys.getenv "HOME" with
  | Some home when String.length home > 1 ->
    String.substr_replace_all s ~pattern:home ~with_:"$HOME"
  | _ -> s
;;

let with_agent
      ?(mode = Approval_mode.Auto)
      ?(answer = fun (_ : string) -> Agent.Confirm_answer.Allow)
      ?(sub_replies = [])
      ?config
      ?(judge = [])
      replies
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Core_unix.mkdir_p (Filename.concat t.dir "laptop/sub");
  Core_unix.mkdir_p (Filename.concat t.dir "desktop");
  Option.iter config ~f:(fun json -> write t ".prigh/config.json" json);
  let agent_ref = ref None in
  let on_host =
    Tool_on_host.create
      ~hosts:(fun () ->
        Option.value_map !agent_ref ~default:[] ~f:Agent.tool_hosts)
      ()
  in
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create sub_replies)
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:t.dir
      ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(provider ~show:(show t) ~judge replies)
      ~tools:
        (Tools.all
         @ (subagent :: Tool_subagent.control_tools)
         @ Tool_jobs.tools
         @ [ on_host ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  Agent.set_mode agent ~approval:mode;
  let running = String.Table.create () in
  let output = Buffer.create 64 in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Tool_exec { exec_id; name; arguments; cwd; host; _ } ->
      printf "%s runs %s in %s\n" host name (show t cwd);
      let cancel = Cancellation.create () in
      Hashtbl.set running ~key:exec_id ~data:cancel;
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Fiber.yield ();
        let result =
          Host_ops.execute
            ~mcp:None
            ~env:t.env
            ~cancel
            ~on_output:(fun chunk ->
              ignore
                (Agent.tool_exec_output agent ~exec_id ~chunk : unit Or_error.t))
            ~cwd
            ~name
            ~arguments
            ()
        in
        Hashtbl.remove running exec_id;
        ignore (Agent.tool_exec_result agent ~exec_id result : unit Or_error.t))
    | Tool_exec_cancel { host; exec_id } ->
      printf "%s cancels its call\n" host;
      Option.iter (Hashtbl.find running exec_id) ~f:Cancellation.cancel
    | Loop (Tool_output { chunk; _ }) -> Buffer.add_string output chunk
    | Loop (Tool_allowed { model; reason; _ }) ->
      printf "allowed by %s: %s\n" model reason
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules; kind }) ->
      printf
        "confirm %s: %s [%s]%s\n"
        name
        (show t summary)
        (String.concat ~sep:" " rules)
        (match kind with
         | Judged { model; reason } -> sprintf " (%s left it: %s)" model reason
         | Tool | Computer_use _ -> "");
      Or_error.ok_exn
        (Agent.respond_confirm ?agent_id agent ~call_id (answer summary))
    | Loop (Tool_end { call; result }) ->
      (* Streamed output, in one piece: how it is split depends on timing. *)
      if Buffer.length output > 0
      then (
        printf "  output: %S\n" (show t (Buffer.contents output));
        Buffer.clear output);
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

let on_host ?(id = "c") args =
  Reply.tool_call ~id ~name:"on_host" ~arguments:args ()
;;

(* The user names laptop, which lets the agent work there; the tests of that
   permission say something else. *)
let run ?(text = "go, on laptop") agent =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let%expect_test "without host: the hosts" =
  with_agent [ on_host "{}"; Reply.text "ok" ]
  @@ fun t agent ->
  run agent;
  ignore t;
  [%expect
    {|
    on_host -> tool hosts:
    - <host> (id backend): home $HOME, the session's active host (call tools directly)
    - laptop (id host-laptop): home $DIR/laptop
    run a tool on another one: on_host {host: "NAME", tool: "bash", args: {command: "uname -a"}}
    |}]
;;

let%expect_test "bash runs on the other host, in its home or the cwd given" =
  with_agent
    [ on_host
        {|{"host":"laptop","tool":"bash","args":{"command":"pwd; echo hi"}}|}
    ; on_host
        ~id:"c2"
        {|{"host":"host-laptop","tool":"bash","args":{"command":"pwd"},"cwd":"sub"}|}
    ; on_host
        ~id:"c3"
        {|{"host":"laptop","tool":"bash","args":{"command":"pwd"},"cwd":"/tmp"}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  run agent;
  [%expect
    {|
    host-laptop runs bash in $DIR/laptop
      output: "$DIR/laptop\nhi\n"
    on_host -> $DIR/laptop
    hi
    host-laptop runs bash in $DIR/laptop/sub
      output: "$DIR/laptop/sub\n"
    on_host -> $DIR/laptop/sub
    host-laptop runs bash in /tmp
      output: "/tmp\n"
    on_host -> /tmp
    |}]
;;

let%expect_test "a cancelled call is cancelled on the other host" =
  with_agent
    [ on_host
        {|{"host":"laptop","tool":"bash","args":{"command":"echo started; sleep 30"}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  (* Waits for the output, which says the command started. *)
  let started, set = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_output _) -> ignore (Eio.Promise.try_resolve set () : bool)
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go, on laptop");
  Eio.Promise.await started;
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  [%expect
    {|
    host-laptop runs bash in $DIR/laptop
    host-laptop cancels its call
      output: "started\n"
    on_host -> ERROR [cancelled]
    |}]
;;

(* What was read on one host does not let a file be overwritten on another,
   even when (as here) the same process serves both. *)
let%expect_test "read before overwrite is per host" =
  with_agent
    [ Reply.tool_call
        ~id:"r"
        ~name:"read"
        ~arguments:{|{"path":"laptop/notes.txt"}|}
        ()
    ; on_host
        {|{"host":"laptop","tool":"write","args":{"path":"notes.txt","content":"new\n"}}|}
    ; on_host
        ~id:"c2"
        {|{"host":"laptop","tool":"read","args":{"path":"notes.txt"}}|}
    ; on_host
        ~id:"c3"
        {|{"host":"laptop","tool":"write","args":{"path":"notes.txt","content":"new\n"}}|}
    ; on_host
        ~id:"c4"
        {|{"host":"laptop","tool":"edit","args":{"path":"notes.txt","edits":[{"old_text":"new","new_text":"newer"}]}}|}
    ; Reply.tool_call
        ~id:"w"
        ~name:"write"
        ~arguments:{|{"path":"laptop/notes.txt","content":"mine\n"}|}
        ()
    ; Reply.text "ok"
    ]
  @@ fun t agent ->
  write t "laptop/notes.txt" "old\n";
  run agent;
  print_endline (read t "laptop/notes.txt");
  [%expect
    {|
    read -> old
    host-laptop runs write in $DIR/laptop
    on_host -> ERROR refusing to overwrite $DIR/laptop/notes.txt: it already exists and you have not read it in this session. Read $DIR/laptop/notes.txt first, then write it again (or edit it to change parts of it)
    host-laptop runs read in $DIR/laptop
    on_host -> old
    host-laptop runs write in $DIR/laptop
    on_host -> overwrote 1 line to $DIR/laptop/notes.txt
    host-laptop runs edit in $DIR/laptop
    on_host -> --- a/notes.txt
    +++ b/notes.txt
    @@ -1,1 +1,1 @@
    -new
    +newer
    write -> ERROR refusing to overwrite $DIR/laptop/notes.txt: it changed since you last read or wrote it (0s ago). Read $DIR/laptop/notes.txt again, then write it again (or edit it to change parts of it)
    newer
    |}]
;;

let%expect_test "refusals" =
  with_agent
    (List.mapi
       ~f:(fun i args -> on_host ~id:(sprintf "c%d" i) args)
       [ {|{"host":"laptp","tool":"bash","args":{"command":"pwd"}}|}
       ; {|{"host":"backend","tool":"bash","args":{"command":"pwd"}}|}
       ; {|{"host":"laptop"}|}
       ; {|{"host":"laptop","tool":"bash"}|}
       ; {|{"host":"laptop","tool":"bash","args":"pwd"}|}
       ; {|{"tool":"bash","args":{"command":"pwd"}}|}
       ; {|{"host":"laptop","tool":"subagent","args":{"task":"x"}}|}
       ; {|{"host":"laptop","tool":"job_status","args":{}}|}
       ; {|{"host":"laptop","tool":"computer","args":{"action":"screenshot"}}|}
       ; {|{"host":"laptop","tool":"on_host","args":{}}|}
       ; {|{"host":"laptop","tool":"web_fetch","args":{"url":"x"}}|}
       ; {|{"host":"laptop","tool":"apply_patch","args":{"input":""}}|}
       ; {|{"host":"laptop","tool":"bash","args":{"command":"sleep 1","background":true}}|}
       ]
     @ [ Reply.text "ok" ])
  @@ fun _t agent ->
  run agent;
  [%expect
    {|
    on_host -> ERROR no connected tool host "laptp" (it may have disconnected: the user can connect it with `prigh tool-host -connect` or a TUI there). Connected:
    - laptop (id host-laptop): home $DIR/laptop
    - <host> (id backend): home $HOME, the session's active host (call tools directly)
    on_host -> ERROR <host> is the session's active host: call bash directly
    on_host -> ERROR on_host needs tool and args with host, e.g. on_host {host: "laptop", tool: "bash", args: {command: "uname -a"}}
    on_host -> ERROR on_host needs args: bash's own arguments, as for a normal call
    on_host -> ERROR on_host's args must be an object: the tool's arguments
    on_host -> ERROR on_host needs host with tool. tool hosts:
    - <host> (id backend): home $HOME, the session's active host (call tools directly)
    - laptop (id host-laptop): home $DIR/laptop
    run a tool on another one: on_host {host: "NAME", tool: "bash", args: {command: "uname -a"}}
    on_host -> ERROR subagent runs in the backend, not on a tool host; a subagent can use on_host itself: on_host runs bash, read, write, edit, apply_patch, ls, grep, find
    on_host -> ERROR background jobs belong to the session's active host: on_host runs bash, read, write, edit, apply_patch, ls, grep, find
    on_host -> ERROR computer drives the session's active host only; the user switches hosts with /host: on_host runs bash, read, write, edit, apply_patch, ls, grep, find
    on_host -> ERROR on_host cannot run itself: on_host runs bash, read, write, edit, apply_patch, ls, grep, find
    on_host -> ERROR web_fetch does not run on a tool host: on_host runs bash, read, write, edit, apply_patch, ls, grep, find
    on_host -> ERROR apply_patch is not one of your tools: on_host runs bash, read, write, edit, ls, grep, find
    on_host -> ERROR background jobs run on the session's active host only: run it on laptop in the foreground (without background)
    |}]
;;

let%expect_test "a host that disconnected" =
  with_agent
    [ on_host {|{"host":"laptop","tool":"bash","args":{"command":"pwd"}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  Agent.set_hosts agent [];
  run agent;
  [%expect
    {|
    on_host -> ERROR no connected tool host "laptop" (it may have disconnected: the user can connect it with `prigh tool-host -connect` or a TUI there). Connected:
    - <host> (id backend): home $HOME, the session's active host (call tools directly)
    |}]
;;

(* The inner call is approved as itself, naming the host; rules allowing
   calls on the session's host do not let calls elsewhere through, and none
   is offered to add. *)
let%expect_test
    "approval names the host; allow rules stay on the session's host"
  =
  with_agent
    ~mode:Ask
    ~config:{|{"permissions": {"allow": ["bash(echo:*)"]}}|}
    ~answer:(fun summary ->
      if String.is_substring summary ~substring:"rm"
      then Deny (Some "not there")
      else Allow)
    [ Reply.tool_call
        ~id:"b"
        ~name:"bash"
        ~arguments:{|{"command":"echo here"}|}
        ()
    ; on_host
        {|{"host":"laptop","tool":"bash","args":{"command":"echo there"}}|}
    ; on_host
        ~id:"c2"
        {|{"host":"laptop","tool":"bash","args":{"command":"rm -r x"}}|}
    ; on_host ~id:"c3" {|{"host":"laptop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  run agent;
  [%expect
    {|
      output: "here\n"
    bash -> here
    confirm on_host: on laptop: bash echo there []
    host-laptop runs bash in $DIR/laptop
      output: "there\n"
    on_host -> there
    confirm on_host: on laptop: bash rm -r x []
    on_host -> ERROR [denied by user] Their reason: not there
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    |}]
;;

let%expect_test "plan mode treats on_host as the call it runs" =
  with_agent
    [ on_host
        {|{"host":"laptop","tool":"write","args":{"path":"x","content":"x"}}|}
    ; on_host
        ~id:"c2"
        {|{"host":"laptop","tool":"bash","args":{"command":"echo hi"}}|}
    ; on_host ~id:"c3" {|{"host":"laptop","tool":"read","args":{"path":"x"}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  Agent.set_mode agent ~plan:true;
  run agent;
  [%expect
    {|
    on_host -> ERROR [plan mode] Plan mode is on: you are planning, not implementing, so files, worktrees and pull requests cannot be changed. Only the user turns plan mode off; do not try to get around this or ask them to. Keep investigating with read-only tools, then present the plan in your reply and wait.
    confirm on_host: on laptop: bash echo hi []
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    host-laptop runs read in $DIR/laptop
    on_host -> ERROR file not found: $DIR/laptop/x
    |}]
;;

let%expect_test "subagents use on_host too" =
  with_agent
    ~sub_replies:
      [ on_host {|{"host":"laptop","tool":"bash","args":{"command":"pwd"}}|}
      ; Reply.text "done"
      ]
    [ Reply.tool_call
        ~id:"s"
        ~name:"subagent"
        ~arguments:{|{"task":"pwd on laptop"}|}
        ()
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  run agent;
  [%expect
    {|
    subagent -> started agent a1 (pwd on laptop); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    host-laptop runs bash in $DIR/laptop
      [subagent] on_host -> $DIR/laptop
    |}]
;;

let%expect_test "summaries, ACP titles and what the model is told" =
  List.iter
    [ {|{}|}
    ; {|{"host":"laptop","tool":"bash","args":{"command":"make test"}}|}
    ; {|{"host":"laptop","tool":"read","args":{"path":"/srv/notes.txt"},"cwd":"/srv"}|}
    ; {|{"host":"laptop","tool":"ls","args":{}}|}
    ; {|{"host":"laptop","tool":"bash","args":"pwd"}|}
    ]
    ~f:(fun json ->
      let args = Json.of_string json in
      printf
        "%-35s | %s\n"
        (Agent_loop.tool_summary ~name:"on_host" args)
        (Acp_tool_call.title ~name:"on_host" ~args ~cwd:"/home/me"));
  print_endline
    (String.prefix
       (Tool_on_host.create ~hosts:(fun () -> []) ()).spec.description
       80);
  [%expect
    {|
    hosts                               | List tool hosts
    on laptop: bash make test           | On laptop: make test
    on laptop: read /srv/notes.txt      | On laptop: Read notes.txt
    on laptop: ls                       | On laptop: List .
    on laptop                           | On laptop
    Run one tool call on another tool host; only when the user named that machine or
    |}]
;;

let%expect_test "names_host: a whole word, ignoring case" =
  List.iter
    [ "check the disk on Laptop"
    ; "laptop's disk"
    ; "on host-laptop please"
    ; "my laptops"
    ; "laptop-2"
    ; "nothing here"
    ]
    ~f:(fun text ->
      printf
        "%-26s %b\n"
        text
        (Approval.names_host text ~names:[ "laptop"; "host-laptop" ]));
  [%expect
    {|
    check the disk on Laptop   true
    laptop's disk              true
    on host-laptop please      true
    my laptops                 false
    laptop-2                   false
    nothing here               false
    |}]
;;

(* A call on a host the user has not named is held for them in every mode,
   reads included; denied, it is not run. *)
let%expect_test "a host the user has not named: held in every mode" =
  List.iter [ Approval_mode.Ask; Auto_edit; Auto ] ~f:(fun mode ->
    printf "-- %s\n" (Approval_mode.to_string mode);
    with_agent
      ~mode
      ~answer:(fun _ -> Deny None)
      [ on_host {|{"host":"laptop","tool":"ls","args":{}}|}; Reply.text "ok" ]
    @@ fun _t agent -> run ~text:"what is in this directory?" agent);
  [%expect
    {|
    -- ask
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    -- auto-edit
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    -- auto
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    |}]
;;

(* Named (by name or id, in any case) in a message of the user's, the host
   is permitted: its calls follow the mode. *)
let%expect_test "a host the user named: calls follow the mode" =
  List.iter [ Approval_mode.Ask; Auto_edit; Auto ] ~f:(fun mode ->
    printf "-- %s\n" (Approval_mode.to_string mode);
    with_agent
      ~mode
      [ on_host {|{"host":"laptop","tool":"ls","args":{}}|}
      ; on_host
          ~id:"c2"
          {|{"host":"laptop","tool":"write","args":{"path":"n","content":"x"}}|}
      ; on_host
          ~id:"c3"
          {|{"host":"laptop","tool":"bash","args":{"command":"echo hi"}}|}
      ; Reply.text "ok"
      ]
    @@ fun _t agent -> run ~text:"what's on the LAPTOP?" agent);
  [%expect
    {|
    -- ask
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    confirm on_host: on laptop: write n []
    host-laptop runs write in $DIR/laptop
    on_host -> wrote 1 line to $DIR/laptop/n
    confirm on_host: on laptop: bash echo hi []
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    -- auto-edit
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    host-laptop runs write in $DIR/laptop
    on_host -> wrote 1 line to $DIR/laptop/n
    confirm on_host: on laptop: bash echo hi []
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    -- auto
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    host-laptop runs write in $DIR/laptop
    on_host -> wrote 1 line to $DIR/laptop/n
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    |}]
;;

(* Letting one call run permits its host for the session (kept in its file
   with the calls smart mode allowed): later calls there follow the mode;
   another host is still held. *)
let%expect_test "an approval permits that host, not the others" =
  with_agent
    ~answer:(fun summary ->
      if String.is_substring summary ~substring:"desktop"
      then Deny None
      else Allow)
    [ on_host {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c2" {|{"host":"laptop","tool":"ls","args":{"path":"sub"}}|}
    ; on_host ~id:"c3" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ]
  @@ fun t agent ->
  Agent.set_hosts agent [ laptop t; desktop t ];
  run ~text:"look around" agent;
  List.iter
    (Session.allowed_calls (Agent.session agent))
    ~f:(fun a ->
      print_endline
        (show
           t
           (Allowed_call.describe a ~host_name:(fun id ->
              String.chop_prefix_if_exists id ~prefix:"host-"))));
  [%expect
    {|
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    host-laptop runs ls in $DIR/laptop
    on_host -> (empty directory)
    confirm on_host: on desktop: ls (runs on desktop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    ls {} on laptop in $DIR/laptop (by you)
    |}]
;;

(* Forgetting a call the user let run on a host takes back that host, unless
   another such call there still permits it; forgetting them all also takes
   back what the user's messages so far permitted, until a later one names
   the host again. Both last across a reload. *)
let%expect_test "forgetting allowed calls takes back their hosts" =
  let allow = ref true in
  with_agent
    ~answer:(fun _ -> if !allow then Allow else Deny None)
    [ on_host {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c2" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ; on_host ~id:"c3" {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c4" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ; on_host ~id:"c5" {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c6" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ; on_host ~id:"c7" {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c8" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ; on_host ~id:"c9" {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host ~id:"c10" {|{"host":"desktop","tool":"ls","args":{}}|}
    ; Reply.text "ok"
    ]
  @@ fun t agent ->
  Agent.set_hosts agent [ laptop t; desktop t ];
  let forget which =
    Or_error.ok_exn (Agent.forget_allowed_calls agent which);
    let path = Session.path (Agent.session agent) in
    Agent.new_session agent;
    Or_error.ok_exn (Agent.switch_session agent ~path);
    Agent.set_hosts agent [ laptop t; desktop t ]
  in
  print_endline "--- both allowed";
  run ~text:"look around" agent;
  allow := false;
  print_endline "--- laptop's call forgotten, reloaded";
  forget
    (List.find (Agent.state agent).allowed_calls ~f:(fun line ->
       String.is_substring line ~substring:"laptop"));
  run ~text:"look around again" agent;
  print_endline "--- laptop named, then all forgotten, reloaded";
  run ~text:"now on laptop" agent;
  forget None;
  run ~text:"once more" agent;
  print_endline "--- laptop named again";
  run ~text:"laptop, please" agent;
  [%expect
    {|
    --- both allowed
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    confirm on_host: on desktop: ls (runs on desktop, which you haven't named in this session) []
    host-desktop runs ls in $DIR/desktop
    on_host -> (empty directory)
    --- laptop's call forgotten, reloaded
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    host-desktop runs ls in $DIR/desktop
    on_host -> (empty directory)
    --- laptop named, then all forgotten, reloaded
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    host-desktop runs ls in $DIR/desktop
    on_host -> (empty directory)
    confirm on_host: on laptop: ls (runs on laptop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    confirm on_host: on desktop: ls (runs on desktop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    --- laptop named again
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    confirm on_host: on desktop: ls (runs on desktop, which you haven't named in this session) []
    on_host -> ERROR [denied by user]
    |}]
;;

let%expect_test "a deny rule wins over everything" =
  with_agent
    ~config:{|{"permissions": {"deny": ["bash(rm:*)"]}}|}
    [ on_host {|{"host":"laptop","tool":"bash","args":{"command":"rm -r x"}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  run ~text:"tidy up" agent;
  [%expect
    {| on_host -> ERROR [denied by the rule bash(rm:*) in permissions.deny; do not try to get around it: ask the user] |}]
;;

(* In smart mode the approval model decides about a host the user has not
   named, told which host the session runs on, which one the call targets
   and which are connected. *)
let%expect_test "smart: the judge weighs the host" =
  with_agent
    ~mode:Smart
    ~answer:(fun _ -> Deny None)
    ~judge:[ verdict "ask" "reads files on laptop, which you didn't mention" ]
    [ on_host {|{"host":"laptop","tool":"read","args":{"path":"notes"}}|}
    ; Reply.text "ok"
    ]
  @@ fun _t agent ->
  run ~text:"what do my notes say?" agent;
  [%expect
    {|
    judge sees:
    <user_requests>
    <user_message>
    what do my notes say?
    </user_message>
    </user_requests>

    <environment>
    session's own host: <host>
    connected hosts: <host>, laptop
    call runs on: laptop (not the session's own host: the agent reaches it with on_host)
    working directory: $DIR/laptop
    </environment>

    <tool_call>
    tool: read
    arguments:
    {
      "path": "notes"
    }
    </tool_call>
    confirm on_host: on laptop: read notes [] (deepseek/deepseek-flash left it: reads files on laptop, which you didn't mention)
    on_host -> ERROR [denied by user]
    |}]
;;

(* Once the user named the host, reads there run unjudged as on the session's
   own host; the judge's allow covers only that exact call on that host. *)
let%expect_test "smart: a named host follows the mode" =
  with_agent
    ~mode:Smart
    ~answer:(fun _ -> Deny None)
    ~judge:
      [ verdict "allow" "prints a word on laptop, as asked"
      ; verdict "ask" "runs on desktop, which you didn't mention"
      ]
    [ on_host {|{"host":"laptop","tool":"ls","args":{}}|}
    ; on_host
        ~id:"c2"
        {|{"host":"laptop","tool":"bash","args":{"command":"echo hi"}}|}
    ; on_host
        ~id:"c3"
        {|{"host":"laptop","tool":"bash","args":{"command":"echo hi"}}|}
    ; on_host
        ~id:"c4"
        {|{"host":"desktop","tool":"bash","args":{"command":"echo hi"}}|}
    ; Reply.text "ok"
    ]
  @@ fun t agent ->
  Agent.set_hosts agent [ laptop t; desktop t ];
  run ~text:"say hi on laptop" agent;
  [%expect
    {|
    host-laptop runs ls in $DIR/laptop
    on_host -> sub/
    judge sees:
    <user_requests>
    <user_message>
    say hi on laptop
    </user_message>
    </user_requests>

    <environment>
    session's own host: <host>
    connected hosts: <host>, laptop, desktop
    call runs on: laptop (not the session's own host: the agent reaches it with on_host)
    working directory: $DIR/laptop
    </environment>

    <tool_call>
    tool: bash
    arguments:
    {
      "command": "echo hi"
    }
    </tool_call>
    allowed by deepseek/deepseek-flash: prints a word on laptop, as asked
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    allowed by deepseek/deepseek-flash: this exact call earlier (prints a word on laptop, as asked)
    host-laptop runs bash in $DIR/laptop
      output: "hi\n"
    on_host -> hi
    judge sees:
    <user_requests>
    <user_message>
    say hi on laptop
    </user_message>
    </user_requests>

    <environment>
    session's own host: <host>
    connected hosts: <host>, laptop, desktop
    call runs on: desktop (not the session's own host: the agent reaches it with on_host)
    working directory: $DIR/desktop
    </environment>

    <tool_call>
    tool: bash
    arguments:
    {
      "command": "echo hi"
    }
    </tool_call>
    confirm on_host: on desktop: bash echo hi [] (deepseek/deepseek-flash left it: runs on desktop, which you didn't mention)
    on_host -> ERROR [denied by user]
    |}]
;;
