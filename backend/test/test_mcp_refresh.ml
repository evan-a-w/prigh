open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* A session on a tool host whose MCP listings the test hands out: the
   model's requests and the host's [$mcp_servers] round trips are printed in
   the order they happen. *)

let echo : Mcp_client.Tool.t =
  { name = "echo"
  ; description = "Echoes"
  ; input_schema = `Object [ "type", `String "object" ]
  ; read_only = true
  }
;;

let listing (status : Mcp_tools.Status.t) : Mcp_tools.Listing.t =
  { servers =
      [ { name = "srv"
        ; source = "/home/.prigh/mcp.json"
        ; project = false
        ; status
        ; tools =
            (match status with
             | Ready -> [ echo ]
             | Failed _ | Needs_approval | Needs_sign_in _ -> [])
        ; url = None
        ; oauth = None
        }
      ]
  ; problems = []
  }
;;

module Fake = struct
  type t =
    { agent : Agent.t
    ; mutable auto : Mcp_tools.Listing.t option
      (** answers [$mcp_servers] at once with this, when set *)
    ; held : string Queue.t (** unanswered [$mcp_servers] exec ids *)
    ; requests : Provider.Request.t Queue.t
    ; request_seen : Eio.Condition.t
    }

  let answer t listing =
    let exec_id = Queue.dequeue_exn t.held in
    print_endline "host answers $mcp_servers";
    Or_error.ok_exn
      (Agent.tool_exec_result
         t.agent
         ~exec_id
         (Tool_result.ok (Json.to_string (Mcp_tools.Listing.to_json listing))))
  ;;

  let wait_for_request t n =
    while Queue.length t.requests < n do
      Eio.Condition.await_no_mutex t.request_seen
    done
  ;;
end

(* What a cached prefix depends on: the tools and the system prompt as
   Anthropic gets them. *)
let prefix_hash (r : Provider.Request.t) =
  let body = Anthropic.For_testing.request_body ~oauth:false r in
  let field name = Option.value (Json.member name body) ~default:`Null in
  Md5.to_hex
    (Md5.digest_string
       (Json.to_string (`Array [ field "tools"; field "system" ])))
;;

(* Prefixes by the order they first appear in: A, B, ... (the hash depends
   on the sandbox's path). *)
let prefix_name seen r =
  let hash = prefix_hash r in
  Hashtbl.find_or_add seen hash ~default:(fun () ->
    String.of_char (Char.of_int_exn (Char.to_int 'A' + Hashtbl.length seen)))
;;

let mcp_names (r : Provider.Request.t) =
  List.filter_map r.tools ~f:(fun s ->
    Option.some_if (String.is_prefix s.name ~prefix:"mcp__") s.name)
;;

let with_fake replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let request_seen = Eio.Condition.create () in
  let seen = String.Table.create () in
  let on_request (r : Provider.Request.t) =
    Queue.enqueue requests r;
    printf
      "model request %d: %s (prefix %s)\n"
      (Queue.length requests)
      (String.concat ~sep:" " (mcp_names r))
      (prefix_name seen r);
    Eio.Condition.broadcast request_seen
  in
  let mcp = Mcp_hub.create ~env:t.env ~sw () in
  let settings = Mcp_settings.create ~home:t.dir in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create ~on_request replies)
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~backend_host:false
      ~mcp
      ~mcp_settings:settings
      ~cwd:t.dir
      ()
  in
  Agent.set_model agent (Option.value_exn (Model.default_for Anthropic));
  let fake =
    { Fake.agent
    ; auto = Some (listing Ready)
    ; held = Queue.create ()
    ; requests
    ; request_seen
    }
  in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Tool_exec { exec_id; name; arguments; cwd; _ } ->
      if String.equal name Host_ops.mcp_servers_op
      then (
        print_endline "host asked for $mcp_servers";
        Queue.enqueue fake.held exec_id;
        Option.iter fake.auto ~f:(Fake.answer fake))
      else if String.equal name Host_ops.mcp_call_op
      then
        Or_error.ok_exn
          (Agent.tool_exec_result
             agent
             ~exec_id
             (Tool_result.error
                "the MCP server \"srv\" is not running: it crashed; once that \
                 is fixed, reconnect it with /mcp"))
      else
        Eio.Fiber.fork ~sw (fun () ->
          let result =
            Host_ops.execute
              ~mcp:None
              ~env:t.env
              ~cancel:Cancellation.never
              ~on_output:ignore
              ~cwd
              ~name
              ~arguments
              ()
          in
          Or_error.ok_exn (Agent.tool_exec_result agent ~exec_id result))
    | Loop (Tool_end { result; _ }) ->
      printf "tool result: %s\n" (String.strip result.text)
    | _ -> ());
  let host id : Agent.Host.t =
    { id
    ; name = id
    ; cwd = t.dir
    ; session_id = None
    ; session_name = None
    ; home = Some t.dir
    ; exec_input = false
    ; git_location = false
    ; computer = None
    }
  in
  Agent.set_hosts agent [ host "laptop"; host "desktop" ];
  f t fake ~settings
;;

let%expect_test "a run starts without waiting for the MCP listing it has" =
  with_fake (List.init 5 ~f:(fun i -> Reply.text (sprintf "reply %d" i)))
  @@ fun _ fake ~settings ->
  let agent = fake.agent in
  (* The first run has no listing: it waits for one. *)
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  [%expect
    {|
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 1: mcp__srv__echo (prefix A)
    |}];
  (* The next one asks the model at once, refreshing meanwhile. *)
  fake.auto <- None;
  Or_error.ok_exn (Agent.prompt agent "two");
  Fake.wait_for_request fake 2;
  Fake.answer fake (listing Ready);
  Agent.wait_idle agent;
  [%expect
    {|
    host asked for $mcp_servers
    model request 2: mcp__srv__echo (prefix A)
    host answers $mcp_servers
    |}];
  (* Changed settings are sent to the host at once, and the next run waits
     for the listing they give. *)
  ignore
    (Or_error.ok_exn
       (Mcp_settings.add
          settings
          [ "other", `Object [ "command", `String "x" ] ])
     : Mcp_settings.Entry.t list);
  Agent.mcp_settings_changed agent;
  [%expect {| host asked for $mcp_servers |}];
  Or_error.ok_exn (Agent.prompt agent "three");
  (* Given time to send its request, it does not. *)
  (match
     Eio.Time.with_timeout
       (Eio.Stdenv.clock (Agent.env agent))
       0.2
       (fun () -> Ok (Fake.wait_for_request fake 3))
   with
   | Ok () -> print_endline "(the run did not wait)"
   | Error `Timeout -> print_endline "(the run waits)");
  Fake.answer fake (listing Ready);
  Fake.wait_for_request fake 3;
  Agent.wait_idle agent;
  [%expect
    {|
    (the run waits)
    host answers $mcp_servers
    model request 3: mcp__srv__echo (prefix A)
    |}];
  (* So does a run on another host. *)
  fake.auto <- Some (listing Ready);
  ignore
    (Or_error.ok_exn (Agent.set_active_host agent "desktop" ~cwd:None)
     : Dir_change.Outcome.t);
  Or_error.ok_exn (Agent.prompt agent "four");
  Agent.wait_idle agent;
  [%expect
    {|
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 4: mcp__srv__echo (prefix A)
    |}]
;;

let%expect_test
    "a failed MCP server's tools stay offered, and the prefix with them"
  =
  with_fake
    [ Reply.text "one"
    ; Reply.text "two"
    ; Reply.tool_call ~id:"c1" ~name:"mcp__srv__echo" ~arguments:"{}" ()
    ; Reply.text "three"
    ; Reply.text "four"
    ]
  @@ fun _ fake ~settings:_ ->
  let agent = fake.agent in
  let prompt text status =
    fake.auto <- Some (listing status);
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent
  in
  (* Each run's background refresh gives the next run its tools. *)
  prompt "one" Ready;
  prompt "two" (Failed "it crashed");
  prompt "three" Ready;
  prompt "four" Ready;
  [%expect
    {|
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 1: mcp__srv__echo (prefix A)
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 2: mcp__srv__echo (prefix A)
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 3: mcp__srv__echo (prefix A)
    tool result: the MCP server "srv" is not running: it crashed; once that is fixed, reconnect it with /mcp
    model request 4: mcp__srv__echo (prefix A)
    host asked for $mcp_servers
    host answers $mcp_servers
    model request 5: mcp__srv__echo (prefix A)
    |}];
  (* /mcp still shows the server as it is: failed, without tools. *)
  fake.auto <- Some (listing (Failed "it crashed"));
  let shown = Or_error.ok_exn (Agent.mcp_servers agent) in
  print_s
    [%sexp
      (List.map shown.servers ~f:(fun s -> s.status, List.length s.tools)
       : (Mcp_tools.Status.t * int) list)];
  [%expect
    {|
    host asked for $mcp_servers
    host answers $mcp_servers
    (((Failed "it crashed") 0))
    |}]
;;
