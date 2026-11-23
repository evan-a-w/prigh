open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* An engine whose runs are never reached: only its capabilities matter. *)
module Limited = struct
  type t = Capabilities.t

  let engine = Engine_id.Composer
  let capabilities t = t
  let run_turn _ _ ~cancel:_ _ = []
end

let with_server caps f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir
      ~home:t.dir
      ?session
      ~runner:(Runner.create (module Limited) caps)
      ~cwd
      ()
  in
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": 1, "method": "%s", "params": %s}|} meth params)
    in
    let response = Rpc_server.handle server client request in
    let field name =
      match response with
      | `Object fields -> List.Assoc.find fields name ~equal:String.equal
      | _ -> None
    in
    match field "ok", field "error" with
    | Some `True, _ -> printf "%s: ok\n" meth
    | _, Some (`String e) -> printf "%s: %s\n" meth e
    | _ -> printf "%s: %s\n" meth (Json.to_string response)
  in
  f t (Rpc_server.agent_of_client server client) call
;;

let observe_only =
  { Capabilities.harness with
    fork = By_handoff
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = Auto_only
  ; btw = false
  ; subagents = Observe
  ; plan_mode = false
  }
;;

let gated (call : ?params:string -> string -> unit) =
  call ~params:{|{"to": "x"}|} "rewind";
  call ~params:{|{"before": "x"}|} "trim";
  call ~params:{|{"after": "x"}|} "trim_after";
  call "untrim";
  call ~params:{|{"message": "x"}|} "delete";
  call "undelete";
  call "compact";
  call "downsize_images";
  call ~params:{|{"question": "why?"}|} "btw";
  call ~params:{|{"agent_id": "a1", "text": "hi"}|} "steer_agent";
  call ~params:{|{"agent_id": "a1"}|} "dequeue_agent";
  call
    ~params:{|{"agent_id": "a1", "queue": "steer", "index": 0}|}
    "remove_agent_queued";
  call ~params:{|{"plan_mode": true}|} "set_mode";
  call ~params:{|{"plan_mode": false}|} "set_mode";
  call ~params:{|{"approval_mode": "auto"}|} "set_mode"
;;

let%expect_test "a session's engine refuses what it lacks, saying what to do" =
  with_server observe_only
  @@ fun _ _ call ->
  gated call;
  (* Forking by handoff counts. *)
  call "clone";
  [%expect
    {|
    rewind: Composer sessions can't rewind in place: fork from that message instead (/fork)
    trim: Composer sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    trim_after: Composer sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    untrim: Composer sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    delete: Composer sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    undelete: Composer sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    compact: Composer compacts its context by itself when it fills: /compact isn't available
    downsize_images: Composer sessions can't downsize the images already sent: start a new session (/new) with smaller images
    btw: Composer sessions have no side questions (/btw): ask in the session instead
    steer_agent: Composer sessions show their subagents but can't steer them: steer the session itself instead
    dequeue_agent: Composer sessions show their subagents but can't steer them: steer the session itself instead
    remove_agent_queued: Composer sessions show their subagents but can't steer them: steer the session itself instead
    set_mode: Composer sessions have no plan mode: ask for a plan in your message instead
    set_mode: ok
    set_mode: ok
    clone: ok
    |}]
;;

let%expect_test "the alternatives offered follow what the engine has" =
  with_server
    { observe_only with fork = Unsupported; compact = Native; subagents = Full }
  @@ fun _ agent call ->
  call ~params:{|{"to": "x"}|} "rewind";
  call ~params:{|{"at": "x"}|} "fork";
  call "clone";
  call ~params:{|{"before": "x"}|} "trim";
  (* The agent's own vals check too, for the ways in that skip the RPC. *)
  print_s [%sexp (Agent.rewind agent ~to_:"x" : unit Or_error.t)];
  print_s [%sexp (Agent.fork agent () : unit Or_error.t)];
  [%expect
    {|
    rewind: Composer sessions can't rewind: start a new session (/new) to begin again
    fork: Composer sessions can't be forked: start a new session (/new)
    clone: Composer sessions can't be forked: start a new session (/new)
    trim: Composer sessions can't trim or delete messages, the agent keeps its own context: compact it instead (/compact)
    (Error
     "Composer sessions can't rewind: start a new session (/new) to begin again")
    (Error "Composer sessions can't be forked: start a new session (/new)")
    |}]
;;

let%expect_test "the harness has every feature" =
  print_s
    [%sexp
      (List.filter Capabilities.Feature.all ~f:(fun feature ->
         not (Capabilities.supports Capabilities.harness feature))
       : Capabilities.Feature.t list)];
  [%expect {| () |}]
;;
