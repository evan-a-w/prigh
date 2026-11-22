open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The [pull_request] tool on {!Fake_forge.set_up_repos}'s repository and
   {!Fake_forge.github}: git runs for real (pushes and fetches go to a local
   bare repository), the API is the fake's. *)

let sign_in store ~token =
  Or_error.ok_exn
    (Auth_store.modify_entry
       store
       (Forge_credential.key ~host:"github.com")
       ~f:(fun _ ->
         Ok
           (Some
              (Forge_credential.to_json
                 { kind = Github
                 ; method_ = Token
                 ; token
                 ; refresh = None
                 ; expires_ms = None
                 ; client_id = None
                 ; user = Some "octocat"
                 }))))
  |> (ignore : Jsonaf.t option -> unit)
;;

let forges ~fake ~home store =
  Forges.create
    ~http:(Fake_forge.http fake)
    ~sleep:(fun _ -> ())
    ~getenv:(fun _ -> None)
    ~home
    ~store
    ()
;;

let with_repo f =
  with_sandbox
  @@ fun t ->
  let repo = Fake_forge.set_up_repos t in
  let fake = Fake_forge.create Fake_forge.github in
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let tool =
    Tool_pull_request.create
      ~forges:(forges ~fake ~home:t.dir store)
      ~worktrees_dir:(fun () -> Filename.concat t.dir "worktrees")
      ()
  in
  let run ?(cwd = repo) args =
    let context =
      Tool.Context.create
        ~env:t.env
        ~cwd
        ~emit:(function
          | Worktrees_changed { added; removed } ->
            print_endline
              (mask_sexp
                 t
                 [%sexp
                   "worktrees_changed"
                 , (added : Session_worktree.t list)
                 , (removed : string list)])
          | _ -> ())
        ()
    in
    let result = Tool.execute tool context (Jsonaf.of_string args) in
    printf
      "%s%s\n"
      (if result.is_error then "ERROR: " else "")
      (mask t result.text)
  in
  f t ~repo ~fake ~store ~tool ~run
;;

let%expect_test "not signed in: says where to sign in, asks nothing of the host"
  =
  with_repo
  @@ fun _ ~repo:_ ~fake ~store:_ ~tool:_ ~run ->
  run {|{"action":"list"}|};
  Fake_forge.print_requests fake;
  [%expect
    {| ERROR: pull_request list: not signed in to github.com: ask the user to sign in under Settings → Git hosting in prigh-web, or with /login github |}]
;;

(* An agent driven by [Faux_provider], printing confirmations and results. *)
let with_agent ~answer replies ~sub_replies f =
  with_repo
  @@ fun t ~repo ~fake ~store ~tool ~run:_ ->
  sign_in store ~token:"ghp_mine";
  Eio.Switch.run
  @@ fun sw ->
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
      ~provider:(Faux_provider.create replies)
      ~tools:(Tools.all @ [ subagent; tool ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:repo
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules; kind }) ->
      printf
        "confirm %s%s %s: %s\n"
        (Option.value_map agent_id ~default:"" ~f:(fun a -> a ^ "/"))
        call_id
        name
        (mask t summary);
      Or_error.ok_exn
        (Agent.respond_confirm
           ?agent_id
           agent
           ~call_id
           (answer
              { Agent.Pending_confirm.call_id
              ; agent_id
              ; name
              ; summary
              ; rules
              ; kind
              }))
    | Loop (Tool_end { call; result }) ->
      printf
        "%s %s -> %s%s\n"
        call.name
        (mask t call.arguments)
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | Loop (Subagent { event = Tool_end { call; result }; agent_id; _ }) ->
      printf
        "  [%s] %s -> %s%s\n"
        agent_id
        call.name
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | _ -> ());
  f t agent ~fake
;;
