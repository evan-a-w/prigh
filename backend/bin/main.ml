open! Core
open Prigh

let home () = Option.value (Sys.getenv "HOME") ~default:"."

let auth_file_flag =
  let%map_open.Command auth_file =
    flag
      "-auth-file"
      (optional string)
      ~doc:"PATH credential file (default: ~/.config/prigh/auth.json)"
  in
  Auth_store.create
    ~path:(Option.value_or_thunk auth_file ~default:Auth_store.default_path)
;;

let provider_arg =
  Command.Arg_type.create (fun s ->
    match Provider_id.of_string s with
    | Some p -> p
    | None ->
      eprintf
        "unknown provider %s; one of: %s\n"
        s
        (String.concat
           ~sep:", "
           (List.map Provider_id.all ~f:Provider_id.to_string));
      exit 2)
;;

(* With no explicit model, prefer a provider the user is logged in to. *)
let default_model store =
  match Provider_auth.status store with
  | Error _ -> Model.default
  | Ok statuses ->
    List.find_map
      [ Provider_id.Anthropic; Openai_codex; Openai; Deepseek ]
      ~f:(fun provider ->
        List.find statuses ~f:(fun s ->
          Provider_id.equal s.provider provider && Option.is_some s.configured))
    |> Option.value_map ~default:Model.default ~f:(fun s ->
      Model.default_for s.provider)
;;

let common_params =
  let%map_open.Command model =
    flag
      "-model"
      (optional string)
      ~doc:
        "ID model id or provider/id (default: the session's, else the first \
         logged-in provider's)"
  and thinking =
    flag "-thinking" (optional string) ~doc:"LEVEL off|on|low|high|max"
  and session =
    flag
      "-session"
      (optional string)
      ~doc:"PATH continue an existing session file"
  and cwd =
    flag
      "-cwd"
      (optional string)
      ~doc:"DIR working directory (default: current)"
  and no_tools = flag "-no-tools" no_arg ~doc:" disable all tools"
  and faux =
    flag
      "-faux"
      no_arg
      ~doc:" use a scripted provider that echoes prompts (for testing)"
  and store = auth_file_flag in
  fun ~env ~sw ->
    let cwd = Option.value cwd ~default:(Core_unix.getcwd ()) in
    let model =
      Option.map model ~f:(fun id ->
        match Model.resolve id with
        | Ok m -> m
        | Error e ->
          eprintf "%s\n" (Error.to_string_hum e);
          exit 2)
    in
    let thinking =
      Option.map thinking ~f:(fun s ->
        match Rpc_json.thinking_of_string s with
        | Ok t -> t
        | Error e ->
          eprintf "%s\n" (Error.to_string_hum e);
          exit 2)
    in
    let provider =
      if faux
      then
        Faux_provider.create
          (List.init 1000 ~f:(fun _ -> Faux_provider.Reply.text "faux reply"))
      else Provider_router.create ~env ~store ()
    in
    let model =
      match model, session with
      | Some m, _ -> Some m
      | None, Some _ -> None
      | None, None -> Some (default_model store)
    in
    let session =
      Option.map session ~f:(fun path ->
        match Session.load path with
        | Ok s -> s
        | Error e ->
          eprintf "cannot load session: %s\n" (Error.to_string_hum e);
          exit 2)
    in
    let agent_ref = ref None in
    let current f default () =
      Option.value_map !agent_ref ~default ~f:(fun a -> f (Agent.state a))
    in
    let subagent =
      Tool_subagent.create
        ~provider
        ~current_model:(current (fun s -> s.model) Model.default)
        ~current_thinking:(current (fun s -> s.thinking) Thinking.Off)
        ~home:(home ())
    in
    let agent =
      Agent.create
        ~env
        ~sw
        ~provider
        ~tools:(if no_tools then [] else Tools.all @ [ subagent ])
        ~sessions_dir:(Session.default_dir ~home:(home ()))
        ~home:(home ())
        ?session
        ?model
        ?thinking
        ~cwd
        ()
    in
    agent_ref := Some agent;
    agent, store
;;

let run_command =
  Command.basic
    ~summary:"Run a single prompt headlessly, streaming the reply to stdout"
    (let%map_open.Command make_agent = common_params
     and prompt = anon ("PROMPT" %: string)
     and show_thinking =
       flag "-show-thinking" no_arg ~doc:" print thinking to stderr"
     and quiet =
       flag "-quiet" no_arg ~doc:" do not print tool activity to stderr"
     in
     fun () ->
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let agent, _store = make_agent ~env ~sw in
       let flush_out () = Out_channel.flush stdout in
       let note fmt =
         ksprintf (fun s -> if not quiet then eprintf "%s\n%!" s) fmt
       in
       let failed = ref None in
       let rec handle ~in_subagent (event : Agent.Event.t) =
         match event with
         | Loop (Subagent { event = inner; _ }) ->
           handle ~in_subagent:true (Loop inner)
         | Loop (Subagent_start { agent_id; task; tools; _ }) ->
           note
             "[subagent %s] %s (tools: %s)"
             agent_id
             (String.prefix (String.strip task) 100)
             (String.concat ~sep:", " tools)
         | Loop
             (Subagent_end
                { call_id = _; agent_id; usage; turns; cost_usd; result }) ->
           note
             "[subagent %s] %d turns, in=%d out=%d cost=$%.4f%s"
             agent_id
             turns
             usage.input
             usage.output
             cost_usd
             (if result.is_error then " (error)" else "")
         | Loop (Message_update { delta = Text_delta s; _ }) ->
           if in_subagent
           then eprintf "%s%!" s
           else (
             Out_channel.output_string stdout s;
             flush_out ())
         | Loop (Message_update { delta = Thinking_delta s; _ }) ->
           if show_thinking then eprintf "%s%!" s
         | Loop (Tool_start call) ->
           note "[tool] %s %s" call.name (String.prefix call.arguments 200)
         | Loop (Tool_output { chunk; _ }) -> eprintf "%s%!" chunk
         | Loop (Tool_end { result; _ }) ->
           note
             "[tool] %s%s"
             (if result.is_error then "error: " else "")
             (String.prefix (String.strip result.text) 200)
         | Loop (Message_end (Assistant a)) ->
           if not in_subagent
           then (
             (match a.stop_reason with
              | Error e -> failed := Some e
              | Aborted | End_turn | Tool_use | Length -> ());
             if not (String.is_empty (Message.Assistant.text a))
             then print_endline "")
         | Notice n -> eprintf "%s\n%!" n
         | _ -> ()
       in
       Agent.subscribe agent ~f:(handle ~in_subagent:false);
       Or_error.ok_exn (Agent.prompt agent prompt);
       Agent.wait_idle agent;
       let state = Agent.state agent in
       note
         "[%s] tokens: in=%d (cached %d) out=%d cost=$%.4f session=%s"
         state.model.id
         state.usage.input
         state.usage.cache_read
         state.usage.output
         state.cost_usd
         state.session_path;
       match !failed with
       | Some e ->
         eprintf "error: %s\n" e;
         exit 1
       | None -> ())
;;

let serve_command =
  Command.basic
    ~summary:"Serve the JSON-lines RPC protocol on stdin/stdout"
    (let%map_open.Command make_agent = common_params in
     fun () ->
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let agent, store = make_agent ~env ~sw in
       let login = Login_manager.create ~env ~sw ~store () in
       Rpc_server.run
         ~env
         ~agent
         ~login
         ~input:(Eio.Stdenv.stdin env)
         ~output:(Eio.Stdenv.stdout env))
;;

let login_command =
  Command.basic
    ~summary:"Log in to a provider (anthropic, openai, openai-codex, deepseek)"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: provider_arg)
     and method_ =
       flag
         "-method"
         (optional string)
         ~doc:"METHOD api_key or oauth (default: the provider's first method)"
     and no_browser =
       flag
         "-no-browser"
         no_arg
         ~doc:" print the login URL instead of opening it"
     in
     fun () ->
       let method_ =
         match method_ with
         | None -> List.hd_exn (Provider_auth.methods provider)
         | Some s ->
           (match Provider_auth.Method.of_string s with
            | Some m -> m
            | None ->
              eprintf "unknown method %s (api_key or oauth)\n" s;
              exit 2)
       in
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let interaction =
         Auth_terminal.create ~env ~sw ~open_urls:(not no_browser) ()
       in
       match Provider_auth.login ~env store provider method_ interaction with
       | Ok () ->
         eprintf
           "Logged in to %s (%s); saved to %s\n"
           (Provider_id.display_name provider)
           (Provider_auth.Method.label provider method_)
           (Auth_store.path store)
       | Error e ->
         eprintf "login failed: %s\n" (Error.to_string_hum e);
         exit 1)
;;

let logout_command =
  Command.basic
    ~summary:"Remove a provider's stored credential"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: provider_arg) in
     fun () ->
       match Provider_auth.logout store provider with
       | Ok () ->
         eprintf "Logged out of %s\n" (Provider_id.display_name provider)
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let auth_command =
  Command.basic
    ~summary:"Show which providers are configured"
    (let%map_open.Command store = auth_file_flag in
     fun () ->
       match Provider_auth.status store with
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1
       | Ok statuses ->
         List.iter statuses ~f:(fun s ->
           printf
             "%-14s %-24s %s\n"
             (Provider_id.to_string s.provider)
             (Provider_id.display_name s.provider)
             (match s.configured with
              | None ->
                sprintf
                  "not configured (login: %s)"
                  (String.concat
                     ~sep:", "
                     (List.map s.methods ~f:Provider_auth.Method.to_string))
              | Some (_, source) -> source)))
;;

let sessions_command =
  Command.basic
    ~summary:"List saved sessions"
    (Command.Param.return (fun () ->
       List.iter
         (Session.list ~dir:(Session.default_dir ~home:(home ())))
         ~f:(fun s ->
           printf
             "%s  %s  %3d msgs  %s  %s\n"
             s.created_at
             s.id
             s.message_count
             s.cwd
             (Option.value_map s.first_prompt ~default:"" ~f:(fun p ->
                String.prefix (String.strip p) 60)))))
;;

let () =
  Random.self_init ();
  Command_unix.run
    ~version:Version.to_string
    (Command.group
       ~summary:"prigh backend"
       [ "run", run_command
       ; "serve", serve_command
       ; "sessions", sessions_command
       ; "login", login_command
       ; "logout", logout_command
       ; "auth", auth_command
       ])
;;
