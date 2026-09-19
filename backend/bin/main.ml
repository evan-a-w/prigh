open! Core
open Prigh

let home () = Option.value (Sys.getenv "HOME") ~default:"."

let common_params =
  let%map_open.Command model =
    flag
      "-model"
      (optional string)
      ~doc:"ID model id (default: deepseek-flash, or the session's)"
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
  in
  fun ~env ~sw ->
    let cwd = Option.value cwd ~default:(Core_unix.getcwd ()) in
    let model =
      Option.map model ~f:(fun id ->
        match Model.find id with
        | Some m -> m
        | None ->
          eprintf
            "unknown model %s; available: %s\n"
            id
            (String.concat ~sep:", " (List.map Model.all ~f:(fun m -> m.id)));
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
      else (
        match Auth.deepseek_api_key () with
        | Ok api_key -> Deepseek.create ~env ~api_key ()
        | Error e ->
          eprintf "%s\n" (Error.to_string_hum e);
          exit 2)
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
    agent
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
       let agent = make_agent ~env ~sw in
       let flush_out () = Out_channel.flush stdout in
       let note fmt =
         ksprintf (fun s -> if not quiet then eprintf "%s\n%!" s) fmt
       in
       let failed = ref None in
       Agent.subscribe agent ~f:(function
         | Loop (Message_update { delta = Text_delta s; _ }) ->
           Out_channel.output_string stdout s;
           flush_out ()
         | Loop (Message_update { delta = Thinking_delta s; _ }) ->
           if show_thinking then eprintf "%s%!" s
         | Loop (Tool_start call) ->
           note "[tool] %s %s" call.name (String.prefix call.arguments 200)
         | Loop (Tool_end { result; _ }) ->
           note
             "[tool] %s%s"
             (if result.is_error then "error: " else "")
             (String.prefix (String.strip result.text) 200)
         | Loop (Message_end (Assistant a)) ->
           (match a.stop_reason with
            | Error e -> failed := Some e
            | Aborted | End_turn | Tool_use | Length -> ());
           if not (String.is_empty (Message.Assistant.text a))
           then print_endline ""
         | Notice n -> eprintf "%s\n%!" n
         | _ -> ());
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
       let agent = make_agent ~env ~sw in
       Rpc_server.run
         ~env
         ~agent
         ~input:(Eio.Stdenv.stdin env)
         ~output:(Eio.Stdenv.stdout env))
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
       ])
;;
