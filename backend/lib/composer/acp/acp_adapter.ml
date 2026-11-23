open! Core
open! Import
module Codec = Acp_client_codec

let capabilities_of
      (caps : Codec.Agent_capabilities.t option)
      (config : Codec.Session_config.t)
  : Capabilities.t
  =
  { fork = By_handoff
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = Unsupported
  ; steer = Between_turns
  ; plan_mode =
      List.exists config.modes ~f:(fun (m : Codec.Mode.t) ->
        String.Caseless.equal m.id "plan")
  ; btw = false
  ; subagents = Unsupported
  ; model_fallback = false
  ; images = Option.value_map caps ~default:false ~f:(fun c -> c.images)
  ; ask_user = true
  }
;;

type t =
  { codec : Codec.t
  ; driver : Codec.Response.t Adapter.Driver.t
  ; mutable lines : int (* lines read, to tell a quiet replay *)
  ; mutable last_line : Time_float.t
  ; mutable before_plan : string option (* the mode plan mode replaced *)
  ; mutable model : string option (* the model last set *)
  }

let mcp_server (name, json) : Codec.Mcp_server.t option =
  let strings key =
    match Json.member key json with
    | Some (`Array l) ->
      List.filter_map l ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> []
  in
  match Json.member "command" json with
  | Some (`String command) ->
    let env =
      match Json.member "env" json with
      | Some (`Object kvs) ->
        List.filter_map kvs ~f:(fun (k, v) ->
          match v with
          | `String v -> Some (k, v)
          | _ -> None)
      | _ -> []
    in
    Some (Stdio { name; command; args = strings "args"; env })
  | _ -> None
;;

let dispatch t (output : Codec.Output.t) =
  match output with
  | Event e -> Adapter.Driver.push t.driver e
  | Send line -> Adapter.Driver.write t.driver line
  | Response r -> Adapter.Driver.answer t.driver r
  | Config_changed _ | Commands _ | Title _ -> ()
;;

let not_logged_in (agent : Acp_agent.t) methods =
  let how =
    match (methods : Codec.Auth_method.t list) with
    | [] -> sprintf "run `%s` there and log in" (List.hd_exn agent.command)
    | ms ->
      sprintf
        "log in with %s (run `%s` there)"
        (String.concat
           ~sep:" or "
           (List.map ms ~f:(fun (m : Codec.Auth_method.t) -> m.name)))
        (List.hd_exn agent.command)
  in
  sprintf "%s is not logged in on this host: %s" agent.display_name how
;;

let create
      ?(load_quiet = Time_float.Span.of_sec 2.)
      ?(load_timeout = Time_float.Span.of_sec 90.)
      (agent : Acp_agent.t)
  : Adapter.t
  =
  (module struct
    type nonrec t = t

    let id = "acp:" ^ agent.name
    let display_name = agent.display_name
    let capabilities = capabilities_of None Codec.Session_config.empty

    let current_capabilities t =
      capabilities_of
        (Codec.agent_capabilities t.codec)
        (Codec.session_config t.codec)
    ;;

    let native_questions = false

    let launch (start : Adapter.Start.t) : Adapter.Launch.t =
      { argv = agent.command
      ; env_set = []
      ; env_unset = []
      ; cwd = start.cwd
      ; mcp = true
      }
    ;;

    let refused what (r : Codec.Response.t) =
      match r with
      | Failed { code = -32000; _ } ->
        Or_error.error_string (not_logged_in agent [])
      | Failed { message; code; _ } ->
        Or_error.errorf
          "%s refused %s (%d): %s"
          agent.display_name
          what
          code
          message
      | _ -> Ok ()
    ;;

    (* [session/load] whose answer may never come: once the replay has been
       quiet for [load_quiet], the session counts as loaded. *)
    let load t line ~session_id =
      let lines_before = t.lines in
      let answered () =
        Adapter.Driver.request
          ~timeout:load_timeout
          t.driver
          line
          ~matches:(function
          | Session_opened { kind = Load_session; _ }
          | Failed { kind = Load_session; _ } -> true
          | _ -> false)
      in
      let quiet () =
        let quiet = Time_float.Span.to_sec load_quiet in
        let rec wait () =
          Eio_unix.sleep quiet;
          let idle = Time_float.diff (Time_float.now ()) t.last_line in
          if
            t.lines > lines_before
            && Float.(Time_float.Span.to_sec idle >= quiet)
          then (
            Codec.assume_loaded t.codec ~session_id;
            Ok
              (Codec.Response.Session_opened
                 { kind = Load_session
                 ; session_id
                 ; config = Codec.session_config t.codec
                 }))
          else wait ()
        in
        wait ()
      in
      let timeout () =
        Eio_unix.sleep (Time_float.Span.to_sec load_timeout);
        Or_error.errorf
          "%s did not load its session within %s"
          agent.display_name
          (Time_float.Span.to_string_hum load_timeout)
      in
      Fiber.any [ answered; quiet; timeout ]
    ;;

    let start
          ~sw
          ?interrupt_grace
          ?request_timeout
          ?injected:_
          transport
          (start : Adapter.Start.t)
      =
      let codec = Codec.create ?cost_total:start.cost_total () in
      let driver =
        Adapter.Driver.create
          ~sw
          ~name:agent.display_name
          ?interrupt_grace
          ?request_timeout
          transport
      in
      let t =
        { codec
        ; driver
        ; lines = 0
        ; last_line = Time_float.now ()
        ; before_plan = None
        ; model = None
        }
      in
      Adapter.Driver.run
        driver
        ~handle_line:(fun line ->
          t.lines <- t.lines + 1;
          t.last_line <- Time_float.now ();
          List.iter (Codec.handle_line codec line) ~f:(dispatch t))
        ~on_exit:(fun exit ->
          List.iter
            (Codec.process_exited
               ~name:agent.display_name
               codec
               ~code:exit.code
               ~stderr_tail:exit.stderr_tail)
            ~f:(dispatch t));
      let open Or_error.Let_syntax in
      let%bind init =
        Adapter.Driver.request
          driver
          (Codec.initialize
             codec
             ~client_name:"prigh"
             ~client_version:Version.to_string)
          ~matches:(function
          | Initialized _ | Failed { kind = Initialize; _ } -> true
          | _ -> false)
        |> Result.map_error
             ~f:(Error.tag ~tag:(agent.display_name ^ " did not start"))
      in
      let%bind auth_methods =
        match init with
        | Initialized { protocol_version = 1; auth_methods; _ } ->
          Ok auth_methods
        | Initialized { protocol_version; _ } ->
          Or_error.errorf
            "%s speaks ACP version %d, and prigh speaks version 1: update \
             prigh or %s"
            agent.display_name
            protocol_version
            agent.display_name
        | r -> refused "to start" r >>| fun () -> []
      in
      let mcp_servers = List.filter_map start.mcp_servers ~f:mcp_server in
      let caps = Codec.agent_capabilities codec in
      let can_load =
        Option.value_map caps ~default:false ~f:(fun c -> c.load_session)
      in
      let can_resume =
        Option.value_map caps ~default:false ~f:(fun c -> c.resume_session)
      in
      let fresh () =
        Adapter.Driver.request
          driver
          (Codec.new_session codec ~cwd:start.cwd ~mcp_servers)
          ~matches:(function
          | Session_opened { kind = New_session; _ }
          | Failed { kind = New_session; _ } -> true
          | _ -> false)
      in
      let%bind opened =
        match start.resume with
        | Some session_id when (not start.fork) && can_resume ->
          Adapter.Driver.request
            driver
            (Codec.resume_session codec ~session_id ~cwd:start.cwd ~mcp_servers)
            ~matches:(function
            | Session_opened { kind = Resume_session; _ }
            | Failed { kind = Resume_session; _ } -> true
            | _ -> false)
        | Some session_id when (not start.fork) && can_load ->
          load
            t
            (Codec.load_session codec ~session_id ~cwd:start.cwd ~mcp_servers)
            ~session_id
        | Some _ ->
          Adapter.Driver.push
            driver
            (Notice
               (sprintf
                  "%s can't continue its earlier session: it starts a new one"
                  agent.display_name));
          fresh ()
        | None -> fresh ()
      in
      let%map () =
        match opened with
        | Failed { code = -32000; _ } ->
          Or_error.error_string (not_logged_in agent auth_methods)
        | r -> refused "to open a session" r
      in
      t
    ;;

    let next_event t = Adapter.Driver.next_event t.driver

    let set_mode t mode_id =
      match Codec.set_mode t.codec ~mode_id with
      | Error _ as e -> e
      | Ok line ->
        let%bind.Or_error r =
          Adapter.Driver.request t.driver line ~matches:(function
            | Mode_set _ | Failed { kind = Set_mode; _ } -> true
            | _ -> false)
        in
        refused "the mode" r
    ;;

    (* Plan mode is the agent's [plan] mode; the model, its [model] option
       when it offers it. What it does not offer is left as it is. *)
    let apply t (s : Adapter.Settings.t) =
      let open Or_error.Let_syntax in
      let config = Codec.session_config t.codec in
      let plan_id =
        List.find_map config.modes ~f:(fun (m : Codec.Mode.t) ->
          Option.some_if (String.Caseless.equal m.id "plan") m.id)
      in
      let in_plan =
        match config.current_mode, plan_id with
        | Some current, Some plan -> String.equal current plan
        | _ -> false
      in
      let%bind () =
        match plan_id with
        | Some plan when s.plan && not in_plan ->
          t.before_plan <- config.current_mode;
          set_mode t plan
        | Some _ when (not s.plan) && in_plan ->
          let back =
            Option.first_some
              t.before_plan
              (List.find_map config.modes ~f:(fun (m : Codec.Mode.t) ->
                 Option.some_if (not (String.Caseless.equal m.id "plan")) m.id))
          in
          t.before_plan <- None;
          (match back with
           | Some mode -> set_mode t mode
           | None -> Ok ())
        | _ -> Ok ()
      in
      match s.model, Codec.Session_config.option config ~category:"model" with
      | Some model, Some ({ value = Select { current; choices }; _ } as option)
        when (not (String.equal current model))
             && (not (Option.equal String.equal t.model (Some model)))
             && List.exists
                  choices
                  ~f:(fun (c : Codec.Config_option.Choice.t) ->
                    String.equal c.value model) ->
        t.model <- Some model;
        (match
           Codec.set_config_option t.codec ~config_id:option.id (`Id model)
         with
         | Error _ as e -> e
         | Ok line ->
           let%bind r =
             Adapter.Driver.request t.driver line ~matches:(function
               | Config_set _ | Failed { kind = Set_config_option; _ } -> true
               | _ -> false)
           in
           refused "the model" r)
      | _ -> Ok ()
    ;;

    let send t settings (input : Adapter.Input.t) =
      if not (Adapter.Driver.alive t.driver)
      then Or_error.errorf "%s is not running" agent.display_name
      else if Adapter.Driver.in_turn t.driver
      then Or_error.errorf "a %s turn is already running" agent.display_name
      else
        let open Or_error.Let_syntax in
        let%bind () = apply t settings in
        let inputs =
          (if String.is_empty input.text
           then []
           else [ Codec.Input.Text input.text ])
          @ List.map input.images ~f:(fun i -> Codec.Input.Image i)
        in
        let%map outputs = Codec.prompt t.codec inputs in
        Adapter.Driver.begin_turn t.driver;
        List.iter outputs ~f:(dispatch t)
    ;;

    (* The choices of its [model] option; the current one is its default. *)
    let models t =
      match
        Codec.Session_config.option
          (Codec.session_config t.codec)
          ~category:"model"
      with
      | Some { value = Select { current; choices }; _ } ->
        let model (c : Codec.Config_option.Choice.t) =
          External_model.model ~adapter:id ~name:c.name c.value
        in
        let default =
          match
            List.find choices ~f:(fun (c : Codec.Config_option.Choice.t) ->
              String.equal c.value current)
          with
          | Some c ->
            { (model c) with
              id = External_model.default_id
            ; name = sprintf "default (%s)" c.name
            }
          | None -> External_model.default ~adapter:id
        in
        default :: List.map choices ~f:model
      | Some _ | None -> []
    ;;

    let compact _ _ ~instructions:_ =
      Or_error.errorf "%s does not compact on request" display_name
    ;;

    let steer _ (_ : Adapter.Input.t) =
      Or_error.errorf "%s takes new messages between turns" agent.display_name
    ;;

    let interrupt t =
      if Adapter.Driver.in_turn t.driver
      then (
        List.iter (Codec.cancel t.codec) ~f:(dispatch t);
        Adapter.Driver.watch_interrupt t.driver)
    ;;

    let reply_approval t ~request_id reply =
      Option.iter
        (Codec.reply_approval t.codec ~request_id reply)
        ~f:(Adapter.Driver.write t.driver)
    ;;

    (* ACP agents ask the user through prigh's [ask_user] tool. *)
    let answer _ ~request_id:_ (_ : User_question.Reply.t) = ()
    let thread_id t = Codec.session_id t.codec
    let cost_total t = Codec.cost_total t.codec
    let in_turn t = Adapter.Driver.in_turn t.driver
    let alive t = Adapter.Driver.alive t.driver
    let stop t = Adapter.Driver.stop t.driver
  end)
;;
