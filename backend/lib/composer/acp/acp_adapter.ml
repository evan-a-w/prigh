open! Core
open! Import
module Codec = Acp_client_codec

let capabilities_of
      ?(quirks = Acp_agent.Quirks.none)
      ?(compact_command = false)
      (caps : Codec.Agent_capabilities.t option)
      (config : Codec.Session_config.t)
  : Capabilities.t
  =
  { fork = By_handoff
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = (if compact_command then Command else Summary)
  ; steer =
      (match quirks.steer with
       | Between_turns -> Between_turns
       | Prompt | Xai_interject -> In_run)
  ; plan_mode =
      List.exists config.modes ~f:(fun (m : Codec.Mode.t) ->
        String.Caseless.equal m.id "plan")
  ; btw = false
  ; subagents = Unsupported
  ; model_fallback = false
  ; images = Option.value_map caps ~default:false ~f:(fun c -> c.images)
  ; ask_user = true
  ; rewind_note = None
  }
;;

type t =
  { codec : Codec.t
  ; driver : Codec.Response.t Adapter.Driver.t
  ; mutable lines : int (* lines read, to tell a quiet replay *)
  ; mutable last_line : Time_float.t
  ; mutable before_plan : string option (* the mode plan mode replaced *)
  ; mutable model : string option (* the model last set *)
  ; mutable agent_default : string option (* its model before we set one *)
  ; mutable login : Antigravity_login.t option
    (* prigh's login, when the process runs on it (Antigravity's) *)
  ; mutable refused : bool (* a turn was refused with its token *)
  ; mutable plan : bool option
    (* prigh's plan mode as last applied: the agent may leave plan mode
       itself (Grok, its plan approved) and is not put back *)
  ; mutable commands : Codec.Command.t list (* its slash commands *)
  ; mutable compacting : bool (* a turn running its compact command *)
  ; mutable held : Adapter_event.t option (* the next event, held back *)
  ; mutable watching_autonomous : bool
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
  | Commands commands -> t.commands <- commands
  | Title (Some title) ->
    let title = String.strip title in
    if not (String.is_empty title)
    then Adapter.Driver.push t.driver (Session_title title)
  | Title None | Config_changed _ -> ()
;;

(* The agent's own command for compacting its context, when it has one
   (Antigravity's [/compact]). *)
let compact_command t =
  List.find t.commands ~f:(fun (c : Codec.Command.t) ->
    List.mem
      [ "compact"; "compress" ]
      (String.lowercase (String.chop_prefix_if_exists c.name ~prefix:"/"))
      ~equal:String.equal)
;;

(* The agent's [thought_level] option, as prigh's thinking levels. *)
module Thought = struct
  let level (c : Codec.Config_option.Choice.t) =
    match String.lowercase c.value with
    | "xhigh" | "extra_high" | "x-high" -> Some Thinking.Level.Xhigh
    | v -> Thinking.Level.of_string v
  ;;

  let is_off (c : Codec.Config_option.Choice.t) =
    List.mem
      [ "off"; "none"; "disabled" ]
      (String.lowercase c.value)
      ~equal:String.equal
  ;;

  let option config =
    match Codec.Session_config.option config ~category:"thought_level" with
    | Some ({ value = Select { choices; current }; _ } as o) ->
      Some (o, current, choices)
    | Some _ | None -> None
  ;;

  (* Its levels, and whether it can be off. *)
  let levels config =
    match option config with
    | None -> [], false
    | Some (_, _, choices) ->
      ( List.filter_map choices ~f:level
        |> List.dedup_and_sort ~compare:Thinking.Level.compare
      , List.exists choices ~f:is_off )
  ;;

  (* The choice for [thinking], when the agent has one ([On None] leaves it
     as it is). *)
  let choice config (thinking : Thinking.t) =
    Option.bind (option config) ~f:(fun (o, current, choices) ->
      let wanted =
        match thinking with
        | On None -> None
        | Off -> List.find choices ~f:is_off
        | On (Some l) ->
          List.find choices ~f:(fun c ->
            Option.equal Thinking.Level.equal (level c) (Some l))
      in
      Option.bind wanted ~f:(fun (c : Codec.Config_option.Choice.t) ->
        Option.some_if (not (String.equal c.value current)) (o, c.value)))
  ;;
end

let not_logged_in (agent : Acp_agent.t) methods =
  let how =
    match (methods : Codec.Auth_method.t list), agent.quirks.login with
    | _, Some login -> sprintf "run `%s` there" login
    | [], None ->
      sprintf "run `%s` there and log in" (List.hd_exn agent.command)
    | ms, None ->
      sprintf
        "log in with %s (run `%s` there)"
        (String.concat
           ~sep:" or "
           (List.map ms ~f:(fun (m : Codec.Auth_method.t) -> m.name)))
        (List.hd_exn agent.command)
  in
  sprintf "%s is not logged in on this host: %s" agent.display_name how
;;

let is_not_logged_in e =
  String.is_substring
    (Error.to_string_hum e)
    ~substring:" is not logged in on this host: "
;;

let create
      ?(load_quiet = Time_float.Span.of_sec 2.)
      ?(load_timeout = Time_float.Span.of_sec 90.)
      ?(autonomous_quiet = Time_float.Span.of_sec 5.)
      (agent : Acp_agent.t)
  : Adapter.t
  =
  (module struct
    type nonrec t = t

    let id = "acp:" ^ agent.name
    let display_name = agent.display_name

    let capabilities =
      capabilities_of ~quirks:agent.quirks None Codec.Session_config.empty
    ;;

    let current_capabilities t =
      capabilities_of
        ~quirks:agent.quirks
        ~compact_command:(Option.is_some (compact_command t))
        (Codec.agent_capabilities t.codec)
        (Codec.session_config t.codec)
    ;;

    let native_questions = agent.quirks.xai_extensions
    let starts_early = true
    let takes_login = String.equal agent.name "antigravity"

    let launch (start : Adapter.Start.t) : Adapter.Launch.t =
      match start.login with
      | Some login when takes_login ->
        let l =
          Antigravity_login.launch
            login
            ~refused:start.login_refused
            ~command:agent.command
        in
        { argv = l.argv
        ; env_set = l.env_set
        ; env_unset = l.env_unset
        ; cwd = start.cwd
        ; mcp = true
        ; secret = l.secret
        }
      | Some _ | None ->
        { argv = agent.command
        ; env_set = []
        ; env_unset = []
        ; cwd = start.cwd
        ; mcp = true
        ; secret = None
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
        let min_step = Float.min 0.05 (quiet *. 0.1) in
        let rec wait delay =
          Eio_unix.sleep delay;
          let idle =
            Time_float.Span.to_sec
              (Time_float.diff (Time_float.now ()) t.last_line)
          in
          if t.lines > lines_before && Float.(idle >= quiet)
          then (
            Codec.assume_loaded t.codec ~session_id;
            Ok
              (Codec.Response.Session_opened
                 { kind = Load_session
                 ; session_id
                 ; config = Codec.session_config t.codec
                 }))
          else (
            let next =
              if t.lines > lines_before
              then Float.max min_step (quiet -. idle)
              else quiet
            in
            wait next)
        in
        wait quiet
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

    let set_mode t mode_id =
      match Codec.set_mode t.codec ~mode_id with
      | Error _ as e -> e
      | Ok line ->
        let%bind.Or_error r =
          Turn_timings.phase "session/set_mode" (fun () ->
            Adapter.Driver.request t.driver line ~matches:(function
              | Mode_set _ | Failed { kind = Set_mode; _ } -> true
              | _ -> false))
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
      let plan_changed = not (Option.equal Bool.equal t.plan (Some s.plan)) in
      let%bind () =
        match plan_id with
        | _ when not plan_changed -> Ok ()
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
      t.plan <- Some s.plan;
      let set_option ~what (option : Codec.Config_option.t) value =
        match
          Codec.set_config_option t.codec ~config_id:option.id (`Id value)
        with
        | Error _ as e -> e
        | Ok line ->
          let%bind r =
            Turn_timings.phase "session/set_config_option" (fun () ->
              Adapter.Driver.request t.driver line ~matches:(function
                | Config_set _ | Failed { kind = Set_config_option; _ } -> true
                | _ -> false))
          in
          refused what r
      in
      (* Its default (no model chosen) is the model it started with. *)
      let%bind () =
        match
          ( Option.first_some s.model t.agent_default
          , Codec.Session_config.option config ~category:"model" )
        with
        | ( Some model
          , Some ({ value = Select { current; choices }; _ } as option) )
          when (not (String.equal current model))
               && (not (Option.equal String.equal t.model (Some model)))
               && List.exists
                    choices
                    ~f:(fun (c : Codec.Config_option.Choice.t) ->
                      String.equal c.value model) ->
          let%map () = set_option ~what:"the model" option model in
          t.model <- Some model
        | _ -> Ok ()
      in
      match Thought.choice (Codec.session_config t.codec) s.thinking with
      | Some (option, value) ->
        set_option ~what:"the thinking level" option value
      | _ -> Ok ()
    ;;

    let open_session
          ~sw
          ?interrupt_grace
          ?request_timeout
          transport
          (start : Adapter.Start.t)
      =
      let codec =
        Codec.create ~quirks:agent.quirks ?cost_total:start.cost_total ()
      in
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
        ; agent_default = None
        ; plan = None
        ; login = None
        ; refused = false
        ; commands = []
        ; compacting = false
        ; held = None
        ; watching_autonomous = false
        }
      in
      let refuse_orphan (event : Adapter_event.t) =
        match event with
        | Approval_request { request_id; _ } ->
          Option.iter
            (Codec.reply_approval t.codec ~request_id Cancel)
            ~f:(Adapter.Driver.write t.driver)
        | User_question { request_id; _ } ->
          Option.iter
            (Codec.answer t.codec ~request_id Cancelled)
            ~f:(Adapter.Driver.write t.driver)
        | _ -> ()
      in
      Adapter.Driver.on_orphan_request driver refuse_orphan;
      let watch_autonomous () =
        if not t.watching_autonomous
        then (
          t.watching_autonomous <- true;
          let text_quiet = Time_float.Span.to_sec autonomous_quiet in
          let wait_quiet = Float.min 60. (text_quiet *. 12.) in
          Fiber.fork_daemon ~sw (fun () ->
            let poll = Float.min 0.5 (Float.max 0.01 (text_quiet /. 2.)) in
            let rec loop () =
              Eio_unix.sleep poll;
              if
                (not (Adapter.Driver.alive t.driver))
                || not (Codec.is_autonomous_turn t.codec)
              then (
                t.watching_autonomous <- false;
                `Stop_daemon)
              else if Codec.has_open_work t.codec
              then loop ()
              else (
                let idle =
                  Time_float.Span.to_sec
                    (Time_float.diff (Time_float.now ()) t.last_line)
                in
                let limit =
                  if Codec.autonomous_has_text t.codec
                  then text_quiet
                  else wait_quiet
                in
                if Float.(idle >= limit)
                then (
                  t.watching_autonomous <- false;
                  List.iter (Codec.finish_autonomous t.codec) ~f:(dispatch t);
                  `Stop_daemon)
                else loop ())
            in
            loop ()))
      in
      Adapter.Driver.run
        driver
        ~handle_line:(fun line ->
          t.lines <- t.lines + 1;
          t.last_line <- Time_float.now ();
          List.iter (Codec.handle_line codec line) ~f:(dispatch t);
          if Codec.is_autonomous_turn codec then watch_autonomous ())
        ~on_exit:(fun exit ->
          List.iter
            (Codec.process_exited
               ~name:agent.display_name
               codec
               ~code:exit.code
               ~stderr_tail:exit.stderr_tail)
            ~f:(dispatch t));
      let open Or_error.Let_syntax in
      let starting r =
        Result.map_error
          r
          ~f:
            (Adapter.Driver.start_failed driver ~hint:(fun exit ->
               Acp_agent.start_hint agent ~stderr:exit.stderr_tail))
      in
      let%bind init =
        Turn_timings.phase "initialize" (fun () ->
          Adapter.Driver.request
            driver
            (Codec.initialize
               codec
               ~client_name:"prigh"
               ~client_version:Version.to_string)
            ~matches:(function
            | Initialized _ | Failed { kind = Initialize; _ } -> true
            | _ -> false))
        |> starting
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
      if takes_login
      then
        Option.iter start.login ~f:(fun credential ->
          match
            Antigravity_login.of_launch credential ~refused:start.login_refused
          with
          | Ok login -> t.login <- Some login
          | Error why -> Adapter.Driver.push driver (Notice why));
      let mcp_servers = List.filter_map start.mcp_servers ~f:mcp_server in
      let caps = Codec.agent_capabilities codec in
      let can_load =
        Option.value_map caps ~default:false ~f:(fun c -> c.load_session)
      in
      let can_resume =
        Option.value_map caps ~default:false ~f:(fun c -> c.resume_session)
      in
      let fresh () =
        Turn_timings.phase "session/new" (fun () ->
          Adapter.Driver.request
            driver
            (Codec.new_session codec ~cwd:start.cwd ~mcp_servers)
            ~matches:(function
            | Session_opened { kind = New_session; _ }
            | Failed { kind = New_session; _ } -> true
            | _ -> false))
      in
      let%bind opened =
        (match start.resume with
         | Some session_id when (not start.fork) && can_resume ->
           Turn_timings.phase "session/resume" (fun () ->
             Adapter.Driver.request
               driver
               (Codec.resume_session
                  codec
                  ~session_id
                  ~cwd:start.cwd
                  ~mcp_servers)
               ~matches:(function
               | Session_opened { kind = Resume_session; _ }
               | Failed { kind = Resume_session; _ } -> true
               | _ -> false))
         | Some session_id when (not start.fork) && can_load ->
           Turn_timings.phase "session/load" (fun () ->
             load
               t
               (Codec.load_session
                  codec
                  ~session_id
                  ~cwd:start.cwd
                  ~mcp_servers)
               ~session_id)
         | Some _ ->
           Adapter.Driver.push
             driver
             (Notice
                (sprintf
                   "%s can't continue its earlier session: it starts a new one"
                   agent.display_name));
           fresh ()
         | None -> fresh ())
        |> starting
      in
      let%map () =
        match opened with
        | Failed { code = -32000; _ } ->
          (match t.login with
           | Some login ->
             Or_error.error_string (Antigravity_login.refused_hint login)
           | None -> Or_error.error_string (not_logged_in agent auth_methods))
        | r -> refused "to open a session" r
      in
      t.agent_default
      <- (match
            Codec.Session_config.option
              (Codec.session_config codec)
              ~category:"model"
          with
          | Some { value = Select { current; _ }; _ } -> Some current
          | Some _ | None -> None);
      t
    ;;

    let start
          ~sw
          ?interrupt_grace
          ?request_timeout
          ?injected:_
          transport
          (start : Adapter.Start.t)
      =
      let%bind.Or_error t =
        open_session ~sw ?interrupt_grace ?request_timeout transport start
      in
      (* Apply initial mode/model/thinking during startup/prewarm so the first
         [session/prompt] does not pay serial config roundtrips. *)
      match apply t (Adapter.Settings.of_start start) with
      | Ok () -> Ok t
      | Error _ as e ->
        Adapter.Driver.stop t.driver;
        e
    ;;

    let next_event' t : Adapter_event.t =
      match Adapter.Driver.next_event t.driver, t.login with
      | ( Turn_end { turn_id; stop = Failed { kind = Auth_required _; message } }
        , Some login ) ->
        t.refused <- true;
        Turn_end
          { turn_id
          ; stop =
              Failed
                { kind =
                    Auth_required
                      { hint = Antigravity_login.refused_hint login }
                ; message
                }
          }
      | event, _ -> event
    ;;

    (* A turn of its compact command that ends well compacted the context:
       said before its end. *)
    let next_event t : Adapter_event.t =
      match t.held with
      | Some event ->
        t.held <- None;
        event
      | None ->
        (match next_event' t with
         | Turn_end { stop = Done; _ } as event when t.compacting ->
           t.compacting <- false;
           t.held <- Some event;
           Compacted { tokens_before = None; tokens_after = None }
         | Turn_end _ as event ->
           t.compacting <- false;
           event
         | event -> event)
    ;;

    let inputs_of (input : Adapter.Input.t) =
      (if String.is_empty input.text
       then []
       else [ Codec.Input.Text input.text ])
      @ List.map input.images ~f:(fun i -> Codec.Input.Image i)
    ;;

    let send t settings (input : Adapter.Input.t) =
      Adapter.Driver.await_idle t.driver;
      if not (Adapter.Driver.alive t.driver)
      then Or_error.errorf "%s is not running" agent.display_name
      else if Adapter.Driver.in_turn t.driver
      then Or_error.errorf "a %s turn is already running" agent.display_name
      else
        let open Or_error.Let_syntax in
        let%bind () = apply t settings in
        let%map outputs = Codec.prompt t.codec (inputs_of input) in
        Adapter.Driver.begin_turn t.driver;
        List.iter outputs ~f:(dispatch t)
    ;;

    let limits _ = None

    (* Its models come with a session. *)
    let on_change _ _ = ()
    let on_autonomous t f = Adapter.Driver.on_autonomous t.driver f
    let on_title t f = Adapter.Driver.on_title t.driver f

    (* The choices of its [model] option, in its order, the one it started
       with first. *)
    let models_of ~agent_default config =
      match Codec.Session_config.option config ~category:"model" with
      | Some { value = Select { current; choices }; _ } ->
        let thinking_levels, can_disable_thinking = Thought.levels config in
        let model (c : Codec.Config_option.Choice.t) =
          External_model.model
            ~adapter:id
            ~name:c.name
            ~thinking_levels
            ~can_disable_thinking
            c.value
        in
        let default = Option.value agent_default ~default:current in
        let first, rest =
          List.partition_tf
            choices
            ~f:(fun (c : Codec.Config_option.Choice.t) ->
              String.equal c.value default)
        in
        List.map (first @ rest) ~f:model
      | Some _ | None -> []
    ;;

    let models t =
      models_of ~agent_default:t.agent_default (Codec.session_config t.codec)
    ;;

    (* A session opened only to read its models. *)
    let catalog ~sw ?request_timeout transport (s : Adapter.Start.t) =
      let%map.Or_error t =
        open_session
          ~sw
          ?request_timeout
          transport
          { s with resume = None; mcp_servers = [] }
      in
      let models = models t in
      Adapter.Driver.stop t.driver;
      { Adapter.Catalog.models; limits = None }
    ;;

    (* Its compact command, as a prompt. Not with [instructions]: Antigravity
       answers a message after its [/compact] as a prompt. *)
    let compact t settings ~instructions:_ =
      match compact_command t with
      | None ->
        Or_error.errorf "%s has no command to compact its context" display_name
      | Some command ->
        let text =
          "/" ^ String.chop_prefix_if_exists command.name ~prefix:"/"
        in
        let%map.Or_error () = send t settings { text; images = [] } in
        t.compacting <- true
    ;;

    let steer t (input : Adapter.Input.t) =
      if not (Adapter.Driver.in_turn t.driver)
      then Or_error.errorf "no %s turn is running" agent.display_name
      else
        Codec.steer t.codec (inputs_of input)
        |> Result.map ~f:(List.iter ~f:(dispatch t))
        |> Result.map_error ~f:(fun e ->
          Error.of_string
            (sprintf "%s: %s" agent.display_name (Error.to_string_hum e)))
    ;;

    let interrupt t =
      if Adapter.Driver.in_turn t.driver
      then (
        let autonomous = Codec.is_autonomous_turn t.codec in
        List.iter (Codec.cancel t.codec) ~f:(dispatch t);
        if autonomous
        then List.iter (Codec.finish_autonomous t.codec) ~f:(dispatch t)
        else Adapter.Driver.watch_interrupt t.driver)
    ;;

    let reply_approval t ~request_id reply =
      Option.iter
        (Codec.reply_approval t.codec ~request_id reply)
        ~f:(Adapter.Driver.write t.driver)
    ;;

    (* Most ACP agents ask the user through prigh's [ask_user] tool; Grok
       asks its own way. *)
    let answer t ~request_id (reply : User_question.Reply.t) =
      Option.iter
        (Codec.answer t.codec ~request_id reply)
        ~f:(Adapter.Driver.write t.driver)
    ;;

    let rollback = Adapter.Rollback.Unsupported
    let fork_native = false

    let revert _ ~before_turn_id:_ =
      Or_error.errorf "%s cannot roll its conversation back" agent.display_name
    ;;

    let mark _ = None
    let thread_id t = Codec.session_id t.codec
    let cost_total t = Codec.cost_total t.codec
    let in_turn t = Adapter.Driver.in_turn t.driver
    let has_open_work t = Codec.has_open_work t.codec
    let alive t = Adapter.Driver.alive t.driver
    let await_exit t = Adapter.Driver.await_exit t.driver
    let login t = Option.map t.login ~f:Antigravity_login.credential

    let renewal t : Adapter.Login_renewal.t option =
      match t.login with
      | None -> None
      | Some login when t.refused ->
        Some (Refused (Antigravity_login.token login))
      | Some login ->
        Option.some_if
          (Antigravity_login.expiring login ~now_ms:(Credential.now_ms ()))
          Adapter.Login_renewal.Expiring
    ;;

    let stop t = Adapter.Driver.stop t.driver
  end)
;;
