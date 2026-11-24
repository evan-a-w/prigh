open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A Codex or Claude Code session's models and usage limits come from the
   harness's own login (its [model/list] and [account/rateLimits/read], its
   [initialize] and [get_usage]), asked by running it briefly before the
   session's first turn: never from prigh's provider logins, which these
   tests have none of. The harnesses are scripted peers. *)

let member path j =
  List.fold path ~init:(Some j) ~f:(fun j key ->
    Option.bind j ~f:(Json.member key))
;;

let string_at path j = Option.bind (member path j) ~f:Json.string

let read (p : Line_transport.Peer.t) =
  Option.map (Line_transport.Peer.read_line p) ~f:(fun line ->
    Json.parse line |> Or_error.ok_exn)
;;

let write (p : Line_transport.Peer.t) j =
  Line_transport.Peer.write_line p (Json.to_string j)
;;

(* Codex logged in to ChatGPT: two models, its rate limits. *)
let fake_codex ~logged_in (p : Line_transport.Peer.t) =
  let reply j result =
    write
      p
      (`Object [ "id", Option.value_exn (Json.member "id" j); "result", result ])
  in
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      let meth = Option.value (string_at [ "method" ] j) ~default:"-" in
      if Option.is_some (Json.member "id" j) then printf "codex got %s\n" meth;
      (match meth with
       | "initialize" ->
         reply j (`Object [ "userAgent", `String "codex/0.166.0" ])
       | "model/list" ->
         let effort e = `Object [ "reasoningEffort", `String e ] in
         reply
           j
           (`Object
               [ ( "data"
                 , `Array
                     [ `Object
                         [ "model", `String "gpt-6.2-sol"
                         ; "displayName", `String "GPT-6.2-Sol"
                         ; "isDefault", `True
                         ; ( "supportedReasoningEfforts"
                           , `Array
                               [ effort "low"; effort "medium"; effort "high" ]
                           )
                         ]
                     ; `Object
                         [ "model", `String "gpt-6.2-mini"
                         ; "displayName", `String "GPT-6.2-Mini"
                         ; ( "supportedReasoningEfforts"
                           , `Array [ effort "low"; effort "xhigh" ] )
                         ]
                     ] )
               ; "nextCursor", `Null
               ])
       | "account/rateLimits/read" when not logged_in ->
         write
           p
           (`Object
               [ "id", Option.value_exn (Json.member "id" j)
               ; ( "error"
                 , `Object
                     [ "code", `Number "-32600"
                     ; ( "message"
                       , `String
                           "codex account authentication required to read rate \
                            limits" )
                     ] )
               ])
       | "account/rateLimits/read" ->
         reply
           j
           (`Object
               [ ( "rateLimits"
                 , `Object
                     [ ( "primary"
                       , `Object
                           [ "usedPercent", `Number "42"
                           ; "windowDurationMins", `Number "300"
                           ; "resetsAt", `Number "1900000000"
                           ] )
                     ; ( "secondary"
                       , `Object
                           [ "usedPercent", `Number "7"
                           ; "windowDurationMins", `Number "10080"
                           ; "resetsAt", `Number "1900500000"
                           ] )
                     ; "planType", `String "pro"
                     ] )
               ])
       | _ -> ());
      loop ()
  in
  loop ()
;;

(* Claude Code logged in to Claude.ai: its models at [initialize], its plan
   limits from [get_usage]. *)
let fake_claude (p : Line_transport.Peer.t) =
  let rec loop () =
    match read p with
    | None -> ()
    | Some j ->
      (match string_at [ "type" ] j, string_at [ "request"; "subtype" ] j with
       | Some "control_request", Some subtype ->
         printf "claude got %s\n" subtype;
         let response =
           match subtype with
           | "initialize" ->
             `Object
               [ ( "models"
                 , `Array
                     [ `Object
                         [ "value", `String "default"
                         ; "displayName", `String "Default (Opus 5.5)"
                         ]
                     ; `Object
                         [ "value", `String "sonnet"
                         ; "displayName", `String "Sonnet 5.5"
                         ]
                     ] )
               ]
           | "get_usage" ->
             `Object
               [ "subscription_type", `String "max"
               ; "rate_limits_available", `True
               ; ( "rate_limits"
                 , `Object
                     [ ( "five_hour"
                       , `Object
                           [ "utilization", `Number "12"
                           ; "resets_at", `String "2030-03-17T13:46:40Z"
                           ] )
                     ] )
               ]
           | _ -> `Object []
         in
         write
           p
           (`Object
               [ "type", `String "control_response"
               ; ( "response"
                 , `Object
                     [ "subtype", `String "success"
                     ; ( "request_id"
                       , `String
                           (Option.value
                              (string_at [ "request_id" ] j)
                              ~default:"") )
                     ; "response", response
                     ] )
               ])
       | _ -> ());
      loop ()
  in
  loop ()
;;

let composer =
  Or_error.ok_exn
    (Engines.create
       ~engines:[ Harness; Composer ]
       ~adapters:[ Codex; Claude ]
       ())
;;

let with_server ?(logged_in = true) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let provider = Faux_provider.create [] in
  let latest = ref None in
  let new_agent ?session ~cwd () =
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:Tools.interaction
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:true
        ~cwd
        ()
    in
    latest := Some agent;
    agent
  in
  let spawn_external _agent (launch : Adapter.Launch.t) =
    printf "spawn %s\n" (List.hd_exn launch.argv);
    Ok
      (Line_transport.Peer.spawn
         ~sw
         (match launch.argv with
          | "codex" :: _ -> fake_codex ~logged_in
          | _ -> fake_claude))
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:t.dir
      ~engines:composer
      ~spawn_external
      ~new_agent
      ()
  in
  let changed, note_changed = Eio.Promise.create () in
  let note_changed = ref (Some note_changed) in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      match string_at [ "event" ] json with
      | Some "models_changed" ->
        print_endline "event: models_changed";
        Option.iter !note_changed ~f:(fun u ->
          note_changed := None;
          Eio.Promise.resolve u ())
      | _ -> ())
  in
  let call ?(params = "{}") meth =
    match
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    with
    | `Object fields ->
      (match List.Assoc.find fields "result" ~equal:String.equal with
       | Some result -> result
       | None ->
         raise_s [%message "failed" meth (Json.to_string (`Object fields))])
    | _ -> assert false
  in
  let models () =
    match call "list_models" with
    | `Array models ->
      List.iter models ~f:(fun m ->
        printf
          "  %s %S thinking=[%s]\n"
          (Option.value_exn (string_at [ "key" ] m))
          (Option.value_exn (string_at [ "name" ] m))
          (match member [ "thinking_options" ] m with
           | Some (`Array l) ->
             String.concat
               ~sep:" "
               (List.filter_map l ~f:(fun o ->
                  match o with
                  | `String s -> Some s
                  | o -> Option.bind (Json.member "level" o) ~f:Json.string))
           | _ -> "?"))
    | _ -> print_endline "models: ?"
  in
  let chip () =
    let (state : Agent.State.t) = Agent.state (Option.value_exn !latest) in
    printf "chip: %s (%s)\n" (Model.key state.model) state.model.name
  in
  let usage () =
    match member [ "providers" ] (call "usage_limits") with
    | Some (`Array providers) ->
      List.iter providers ~f:(fun p ->
        printf
          "usage %s %S %s%s\n"
          (Option.value_exn (string_at [ "provider" ] p))
          (Option.value_exn (string_at [ "name" ] p))
          (Option.value_exn (string_at [ "status" ] p))
          (match member [ "windows" ] p with
           | Some (`Array windows) ->
             String.concat
               (List.map windows ~f:(fun w ->
                  sprintf
                    " %s=%s%%"
                    (Option.value_exn (string_at [ "id" ] w))
                    (match member [ "used_percent" ] w with
                     | Some (`Number n) -> n
                     | _ -> "?")))
           | _ ->
             Option.value_map
               (string_at [ "error" ] p)
               ~default:""
               ~f:(sprintf " %S")))
    | _ -> print_endline "usage: ?"
  in
  f ~call ~models ~chip ~usage ~changed;
  Rpc_server.shutdown server
;;

let%expect_test "codex: the picker lists Codex's models before the first turn" =
  with_server
  @@ fun ~call ~models ~chip ~usage ~changed ->
  ignore (call ~params:{|{"adapter": "codex"}|} "new_session" : Json.t);
  chip ();
  print_endline "list_models, before Codex said:";
  models ();
  Eio.Promise.await changed;
  print_endline "list_models, after:";
  models ();
  chip ();
  [%expect
    {|
    chip: codex/default (Default)
    list_models, before Codex said:
    spawn codex
      codex/default "Default" thinking=[default low medium high]
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
    list_models, after:
      codex/default "Default (GPT-6.2-Sol)" thinking=[default low medium high]
      codex/gpt-6.2-sol "GPT-6.2-Sol" thinking=[default low medium high]
      codex/gpt-6.2-mini "GPT-6.2-Mini" thinking=[default low xhigh]
    chip: codex/default (Default (GPT-6.2-Sol))
    |}];
  ignore (call ~params:{|{"model": "gpt-6.2-mini"}|} "set_model" : Json.t);
  chip ();
  (* Kept: no second run of Codex. *)
  usage ();
  [%expect
    {|
    chip: codex/gpt-6.2-mini (GPT-6.2-Mini)
    usage codex "Codex" limits primary=42% secondary=7%
    |}]
;;

let%expect_test "claude: its models and plan limits from its own login" =
  with_server
  @@ fun ~call ~models ~chip ~usage ~changed ->
  ignore (call ~params:{|{"adapter": "claude"}|} "new_session" : Json.t);
  usage ();
  Eio.Promise.await changed;
  models ();
  chip ();
  [%expect
    {|
    spawn claude
    claude got initialize
    claude got get_usage
    event: models_changed
    usage claude "Claude Code" limits five_hour=12%
      claude/default "Default (Opus 5.5)" thinking=[off default low medium high max]
      claude/sonnet "Sonnet 5.5" thinking=[off default low medium high max]
    chip: claude/default (Default (Opus 5.5))
    |}]
;;

let%expect_test
    "codex logged out: its models still, and what it said of its limits"
  =
  with_server ~logged_in:false
  @@ fun ~call ~models:_ ~chip:_ ~usage ~changed:_ ->
  ignore (call ~params:{|{"adapter": "codex"}|} "new_session" : Json.t);
  usage ();
  [%expect
    {|
    spawn codex
    codex got initialize
    codex got model/list
    codex got account/rateLimits/read
    event: models_changed
    usage codex "Codex" error "Codex: codex account authentication required to read rate limits (codex login, on the session's tool host)"
    |}]
;;
