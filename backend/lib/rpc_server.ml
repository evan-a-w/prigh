open! Core
open! Import

let methods =
  [ "ping"
  ; "prompt"
  ; "steer"
  ; "follow_up"
  ; "abort"
  ; "get_state"
  ; "get_messages"
  ; "get_entries"
  ; "set_model"
  ; "set_thinking"
  ; "list_models"
  ; "compact"
  ; "new_session"
  ; "switch_session"
  ; "list_sessions"
  ; "fork"
  ; "rewind"
  ; "auth_status"
  ; "login"
  ; "auth_respond"
  ; "auth_cancel"
  ; "logout"
  ]
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_param params name =
  match param params name with
  | Some (`String s) -> Ok s
  | Some _ -> Or_error.errorf "param %S must be a string" name
  | None -> Or_error.errorf "missing param %S" name
;;

let ok json = Ok json
let empty = ok (`Object [])
let unit_result r = Or_error.map r ~f:(fun () -> `Object [])

let provider_param params =
  Or_error.bind (string_param params "provider") ~f:(fun s ->
    match Provider_id.of_string s with
    | Some p -> Ok p
    | None ->
      Or_error.errorf
        "unknown provider %S (one of: %s)"
        s
        (String.concat
           ~sep:", "
           (List.map Provider_id.all ~f:Provider_id.to_string)))
;;

let dispatch agent login ~meth ~params : Json.t Or_error.t =
  match meth with
  | "ping" -> ok (`String "pong")
  | "prompt" ->
    Or_error.bind (string_param params "text") ~f:(fun text ->
      unit_result (Agent.prompt agent text))
  | "steer" ->
    Or_error.map (string_param params "text") ~f:(fun text ->
      Agent.steer agent text;
      `Object [])
  | "follow_up" ->
    Or_error.map (string_param params "text") ~f:(fun text ->
      Agent.follow_up agent text;
      `Object [])
  | "abort" ->
    let restored = Agent.abort agent in
    ok
      (`Object
          [ "restored", `Array (List.map restored ~f:(fun s -> `String s)) ])
  | "get_state" -> ok (Rpc_json.state (Agent.state agent))
  | "get_messages" ->
    ok (`Array (List.map (Agent.messages agent) ~f:Rpc_json.message))
  | "get_entries" ->
    ok
      (`Array
          (List.map
             (Session.active_path (Agent.session agent))
             ~f:Rpc_json.entry))
  | "set_model" ->
    Or_error.bind (string_param params "model") ~f:(fun id ->
      Or_error.map (Model.resolve id) ~f:(fun model ->
        Agent.set_model agent model;
        `Object []))
  | "set_thinking" ->
    Or_error.bind (string_param params "thinking") ~f:(fun s ->
      Or_error.map (Rpc_json.thinking_of_string s) ~f:(fun thinking ->
        Agent.set_thinking agent thinking;
        `Object []))
  | "list_models" -> ok (`Array (List.map Model.all ~f:Rpc_json.model))
  | "compact" ->
    Or_error.map (Agent.compact agent) ~f:(fun summary ->
      `Object [ "summary", `String summary ])
  | "new_session" ->
    Agent.new_session agent;
    empty
  | "switch_session" ->
    Or_error.bind (string_param params "path") ~f:(fun path ->
      unit_result (Agent.switch_session agent ~path))
  | "list_sessions" ->
    let dir = Filename.dirname (Agent.state agent).session_path in
    ok (`Array (List.map (Session.list ~dir) ~f:Rpc_json.session_summary))
  | "fork" ->
    let at =
      match param params "at" with
      | Some (`String s) -> Some s
      | _ -> None
    in
    unit_result (Agent.fork agent ?at ())
  | "rewind" ->
    Or_error.bind (string_param params "to") ~f:(fun to_ ->
      unit_result (Agent.rewind agent ~to_))
  | "auth_status" ->
    Or_error.map (Login_manager.status login) ~f:(fun statuses ->
      `Array (List.map statuses ~f:Rpc_json.auth_status))
  | "login" ->
    Or_error.bind (provider_param params) ~f:(fun provider ->
      let method_ =
        match param params "method" with
        | Some (`String s) ->
          (match Provider_auth.Method.of_string s with
           | Some m -> Ok m
           | None -> Or_error.errorf "unknown login method %S" s)
        | _ -> Ok (List.hd_exn (Provider_auth.methods provider))
      in
      Or_error.bind method_ ~f:(fun method_ ->
        unit_result (Login_manager.start login provider method_)))
  | "auth_respond" ->
    Or_error.bind (string_param params "id") ~f:(fun id ->
      Or_error.bind (string_param params "value") ~f:(fun value ->
        unit_result (Login_manager.respond login ~id value)))
  | "auth_cancel" ->
    Login_manager.cancel login;
    empty
  | "logout" ->
    Or_error.bind (provider_param params) ~f:(fun provider ->
      unit_result (Login_manager.logout login provider))
  | _ -> Or_error.errorf "unknown method %S" meth
;;

let handle agent login (request : Json.t) : Json.t =
  let id = Option.value (param request "id") ~default:`Null in
  let response =
    match param request "method" with
    | Some (`String meth) ->
      let params =
        Option.value (param request "params") ~default:(`Object [])
      in
      (match dispatch agent login ~meth ~params with
       | result -> result
       | exception exn ->
         Or_error.error_s [%message "internal error" (exn : exn)])
    | _ -> Or_error.error_string "request must have a string \"method\""
  in
  match response with
  | Ok result ->
    `Object
      [ "type", `String "response"; "id", id; "ok", `True; "result", result ]
  | Error e ->
    `Object
      [ "type", `String "response"
      ; "id", id
      ; "ok", `False
      ; "error", `String (Error.to_string_hum e)
      ]
;;

let run ~env:_ ~agent ~login ~input ~output =
  Switch.run
  @@ fun sw ->
  let outbox : string option Eio.Stream.t = Eio.Stream.create 1024 in
  let send json = Eio.Stream.add outbox (Some (Json.to_string json)) in
  Fiber.fork ~sw (fun () ->
    let rec loop () =
      match Eio.Stream.take outbox with
      | None -> ()
      | Some line ->
        Eio.Flow.copy_string (line ^ "\n") output;
        loop ()
    in
    loop ());
  Agent.subscribe agent ~f:(fun event -> send (Rpc_json.event event));
  Login_manager.subscribe login ~f:(fun event ->
    send (Rpc_json.login_event event));
  let reader = Eio.Buf_read.of_flow input ~max_size:(64 * 1024 * 1024) in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | exception End_of_file -> ()
    | line ->
      if not (String.is_empty (String.strip line))
      then (
        match Json.parse line with
        | Error e ->
          send
            (`Object
                [ "type", `String "response"
                ; "id", `Null
                ; "ok", `False
                ; "error", `String ("invalid JSON: " ^ Error.to_string_hum e)
                ])
        | Ok request -> send (handle agent login request));
      loop ()
  in
  loop ();
  ignore (Agent.abort agent : string list);
  Login_manager.cancel login;
  Agent.wait_idle agent;
  Login_manager.wait login;
  Eio.Stream.add outbox None
;;
