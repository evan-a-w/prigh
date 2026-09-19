open! Core
open! Async
open Prigh_protocol
module Client = Prigh_client.Client

let rec find_root dir =
  if Sys_unix.file_exists_exn (Filename.concat dir "backend/dune-project")
  then Some dir
  else if String.equal dir "/"
  then None
  else find_root (Filename.dirname dir)
;;

let backend () =
  match Sys.getenv "PRIGH_BACKEND" with
  | Some p -> p
  | None ->
    (match find_root (Core_unix.getcwd ()) with
     | Some root -> Filename.concat root "backend/_build/default/bin/main.exe"
     | None -> "prigh")
;;

let tmp_dir = ref ""

let normalise s =
  let sub re by s = Re.replace_string (Re.compile re) ~by s in
  s
  |> sub
       (Re.seq
          [ Re.str "duration_seconds"
          ; Re.rep1 Re.space
          ; Re.rep1
              (Re.alt
                 [ Re.digit
                 ; Re.char 'e'
                 ; Re.char 'E'
                 ; Re.char '.'
                 ; Re.char '+'
                 ; Re.char '-'
                 ])
          ])
       "duration_seconds <t>"
  |> sub (Re.str !tmp_dir) "$TMP"
  |> sub
       (Re.seq
          [ Re.repn Re.digit 8 (Some 8)
          ; Re.char '-'
          ; Re.repn Re.digit 9 (Some 9)
          ])
       "<stamp>"
  |> sub (Re.repn (Re.alt [ Re.digit; Re.rg 'a' 'f' ]) 16 (Some 16)) "<id>"
;;

let show label sexp =
  printf "%s: %s\n" label (normalise (Sexp.to_string_hum sexp))
;;

let call client method_ params =
  match%map Client.call client method_ params with
  | Ok json -> show ("<- " ^ method_) [%sexp (json : Json.t)]
  | Error e ->
    show ("<- " ^ method_ ^ " ERROR") [%sexp (Error.to_string_hum e : string)]
;;

(* Reads events until [stop] accepts one. *)
let rec summarise (e : Event.t) : string option =
  match e with
  | Message_update { delta = Text_delta t; _ } ->
    Some (sprintf "text_delta %S" t)
  | Message_update _ | Agent_start | Agent_end _ | Turn_start | Turn_end _ ->
    None
  | Message_start (User t) -> Some (sprintf "user %S" t)
  | Message_start _ -> Some "message_start"
  | Message_end (Assistant a) ->
    Some
      (sprintf
         "assistant_end %s"
         (Sexp.to_string [%sexp (a.stop_reason : Stop_reason.t)]))
  | Message_end _ -> Some "message_end"
  | State s -> Some (sprintf "state running=%b model=%s" s.running s.model.key)
  | Notice t -> Some (sprintf "notice %S" t)
  | Auth a ->
    Some (sprintf "auth %s" (Sexp.to_string [%sexp (a : Auth_event.t)]))
  | Tool_start c -> Some (sprintf "tool_start %s" c.name)
  | Tool_output _ -> None
  | Tool_end { result; _ } -> Some (sprintf "tool_end %s" result.tool_name)
  | Tool_confirm _ -> None
  | Compacted _ -> Some "compacted"
  | Config_changed _ -> None
  | Queue_update _ -> None
  | Subagent_start { agent_id; task; model; tools; _ } ->
    Some
      (sprintf
         "subagent_start %s model=%s task=%S tools=[%s]"
         agent_id
         model
         task
         (String.concat ~sep:"," tools))
  | Subagent { agent_id; event; _ } ->
    Option.map (summarise event) ~f:(fun s ->
      sprintf "subagent %s: %s" agent_id s)
  | Subagent_end { agent_id; turns; cost_usd; result; _ } ->
    Some
      (sprintf
         "subagent_end %s turns=%d cost=%.4f is_error=%b text=%S"
         agent_id
         turns
         cost_usd
         result.is_error
         result.text)
;;

let rec drain client ~stop =
  match%bind Pipe.read (Client.incoming client) with
  | `Eof -> return ()
  | `Ok (Event e) ->
    Option.iter (summarise e) ~f:(fun s -> printf "  event %s\n" (normalise s));
    if stop e then return () else drain client ~stop
  | `Ok other ->
    print_s [%sexp (other : Client.Incoming.t)];
    drain client ~stop
;;

let main () =
  let tmp = Filename_unix.temp_dir "prigh-e2e" "" in
  tmp_dir := tmp;
  let auth_file = Filename.concat tmp "auth.json" in
  let args =
    [ "serve"
    ; "-faux"
    ; "-auth-file"
    ; auth_file
    ; "-cwd"
    ; tmp
    ; "-model"
    ; "deepseek/deepseek-flash"
    ]
  in
  match%bind
    Prigh_client.Stdio_transport.spawn
      ~env:(`Extend [ "HOME", tmp ])
      ~prog:(backend ())
      ~args
      ()
  with
  | Error e ->
    print_s [%message "cannot start backend" (e : Error.t)];
    return ()
  | Ok transport ->
    let client = Client.create transport in
    let%bind () = call client "ping" [] in
    let%bind () = call client "get_state" [] in
    let%bind () = call client "prompt" [ "text", Json.str "hello there" ] in
    let%bind () =
      drain client ~stop:(function
        | State { running = false; _ } -> true
        | _ -> false)
    in
    let%bind () =
      call client "set_model" [ "model", Json.str "Claude Fable 5.1" ]
    in
    let%bind () =
      drain client ~stop:(function
        | State _ -> true
        | _ -> false)
    in
    let%bind () = call client "set_model" [ "model", Json.str "gpt-9" ] in
    let%bind () = call client "set_thinking" [ "thinking", Json.str "high" ] in
    let%bind () =
      drain client ~stop:(function
        | State _ -> true
        | _ -> false)
    in
    let%bind () =
      match%map Client.list_models client with
      | Ok models ->
        printf
          "<- list_models: %d models, first %s\n"
          (List.length models)
          (List.hd_exn models).key
      | Error e -> print_s [%message "list_models" (e : Error.t)]
    in
    let%bind () = call client "auth_status" [] in
    let%bind () =
      call
        client
        "login"
        [ "provider", Json.str "deepseek"; "method", Json.str "api_key" ]
    in
    let%bind () =
      drain client ~stop:(function
        | Auth (Prompt _) -> true
        | _ -> false)
    in
    let%bind () =
      call
        client
        "auth_respond"
        [ "id", Json.str "p1"; "value", Json.str "sk-test-key" ]
    in
    let%bind () =
      drain client ~stop:(function
        | Auth (Done _ | Failed _) -> true
        | _ -> false)
    in
    let%bind () = call client "auth_status" [] in
    let%bind () = call client "logout" [ "provider", Json.str "deepseek" ] in
    let%bind () =
      drain client ~stop:(function
        | Auth (Logged_out _) -> true
        | _ -> false)
    in
    let%bind () =
      match%map Client.list_sessions client with
      | Ok sessions ->
        printf
          "<- list_sessions: %d session(s), %d message(s)\n"
          (List.length sessions)
          (List.hd_exn sessions).message_count
      | Error e -> print_s [%message "list_sessions" (e : Error.t)]
    in
    let%bind () =
      call client "set_session_name" [ "name", Json.str "e2e session" ]
    in
    let%bind () = call client "get_state" [] in
    let%bind () = call client "session_stats" [] in
    let%bind () = call client "get_entries" [] in
    let%bind () = call client "bogus" [] in
    Client.close client;
    let%bind () = Client.closed client in
    print_endline "backend exited";
    let script = Filename.concat tmp "subagent.json" in
    let script_json =
      {|[
  {"text":"delegating","tool_calls":[{"id":"s1","name":"subagent","arguments":{"task":"say hi","tools":["read"]}}]},
  {"text":"child says hi"},
  {"text":"parent done"}
]|}
    in
    Out_channel.write_all script ~data:script_json;
    let subagent_args =
      [ "serve"
      ; "-faux"
      ; "-faux-script"
      ; script
      ; "-auth-file"
      ; Filename.concat tmp "auth2.json"
      ; "-cwd"
      ; tmp
      ; "-model"
      ; "deepseek/deepseek-flash"
      ]
    in
    let%bind () =
      match%bind
        Prigh_client.Stdio_transport.spawn
          ~env:(`Extend [ "HOME", tmp ])
          ~prog:(backend ())
          ~args:subagent_args
          ()
      with
      | Error e ->
        print_s [%message "cannot start subagent backend" (e : Error.t)];
        return ()
      | Ok transport ->
        let client = Client.create transport in
        let%bind () = call client "ping" [] in
        let%bind () =
          call client "prompt" [ "text", Json.str "delegate something" ]
        in
        let%bind () =
          drain client ~stop:(function
            | State { running = false; _ } -> true
            | _ -> false)
        in
        Client.close client;
        let%bind () = Client.closed client in
        print_endline "subagent backend exited";
        return ()
    in
    return ()
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"prigh frontend e2e against the faux backend"
       (Command.Param.return main))
;;
