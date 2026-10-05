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
       (Re.str (sprintf "(name %s)" (Core_unix.gethostname ())))
       "(name <host>)"
  |> sub
       (Re.seq
          [ Re.repn Re.digit 8 (Some 8)
          ; Re.char '-'
          ; Re.repn Re.digit 9 (Some 9)
          ])
       "<stamp>"
  |> sub (Re.repn (Re.alt [ Re.digit; Re.rg 'a' 'f' ]) 16 (Some 16)) "<id>"
;;

(* Normalised before the layout, so that the real paths' lengths do not
   change where lines wrap. *)
let show label sexp =
  let rec go : Sexp.t -> Sexp.t = function
    | List [ Atom "duration_seconds"; Atom _ ] ->
      List [ Atom "duration_seconds"; Atom "<t>" ]
    | List [ Atom "name"; Atom host ]
      when String.equal host (Core_unix.gethostname ()) ->
      List [ Atom "name"; Atom "<host>" ]
    | Atom s -> Atom (normalise s)
    | List l -> List (List.map l ~f:go)
  in
  printf "%s: %s\n" label (Sexp.to_string_hum (go sexp))
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
  | Message_start (User { text = t; _ }) -> Some (sprintf "user %S" t)
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
  | Tool_exec { name; _ } -> Some (sprintf "tool_exec %s" name)
  | Tool_exec_cancel id -> Some (sprintf "tool_exec_cancel %s" id)
  | Terminal_open { term_id; _ } -> Some (sprintf "terminal_open %s" term_id)
  | Terminal_frame _ -> None
  | Terminal_close id -> Some (sprintf "terminal_close %s" id)
  | Btw_delta { btw_id; delta } -> Some (sprintf "btw_delta %s %S" btw_id delta)
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

(* Background subagents run concurrently with the main agent, so the two event
   streams are printed separately (each is deterministic on its own), and state
   events, whose position depends on timing, are left out. Stops once a
   delivered report has been answered and nothing is left. *)
(* Until the job's report has been delivered and the agent is idle again. *)
let drain_jobs client =
  let delivered = ref false in
  let last_state = ref "" in
  let rec go () =
    match%bind Pipe.read (Client.incoming client) with
    | `Eof -> return ()
    | `Ok (Event (State s)) ->
      let jobs =
        String.concat
          ~sep:" "
          (List.map s.jobs ~f:(fun (j : State.Job.t) ->
             sprintf "%s:%s" j.id (Option.value j.exit ~default:"running")))
      in
      let line = sprintf "  event state running=%b jobs=[%s]" s.running jobs in
      if not (String.equal line !last_state) then print_endline line;
      last_state := line;
      if !delivered && (not s.running) && List.is_empty s.jobs
      then return ()
      else go ()
    | `Ok (Event e) ->
      (match e with
       | Message_start (User { text = t; _ })
         when String.is_prefix t ~prefix:"[job " -> delivered := true
       | _ -> ());
      Option.iter (summarise e) ~f:(fun line ->
        printf "  event %s\n" (normalise line));
      go ()
    | `Ok other ->
      print_s [%sexp (other : Client.Incoming.t)];
      go ()
  in
  go ()
;;

let drain_background client =
  let subagent_lines = Queue.create () in
  let delivered = ref false in
  let rec go () =
    match%bind Pipe.read (Client.incoming client) with
    | `Eof -> return ()
    | `Ok (Event (State s)) ->
      if !delivered && (not s.running) && List.is_empty s.subagents
      then (
        printf "  event state running=false subagents=[]\n";
        return ())
      else go ()
    | `Ok (Event e) ->
      (match e with
       | Message_start (User { text = t; _ })
         when String.is_prefix t ~prefix:"[subagent " -> delivered := true
       | _ -> ());
      (match e, summarise e with
       | (Subagent_start _ | Subagent _ | Subagent_end _), Some line ->
         Queue.enqueue subagent_lines line
       | _, Some line -> printf "  event %s\n" (normalise line)
       | _, None -> ());
      go ()
    | `Ok other ->
      print_s [%sexp (other : Client.Incoming.t)];
      go ()
  in
  let%map () = go () in
  print_endline "  subagent events:";
  Queue.iter subagent_lines ~f:(fun line ->
    printf "  event %s\n" (normalise line))
;;

(* A missing delivery shows up as a diff rather than a hung test. *)
let with_deadline what d =
  match%map Clock.with_timeout (Time_float.Span.of_sec 60.) d with
  | `Result () -> ()
  | `Timeout -> printf "%s: timed out\n" what
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
  let spawn args () =
    Prigh_client_unix.Stdio_transport.spawn
      ~env:(`Extend [ "HOME", tmp ])
      ~prog:(backend ())
      ~args
      ()
  in
  let client = Client.create ~connect:(spawn args) in
  match%bind Client.connect client with
  | Error e ->
    print_s [%message "cannot start backend" (e : Error.t)];
    return ()
  | Ok () ->
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
        printf "<- list_models: first %s\n" (List.hd_exn models).key
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
    let%bind () = Client.close client in
    print_endline "backend exited";
    let script = Filename.concat tmp "subagent.json" in
    let script_json =
      {|[
  {"text":"delegating","tool_calls":[{"id":"s1","name":"subagent","arguments":{"task":"say hi","tools":["read"]}}]},
  {"text":"child says hi"},
  {"text":"parent done"},
  {"text":"got the report"}
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
      let client = Client.create ~connect:(spawn subagent_args) in
      match%bind Client.connect client with
      | Error e ->
        print_s [%message "cannot start subagent backend" (e : Error.t)];
        return ()
      | Ok () ->
        let%bind () = call client "ping" [] in
        let%bind () =
          call client "prompt" [ "text", Json.str "delegate something" ]
        in
        let%bind () = with_deadline "subagents" (drain_background client) in
        let%bind () = Client.close client in
        print_endline "subagent backend exited";
        return ()
    in
    let jobs_script = Filename.concat tmp "jobs.json" in
    Out_channel.write_all
      jobs_script
      ~data:
        {|[
  {"text":"building","tool_calls":[{"id":"j1","name":"bash","arguments":{"command":"sleep 0.5; echo built","background":true}}]},
  {"text":"started the build"},
  {"text":"the build passed"}
]|};
    let%bind () =
      let client =
        Client.create
          ~connect:
            (spawn
               [ "serve"
               ; "-faux-script"
               ; jobs_script
               ; "-auth-file"
               ; Filename.concat tmp "auth4.json"
               ; "-cwd"
               ; tmp
               ; "-model"
               ; "deepseek/deepseek-flash"
               ])
      in
      match%bind Client.connect client with
      | Error e ->
        print_s [%message "cannot start jobs backend" (e : Error.t)];
        return ()
      | Ok () ->
        let%bind () = call client "prompt" [ "text", Json.str "build it" ] in
        let%bind () = with_deadline "jobs" (drain_jobs client) in
        let%bind () =
          match%map
            Client.call
              client
              "job_output"
              [ "job_id", Json.str "j1"; "lines", Json.int 5 ]
          with
          | Ok json ->
            printf
              "<- job_output: %s\n"
              (Re.replace_string
                 (Re.Perl.compile_pat {|after \d+s|})
                 ~by:"after Ns"
                 (Json.to_string json))
          | Error e -> print_s [%message "job_output" (e : Error.t)]
        in
        let%bind () = Client.close client in
        print_endline "jobs backend exited";
        return ()
    in
    let btw_script = Filename.concat tmp "btw.json" in
    Out_channel.write_all
      btw_script
      ~data:
        {|[
  {"text":"sleeping","tool_calls":[{"id":"b1","name":"bash","arguments":{"command":"sleep 1"}}]},
  {"text":"You asked me to sleep.","chunks":2},
  {"text":"slept"}
]|};
    let btw_args =
      [ "serve"
      ; "-faux-script"
      ; btw_script
      ; "-auth-file"
      ; Filename.concat tmp "auth3.json"
      ; "-cwd"
      ; tmp
      ; "-model"
      ; "deepseek/deepseek-flash"
      ]
    in
    let client = Client.create ~connect:(spawn btw_args) in
    let%bind () =
      match%bind Client.connect client with
     | Error e ->
        print_s [%message "cannot start btw backend" (e : Error.t)];
        return ()
      | Ok () ->
        let%bind () = call client "prompt" [ "text", Json.str "sleep a bit" ] in
        let%bind () =
          drain client ~stop:(function
            | Tool_start _ -> true
            | _ -> false)
        in
        let%bind () =
          call
            client
            "btw"
            [ "question", Json.str "what did I ask?"
            ; "btw_id", Json.str "e2e-1"
            ]
        in
        let%bind () =
          drain client ~stop:(function
            | State { running = false; _ } -> true
            | _ -> false)
        in
        let%bind () =
          match%map Client.call client "get_messages" [] with
          | Ok (`Array messages) ->
            printf "<- get_messages: %d message(s)\n" (List.length messages)
          | Ok _ | Error _ -> print_endline "<- get_messages: unexpected"
        in
        let%bind () = Client.close client in
        print_endline "btw backend exited";
       return ()
    in
    (* Images: a prompt's own, an attached image file, and [read] on one;
       the model sees them and the messages carry them back. *)
    Out_channel.write_all
      (Filename.concat tmp "shot.png")
      ~data:
        "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\x00\x03\x00\x00\x00\x02\x01\x03\x00\x00\x00\xa7\xba\xf4\x59\x00\x00\x00\x03\x50\x4c\x54\x45\xff\x00\x00\x19\xe2\x09\x37\x00\x00\x00\x0c\x49\x44\x41\x54\x08\xd7\x63\x60\x60\x60\x00\x00\x00\x04\x00\x01\x27\x34\x27\x0a\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82";
    let images_script = Filename.concat tmp "images.json" in
    Out_channel.write_all
      images_script
      ~data:
        {|[
  {"text":"reading","tool_calls":[{"id":"i1","name":"read","arguments":{"path":"shot.png"}}]},
  {"text":"a red square"}
]|};
    let client =
      Client.create
        ~connect:
          (spawn
             [ "serve"
             ; "-faux-script"
             ; images_script
             ; "-auth-file"
             ; Filename.concat tmp "auth5.json"
             ; "-cwd"
             ; tmp
             ; "-model"
             ; "deepseek/deepseek-flash"
             ])
    in
    match%bind Client.connect client with
    | Error e ->
      print_s [%message "cannot start images backend" (e : Error.t)];
      return ()
    | Ok () ->
      let%bind () =
        call
          client
          "prompt"
          [ "text", Json.str "what is in @shot.png?"
          ; "attachments", `Array [ Json.str "shot.png" ]
          ; ( "images"
            , `Array
                [ `Object
                    [ "mime_type", Json.str "image/gif"
                    ; ( "data"
                      , Json.str
                          "R0lGODlhBQAEAPAAAAAA/wAAACH5BAAAAAAALAAAAAAFAAQAAAIEhI+ZBQA7"
                      )
                    ]
                ] )
          ]
      in
      let%bind () =
        drain client ~stop:(function
          | State { running = false; _ } -> true
          | _ -> false)
      in
      let%bind () =
        match%map Client.call client "get_messages" [] with
        | Ok (`Array messages) ->
          List.iter messages ~f:(fun json ->
            match Message.of_json json with
            | Ok (User u) -> show "<- user" [%sexp (u : Message.User.t)]
            | Ok (Tool_result r) ->
              show "<- tool_result" [%sexp (r : Message.Tool_result.t)]
            | Ok (Assistant _) -> ()
            | Error e -> show "<- message ERROR" [%sexp (e : Error.t)])
        | Ok _ | Error _ -> print_endline "<- get_messages: unexpected"
      in
      let%bind () = Client.close client in
      print_endline "images backend exited";
      return ()
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"prigh frontend e2e against the faux backend"
       (Command.Param.return main))
;;
