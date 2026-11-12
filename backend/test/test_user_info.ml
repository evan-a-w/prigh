open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let info t = User_info.create ~home:t.dir
let set info form = print_s [%sexp (User_info.set info form : unit Or_error.t)]

let form
      ?name
      ?country
      ?time_zone
      ?language
      ?units
      ?date_format
      ?occupation
      ?background
      ?anything_else
      ()
  =
  { User_info.Form.name
  ; country
  ; time_zone
  ; language
  ; units
  ; date_format
  ; occupation
  ; background
  ; anything_else
  }
;;

let file_of path =
  match In_channel.read_all path with
  | data -> data
  | exception Sys_error _ -> "none\n"
;;

let show t info =
  (match User_info.get info with
   | Ok form -> print_s [%sexp (form : User_info.Form.t)]
   | Error e -> print_s [%sexp (e : Error.t)]);
  printf
    "rendered:\n%s\nfile: %s"
    (Option.value (User_info.text info) ~default:"(none)")
    (file_of (User_info.path ~home:t.dir))
;;

let%expect_test "set, get, clear; rendering; an edit by hand counts" =
  with_sandbox
  @@ fun t ->
  let info = info t in
  User_info.subscribe info ~f:(fun () -> print_endline "(changed)");
  show t info;
  [%expect
    {|
    ((name ()) (country ()) (time_zone ()) (language ()) (units ())
     (date_format ()) (occupation ()) (background ()) (anything_else ()))
    rendered:
    (none)
    file: none
    |}];
  set
    info
    (form
       ~name:" Lloyd "
       ~country:"Australia"
       ~time_zone:"Australia/Sydney"
       ~language:"English, Australian spelling"
       ~units:Metric
       ~date_format:Dmy
       ~occupation:"student"
       ~background:"Undergrad pure maths and CS.\n\nKnows OCaml well."
       ~anything_else:"  "
       ());
  show t info;
  [%expect
    {|
    (changed)
    (Ok ())
    ((name (Lloyd)) (country (Australia)) (time_zone (Australia/Sydney))
     (language ("English, Australian spelling")) (units (Metric))
     (date_format (Dmy)) (occupation (student))
     (background ( "Undergrad pure maths and CS.\
                  \n\
                  \nKnows OCaml well."))
     (anything_else ()))
    rendered:
    - Name: Lloyd
    - Country/region: Australia
    - Time zone: Australia/Sydney
    - Language and spelling: English, Australian spelling
    - Units: metric (°C, km, kg, L)
    - Date format: day/month/year (25/12/2026)
    - Occupation/role: student
    - Background and expertise:
      Undergrad pure maths and CS.

      Knows OCaml well.
    file: {
      "version": 1,
      "name": "Lloyd",
      "country": "Australia",
      "time_zone": "Australia/Sydney",
      "language": "English, Australian spelling",
      "units": "metric",
      "date_format": "dmy",
      "occupation": "student",
      "background": "Undergrad pure maths and CS.\n\nKnows OCaml well."
    }
    |}];
  List.iter
    [ form ~units:Imperial ~date_format:Mdy ()
    ; form ~units:Us_customary ~date_format:Iso ()
    ]
    ~f:(fun f -> print_endline (Option.value_exn (User_info.Form.render f)));
  [%expect
    {|
    - Units: imperial (miles, stones and pounds, pints; °C)
    - Date format: month/day/year (12/25/2026)
    - Units: US customary (°F, miles, lb, gallons)
    - Date format: ISO 8601 (2026-12-25)
    |}];
  Out_channel.write_all
    (User_info.path ~home:t.dir)
    ~data:{|{"version": 1, "name": "Ll", "units": "us", "extra": 3}|};
  show t info;
  [%expect
    {|
    ((name (Ll)) (country ()) (time_zone ()) (language ()) (units (Us_customary))
     (date_format ()) (occupation ()) (background ()) (anything_else ()))
    rendered:
    - Name: Ll
    - Units: US customary (°F, miles, lb, gallons)
    file: {"version": 1, "name": "Ll", "units": "us", "extra": 3}
    |}];
  (* Broken by hand: an error saying what to do; nothing in context. *)
  List.iter
    [ {|{"name": 3}|}
    ; {|{"units": "furlongs"}|}
    ; "not json"
    ; {|{"version": 2}|}
    ]
    ~f:(fun data ->
      Out_channel.write_all (User_info.path ~home:t.dir) ~data;
      print_s
        [%sexp
          (Or_error.map (User_info.get info) ~f:(fun _ -> ())
           |> Result.map_error ~f:(fun e -> mask t (Error.to_string_hum e))
           : (unit, string) Result.t)];
      print_s [%sexp (User_info.text info : string option)]);
  [%expect
    {|
    (Error
     "$DIR/.prigh/about_you.json name must be a string: fix it, or move it aside")
    ()
    (Error
     "$DIR/.prigh/about_you.json units \"furlongs\" is not one of metric, imperial, us_customary: fix it, or move it aside")
    ()
    (Error
     "$DIR/.prigh/about_you.json is not valid JSON (json: unexpected string: 'not'): fix it, or move it aside")
    ()
    (Error
     "$DIR/.prigh/about_you.json is version 2, from a newer prigh (this one reads 1): fix it, or move it aside")
    ()
    |}];
  set info User_info.Form.empty;
  show t info;
  [%expect
    {|
    (changed)
    (Ok ())
    ((name ()) (country ()) (time_zone ()) (language ()) (units ())
     (date_format ()) (occupation ()) (background ()) (anything_else ()))
    rendered:
    (none)
    file: none
    |}]
;;

let%expect_test "limits: per field and in all" =
  with_sandbox
  @@ fun t ->
  let info = info t in
  let x n = String.make n 'x' in
  set info (form ~name:(x 101) ());
  set info (form ~background:(x 1001) ());
  set info (form ~anything_else:(x 4001) ());
  set info (form ~background:(x 1000) ~anything_else:(x 4000) ~name:(x 1) ());
  set info (form ~anything_else:(x 4000) ~background:(x 900) ~name:(x 100) ());
  [%expect
    {|
    (Error
     "name is limited to 100 bytes (this one has 101): shorten it, and keep what is only sometimes needed in memory notes")
    (Error
     "background is limited to 1000 bytes (this one has 1001): shorten it, and keep what is only sometimes needed in memory notes")
    (Error
     "anything_else is limited to 4000 bytes (this one has 4001): shorten it, and keep what is only sometimes needed in memory notes")
    (Error
     "about you is limited to 5000 bytes in all (this has 5001): shorten it, and keep what is only sometimes needed in memory notes")
    (Ok ())
    |}];
  let f = Or_error.ok_exn (User_info.get info) in
  print_s
    [%sexp
      (List.map
         [ "units", "Metric"
         ; "date_format", "D/M/Y"
         ; "date_format", "iso"
         ; "units", "cubits"
         ; "name", " Bob "
         ; "occupation", ""
         ; "age", "3"
         ]
         ~f:(fun (field, value) ->
           Or_error.map (User_info.Form.set_field f field value) ~f:(fun f ->
             ( f.units
             , f.date_format
             , f.name
             , Option.map f.occupation ~f:String.length )))
       : (User_info.Units.t option
         * User_info.Date_format.t option
         * string option
         * int option)
           Or_error.t
           list)];
  [%expect
    {|
    ((Ok
      ((Metric) ()
       (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
       ()))
     (Ok
      (() (Dmy)
       (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
       ()))
     (Ok
      (() (Iso)
       (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
       ()))
     (Error "units \"cubits\" is not one of metric, imperial, us_customary")
     (Ok (() () (Bob) ()))
     (Ok
      (() ()
       (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
       ()))
     (Error
      "no field \"age\": the fields are name, country, time_zone, language, units, date_format, occupation, background, anything_else"))
    |}]
;;

let%expect_test "the free text of earlier versions becomes Anything else" =
  with_sandbox
  @@ fun t ->
  let legacy = User_info.legacy_path ~home:t.dir in
  Core_unix.mkdir_p (Filename.dirname legacy);
  Out_channel.write_all legacy ~data:"\n  I'm Lloyd.\nI write OCaml.\n\n";
  let info = info t in
  show t info;
  printf
    "old: %sold, renamed: %s"
    (file_of legacy)
    (file_of (legacy ^ ".migrated"));
  [%expect
    {|
    ((name ()) (country ()) (time_zone ()) (language ()) (units ())
     (date_format ()) (occupation ()) (background ())
     (anything_else ( "I'm Lloyd.\
                     \nI write OCaml.")))
    rendered:
    - Anything else:
      I'm Lloyd.
      I write OCaml.
    file: {
      "version": 1,
      "anything_else": "I'm Lloyd.\nI write OCaml."
    }
    old: none
    old, renamed:
      I'm Lloyd.
    I write OCaml.
    |}];
  (* Once only: a new free text beside the form is ignored. *)
  Out_channel.write_all legacy ~data:"newer";
  print_s [%sexp (User_info.text info : string option)];
  [%expect
    {|
    ( "- Anything else:\
     \n  I'm Lloyd.\
     \n  I write OCaml.")
    |}];
  (* A setting made before any read keeps nothing of it either. *)
  Core_unix.unlink (User_info.path ~home:t.dir);
  Out_channel.write_all legacy ~data:"older text";
  set info (form ~name:"Lloyd" ());
  show t info;
  printf "old: %s" (file_of legacy);
  [%expect
    {|
    (Ok ())
    ((name (Lloyd)) (country ()) (time_zone ()) (language ()) (units ())
     (date_format ()) (occupation ()) (background ()) (anything_else ()))
    rendered:
    - Name: Lloyd
    file: {
      "version": 1,
      "name": "Lloyd"
    }
    old: none
    |}];
  (* Beyond the old limit by hand: cut, at a character. An empty one leaves
     no form. *)
  Core_unix.unlink (User_info.path ~home:t.dir);
  Out_channel.write_all
    legacy
    ~data:(String.make (User_info.max_field "anything_else" - 1) 'x' ^ "é more");
  printf
    "%d bytes\n"
    (String.length
       (Option.value_exn (Or_error.ok_exn (User_info.get info)).anything_else));
  Core_unix.unlink (User_info.path ~home:t.dir);
  Out_channel.write_all legacy ~data:"\n";
  show t info;
  [%expect
    {|
    3999 bytes
    ((name ()) (country ()) (time_zone ()) (language ()) (units ())
     (date_format ()) (occupation ()) (background ()) (anything_else ()))
    rendered:
    (none)
    file: none
    |}]
;;

let%expect_test "each namespace has its own" =
  with_sandbox
  @@ fun t ->
  let world name =
    Namespace.world
      { Namespace.name; token = name ^ "-token" }
      ~home:t.dir
      ~legacy_auth_file:(t.dir ^/ ".config/prigh/auth.json")
  in
  let alice = world "alice"
  and bob = world "bob"
  and default = world "default" in
  let of_world (w : Namespace.World.t) = User_info.create ~home:w.home in
  Or_error.ok_exn (User_info.set (of_world alice) (form ~name:"Alice" ()));
  Or_error.ok_exn
    (User_info.set (of_world default) (form ~name:"the default user" ()));
  List.iter [ alice; bob; default ] ~f:(fun w ->
    printf
      "%s: %s\n"
      (mask t (User_info.path ~home:w.home))
      (Option.value (User_info.text (of_world w)) ~default:"(none)"));
  [%expect
    {|
    $DIR/.prigh/namespaces/alice/.prigh/about_you.json: - Name: Alice
    $DIR/.prigh/namespaces/bob/.prigh/about_you.json: (none)
    $DIR/.prigh/about_you.json: - Name: the default user
    |}]
;;

let%expect_test "RPC: get and set; every client hears; errors say what to do" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
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
      ~cwd
      ()
  in
  (* The server's own, under its home. *)
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home:t.dir
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client name =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "user_info_changed") ->
        printf "%s hears %s\n" name (Json.to_string json)
      | _ -> ())
  in
  let a = client "a" in
  let b = client "b" in
  let call client meth params =
    let response =
      Rpc_server.handle
        server
        client
        (`Object [ "id", `Number "1"; "method", `String meth; "params", params ])
    in
    match Json.member "ok" response, Json.member "result" response with
    | Some `True, Some result -> print_endline (Json.to_string result)
    | _ ->
      print_endline
        (Json.to_string (Option.value_exn (Json.member "error" response)))
  in
  call a "hello" (`Object []);
  call b "hello" (`Object []);
  ignore ([%expect.output] : string);
  call a "get_user_info" (`Object []);
  [%expect
    {| {"form":{},"text":"","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}} |}];
  call
    a
    "set_user_info"
    (`Object
        [ ( "form"
          , `Object [ "name", `String " Lloyd "; "units", `String "metric" ] )
        ]);
  [%expect
    {|
    a hears {"type":"event","event":"user_info_changed","form":{"name":"Lloyd","units":"metric"},"text":"- Name: Lloyd\n- Units: metric (°C, km, kg, L)","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    b hears {"type":"event","event":"user_info_changed","form":{"name":"Lloyd","units":"metric"},"text":"- Name: Lloyd\n- Units: metric (°C, km, kg, L)","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    {"form":{"name":"Lloyd","units":"metric"},"text":"- Name: Lloyd\n- Units: metric (°C, km, kg, L)","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    |}];
  (* One field, as the terminal sets it. *)
  call
    b
    "set_user_info"
    (`Object [ "field", `String "date_format"; "value", `String "ISO" ]);
  ignore ([%expect.output] : string);
  call b "get_user_info" (`Object []);
  [%expect
    {| {"form":{"name":"Lloyd","units":"metric","date_format":"iso"},"text":"- Name: Lloyd\n- Units: metric (°C, km, kg, L)\n- Date format: ISO 8601 (2026-12-25)","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}} |}];
  call
    a
    "set_user_info"
    (`Object [ "form", `Object [ "name", `String (String.make 101 'x') ] ]);
  call a "set_user_info" (`Object []);
  call
    a
    "set_user_info"
    (`Object [ "form", `Object [ "units", `String "cubits" ] ]);
  call
    a
    "set_user_info"
    (`Object [ "field", `String "age"; "value", `String "3" ]);
  call a "set_user_info" (`Object [ "field", `String "name" ]);
  [%expect
    {|
    "name is limited to 100 bytes (this one has 101): shorten it, and keep what is only sometimes needed in memory notes"
    "set_user_info needs a form, or a field and its value"
    "units \"cubits\" is not one of metric, imperial, us_customary"
    "no field \"age\": the fields are name, country, time_zone, language, units, date_format, occupation, background, anything_else"
    "missing param \"value\""
    |}];
  call b "set_user_info" (`Object [ "form", `Object [] ]);
  [%expect
    {|
    a hears {"type":"event","event":"user_info_changed","form":{},"text":"","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    b hears {"type":"event","event":"user_info_changed","form":{},"text":"","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    {"form":{},"text":"","limits":{"name":100,"background":1000,"anything_else":4000,"total":5000}}
    |}]
;;

(* What the latest request tells the model about the user: the system
   prompt's block and the notes its last user message starts with. *)
let print_request requests ~text =
  let request : Provider.Request.t = Queue.last_exn requests in
  let system = Option.value request.system ~default:"" in
  (match String.substr_index system ~pattern:"About the user" with
   | Some i -> printf "system: … %s\n" (String.drop_prefix system i)
   | None -> print_endline "system: nothing about the user");
  (match List.last_exn request.messages with
   | Message.User u ->
     let notes =
       Option.value_map u.origin ~default:[] ~f:(fun (i : Injected.t) ->
         List.filter i.notes ~f:(fun n ->
           match n.kind with
           | User_info _ -> true
           | _ -> false))
     in
     assert (String.is_suffix u.text ~suffix:text);
     (match notes with
      | [] -> printf "%s: nothing told\n" text
      | notes ->
        List.iter notes ~f:(fun n ->
          assert (String.is_substring u.text ~substring:n.text);
          printf "%s: %s\n%s\n" text (Injected.Note.title n) n.text))
   | _ -> print_endline "no user message");
  Queue.clear requests
;;

let with_agents f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(Queue.enqueue requests)
      (List.init 20 ~f:(fun _ -> Reply.text "ok"))
  in
  let make ?session ?user_info () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ?user_info
      ~cwd:t.dir
      ()
  in
  (* A backend restart: a new agent over the session as saved. *)
  let restart ?user_info agent =
    make
      ?user_info
      ~session:(Or_error.ok_exn (Session.load (Agent.state agent).session_path))
      ()
  in
  let send agent text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent;
    print_request requests ~text
  in
  f t ~make ~restart ~send
;;

let%expect_test
    "a new session has it in its system prompt and is not told it again; an \
     edit and its removal are told once, with the next message"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let info = info t in
  Or_error.ok_exn
    (User_info.set
       info
       (form ~name:"Lloyd" ~anything_else:"I prefer short answers." ()));
  let agent = make ~user_info:info () in
  send agent "one";
  send agent "two";
  [%expect
    {|
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    one: nothing told
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    two: nothing told
    |}];
  Or_error.ok_exn
    (User_info.set info (form ~name:"Lloyd" ~background:"OCaml" ()));
  send agent "three";
  send agent "four";
  [%expect
    {|
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    three: About you: the model was given your details
    [User info: what the user set about themselves in prigh's settings, for every session, is now:
    - Name: Lloyd
    - Background and expertise: OCaml
    It replaces anything earlier about the user from their settings.]
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    four: nothing told
    |}];
  (* Changed and changed back: nothing to tell. Removed, then the backend
     restarts before the next message. *)
  Or_error.ok_exn (User_info.set info (form ~name:"Someone else" ()));
  Or_error.ok_exn
    (User_info.set info (form ~name:"Lloyd" ~background:"OCaml" ()));
  send agent "five";
  Or_error.ok_exn (User_info.set info User_info.Form.empty);
  let agent = restart ~user_info:info agent in
  send agent "six";
  let agent = restart ~user_info:info agent in
  send agent "seven";
  [%expect
    {|
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    five: nothing told
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    six: About you: the model was told you cleared them
    [User info: the user cleared what they set about themselves in prigh's settings: anything earlier about the user from their settings no longer applies.]
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    - Anything else: I prefer short answers.
    seven: nothing told
    |}]
;;

let%expect_test
    "a session from before it is told the text once, with its next message; \
     without a text it is told nothing"
  =
  with_agents
  @@ fun t ~make ~restart ~send ->
  let info = info t in
  (* As a backend from before the feature: no user info at all. *)
  let agent = make () in
  send agent "one";
  let agent = restart ~user_info:info agent in
  send agent "two";
  [%expect
    {|
    system: nothing about the user
    one: nothing told
    system: nothing about the user
    two: nothing told
    |}];
  Or_error.ok_exn (User_info.set info (form ~name:"Lloyd" ()));
  let agent = restart ~user_info:info agent in
  send agent "three";
  let agent = restart ~user_info:info agent in
  send agent "four";
  [%expect
    {|
    system: nothing about the user
    three: About you: the model was given your details
    [User info: what the user set about themselves in prigh's settings, for every session, is now:
    - Name: Lloyd
    It replaces anything earlier about the user from their settings.]
    system: nothing about the user
    four: nothing told
    |}]
;;

let%expect_test
    "written before the first message of an open session: in its system \
     prompt, not also a note"
  =
  with_agents
  @@ fun t ~make ~restart:_ ~send ->
  let info = info t in
  let agent = make ~user_info:info () in
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (User_info.set info (form ~name:"Lloyd" ()));
  send agent "one";
  [%expect
    {|
    system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    one: nothing told
    |}]
;;

let%expect_test "a subagent's own system prompt has it; the note's wire form" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let info = info t in
  Or_error.ok_exn (User_info.set info (form ~name:"Lloyd" ()));
  let requests = Queue.create () in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key r -> Queue.enqueue requests (key, r))
      ~main:
        [ Reply.tool_call
            ~id:"c1"
            ~name:"subagent"
            ~arguments:{|{"task":"look around"}|}
            ()
        ; Reply.text "started"
        ; Reply.text "thanks"
        ]
      [ "look around", [ Reply.text "looked" ] ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ [ Tool_subagent.create
               ~provider
               ~user_info:(fun () -> User_info.text info)
               ~current_model:(fun () -> Model.default)
               ~current_thinking:(fun () -> Off)
               ~home:t.dir
               ()
           ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~user_info:info
      ~cwd:t.dir
      ()
  in
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Agent.wait_idle agent;
    Eio.Fiber.yield ()
  done;
  Queue.iter requests ~f:(fun (key, (r : Provider.Request.t)) ->
    if String.equal key "look around"
    then (
      let system = Option.value r.system ~default:"" in
      match String.substr_index system ~pattern:"About the user" with
      | Some i ->
        printf "subagent's system: … %s\n" (String.drop_prefix system i)
      | None -> print_endline "subagent's system: nothing about the user"));
  [%expect
    {|
    subagent's system: … About the user (from their settings in prigh; applies to every session):
    - Name: Lloyd
    |}];
  let note =
    { Injected.Note.kind = User_info { cleared = false }
    ; text = System_prompt.user_info_note (Some "I'm Lloyd.")
    }
  in
  print_endline
    (Json.to_string
       (Injected.to_json
          { Injected.empty with notes = [ note ] }
          ~text:(Injected.prefix_notes [ note ] "hello")));
  [%expect
    {| {"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given your details","text":"[User info: what the user set about themselves in prigh's settings, for every session, is now:\nI'm Lloyd.\nIt replaces anything earlier about the user from their settings.]"}]} |}]
;;
