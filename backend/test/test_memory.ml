open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let day n =
  Time_float.add Time_float.epoch (Time_float.Span.of_day (20_000. +. n))
;;

let memory t = Memory.create ~home:t.dir

let print_file t =
  match In_channel.read_all (Memory.path ~home:t.dir) with
  | data ->
    print_string (String.substr_replace_all data ~pattern:t.dir ~with_:"$DIR")
  | exception _ -> print_endline "(no file)"
;;

let show_notes m =
  match Memory.notes m with
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
  | Ok notes ->
    List.iter notes ~f:(fun (n : Memory.Note.t) ->
      printf
        "%s %s: %s\n"
        n.id
        (Option.value_map n.title ~default:"-" ~f:(sprintf "[%s]"))
        n.text)
;;

let save ?title ?(now = day 0.) m text =
  match Memory.save m ?title ~now text with
  | Ok saved -> print_s [%sexp (saved : Memory.Saved.t)]
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let forget m key =
  match Memory.forget m key with
  | Ok note -> printf "forgot %s\n" note.id
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let%expect_test "save, replace by title, forget; ids are never reused" =
  with_sandbox
  @@ fun t ->
  let m = memory t in
  show_notes m;
  print_file t;
  [%expect {| (no file) |}];
  save m ~title:"editor" "The user's editor is helix.";
  save m ~now:(day 1.) "  Prefers squash merges.\n";
  save m ~title:" Editor " ~now:(day 2.) "The user's editor is neovim now.";
  [%expect
    {|
    (Added
     ((id m1) (title (editor)) (text "The user's editor is helix.")
      (saved "2024-10-04 00:00:00.000000Z")))
    (Added
     ((id m2) (title ()) (text "Prefers squash merges.")
      (saved "2024-10-05 00:00:00.000000Z")))
    (Replaced
     ((id m1) (title (Editor)) (text "The user's editor is neovim now.")
      (saved "2024-10-06 00:00:00.000000Z")))
    |}];
  print_file t;
  [%expect
    {|
    {
      "next": 3,
      "notes": [
        {
          "id": "m1",
          "title": "Editor",
          "text": "The user's editor is neovim now.",
          "saved": "2024-10-06 00:00:00.000000Z"
        },
        {
          "id": "m2",
          "text": "Prefers squash merges.",
          "saved": "2024-10-05 00:00:00.000000Z"
        }
      ]
    }
    |}];
  forget m "m2";
  save m "Uses fish.";
  forget m "EDITOR";
  forget m "m9";
  show_notes m;
  [%expect
    {|
    forgot m2
    (Added
     ((id m3) (title ()) (text "Uses fish.")
      (saved "2024-10-04 00:00:00.000000Z")))
    forgot m1
    error: no note "m9"; the notes are m3 (recall shows them)
    m3 -: Uses fish.
    |}];
  (* The counter survives forgetting the newest note, and a reopened
     store. *)
  forget m "m3";
  forget m "m3";
  let m = memory t in
  save m "Again.";
  show_notes m;
  [%expect
    {|
    forgot m3
    error: no note "m3": memory is empty
    (Added
     ((id m4) (title ()) (text Again.) (saved "2024-10-04 00:00:00.000000Z")))
    m4 -: Again.
    |}]
;;

let%expect_test "limits, and a broken file is reported, not overwritten" =
  with_sandbox
  @@ fun t ->
  let m = memory t in
  save m "   ";
  save m (String.make (Memory.max_text + 1) 'x');
  save m ~title:(String.make (Memory.max_title + 1) 't') "ok";
  for i = 1 to Memory.max_notes do
    ignore (Memory.save m ~now:(day 0.) (sprintf "note %d" i) : _ Or_error.t)
  done;
  save m "one too many";
  (* Replacing a note is not adding one. *)
  save m ~title:"t" "x";
  [%expect
    {|
    error: the note's text is empty
    error: a note is limited to 2000 bytes (this one has 2001): keep only what must be remembered
    error: a title is limited to 80 bytes: use a few words
    error: memory is full (200 notes): forget some first
    error: memory is full (200 notes): forget some first
    |}];
  Out_channel.write_all (Memory.path ~home:t.dir) ~data:"{not json";
  let show_error = function
    | Ok _ -> print_endline "ok"
    | Error e -> print_endline (mask t (Error.to_string_hum e))
  in
  show_error (Memory.save m ~now:(day 0.) "x");
  show_error (Memory.notes m);
  print_file t;
  [%expect
    {|
    $DIR/.prigh/memory.json is not valid JSON (json > object: char '}'): fix it, or move it aside
    $DIR/.prigh/memory.json is not valid JSON (json > object: char '}'): fix it, or move it aside
    {not json
    |}]
;;

let%expect_test "search: ids, words anywhere, one typo, best and newest first" =
  let note ?title id n text =
    { Memory.Note.id; title; text; saved = day (Float.of_int n) }
  in
  let notes =
    [ note
        "m1"
        1
        "Deploys go through the staging cluster first."
        ~title:"deploy"
    ; note "m2" 2 "The user's editor is helix."
    ; note "m3" 3 "Staging cluster is called kestrel."
    ; note "m4" 4 "Prefers squash merges for pull requests."
    ]
  in
  let search q =
    printf
      "%-24s -> %s\n"
      (sprintf "%S" q)
      (String.concat
         ~sep:" "
         (List.map (Memory.search notes q) ~f:(fun n -> n.Memory.Note.id)))
  in
  List.iter
    ~f:search
    [ "m2"
    ; "M4"
    ; "staging"
    ; "staging deploy"
    ; "EDITOR"
    ; "editr"
    ; "merge"
    ; "kestral cluster"
    ; "nothing here"
    ; ""
    ];
  [%expect
    {|
    "m2"                     -> m2
    "M4"                     -> m4
    "staging"                -> m3 m1
    "staging deploy"         -> m1 m3
    "EDITOR"                 -> m2
    "editr"                  -> m2
    "merge"                  -> m4
    "kestral cluster"        -> m3 m1
    "nothing here"           ->
    ""                       ->
    |}]
;;

let%expect_test "each namespace has its own memory" =
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
  let m (w : Namespace.World.t) = Memory.create ~home:w.home in
  List.iter [ alice; bob; default ] ~f:(fun w ->
    print_endline (mask t (Memory.path ~home:w.home)));
  save (m alice) "alice's note";
  save (m default) "the default user's note";
  print_endline "alice:";
  show_notes (m alice);
  print_endline "bob:";
  show_notes (m bob);
  print_endline "default:";
  show_notes (m default);
  [%expect
    {|
    $DIR/.prigh/namespaces/alice/.prigh/memory.json
    $DIR/.prigh/namespaces/bob/.prigh/memory.json
    $DIR/.prigh/memory.json
    (Added
     ((id m1) (title ()) (text "alice's note")
      (saved "2024-10-04 00:00:00.000000Z")))
    (Added
     ((id m1) (title ()) (text "the default user's note")
      (saved "2024-10-04 00:00:00.000000Z")))
    alice:
    m1 -: alice's note
    bob:
    default:
    m1 -: the default user's note
    |}]
;;

let%expect_test "concurrent saves from two stores on one file lose nothing" =
  with_sandbox
  @@ fun t ->
  let a = memory t
  and b = memory t in
  Eio.Fiber.all
    (List.init 20 ~f:(fun i () ->
       let m = if i % 2 = 0 then a else b in
       ignore (Memory.save m ~now:(day 0.) (sprintf "note %d" i) : _ Or_error.t)));
  let notes = Or_error.ok_exn (Memory.notes a) in
  printf
    "%d notes, ids %s\n"
    (List.length notes)
    (String.concat
       ~sep:" "
       (List.sort ~compare:String.compare (List.map notes ~f:(fun n -> n.id))));
  printf
    "texts all distinct: %b\n"
    (not
       (List.contains_dup notes ~compare:(fun (x : Memory.Note.t) y ->
          String.compare x.text y.text)));
  [%expect
    {|
    20 notes, ids m1 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m2 m20 m3 m4 m5 m6 m7 m8 m9
    texts all distinct: true
    |}]
;;

let tool t =
  let clock = ref 0. in
  Tool_memory.create
    ~now:(fun () ->
      clock := !clock +. 1.;
      day !clock)
    ~memory:(memory t)
    ()
;;

let%expect_test "the tool: save, recall, forget" =
  with_sandbox
  @@ fun t ->
  let tool = tool t in
  let run args = run t tool args in
  run {|{"action":"recall"}|};
  run
    {|{"action":"save","text":"The user's editor is helix.","title":"editor"}|};
  run
    {|{"action":"save","text":"Release notes go in CHANGES.md,\nnewest first."}|};
  run {|{"action":"save","text":"Editor is neovim now.","title":"Editor"}|};
  [%expect
    {|
    Memory is empty.
    Saved m1 · editor · 2024-10-05
    Saved m2 · 2024-10-06
    Replaced m1 · Editor · 2024-10-07
    |}];
  run {|{"action":"recall"}|};
  run {|{"action":"recall","query":"editor"}|};
  run {|{"action":"recall","query":"release"}|};
  run {|{"action":"recall","query":"emacs"}|};
  [%expect
    {|
    2 notes, newest first:
    m2 · 2024-10-06: Release notes go in CHANGES.md, newest first.
    m1 · Editor · 2024-10-07: Editor is neovim now.
    m1 · Editor · 2024-10-07
    Editor is neovim now.
    m2 · 2024-10-06
    Release notes go in CHANGES.md,
    newest first.
    No note matches "emacs" (2 notes in memory; recall without a query lists them).
    |}];
  run {|{"action":"forget","id":"editor"}|};
  run {|{"action":"forget","id":"m1"}|};
  run {|{"action":"save"}|};
  run {|{"action":"save","text":"  "}|};
  run {|{"action":"forget"}|};
  run {|{"action":"remember","text":"x"}|};
  run {|{"text":"x"}|};
  [%expect
    {|
    Forgot m1 · Editor · 2024-10-07: Editor is neovim now.
    ERROR: no note "m1"; the notes are m2 (recall shows them)
    ERROR: invalid arguments: save needs text (the note)
    ERROR: invalid arguments: save needs text (the note)
    ERROR: invalid arguments: forget needs id (the note's id or title)
    ERROR: unknown action "remember": use save, recall or forget
    ERROR: invalid arguments: missing required argument "action"
    |}]
;;

let%expect_test "recall without a query: the newest 50, long notes cut" =
  with_sandbox
  @@ fun t ->
  let tool = tool t in
  for i = 1 to 52 do
    let text = if i = 52 then String.make 300 'x' else sprintf "note %d" i in
    ignore
      (Tool.execute
         tool
         (Tool.Context.create ~env:t.env ~cwd:t.dir ())
         (`Object [ "action", `String "save"; "text", `String text ])
       : Tool.Result.t)
  done;
  let context = Tool.Context.create ~env:t.env ~cwd:t.dir () in
  let result =
    Tool.execute tool context (Jsonaf.of_string {|{"action":"recall"}|})
  in
  let lines = String.split_lines result.text in
  List.iter (List.take lines 3) ~f:print_endline;
  print_endline "...";
  List.iter (List.drop lines (List.length lines - 2)) ~f:print_endline;
  [%expect
    {|
    52 notes, newest first:
    m52 · 2024-11-25: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx…
    m51 · 2024-11-24: note 51
    ...
    m3 · 2024-10-07: note 3
    (2 older not shown: recall with a query)
    |}]
;;

let%expect_test "spec: backend-side, main agent only, only changes ask" =
  with_sandbox
  @@ fun t ->
  let spec = (tool t).spec in
  printf "on_host=%b parallel_safe=%b\n" spec.on_host spec.parallel_safe;
  List.iter [ "save"; "recall"; "forget" ] ~f:(fun action ->
    printf
      "%s destructive=%b\n"
      action
      (Tool_spec.Destructive.call
         spec.destructive
         (`Object [ "action", `String action ])));
  print_s
    [%sexp
      (List.map [ 0; 1 ] ~f:(fun depth -> Tools.unavailable ~depth "memory")
       : string option list)];
  print_endline spec.description;
  [%expect
    {|
    on_host=false parallel_safe=false
    save destructive=true
    recall destructive=false
    forget destructive=true
    (()
     ("it is the main agent's (the user's plan, questions, components, goal and memory), and you are a subagent: put what you need from the user in your report"))
    The user's persistent memory, kept across sessions. Use it VERY RARELY. save (text, optional title) only when the user explicitly asks you to remember something, or states a lasting preference and asks that it be kept; saving with an existing title replaces that note. Never save routine task details, facts you can find in the code or the repository, or anything inferred or speculative. recall (optional query; without one it lists the notes) only when the user refers to something they told you before or the request obviously depends on it ("use my usual X", "what did I say about Y"); never recall by default or at the start of a task. forget (id or title) removes a wrong or unwanted note.
    |}];
  List.iter
    [ {|{"action":"save","title":"editor","text":"helix"}|}
    ; {|{"action":"save","text":"Uses fish.\nAlways."}|}
    ; {|{"action":"recall","query":"editor"}|}
    ; {|{"action":"recall"}|}
    ; {|{"action":"forget","id":"m2"}|}
    ]
    ~f:(fun args ->
      let args = Jsonaf.of_string args in
      printf
        "%-24s | %s\n"
        (Agent_loop.tool_summary ~name:"memory" args)
        (Acp_tool_call.title ~name:"memory" ~args ~cwd:t.dir));
  [%expect
    {|
    save editor: helix       | Remember: editor
    save Uses fish. Always.  | Remember: Uses fish. …
    recall editor            | Recall from memory: editor
    recall                   | Recall from memory
    forget m2                | Forget from memory: m2
    |}]
;;

let%expect_test "an agent saves in one run and recalls in the next" =
  with_sandbox
  @@ fun t ->
  let tools = [ tool t ] in
  let run replies prompt =
    let provider = Faux_provider.create replies in
    let config =
      { Agent_loop.Config.model = Model.default
      ; thinking = Off
      ; system = None
      ; tools
      ; max_turns = None
      ; max_tokens = None
      ; retries = 0
      ; cache_key = None
      ; compaction = Compaction_settings.window_only
      }
    in
    let added =
      Agent_loop.run
        ~env:t.env
        ~provider
        ~config
        ~cwd:t.dir
        ~emit:ignore
        ~context:[]
        ~prompts:[ Message.user prompt ]
        ()
    in
    List.iter added ~f:(function
      | Message.User u -> printf "user: %s\n" u.text
      | Assistant a ->
        List.iter a.content ~f:(function
          | Content.Tool_call c -> printf "call %s %s\n" c.name c.arguments
          | Text s -> printf "assistant: %s\n" s
          | _ -> ())
      | Tool_result r ->
        printf "%s: %s\n" (if r.is_error then "error" else "result") r.text)
  in
  run
    [ Reply.tool_call
        ~id:"c1"
        ~name:"memory"
        ~arguments:
          {|{"action":"save","title":"deploys","text":"Deploy with ./ship, never by hand."}|}
        ()
    ; Reply.text "I'll remember that."
    ]
    "Remember: deploy with ./ship, never by hand.";
  run
    [ Reply.tool_call
        ~id:"c2"
        ~name:"memory"
        ~arguments:{|{"action":"recall","query":"deploy"}|}
        ()
    ; Reply.text "You said to deploy with ./ship."
    ]
    "How did I say to deploy?";
  [%expect
    {|
    user: Remember: deploy with ./ship, never by hand.
    call memory {"action":"save","title":"deploys","text":"Deploy with ./ship, never by hand."}
    result: Saved m1 · deploys · 2024-10-05
    assistant: I'll remember that.
    user: How did I say to deploy?
    call memory {"action":"recall","query":"deploy"}
    result: m1 · deploys · 2024-10-05
    Deploy with ./ship, never by hand.
    assistant: You said to deploy with ./ship.
    |}]
;;

let%expect_test "edit keeps the id; subscribers hear every change" =
  with_sandbox
  @@ fun t ->
  let m = memory t in
  Memory.subscribe m ~f:(fun () -> print_endline "(changed)");
  save m ~title:"editor" "helix";
  save m ~title:"shell" "fish";
  [%expect
    {|
    (changed)
    (Added
     ((id m1) (title (editor)) (text helix)
      (saved "2024-10-04 00:00:00.000000Z")))
    (changed)
    (Added
     ((id m2) (title (shell)) (text fish) (saved "2024-10-04 00:00:00.000000Z")))
    |}];
  let edit ?title id text =
    match Memory.edit m ~id ?title ~now:(day 3.) text with
    | Ok note -> print_s [%sexp (note : Memory.Note.t)]
    | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
  in
  edit "m1" ~title:"Editor" "neovim";
  edit "m2" ~title:"editor" "taken";
  edit "m1" "untitled now";
  edit "m9" "gone";
  edit "m2" "  ";
  show_notes m;
  [%expect
    {|
    (changed)
    ((id m1) (title (Editor)) (text neovim)
     (saved "2024-10-07 00:00:00.000000Z"))
    error: note m1 is titled "Editor" already: choose another title
    (changed)
    ((id m1) (title ()) (text "untitled now")
     (saved "2024-10-07 00:00:00.000000Z"))
    error: no note "m9": it was forgotten
    error: the note's text is empty
    m1 -: untitled now
    m2 [shell]: fish
    |}]
;;

let%expect_test "off: not offered to the model, refused, back on the next turn" =
  with_sandbox
  @@ fun t ->
  let m = memory t in
  Memory.subscribe m ~f:(fun () ->
    printf "(changed: enabled=%b)\n" (Memory.enabled m));
  print_s [%sexp (Memory.enabled m : bool)];
  [%expect {| true |}];
  Or_error.ok_exn (Memory.set_enabled m false);
  Or_error.ok_exn (Memory.set_enabled m false);
  print_endline
    (String.substr_replace_all
       (In_channel.read_all (User_prefs.path ~home:t.dir))
       ~pattern:"\n"
       ~with_:" ");
  [%expect
    {|
    (changed: enabled=false)
    {   "memory.enabled": false }
    |}];
  let tool = tool t in
  let result =
    Tool.execute
      tool
      (Tool.Context.create ~env:t.env ~cwd:t.dir ())
      (Jsonaf.of_string {|{"action":"recall"}|})
  in
  printf "error=%b %s\n" result.is_error result.text;
  [%expect
    {| error=true memory is off: the user turned it off (Settings, Memory in prigh-web; /memory on in the terminal). Do not use it. |}];
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Faux_provider.create
      ~on_request:(fun request ->
        print_s
          [%sexp
            (List.map request.tools ~f:(fun (s : Tool_spec.t) -> s.name)
             : string list)])
      ~loop:true
      [ Reply.text "ok" ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[ tool; Tool_read.tool ]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~offered:(fun tool ->
        (not (String.equal (Tool.name tool) Tool_memory.name))
        || Memory.enabled m)
      ~cwd:t.dir
      ()
  in
  let prompt text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent
  in
  prompt "one";
  [%expect {| (read) |}];
  Or_error.ok_exn (Memory.set_enabled m true);
  prompt "two";
  [%expect
    {|
    (changed: enabled=true)
    (memory read)
    |}]
;;

let rec undated : Jsonaf.t -> Jsonaf.t = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         ( key
         , if String.equal key "saved" then `String "$SAVED" else undated value
         )))
  | `Array items -> `Array (List.map items ~f:undated)
  | json -> json
;;

let%expect_test "RPC: get, save, edit, forget, on and off; every client hears" =
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
  let memory = memory t in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home:t.dir
      ~memory
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client name =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "memory_changed") ->
        printf "%s hears %s\n" name (Json.to_string (undated json))
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
    | Some `True, Some result -> print_endline (Json.to_string (undated result))
    | _ ->
      print_endline
        (Json.to_string (Option.value_exn (Json.member "error" response)))
  in
  call a "hello" (`Object []);
  call b "hello" (`Object []);
  ignore ([%expect.output] : string);
  call a "get_memory" (`Object []);
  [%expect {| {"enabled":true,"notes":[]} |}];
  call
    a
    "save_memory"
    (`Object [ "title", `String "editor"; "text", `String "helix" ]);
  [%expect
    {|
    a hears {"type":"event","event":"memory_changed","enabled":true,"notes":[{"id":"m1","title":"editor","text":"helix","saved":"$SAVED"}]}
    b hears {"type":"event","event":"memory_changed","enabled":true,"notes":[{"id":"m1","title":"editor","text":"helix","saved":"$SAVED"}]}
    {"note":{"id":"m1","title":"editor","text":"helix","saved":"$SAVED"}}
    |}];
  call
    a
    "save_memory"
    (`Object [ "id", `String "m1"; "text", `String "neovim" ]);
  call a "save_memory" (`Object [ "id", `String "m7"; "text", `String "x" ]);
  call a "save_memory" (`Object [ "text", `Number "1" ]);
  [%expect
    {|
    a hears {"type":"event","event":"memory_changed","enabled":true,"notes":[{"id":"m1","text":"neovim","saved":"$SAVED"}]}
    b hears {"type":"event","event":"memory_changed","enabled":true,"notes":[{"id":"m1","text":"neovim","saved":"$SAVED"}]}
    {"note":{"id":"m1","text":"neovim","saved":"$SAVED"}}
    "no note \"m7\": it was forgotten"
    "param \"text\" must be a string"
    |}];
  call b "set_memory_enabled" (`Object [ "enabled", `False ]);
  call b "set_memory_enabled" (`Object [ "enabled", `String "no" ]);
  call a "get_memory" (`Object []);
  [%expect
    {|
    a hears {"type":"event","event":"memory_changed","enabled":false,"notes":[{"id":"m1","text":"neovim","saved":"$SAVED"}]}
    b hears {"type":"event","event":"memory_changed","enabled":false,"notes":[{"id":"m1","text":"neovim","saved":"$SAVED"}]}
    {"enabled":false}
    "param \"enabled\" must be a boolean"
    {"enabled":false,"notes":[{"id":"m1","text":"neovim","saved":"$SAVED"}]}
    |}];
  call a "forget_memory" (`Object [ "id", `String "m1" ]);
  call a "forget_memory" (`Object [ "id", `String "m1" ]);
  [%expect
    {|
    a hears {"type":"event","event":"memory_changed","enabled":false,"notes":[]}
    b hears {"type":"event","event":"memory_changed","enabled":false,"notes":[]}
    {"note":{"id":"m1","text":"neovim","saved":"$SAVED"}}
    "no note \"m1\": memory is empty"
    |}]
;;
