open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let show = function
  | Ok json -> print_endline (Json.to_string json)
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let file t = User_prefs.path ~home:t.dir

let print_file t =
  match In_channel.read_all (file t) with
  | data -> print_string data
  | exception _ -> print_endline "(no file)"
;;

let%expect_test "set, get and remove keys; the file is replaced whole" =
  with_sandbox
  @@ fun t ->
  let prefs = User_prefs.create ~path:(file t) in
  let changes c = User_prefs.Change.to_json c in
  show (Ok (`Object (Or_error.ok_exn (User_prefs.get prefs))));
  print_file t;
  [%expect
    {|
    {}
    (no file)
    |}];
  show
    (Or_error.map
       (User_prefs.set
          prefs
          [ "web.theme", Some (`String "dark")
          ; "web.notify", Some `True
          ; "web.sidebar-width", Some (`Number "300")
          ])
       ~f:changes);
  print_file t;
  [%expect
    {|
    {"web.theme":"dark","web.notify":true,"web.sidebar-width":300}
    {
      "web.theme": "dark",
      "web.notify": true,
      "web.sidebar-width": 300
    }
    |}];
  (* Only what changes is reported; a removed key goes, the others keep
     their places. *)
  show
    (Or_error.map
       (User_prefs.set
          prefs
          [ "web.theme", Some (`String "light")
          ; "web.notify", Some `True
          ; "web.sidebar-width", None
          ; "web.missing", None
          ])
       ~f:changes);
  show
    (Or_error.map
       (User_prefs.get prefs ~keys:[ "web.theme"; "nope" ])
       ~f:(fun f -> `Object f));
  print_file t;
  [%expect
    {|
    {"web.theme":"light","web.sidebar-width":null}
    {"web.theme":"light"}
    {
      "web.theme": "light",
      "web.notify": true
    }
    |}];
  print_s [%sexp (Sys_unix.file_exists_exn (file t ^ ".tmp") : bool)];
  [%expect {| false |}]
;;

let%expect_test "bad keys and values change nothing" =
  with_sandbox
  @@ fun t ->
  let prefs = User_prefs.create ~path:(file t) in
  let set changes =
    show
      (Or_error.map (User_prefs.set prefs changes) ~f:User_prefs.Change.to_json)
  in
  let show_error = function
    | Ok _ -> print_endline "ok"
    | Error e -> print_endline (mask t ("error: " ^ Error.to_string_hum e))
  in
  set [ "a", Some `True; "", Some `True ];
  set [ "a", Some `True; String.make 201 'k', Some `True ];
  set [ "a", Some `True; "big", Some (`String (String.make (256 * 1024) 'x')) ];
  print_file t;
  [%expect
    {|
    error: a preference's key must not be empty
    error: a preference's key is limited to 200 bytes
    error: the preference "big" is limited to 256 KiB
    (no file)
    |}];
  (* A file that is not ours to overwrite says how to fix it. *)
  Core_unix.mkdir_p (Filename.dirname (file t));
  Out_channel.write_all (file t) ~data:"[1]";
  show_error (User_prefs.set prefs [ "a", Some `True ]);
  Out_channel.write_all (file t) ~data:"{oops";
  show_error (User_prefs.get prefs);
  print_file t;
  [%expect
    {|
    error: $DIR/.prigh/prefs.json must be a JSON object: fix it, or move it aside
    error: $DIR/.prigh/prefs.json is not valid JSON (json > object: char '}'): fix it, or move it aside
    {oops
    |}]
;;

let%expect_test "push keeps a bounded history, newest first" =
  with_sandbox
  @@ fun t ->
  let prefs = User_prefs.create ~path:(file t) in
  let push value =
    show (User_prefs.push prefs ~key:"history" (`String value) ~max:3)
  in
  push "one";
  push "two";
  push "two";
  push "three";
  push "four";
  [%expect
    {|
    ["one"]
    ["two","one"]
    ["two","one"]
    ["three","two","one"]
    ["four","three","two"]
    |}];
  (* A value that is not a list starts over; the oldest go when the list
     would be too large. *)
  ignore (User_prefs.set prefs [ "history", Some (`Number "1") ] : _ Or_error.t);
  push "again";
  let big c = String.make (100 * 1024) c in
  List.iter [ 'a'; 'b'; 'c' ] ~f:(fun c ->
    ignore
      (User_prefs.push prefs ~key:"long" (`String (big c)) ~max:10
       : _ Or_error.t));
  (match User_prefs.get prefs ~keys:[ "long" ] with
   | Ok [ (_, `Array items) ] ->
     print_s
       [%sexp
         (List.map items ~f:(function
            | `String s -> String.prefix s 1
            | _ -> "?")
          : string list)]
   | _ -> print_endline "?");
  [%expect
    {|
    ["again"]
    (c b)
    |}]
;;

let make_server t ~sw =
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
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  Rpc_server.create
    ~env:t.env
    ~sw
    ~home:t.dir
    ~login
    ~sessions_dir
    ~cwd:t.dir
    ~new_agent
    ()
;;

let%expect_test "the RPC methods; the user's other clients hear the changes" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw in
  let client name =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "prefs_changed") ->
        printf "%s hears %s\n" name (Json.to_string json)
      | _ -> ())
  in
  let a = client "a" in
  let _b = client "b" in
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
        (mask
           t
           (Json.to_string (Option.value_exn (Json.member "error" response))))
  in
  call a "hello" (`Object []);
  call _b "hello" (`Object []);
  ignore ([%expect.output] : string);
  call a "get_prefs" (`Object []);
  call
    a
    "set_prefs"
    (`Object [ "prefs", `Object [ "web.theme", `String "dark" ] ]);
  (* Nothing changes: nobody hears it. *)
  call
    a
    "set_prefs"
    (`Object [ "prefs", `Object [ "web.theme", `String "dark" ] ]);
  call
    a
    "push_pref"
    (`Object [ "key", `String "history"; "value", `String "hi" ]);
  call a "get_prefs" (`Object [ "keys", `Array [ `String "web.theme" ] ]);
  call a "set_prefs" (`Object [ "prefs", `Object [ "web.theme", `Null ] ]);
  call a "set_prefs" (`Object [ "prefs", `Array [] ]);
  call a "push_pref" (`Object [ "key", `String "history" ]);
  [%expect
    {|
    {"prefs":{}}
    b hears {"type":"event","event":"prefs_changed","prefs":{"web.theme":"dark"}}
    {"prefs":{"web.theme":"dark"}}
    {"prefs":{}}
    b hears {"type":"event","event":"prefs_changed","prefs":{"history":["hi"]}}
    {"value":["hi"]}
    {"prefs":{"web.theme":"dark"}}
    b hears {"type":"event","event":"prefs_changed","prefs":{"web.theme":null}}
    {"prefs":{"web.theme":null}}
    "param \"prefs\" must be an object of keys to values (null removes one)"
    "missing param \"value\""
    |}];
  Rpc_server.shutdown server
;;
