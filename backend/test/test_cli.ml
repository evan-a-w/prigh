open! Core
open! Prigh
open Tool_test_helpers

(* The [prigh] binary as a user meets it: wrong flags, missing logins and
   broken files, each error saying what to do next. *)

let exe = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"

let spawn t args =
  Core_unix.create_process_env
    ~prog:exe
    ~args
    ~env:(`Replace [ "HOME", t.dir; "PATH", "/usr/bin:/bin" ])
    ()
;;

(* Runs [prigh ARGS] to completion; prints stdout, stderr and the exit
   status, with the sandbox and [hide]'s patterns masked. *)
let prigh ?(hide = []) t args =
  let p = spawn t args in
  Core_unix.close p.stdin;
  let read fd = In_channel.input_all (Core_unix.in_channel_of_descr fd) in
  let out = read p.stdout in
  let err = read p.stderr in
  let status = Core_unix.waitpid p.pid in
  let show s =
    List.fold hide ~init:(mask t s) ~f:(fun s (pattern, with_) ->
      String.substr_replace_all s ~pattern ~with_)
    |> print_string
  in
  show (sprintf "$ prigh %s\n" (String.concat ~sep:" " args));
  show out;
  show err;
  match status with
  | Ok () -> ()
  | Error (`Exit_non_zero n) -> printf "[exit %d]\n" n
  | Error (`Signal s) -> printf "[%s]\n" (Signal.to_string s)
;;

let%expect_test "flags: bad values name the flag and the fix" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-model"; "anthropic/nope"; "hi" ];
  prigh t [ "run"; "-thinking"; "medium"; "hi" ];
  prigh t [ "run"; "-cwd"; "/nonexistent"; "-faux"; "hi" ];
  prigh t [ "models"; "-provider"; "anthropc" ];
  prigh t [ "serve"; "-listen"; "7777" ];
  prigh t [ "serve"; "-web"; "localhost" ];
  prigh t [ "serve"; "-prigh-web"; ":99999" ];
  prigh t [ "tool-host"; "-connect"; "7777" ];
  [%expect
    {|
    $ prigh run -model anthropic/nope hi
    -model: unknown model "anthropic/nope"; did you mean: anthropic/claude-opus-5 (Claude Opus 5), anthropic/claude-fable-5 (Claude Fable 5), anthropic/claude-opus-4-5 (Claude Opus 4.5 (latest))
    prigh models lists them all
    [exit 2]
    $ prigh run -thinking medium hi
    -thinking medium: thinking must be one of: off, on, low, high, max
    [exit 2]
    $ prigh run -cwd /nonexistent -faux hi
    -cwd /nonexistent: no such directory
    [exit 2]
    $ prigh models -provider anthropc
    unknown provider anthropc; one of: anthropic, openai, openai-codex, deepseek (or custom, to add an OpenAI-compatible endpoint)
    [exit 2]
    $ prigh serve -listen 7777
    -listen must be HOST:PORT, got "7777"; did you mean 127.0.0.1:7777 (this machine only) or 0.0.0.0:7777 (all interfaces)?
    [exit 2]
    $ prigh serve -web localhost
    -web must be HOST:PORT such as 127.0.0.1:7788, got "localhost"
    [exit 2]
    $ prigh serve -prigh-web :99999
    -prigh-web :99999: the port must be a number from 0 to 65535
    [exit 2]
    $ prigh tool-host -connect 7777
    -connect must be HOST:PORT (the backend's -listen or -web address), got "7777"
    [exit 2]
    |}]
;;

let%expect_test "run without a login says how to log in, as a command" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-quiet"; "hi" ];
  [%expect
    {|
    $ prigh run -quiet hi
    error: not logged in to DeepSeek: use prigh login deepseek or set DEEPSEEK_API_KEY
    [exit 1]
    |}]
;;

let%expect_test "-session takes the id prigh sessions list shows" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-faux"; "-quiet"; "first" ];
  let id =
    match Session.list ~dir:(Filename.concat t.dir ".prigh/sessions") with
    | [ s ] -> s.id
    | _ -> assert false
  in
  let hide = [ String.prefix id 6, "<id-prefix>" ] in
  prigh ~hide t [ "run"; "-faux"; "-session"; String.prefix id 6; "second" ];
  print_s
    [%sexp
      (List.map
         (Session.list ~dir:(Filename.concat t.dir ".prigh/sessions"))
         ~f:(fun s -> s.message_count)
       : int list)];
  prigh t [ "run"; "-faux"; "-session"; "nosuch"; "hi" ];
  prigh t [ "run"; "-faux"; "-session"; "/no/such.jsonl"; "hi" ];
  prigh ~hide t [ "sessions"; "delete"; String.prefix id 6; "nosuch" ];
  [%expect
    {|
    $ prigh run -faux -quiet first
    faux reply
    $ prigh run -faux -session <id-prefix> second
    faux reply
    [deepseek-flash] tokens: in=20 (cached 0) out=10 cost=$0.0000 session=$DIR/.prigh/sessions/<stamp>_<id>.jsonl
    (4)
    $ prigh run -faux -session nosuch hi
    -session nosuch: no session "nosuch"
    prigh sessions list shows the saved sessions (in $DIR/.prigh/sessions)
    [exit 2]
    $ prigh run -faux -session /no/such.jsonl hi
    -session /no/such.jsonl: no such file
    prigh sessions list shows the saved sessions (in $DIR/.prigh/sessions)
    [exit 2]
    $ prigh sessions delete <id-prefix> nosuch
    no session "nosuch"
    Nothing deleted; prigh sessions list shows the saved sessions
    [exit 1]
    |}]
;;

let%expect_test "broken files say which file and what to do" =
  with_sandbox
  @@ fun t ->
  write t ".prigh/config.json" {|{"default_model": "deepseek-flash",|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  write
    t
    ".prigh/config.json"
    {|{"confirm_tools": "yes", "default_modl": "deepseek-flash"}|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  write t ".prigh/config.json" {|{"default_model": "deepseek-flsh"}|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  Core_unix.unlink (Filename.concat t.dir ".prigh/config.json");
  write t ".config/prigh/auth.json" "{oops";
  prigh t [ "auth" ];
  write t ".config/prigh/auth.json" {|{"deepseek": {"type": "password"}}|};
  prigh t [ "run"; "-quiet"; "hi" ];
  [%expect
    {|
    $ prigh run -faux -quiet hi
    faux reply
    $DIR/.prigh/config.json is not valid JSON (json > object: char '}'); its settings and custom providers are ignored until it is fixed
    $ prigh run -faux -quiet hi
    faux reply
    config.confirm_tools must be a boolean (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    unknown setting "default_modl" in $DIR/.prigh/config.json is ignored; did you mean "default_model"?
    $ prigh run -faux -quiet hi
    faux reply
    default_model "deepseek-flsh" (in $DIR/.prigh/config.json) is not a known model, so new sessions start on another; did you mean deepseek/deepseek-flash? (/change_default saves the current model as the default)
    $ prigh auth
    $DIR/.config/prigh/auth.json is not valid JSON (json > object: char '}'): fix it, or move it aside and log in again
    [exit 1]
    $ prigh run -quiet hi
    error: auth: $DIR/.config/prigh/auth.json: the deepseek credential is unreadable (credential: unknown type "password"): log in to deepseek again
    [exit 1]
    |}]
;;

(* Starts [prigh serve ARGS] and returns it with the port it announced. *)
let serve t args =
  let p = spawn t ("serve" :: "-faux" :: args) in
  let err = Core_unix.in_channel_of_descr p.stderr in
  let line = Option.value_exn (In_channel.input_line err) in
  let port =
    String.rsplit2_exn line ~on:':' |> snd |> String.strip |> Int.of_string
  in
  p, port
;;

let stop (p : Core_unix.Process_info.t) =
  Signal_unix.send_i Signal.term (`Pid p.pid);
  ignore (Core_unix.waitpid p.pid : Core_unix.Exit_or_signal.t)
;;

let%expect_test "serve: a taken port, a refused tool host" =
  with_sandbox
  @@ fun t ->
  let server, port = serve t [ "-listen"; "127.0.0.1:0"; "-token"; "good" ] in
  let address = sprintf "127.0.0.1:%d" port in
  let prigh = prigh ~hide:[ address, "127.0.0.1:PORT" ] in
  prigh t [ "serve"; "-faux"; "-listen"; address ];
  prigh t [ "serve"; "-faux"; "-web"; address ];
  prigh t [ "tool-host"; "-connect"; address ];
  prigh t [ "tool-host"; "-connect"; address; "-token"; "bad" ];
  prigh t [ "tool-host"; "-connect"; address; "-token"; "bad"; "-user"; "me" ];
  stop server;
  [%expect
    {|
    $ prigh serve -faux -listen 127.0.0.1:PORT
    prigh: cannot listen on tcp:127.0.0.1:PORT for -listen: address already in use
    Another program (perhaps another prigh serve) has that port: stop it, or pass -listen another port (0 picks a free one).
    [exit 2]
    $ prigh serve -faux -web 127.0.0.1:PORT
    prigh: cannot listen on tcp:127.0.0.1:PORT for -web: address already in use
    Another program (perhaps another prigh serve) has that port: stop it, or pass -web another port (0 picks a free one).
    [exit 2]
    $ prigh tool-host -connect 127.0.0.1:PORT
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    No token was given: pass the backend's with -token SECRET (or $PRIGH_TOKEN).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -token (or $PRIGH_TOKEN) against the backend's; if it serves several users (-tokens), also give -user NAME (or $PRIGH_USER).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad -user me
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the backend's -tokens NAME=TOKEN (or -host-tokens) entries.
    [exit 1]
    |}]
;;
