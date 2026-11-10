open! Core
open! Prigh
open Tool_test_helpers

(* A program the browser cannot reach (over SSH) also takes the page's code
   pasted into it: when it offers that, and the listener and the paste
   racing. *)

let remote = Backend_address.parse "https://ai.example.com/" |> Or_error.ok_exn

let%expect_test "offered over SSH, with -paste, or as PRIGH_SIGNIN_PASTE says" =
  List.iter
    [ false, []
    ; true, []
    ; false, [ "SSH_CONNECTION", "10.0.0.2 51000 10.0.0.1 22" ]
    ; false, [ "SSH_TTY", "/dev/pts/3" ]
    ; false, [ "SSH_TTY", "" ]
    ; ( false
      , [ "SSH_CONNECTION", "10.0.0.2 51000 10.0.0.1 22"
        ; "PRIGH_SIGNIN_PASTE", "0"
        ] )
    ; false, [ "PRIGH_SIGNIN_PASTE", "1" ]
    ; true, [ "PRIGH_SIGNIN_PASTE", "0" ]
    ; false, [ "PRIGH_SIGNIN_PASTE", "yes" ]
    ]
    ~f:(fun (flag, env) ->
      let wanted =
        Browser_signin.Paste.wanted ~flag ~getenv:(fun name ->
          List.Assoc.find env ~equal:String.equal name)
      in
      print_s
        [%message
          ""
            ~paste_flag:(flag : bool)
            (env : (string * string) list)
            (wanted : bool)]);
  [%expect
    {|
    ((paste_flag false) (env ()) (wanted false))
    ((paste_flag true) (env ()) (wanted true))
    ((paste_flag false) (env ((SSH_CONNECTION "10.0.0.2 51000 10.0.0.1 22")))
     (wanted true))
    ((paste_flag false) (env ((SSH_TTY /dev/pts/3))) (wanted true))
    ((paste_flag false) (env ((SSH_TTY ""))) (wanted false))
    ((paste_flag false)
     (env ((SSH_CONNECTION "10.0.0.2 51000 10.0.0.1 22") (PRIGH_SIGNIN_PASTE 0)))
     (wanted false))
    ((paste_flag false) (env ((PRIGH_SIGNIN_PASTE 1))) (wanted true))
    ((paste_flag true) (env ((PRIGH_SIGNIN_PASTE 0))) (wanted true))
    ((paste_flag false) (env ((PRIGH_SIGNIN_PASTE yes))) (wanted false))
    |}]
;;

(* A fake stdin: the lines the test sends ([None]: the end of the input). *)
let fake_stdin () =
  let lines = Eio.Stream.create Int.max_value in
  ( Browser_signin.Paste.create
      ~secret:Paste_vector.program_secret
      ~from:"build-box"
      ~read_line:(fun () ->
        let line = Eio.Stream.take lines in
        printf
          "(stdin: %s)\n"
          (Option.value_map line ~default:"<end>" ~f:(fun l ->
             if String.length l > 20 then String.prefix l 20 ^ "..." else l));
        line)
      ()
  , fun line -> Eio.Stream.add lines line )
;;

(* Runs the sign-in with the vector's state and key; [user] gets the port
   and the fake stdin and acts as the user would. *)
let with_signin t ~program user =
  let paste, send = fake_stdin () in
  let port = Eio.Promise.create () in
  let result =
    Eio.Fiber.pair
      (fun () ->
         Browser_signin.run
           ~env:t.env
           ~state:Paste_vector.state
           ~paste
           ~on_refused:(printf "refused: %s\n")
           ~program
           remote
           ~on_url:(fun ~url ~port:p ->
             print_endline
               (String.substr_replace_all
                  url
                  ~pattern:(Int.to_string p)
                  ~with_:"PORT");
             Eio.Promise.resolve (snd port) p))
      (fun () -> user ~port:(Eio.Promise.await (fst port)) ~send)
    |> fst
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  Result.iter result ~f:(fun grant ->
    print_s [%message (grant.proxy_cookie : string)]);
  Eio.Promise.await (fst port)
;;

let callback_closed t ~port =
  match Fake_signin_proxy.request ~env:t.env ~port ~meth:"GET" "/callback" with
  | (_ : string) -> print_endline "the listener is still open"
  | exception exn -> print_endline (Net_error.to_string_hum exn)
;;

let%expect_test "the paste wins: bad codes are refused, the right one ends it" =
  with_sandbox
  @@ fun t ->
  let port =
    with_signin t ~program:Tui (fun ~port:_ ~send ->
      send (Some "");
      send (Some "hello");
      send (Some (String.drop_suffix Paste_vector.code 4 ^ "AAAA"));
      send (Some ("  " ^ Paste_vector.code ^ " ")))
  in
  callback_closed t ~port;
  [%expect
    {|
    https://ai.example.com/#cli-signin=PORT.0123456789abcdef0123456789abcdef&program=tui&paste=B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw&from=build-box
    (stdin: )
    (stdin: hello)
    refused: that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again
    (stdin: prigh1:WGmv9FBUlzLLq...)
    refused: that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again
    (stdin:   prigh1:WGmv9FBUlzL...)
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    (grant.proxy_cookie proxy_session=s3cret)
    connection refused
    |}]
;;

let%expect_test "the listener wins (a forwarded port) while the paste waits" =
  with_sandbox
  @@ fun t ->
  let port =
    with_signin t ~program:Tool_host (fun ~port ~send ->
      send (Some "not yet");
      (* Waits for the refusal, so the paste is read before the post. *)
      Eio.Fiber.yield ();
      let response =
        Fake_signin_proxy.request
          ~env:t.env
          ~port
          ~meth:"POST"
          ~headers:[ "Content-Type", "application/x-www-form-urlencoded" ]
          ~body:
            (Uri.encoded_of_query
               [ "state", [ Paste_vector.state ]
               ; "cookie", [ "proxy_session=fwd" ]
               ])
          "/callback"
      in
      print_endline (List.hd_exn (String.split_lines response)))
  in
  callback_closed t ~port;
  [%expect
    {|
    https://ai.example.com/#cli-signin=PORT.0123456789abcdef0123456789abcdef&program=tool-host&paste=B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw&from=build-box
    (stdin: not yet)
    refused: that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again
    HTTP/1.1 200 OK
    (Ok ((proxy_cookie <opaque>) (token ()) (user ())))
    (grant.proxy_cookie proxy_session=fwd)
    connection refused
    |}]
;;

let%expect_test "the input ends: the listener still finishes" =
  with_sandbox
  @@ fun t ->
  ignore
    (with_signin t ~program:Web (fun ~port ~send ->
       send None;
       Eio.Fiber.yield ();
       let response =
         Fake_signin_proxy.request
           ~env:t.env
           ~port
           ~meth:"POST"
           ~headers:[ "Content-Type", "application/x-www-form-urlencoded" ]
           ~body:
             (Uri.encoded_of_query
                [ "state", [ Paste_vector.state ]; "cancel", [ "1" ] ])
           "/callback"
       in
       print_endline (List.hd_exn (String.split_lines response)))
     : int);
  [%expect
    {|
    https://ai.example.com/#cli-signin=PORT.0123456789abcdef0123456789abcdef&program=web&paste=B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw&from=build-box
    (stdin: <end>)
    HTTP/1.1 200 OK
    (Error "the sign-in was cancelled in the browser")
    |}]
;;

let%expect_test "on a terminal: the link, the ssh -L, the prompt, a refusal" =
  with_sandbox
  @@ fun t ->
  let paste, send = fake_stdin () in
  send (Some "prigh1:garbage");
  send (Some Paste_vector.code);
  let result =
    Browser_signin.run_on_terminal
      ~env:t.env
      ~program:Tool_host
      ~say:(fun text ->
        print_endline
          (Re.replace_string
             (Re.compile (Re.Perl.re "\\b[0-9]{4,5}\\b"))
             ~by:"PORT"
             text))
      ~state:Paste_vector.state
      ~paste
      ~why:"the backend wants a sign-in"
      remote
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  [%expect
    {|
    the backend wants a sign-in: sign in through the browser: open
    https://ai.example.com/#cli-signin=PORT.0123456789abcdef0123456789abcdef&program=tool-host&paste=B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw&from=build-box
    (Over SSH, forward the port to the browser's computer first: ssh -L PORT:127.0.0.1:PORT ...)
    Or paste the code the page shows after Allow:
    (stdin: prigh1:garbage)
    that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again. Paste the code:
    (stdin: prigh1:WGmv9FBUlzLLq...)
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    |}]
;;
