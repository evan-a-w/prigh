open! Core
open Prigh_web

let state = "0123456789abcdef0123456789abcdef"

let%expect_test "the request in the fragment: only a loopback port and a hex state" =
  List.iter
    [ sprintf "#cli-signin=41234.%s&program=tui" state
    ; sprintf "#cli-signin=41234.%s&program=tool-host" state
    ; sprintf "#program=web&cli-signin=1.%s" state
    ; sprintf "#cli-signin=41234.%s&program=evil" state
    ; sprintf "#cli-signin=41234.%s" state
    ; sprintf "#cli-signin=0.%s" state
    ; sprintf "#cli-signin=65536.%s" state
    ; sprintf "#cli-signin=-1.%s" state
    ; "#cli-signin=41234.0123"
    ; sprintf "#cli-signin=41234.%sxyz" state
    ; sprintf "#cli-signin=41234.%s" (String.make 130 'a')
    ; sprintf "#cli-signin=1.2.%s" state
    ; "#signin=CODE"
    ; ""
    ]
    ~f:(fun hash ->
      let request = Cli_signin.Request.of_hash hash in
      print_s [%message hash (request : Cli_signin.Request.t option)];
      Option.iter request ~f:(fun request ->
        assert (
          [%equal: Cli_signin.Request.t option]
            (Cli_signin.Request.of_string (Cli_signin.Request.to_string request))
            (Some request))));
  [%expect {|
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=tui
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Tui)))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=tool-host
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef)
        (program Tool_host)))))
    (#program=web&cli-signin=1.0123456789abcdef0123456789abcdef
     (request
      (((port 1) (state 0123456789abcdef0123456789abcdef) (program Web)))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=evil
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Other)))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Other)))))
    (#cli-signin=0.0123456789abcdef0123456789abcdef (request ()))
    (#cli-signin=65536.0123456789abcdef0123456789abcdef (request ()))
    (#cli-signin=-1.0123456789abcdef0123456789abcdef (request ()))
    (#cli-signin=41234.0123 (request ()))
    (#cli-signin=41234.0123456789abcdef0123456789abcdefxyz (request ()))
    (#cli-signin=41234.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     (request ()))
    (#cli-signin=1.2.0123456789abcdef0123456789abcdef (request ()))
    (#signin=CODE (request ()))
    ("" (request ()))
    |}]
;;

let%expect_test "the confirmation, the grant, the form for the listener" =
  let request =
    Option.value_exn
      (Cli_signin.Request.of_hash (sprintf "#cli-signin=41234.%s&program=tui" state))
  in
  let other = { request with program = Other } in
  print_endline (Cli_signin.heading request);
  List.iter
    (Cli_signin.explanation request ~host:"prigh.example.com" ~user:(Some "alice"))
    ~f:print_endline;
  print_endline (Cli_signin.heading other);
  List.iter
    (Cli_signin.explanation other ~host:"prigh.example.com" ~user:None)
    ~f:print_endline;
  print_endline Cli_signin.grant_path;
  print_endline (Cli_signin.grant_body ~user:(Some "alice") ~token:(Some "t"));
  print_endline (Cli_signin.grant_body ~user:None ~token:None);
  List.iter
    [ {|{"cookie":"proxy_session=abc"}|}; {|{"cookie":""}|}; "{}"; "nope" ]
    ~f:(fun body ->
      print_s [%message body ~cookie:(Cli_signin.cookie_of_grant body : string option)]);
  print_endline (Cli_signin.callback_url request);
  print_s
    [%sexp
      (Cli_signin.callback_fields
         request
         (Allow { cookie = "proxy_session=abc"; token = Some "t"; user = None })
       : (string * string) list)];
  print_s
    [%sexp (Cli_signin.callback_fields request Cancel : (string * string) list)];
  [%expect {|
    Allow prigh-tui to connect?
    prigh-tui on this computer wants to connect to prigh.example.com as alice.
    It gets this browser's sign-in to the site and your prigh sign-in. Allow it only if you just started it.
    Allow a program to connect?
    A program on this computer wants to connect to prigh.example.com with this page's sign-in.
    It gets this browser's sign-in to the site and your prigh sign-in. Allow it only if you just started it.
    cli-signin/grant
    {"token":"t","user":"alice"}
    {"token":null,"user":null}
    ("{\"cookie\":\"proxy_session=abc\"}" (cookie (proxy_session=abc)))
    ("{\"cookie\":\"\"}" (cookie ()))
    ({} (cookie ()))
    (nope (cookie ()))
    http://127.0.0.1:41234/callback
    ((state 0123456789abcdef0123456789abcdef) (cookie proxy_session=abc)
     (token t) (user ""))
    ((state 0123456789abcdef0123456789abcdef) (cancel 1))
    |}]
;;

let%expect_test "what a refused grant says" =
  List.iter
    [ 401, "unauthorised: bad user name or password"
    ; 401, ""
    ; 403, "only prigh-web's own page can ask for the sign-in"
    ; 404, "not found"
    ; 422, ""
    ; 0, ""
    ]
    ~f:(fun (status, body) ->
      let message, next = Cli_signin.grant_error ~status ~body in
      print_s
        [%message
          (status : int) message (next : [ `Sign_in | `Stop ])]);
  [%expect {|
    ((status 401)
     "unauthorised: bad user name or password: sign in to prigh, then allow the program again."
     (next Sign_in))
    ((status 401)
     "prigh did not accept this page's sign-in: sign in to prigh, then allow the program again."
     (next Sign_in))
    ((status 403) "only prigh-web's own page can ask for the sign-in"
     (next Stop))
    ((status 404)
     "This prigh server cannot hand sign-ins to programs (it is older than the program): update it, or exempt /ws and /terminal from the sign-in proxy."
     (next Stop))
    ((status 422) "The server answered 422." (next Stop))
    ((status 0)
     "The server could not be reached: check the connection and try again."
     (next Stop))
    |}]
;;
