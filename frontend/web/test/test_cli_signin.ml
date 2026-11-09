open! Core
open Prigh_web

let state = "0123456789abcdef0123456789abcdef"

let%expect_test
    "the request in the fragment: only a loopback port and a hex state"
  =
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
            (Cli_signin.Request.of_string
               (Cli_signin.Request.to_string request))
            (Some request))));
  [%expect
    {|
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=tui
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Tui)
        (paste ())))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=tool-host
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Tool_host)
        (paste ())))))
    (#program=web&cli-signin=1.0123456789abcdef0123456789abcdef
     (request
      (((port 1) (state 0123456789abcdef0123456789abcdef) (program Web)
        (paste ())))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef&program=evil
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Other)
        (paste ())))))
    (#cli-signin=41234.0123456789abcdef0123456789abcdef
     (request
      (((port 41234) (state 0123456789abcdef0123456789abcdef) (program Other)
        (paste ())))))
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
      (Cli_signin.Request.of_hash
         (sprintf "#cli-signin=41234.%s&program=tui" state))
  in
  let other = { request with program = Other } in
  print_endline (Cli_signin.heading request);
  List.iter
    (Cli_signin.explanation
       request
       ~host:"prigh.example.com"
       ~user:(Some "alice"))
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
      print_s
        [%message
          body ~cookie:(Cli_signin.cookie_of_grant body : string option)]);
  print_endline (Cli_signin.callback_url request);
  print_s
    [%sexp
      (Cli_signin.callback_fields
         request
         (Allow { cookie = "proxy_session=abc"; token = Some "t"; user = None })
       : (string * string) list)];
  print_s
    [%sexp (Cli_signin.callback_fields request Cancel : (string * string) list)];
  [%expect
    {|
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
      print_s [%message (status : int) message (next : [ `Sign_in | `Stop ])]);
  [%expect
    {|
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

let key = "B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw"

let%expect_test "paste mode: the program's key and the computer's name" =
  List.iter
    [ sprintf
        "#cli-signin=41234.%s&program=tui&paste=%s&from=build-box"
        state
        key
    ; sprintf "#cli-signin=41234.%s&program=tui&paste=%s" state key
    ; sprintf "#cli-signin=41234.%s&program=tui&paste=%s&from=a%%20b" state key
    ; sprintf
        "#cli-signin=41234.%s&program=tui&paste=%s&from=%s"
        state
        key
        (String.make 65 'h')
    ; sprintf "#cli-signin=41234.%s&program=tui&paste=short" state
    ; sprintf
        "#cli-signin=41234.%s&program=tui&paste=%s!"
        state
        (String.prefix key 42)
    ]
    ~f:(fun hash ->
      let request = Option.value_exn (Cli_signin.Request.of_hash hash) in
      print_s [%sexp (request.paste : Cli_signin.Paste.t option)];
      assert (
        [%equal: Cli_signin.Request.t option]
          (Cli_signin.Request.of_string (Cli_signin.Request.to_string request))
          (Some request)));
  [%expect
    {|
    (((key B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw) (from (build-box))))
    (((key B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw) (from ())))
    (((key B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw) (from ())))
    (((key B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw) (from ())))
    ()
    ()
    |}]
;;

(* The page as text: paragraphs on lines, buttons in brackets with their
   ids, the code's box in braces. *)
let show_page html =
  let replace re ~f s = Re.replace (Re.compile (Re.Perl.re re)) ~f s in
  html
  |> replace {|<button[^>]*id="([^"]*)"([^>]*)>([^<]*)</button>|} ~f:(fun g ->
    sprintf
      "[%s #%s%s]"
      (Re.Group.get g 3)
      (Re.Group.get g 1)
      (if String.is_substring (Re.Group.get g 2) ~substring:"disabled"
       then " (disabled)"
       else ""))
  |> replace {|<textarea[^>]*>([^<]*)</textarea>|} ~f:(fun g ->
    sprintf "{%s}\n" (Re.Group.get g 1))
  |> replace {|<p class="([^"]*)"[^>]*>([^<]*)</p>|} ~f:(fun g ->
    match Re.Group.get g 2 with
    | "" -> ""
    | text ->
      sprintf
        "\n%s%s\n"
        (if String.is_prefix (Re.Group.get g 1) ~prefix:"signin-error"
         then "! "
         else "")
        text)
  |> replace {|<h1[^>]*>([^<]*)</h1>|} ~f:(fun g ->
    sprintf "# %s\n" (Re.Group.get g 1))
  |> replace {|<[^>]*>|} ~f:(fun _ -> "")
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let%expect_test "paste mode: asked, the code and Copy, no code, cancelled" =
  let request =
    Option.value_exn
      (Cli_signin.Request.of_hash
         (sprintf
            "#cli-signin=41234.%s&program=tui&paste=%s&from=build-box"
            state
            key))
  in
  let page view =
    print_endline "---";
    show_page
      (Cli_signin.html
         request
         ~host:"prigh.example.com"
         ~user:(Some "alice")
         view)
  in
  page (Ask { working = false; error = None });
  page (Ask { working = true; error = None });
  page (Code { code = "prigh1:CODE<&>"; copied = `Not_yet });
  page (Code { code = "prigh1:CODE"; copied = `Copied });
  page (Code { code = "prigh1:CODE"; copied = `Failed });
  page
    (No_code (Cli_signin.seal_error request ~secure:false ~name:"" ~message:""));
  page
    (No_code
       (Cli_signin.seal_error
          request
          ~secure:true
          ~name:"NotSupportedError"
          ~message:"Unrecognized name."));
  page Cancelled;
  print_endline
    (Cli_signin.paste_plaintext
       ~cookie:"proxy_session=s3cret"
       ~token:(Some "sekrit")
       ~user:None);
  [%expect
    {|
    ---
    prighsign-in
    # Allow prigh-tui to connect?
    prigh-tui on build-box wants to connect to prigh.example.com as alice.
    It gets this browser's sign-in to the site and your prigh sign-in. Allow it only if you just started it.
    [Allow #cli-signin-allow]
    [Cancel #cli-signin-cancel]
    ---
    prighsign-in
    # Allow prigh-tui to connect?
    prigh-tui on build-box wants to connect to prigh.example.com as alice.
    It gets this browser's sign-in to the site and your prigh sign-in. Allow it only if you just started it.
    [Allowing… #cli-signin-allow (disabled)]
    [Cancel #cli-signin-cancel]
    ---
    prighsign-in
    # Paste this into the terminal on build-box
    prigh-tui asked for it. The code works once, only for this sign-in.
    {prigh1:CODE&lt;&amp;&gt;}
    [Copy #cli-signin-copy]
    [Send to the forwarded port #cli-signin-send]
    ---
    prighsign-in
    # Paste this into the terminal on build-box
    prigh-tui asked for it. The code works once, only for this sign-in.
    {prigh1:CODE}
    ! Copied: paste it into the terminal and press Enter.
    [Copied #cli-signin-copy]
    [Send to the forwarded port #cli-signin-send]
    ---
    prighsign-in
    # Paste this into the terminal on build-box
    prigh-tui asked for it. The code works once, only for this sign-in.
    {prigh1:CODE}
    ! Could not copy: select the code and copy it yourself.
    [Copy #cli-signin-copy]
    [Send to the forwarded port #cli-signin-send]
    ---
    prighsign-in
    # This browser cannot make the code
    ! This page is not on https (or localhost), so the browser cannot make the code: open the site over https, or forward the port (ssh -L 41234:127.0.0.1:41234 when you connect) and choose Send to the forwarded port.
    [Send to the forwarded port #cli-signin-send]
    ---
    prighsign-in
    # This browser cannot make the code
    ! This browser cannot make the code (NotSupportedError: Unrecognized name.): use a recent Chrome, Firefox or Safari, or forward the port (ssh -L 41234:127.0.0.1:41234 when you connect) and choose Send to the forwarded port.
    [Send to the forwarded port #cli-signin-send]
    ---
    prighsign-in
    # Not allowed
    Nothing was sent. Stop prigh-tui in its terminal (Ctrl-C) if it is still waiting. You can close this tab.
    {"cookie":"proxy_session=s3cret","token":"sekrit","user":null}
    |}]
;;

let%expect_test "without paste: the page as before" =
  let request =
    Option.value_exn
      (Cli_signin.Request.of_hash
         (sprintf "#cli-signin=41234.%s&program=web" state))
  in
  show_page
    (Cli_signin.html
       request
       ~host:"prigh.example.com"
       ~user:None
       (Ask { working = false; error = Some "The server answered 500." }));
  [%expect
    {|
    prighsign-in
    # Allow prigh web to connect?
    prigh web on this computer wants to connect to prigh.example.com with this page's sign-in.
    It gets this browser's sign-in to the site and your prigh sign-in. Allow it only if you just started it.
    ! The server answered 500.
    [Allow #cli-signin-allow]
    [Cancel #cli-signin-cancel]
    |}]
;;
