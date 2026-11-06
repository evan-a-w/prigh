open! Core
open! Prigh
open Tool_test_helpers

(* Signing a program in through the browser when a sign-in proxy guards the
   backend: the server's grant, the program's one-shot listener, and how a
   refused WebSocket is recognised as the proxy's login. *)

let remote = Backend_address.parse "https://ai.example.com/" |> Or_error.ok_exn

(* The status line and, for a page, its text without the markup. *)
let show_response response =
  let head, body =
    match String.substr_index response ~pattern:"\r\n\r\n" with
    | Some i -> String.prefix response i, String.drop_prefix response (i + 4)
    | None -> response, ""
  in
  let status = List.hd_exn (String.split_lines head) in
  let text =
    Re.replace_string (Re.compile (Re.Perl.re "<[^>]*>")) ~by:" " body
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.filter ~f:(fun l -> not (String.is_prefix l ~prefix:"prigh:"))
    |> String.concat ~sep:" | "
  in
  printf "%s\n  %s\n" status text
;;

let form fields =
  Uri.encoded_of_query (List.map fields ~f:(fun (k, v) -> k, [ v ]))
;;

let post_form t ~port fields =
  Fake_signin_proxy.request
    ~env:t.env
    ~port
    ~meth:"POST"
    ~headers:[ "Content-Type", "application/x-www-form-urlencoded" ]
    ~body:(form fields)
    "/callback"
;;

(* Runs the listener with a known state; [browser] gets the port and acts as
   the browser would; prints the URL and what [run] returned. *)
let with_listener t ~program browser =
  let port = Eio.Promise.create () in
  let result =
    Eio.Fiber.pair
      (fun () ->
         Browser_signin.run
           ~env:t.env
           ~state:"0123abcd"
           ~program
           remote
           ~on_url:(fun ~url ~port:p ->
             print_endline
               (String.substr_replace_all
                  url
                  ~pattern:(Int.to_string p)
                  ~with_:"PORT");
             Eio.Promise.resolve (snd port) p))
      (fun () -> browser (Eio.Promise.await (fst port)))
    |> fst
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  Eio.Promise.await (fst port)
;;

let%expect_test
    "listener: wrong requests are answered and it keeps waiting; the right one \
     ends it"
  =
  with_sandbox
  @@ fun t ->
  let port =
    with_listener t ~program:Tool_host (fun port ->
      let get target =
        show_response
          (Fake_signin_proxy.request ~env:t.env ~port ~meth:"GET" target)
      in
      get "/callback";
      get "/";
      show_response (post_form t ~port [ "state", "nope"; "cookie", "a=b" ]);
      show_response (post_form t ~port [ "cookie", "a=b" ]);
      show_response
        (post_form
           t
           ~port
           [ "state", "0123abcd"
           ; "cookie", "proxy_session=x; other=y+z"
           ; "token", "sekrit"
           ; "user", "me"
           ]))
  in
  (* It closed after the sign-in. *)
  (match Fake_signin_proxy.request ~env:t.env ~port ~meth:"GET" "/callback" with
   | response -> show_response response
   | exception exn -> print_endline (Net_error.to_string_hum exn));
  [%expect
    {|
    https://ai.example.com/#cli-signin=PORT.0123abcd&program=tool-host
    HTTP/1.1 405 Method Not Allowed
      Not here | This address only takes a sign-in from prigh-web's page: open the link prigh tool-host printed.
    HTTP/1.1 404 Not Found
      Not here | This address only takes a sign-in from prigh-web's page: open the link prigh tool-host printed.
    HTTP/1.1 403 Forbidden
      Not this sign-in | prigh tool-host is not waiting for this sign-in (a link from an earlier one?): start the sign-in again from prigh tool-host.
    HTTP/1.1 403 Forbidden
      Not this sign-in | prigh tool-host is not waiting for this sign-in (a link from an earlier one?): start the sign-in again from prigh tool-host.
    HTTP/1.1 200 OK
      Signed in | prigh tool-host on this computer is signed in to ai.example.com. You can close this tab.
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (me))))
    connection refused
    |}]
;;

let%expect_test "listener: the grant's fields; cancel; no cookie" =
  with_sandbox
  @@ fun t ->
  let grant fields =
    ignore
      (with_listener t ~program:Tui (fun port ->
         let response = post_form t ~port fields in
         show_response response)
       : int)
  in
  ignore
    (with_listener t ~program:Web (fun port ->
       show_response
         (post_form
            t
            ~port
            [ "state", "0123abcd"; "cookie", "proxy_session=a+b%; x=1" ]))
     : int);
  grant [ "state", "0123abcd"; "cancel", "1"; "cookie", "a=b" ];
  grant [ "state", "0123abcd" ];
  [%expect
    {|
    https://ai.example.com/#cli-signin=PORT.0123abcd&program=web
    HTTP/1.1 200 OK
      Signed in | prigh web on this computer is signed in to ai.example.com. You can close this tab.
    (Ok ((proxy_cookie <opaque>) (token ()) (user ())))
    https://ai.example.com/#cli-signin=PORT.0123abcd&program=tui
    HTTP/1.1 200 OK
      Not signed in | You cancelled: prigh-tui was not signed in. You can close this tab.
    (Error "the sign-in was cancelled in the browser")
    https://ai.example.com/#cli-signin=PORT.0123abcd&program=tui
    HTTP/1.1 400 Bad Request
      Not signed in | The page sent no sign-in cookie, so prigh-tui was not signed in: start the sign-in again from prigh-tui.
    (Error "the page sent no sign-in cookie")
    |}]
;;

let%expect_test "listener: a fresh random state, bound to the port" =
  with_sandbox
  @@ fun t ->
  let started = Eio.Promise.create () in
  let result, () =
    Eio.Fiber.pair
      (fun () ->
         Browser_signin.run
           ~env:t.env
           ~program:Desktop
           remote
           ~on_url:(fun ~url ~port ->
             let fragment = snd (String.lsplit2_exn url ~on:'#') in
             let value =
               String.chop_prefix_exn fragment ~prefix:"cli-signin="
               |> String.lsplit2_exn ~on:'&'
               |> fst
             in
             let p, state = String.lsplit2_exn value ~on:'.' in
             printf
               "port in the URL is the listener's: %b; state: %d hex digits\n"
               (Int.equal (Int.of_string p) port)
               (String.length state);
             Eio.Promise.resolve (snd started) (port, state)))
      (fun () ->
         let port, state = Eio.Promise.await (fst started) in
         show_response
           (post_form t ~port [ "state", "0123abcd"; "cookie", "a=b" ]);
         show_response (post_form t ~port [ "state", state; "cookie", "a=b" ]))
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  [%expect
    {|
    port in the URL is the listener's: true; state: 32 hex digits
    HTTP/1.1 403 Forbidden
      Not this sign-in | prigh desktop is not waiting for this sign-in (a link from an earlier one?): start the sign-in again from prigh desktop.
    HTTP/1.1 200 OK
      Signed in | prigh desktop on this computer is signed in to ai.example.com. You can close this tab.
    (Ok ((proxy_cookie <opaque>) (token ()) (user ())))
    |}]
;;

let%expect_test "the sign-in page: a proxy's login, not prigh's errors" =
  let check address status headers =
    let remote = Backend_address.parse address |> Or_error.ok_exn in
    printf
      "%s %s: %s\n"
      address
      status
      (Option.value
         (Backend_address.signin_page remote { status; headers })
         ~default:"(not a sign-in)")
  in
  let authentik =
    [ "Location", "https://auth.example.com/outpost.goauthentik.io/start?rd=x" ]
  in
  check "wss://ai.example.com/ws" "302 Found" authentik;
  check "wss://ai.example.com/ws" "401 Unauthorized" [];
  check "wss://ai.example.com/ws" "200 OK" [ "Content-Type", "text/html" ];
  check
    "ws://ai.example.com/ws"
    "301 Moved Permanently"
    [ "Location", "https://ai.example.com/ws" ];
  check
    "wss://ai.example.com/ws"
    "404 Not Found"
    [ "Content-Type", "text/html" ];
  check
    "wss://ai.example.com/ws"
    "502 Bad Gateway"
    [ "Content-Type", "text/html" ];
  check "wss://ai.example.com/ws" "200 OK" [];
  [%expect
    {|
    wss://ai.example.com/ws 302 Found: ai.example.com answered with a login page (HTTP 302 Found to https://auth.example.com/outpost.goauthentik.io/start?rd=x)
    wss://ai.example.com/ws 401 Unauthorized: ai.example.com refused the WebSocket (HTTP 401 Unauthorized)
    wss://ai.example.com/ws 200 OK: ai.example.com answered with a web page (HTTP 200 OK) instead of opening the WebSocket
    ws://ai.example.com/ws 301 Moved Permanently: (not a sign-in)
    wss://ai.example.com/ws 404 Not Found: (not a sign-in)
    wss://ai.example.com/ws 502 Bad Gateway: (not a sign-in)
    wss://ai.example.com/ws 200 OK: (not a sign-in)
    |}]
;;

(* The server's half, as [prigh serve -web] answers it. *)
let%expect_test "grant: the cookie only for prigh's own signed-in page" =
  let authenticate ~token ~user =
    match token, user with
    | Some "sekrit", (None | Some "me") -> Ok ()
    | _ -> Error User_access.unauthorised
  in
  let grant
        ?(meth = "POST")
        ?(body = {|{"token":"sekrit","user":"me"}|})
        headers
    =
    let request =
      { Web_server.Request.meth
      ; target = Cli_signin_grant.path
      ; path = Cli_signin_grant.path
      ; query = []
      ; headers
      }
    in
    let response = Cli_signin_grant.respond ~authenticate request ~body in
    printf "%d %s\n" response.status (String.strip response.body)
  in
  let page =
    [ "Host", "ai.example.com"
    ; "Origin", "https://ai.example.com"
    ; "Content-Type", "application/json"
    ; "Cookie", "authentik_proxy_abc=xyz; other=1"
    ]
  in
  let without name =
    List.filter page ~f:(fun (n, _) -> not (String.equal n name))
  in
  let replace name value = without name @ [ name, value ] in
  print_endline "the page, signed in:";
  grant page;
  grant (page @ [ "Sec-Fetch-Site", "same-origin" ]);
  print_endline "behind a proxy that rewrites Host:";
  grant (replace "Host" "127.0.0.1:7790");
  grant
    (replace "Host" "127.0.0.1:7790" @ [ "X-Forwarded-Host", "ai.example.com" ]);
  grant (replace "Host" "127.0.0.1:7790" @ [ "Sec-Fetch-Site", "same-origin" ]);
  print_endline "another site's page, or not a browser's page:";
  grant (replace "Origin" "https://evil.example.com");
  grant (page @ [ "Sec-Fetch-Site", "cross-site" ]);
  grant (page @ [ "Sec-Fetch-Site", "same-site" ]);
  grant (without "Origin");
  grant (replace "Content-Type" "text/plain");
  grant (replace "Origin" "null");
  print_endline "not signed in to prigh:";
  grant ~body:{|{"token":"wrong"}|} page;
  grant ~body:{|{"token":"sekrit","user":"alice"}|} page;
  grant ~body:{|{}|} page;
  grant ~body:"nonsense" page;
  print_endline "no cookies, a GET:";
  grant (without "Cookie");
  grant ~meth:"GET" page;
  [%expect
    {|
    the page, signed in:
    200 {"cookie":"authentik_proxy_abc=xyz; other=1"}
    200 {"cookie":"authentik_proxy_abc=xyz; other=1"}
    behind a proxy that rewrites Host:
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    200 {"cookie":"authentik_proxy_abc=xyz; other=1"}
    200 {"cookie":"authentik_proxy_abc=xyz; other=1"}
    another site's page, or not a browser's page:
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    403 only prigh-web's own page may ask for this: open the sign-in link in the browser (if a proxy in front of prigh rewrites Host, have it pass the site's host as Host or X-Forwarded-Host)
    not signed in to prigh:
    401 unauthorised: bad user name or password
    401 unauthorised: bad user name or password
    401 unauthorised: bad user name or password
    400 expected {"token", "user"}
    no cookies, a GET:
    422 no cookies reached prigh with this request: the program only needs this when a sign-in proxy guards the site, and then the proxy must pass the Cookie header on (nginx and Caddy do by default)
    405 method not allowed
    |}]
;;
