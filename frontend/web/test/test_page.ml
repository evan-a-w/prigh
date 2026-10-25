open! Core
open! Expect_test_helpers_core
open Prigh_web_app

(* The page's own modules (prigh_web_app): what it keeps in localStorage and
   the URLs it builds. *)

(* The web frontend runs under js_of_ocaml with 32-bit OCaml ints, so
   epoch-millisecond fields must not be decoded as [int]. [expires_ms] arrives
   from a real [auth.json] and used to throw, leaving the app stuck at
   "connecting…". *)
let%expect_test
    "protocol: epoch-millisecond fields decode on the 32-bit runtime"
  =
  let auth =
    Or_error.ok_exn
      (Prigh_protocol.Auth_status.of_json
         (Or_error.ok_exn
            (Prigh_protocol.Json.parse
               {|{"provider":"anthropic","name":"Anthropic","methods":[],"configured":{"method":"oauth","source":"auth.json"},"expires_ms":1789820702579}|})))
  in
  print_s [%sexp (auth.expires_ms : Int64.t option)];
  (* [int_field] stays total: an out-of-range value is an error, not a raise. *)
  (match
     Prigh_protocol.Json.int_field
       (Or_error.ok_exn (Prigh_protocol.Json.parse {|{"n":1789820702579}|}))
       "n"
   with
   | Ok n -> printf "unexpected int: %d\n" n
   | Error e -> printf "out of range: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    (1789820702579)
    out of range: field "n": integer "1789820702579" out of range
    |}]
;;

let%expect_test
    "web connection: the terminal URL and the session in the page URL"
  =
  let terminal backend ?user ?as_user ?token ?session () =
    Terminal_panel.url ~backend ~user ~as_user ~token ~session |> print_endline
  in
  terminal "ws://127.0.0.1:7777/ws" ();
  terminal "wss://host.example/ws?x=1" ~token:"sekrit&x=y" ~session:"abc" ();
  terminal "ws://host:9000/" ~session:"s" ();
  terminal "ws://host:9000/ws" ~user:"lloyd o'k" ~token:"pw" ~session:"s" ();
  terminal "ws://host:9000/ws" ~user:"s" ~as_user:"lloyd" ~token:"pw" ();
  let with_session search =
    Browser.with_query_param ~search "session" "a b&c" |> print_endline
  in
  with_session "";
  with_session "?backend=ws%3A%2F%2Fx&session=old&name=laptop";
  let without_session search =
    print_s [%sexp (Browser.without_query_param ~search "session" : string)]
  in
  without_session "";
  without_session "?session=abc";
  without_session "?backend=ws%3A%2F%2Fx&session=old&name=laptop";
  [%expect
    {|
    ws://127.0.0.1:7777/terminal
    wss://host.example/terminal?token=sekrit%26x%3Dy&session=abc
    ws://host:9000/terminal?session=s
    ws://host:9000/terminal?user=lloyd%20o'k&token=pw&session=s
    ws://host:9000/terminal?user=s&as_user=lloyd&token=pw
    ?session=a%20b%26c
    ?backend=ws%3A%2F%2Fx&name=laptop&session=a%20b%26c
    ""
    ""
    ?backend=ws%3A%2F%2Fx&name=laptop
    |}]
;;

let%expect_test "page URL: switching backend keeps the other parameters" =
  let href search =
    Browser.href_with_backend
      ~pathname:"/ui/"
      ~search
      ~backend:"ws://server.example:9000/ws?x=1&y=2"
    |> print_endline
  in
  href "";
  href
    "?token=sekrit%26x%3Dy&session=abc&backend=ws%3A%2F%2Fold%2Fws&name=laptop";
  [%expect
    {|
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2&session=abc&name=laptop
    |}]
;;

let%expect_test "login: user name and password in localStorage, signing out" =
  let items = String.Table.create () in
  let storage : Login.Storage.t =
    { get = Hashtbl.find items
    ; set = (fun key data -> Hashtbl.set items ~key ~data)
    ; remove = Hashtbl.remove items
    }
  in
  let show () =
    let login = Login.load storage in
    print_s
      [%message
        ""
          ~stored:
            (Hashtbl.to_alist items
             |> List.sort ~compare:[%compare: string * string]
             : (string * string) list)
          (login : Login.t)
          ~hello:
            (List.map (Login.hello_fields login) ~f:(fun (k, v) ->
               k, Prigh_protocol.Json.to_string v)
             : (string * string) list)]
  in
  show ();
  (* A token saved before user names existed is still the password. *)
  Hashtbl.set items ~key:"prigh.token" ~data:"old-token";
  show ();
  Login.save storage ~user:" lloyd " ~password:" sekrit\t";
  show ();
  (* No user name: fine outside namespace mode. *)
  Login.save storage ~user:"" ~password:"sekrit";
  show ();
  Login.save storage ~user:"lloyd" ~password:"sekrit";
  Hashtbl.set items ~key:"prigh.history.abc" ~data:"[]";
  Login.forget storage;
  show ();
  print_s [%sexp (Login.take_signed_out storage : bool)];
  print_s [%sexp (Login.take_signed_out storage : bool)];
  show ();
  [%expect
    {|
    ((stored ())
     (login (
       (user     ())
       (password ())))
     (hello ()))
    ((stored ((prigh.token old-token)))
     (login ((user ()) (password (old-token))))
     (hello ((token "\"old-token\""))))
    ((stored (
       (prigh.token sekrit)
       (prigh.user  lloyd)))
     (login (
       (user     (lloyd))
       (password (sekrit))))
     (hello (
       (user  "\"lloyd\"")
       (token "\"sekrit\""))))
    ((stored ((prigh.token sekrit)))
     (login ((user ()) (password (sekrit))))
     (hello ((token "\"sekrit\""))))
    ((stored (
       (prigh.history.abc [])
       (prigh.signed_out  1)))
     (login (
       (user     ())
       (password ())))
     (hello ()))
    true
    false
    ((stored ((prigh.history.abc [])))
     (login (
       (user     ())
       (password ())))
     (hello ()))
    |}]
;;

let%expect_test
    "preferences: a page from before's keys move into one cache, which only \
     its owner gets back"
  =
  let items = String.Table.create () in
  let storage : Login.Storage.t =
    { get = Hashtbl.find items
    ; set = (fun key data -> Hashtbl.set items ~key ~data)
    ; remove = Hashtbl.remove items
    }
  in
  let stored () =
    print_s
      [%sexp
        (Hashtbl.to_alist items |> List.sort ~compare:[%compare: string * string]
         : (string * string) list)]
  in
  let load owner =
    print_s
      [%sexp
        (Pref_cache.load storage ~owner
         : Prigh_protocol.Prefs.Change.t list * bool)]
  in
  List.iter
    [ "prigh-web.notify", "off"
    ; "prigh-web.history", {|["newest","older"]|}
    ; "prigh-web.sidebar-width", "320"
    ; "prigh-web.terminal-open", "1"
    ; "prigh-web.files-width", "wide"
    ; "prigh.token", "sekrit"
    ]
    ~f:(fun (key, data) -> Hashtbl.set items ~key ~data);
  load "ws://h/ws alice";
  stored ();
  [%expect
    {|
    ((((key web.notify) (value (false)))
      ((key history) (value ((newest older))))
      ((key web.terminal-open) (value (true)))
      ((key web.sidebar-width) (value (320))))
     true)
    ((prigh-web.prefs
      "{\"owner\":\"ws://h/ws alice\",\"legacy\":true,\"prefs\":{\"web.notify\":false,\"history\":[\"newest\",\"older\"],\"web.terminal-open\":true,\"web.sidebar-width\":320}}")
     (prigh.token sekrit))
    |}];
  (* Saved: no longer legacy; another account's are not restored. *)
  Pref_cache.save
    storage
    ~owner:"ws://h/ws alice"
    [ { key = "web.notify"; value = None }
    ; { key = "web.theme"; value = Some (`String "dark") }
    ];
  load "ws://h/ws alice";
  load "ws://h/ws bob";
  Pref_cache.save_theme storage { name = Light; accent = Custom "#123456" };
  print_endline (Hashtbl.find_exn items Pref_cache.theme_key);
  [%expect {|
    ((((key history) (value ((newest older))))
      ((key web.terminal-open) (value (true)))
      ((key web.sidebar-width) (value (320)))
      ((key web.theme)         (value (dark))))
     false)
    (() false)
    {"attrs":{"data-theme":"light","data-accent":"custom"},"vars":{"--sodium":"#123456","--sodium-ink":"#ffffff"}}
    |}]
;;
