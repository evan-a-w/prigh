open! Core
open! Expect_test_helpers_core
open Prigh_ui
module Key_of_dom = Prigh_ui_web.Key_of_dom
module Dom_of_screen = Prigh_ui_web.Dom_of_screen
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

(* The web frontend runs under js_of_ocaml with 32-bit OCaml ints, so
   epoch-millisecond fields must not be decoded as [int]. [expires_ms] arrives
   from a real [auth.json] and used to throw, leaving the app stuck at
   "connecting…". *)
let%expect_test "protocol: epoch-millisecond fields decode on the 32-bit \
                 runtime"
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

let%expect_test "web connection: same-origin is stable unless explicitly \
                 overridden"
  =
  let choose ?query () =
    Prigh_ui_web_app.Web_app.For_testing.choose_backend
      ~query
      ~same_origin:"ws://127.0.0.1:7777/ws"
    |> print_endline
  in
  choose ();
  choose ~query:"" ();
  choose ~query:"ws://server.example:9000/ws" ();
  let href search =
    Prigh_ui_web_app.Web_app.For_testing.href_with_backend
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
    ws://127.0.0.1:7777/ws
    ws://127.0.0.1:7777/ws
    ws://server.example:9000/ws
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2&session=abc&name=laptop
    |}]
;;

let%expect_test "prompt history is stored per token, without the token" =
  (* FNV-1a reference values: "" 811c9dc5, "a" e40c292c, "foobar" bf9cf968. *)
  List.iter
    [ None
    ; Some ""
    ; Some "a"
    ; Some "foobar"
    ; Some "sekrit-token-1"
    ; Some "sekrit-token-2"
    ]
    ~f:(fun token ->
      let key = Prigh_ui_web_app.Web_app.For_testing.history_key ~token in
      print_s [%message (token : string option) key]);
  [%expect
    {|
    ((token ()) prigh.history)
    ((token ("")) prigh.history.811c9dc5)
    ((token (a)) prigh.history.e40c292c)
    ((token (foobar)) prigh.history.bf9cf968)
    ((token (sekrit-token-1)) prigh.history.8fcc21e3)
    ((token (sekrit-token-2)) prigh.history.90cc2376)
    |}]
;;

let%expect_test "web connection: the terminal URL and the session in the page \
                 URL"
  =
  let terminal backend ?user ?as_user ?token ?session () =
    Prigh_ui_web_app.Web_app.For_testing.terminal_url
      ~backend
      ~user
      ~as_user
      ~token
      ~session
    |> print_endline
  in
  terminal "ws://127.0.0.1:7777/ws" ();
  terminal "wss://host.example/ws?x=1" ~token:"sekrit&x=y" ~session:"abc" ();
  terminal "ws://host:9000/" ~session:"s" ();
  terminal "ws://host:9000/ws" ~user:"lloyd o'k" ~token:"pw" ~session:"s" ();
  terminal "ws://host:9000/ws" ~user:"s" ~as_user:"lloyd" ~token:"pw" ();
  let with_session search =
    Prigh_ui_web_app.Web_app.For_testing.with_query_param
      ~search
      "session"
      "a b&c"
    |> print_endline
  in
  with_session "";
  with_session "?backend=ws%3A%2F%2Fx&session=old&name=laptop";
  let without_session search =
    print_s
      [%sexp
        (Prigh_ui_web_app.Web_app.For_testing.without_query_param
           ~search
           "session"
         : string)]
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
    ?session=a%20b%26c
    ?backend=ws%3A%2F%2Fx&name=laptop&session=a%20b%26c
    ""
    ""
    ?backend=ws%3A%2F%2Fx&name=laptop
    |}]
;;

let%expect_test "login: user name and password in localStorage, signing out" =
  let module Login = Prigh_ui_web_app.Login in
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

let event
  ?(code = "")
  ?(ctrl = false)
  ?(alt = false)
  ?(shift = false)
  ?(meta = false)
  key
  : Key_of_dom.Event.t
  =
  { key; code; ctrl; alt; shift; meta }
;;

let%expect_test "key_of_dom: named keys, characters, modifiers and \
                 browser-owned combos"
  =
  let show e =
    printf
      "%-28s -> %s\n"
      (Sexp.to_string [%sexp (e : Key_of_dom.Event.t)])
      (match Key_of_dom.key e with
       | Some key -> Key.to_string key
       | None -> "(none)")
  in
  List.iter
    ~f:show
    [ event "Enter"
    ; event "Enter" ~alt:true
    ; event "Tab" ~shift:true
    ; event "Escape"
    ; event "Backspace"
    ; event "Delete"
    ; event "ArrowUp" ~ctrl:true
    ; event "PageDown"
    ; event "Home"
    ; event "F5"
    ; event "F13"
    ; event "a"
    ; event "A" ~shift:true
    ; event "é"
    ; event "€" ~alt:true ~code:"Digit5"
    ; event " "
    ; event "o" ~ctrl:true ~code:"KeyO"
    ; event "O" ~ctrl:true ~shift:true ~code:"KeyO"
    ; event "∫" ~alt:true ~code:"KeyB"
    ; event "¡" ~alt:true ~code:"Digit1"
    ; event "_" ~ctrl:true ~shift:true ~code:"Minus"
    ; event "Shift" ~shift:true
    ; event "Control" ~ctrl:true
    ; event "Dead" ~code:"Quote"
    ; event "v" ~ctrl:true ~code:"KeyV"
    ; event "V" ~ctrl:true ~shift:true ~code:"KeyV"
    ; event "Insert" ~shift:true
    ; event "r" ~meta:true ~code:"KeyR"
    ];
  [%expect
    {|
    ((key Enter)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> Enter
    ((key Enter)(code"")(ctrl false)(alt true)(shift false)(meta false)) -> Alt+Enter
    ((key Tab)(code"")(ctrl false)(alt false)(shift true)(meta false)) -> Shift+Tab
    ((key Escape)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> Esc
    ((key Backspace)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> Backspace
    ((key Delete)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> Delete
    ((key ArrowUp)(code"")(ctrl true)(alt false)(shift false)(meta false)) -> Ctrl+Up
    ((key PageDown)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> PageDown
    ((key Home)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> Home
    ((key F5)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> F5
    ((key F13)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> F13
    ((key a)(code"")(ctrl false)(alt false)(shift false)(meta false)) -> A
    ((key A)(code"")(ctrl false)(alt false)(shift true)(meta false)) -> Shift+A
    ((key"\195\169")(code"")(ctrl false)(alt false)(shift false)(meta false)) -> é
    ((key"\226\130\172")(code Digit5)(ctrl false)(alt true)(shift false)(meta false)) -> Alt+5
    ((key" ")(code"")(ctrl false)(alt false)(shift false)(meta false)) ->
    ((key o)(code KeyO)(ctrl true)(alt false)(shift false)(meta false)) -> Ctrl+O
    ((key O)(code KeyO)(ctrl true)(alt false)(shift true)(meta false)) -> Ctrl+Shift+O
    ((key"\226\136\171")(code KeyB)(ctrl false)(alt true)(shift false)(meta false)) -> Alt+B
    ((key"\194\161")(code Digit1)(ctrl false)(alt true)(shift false)(meta false)) -> Alt+1
    ((key _)(code Minus)(ctrl true)(alt false)(shift true)(meta false)) -> Ctrl+Shift+_
    ((key Shift)(code"")(ctrl false)(alt false)(shift true)(meta false)) -> (none)
    ((key Control)(code"")(ctrl true)(alt false)(shift false)(meta false)) -> (none)
    ((key Dead)(code Quote)(ctrl false)(alt false)(shift false)(meta false)) -> (none)
    ((key v)(code KeyV)(ctrl true)(alt false)(shift false)(meta false)) -> (none)
    ((key V)(code KeyV)(ctrl true)(alt false)(shift true)(meta false)) -> (none)
    ((key Insert)(code"")(ctrl false)(alt false)(shift true)(meta false)) -> (none)
    ((key r)(code KeyR)(ctrl false)(alt false)(shift false)(meta true)) -> (none)
    |}]
;;

(* Every keymap binding must be reachable from a browser event. *)
let%expect_test "key_of_dom: every keymap binding is producible" =
  let of_key (k : Key.t) : Key_of_dom.Event.t =
    let key, code =
      match k.code with
      | Enter -> "Enter", "Enter"
      | Tab -> "Tab", "Tab"
      | Escape -> "Escape", "Escape"
      | Backspace -> "Backspace", "Backspace"
      | Delete -> "Delete", "Delete"
      | Insert -> "Insert", "Insert"
      | Home -> "Home", "Home"
      | End -> "End", "End"
      | Up -> "ArrowUp", "ArrowUp"
      | Down -> "ArrowDown", "ArrowDown"
      | Left -> "ArrowLeft", "ArrowLeft"
      | Right -> "ArrowRight", "ArrowRight"
      | Page_up -> "PageUp", "PageUp"
      | Page_down -> "PageDown", "PageDown"
      | Function n -> sprintf "F%d" n, sprintf "F%d" n
      | Char c ->
        ( c
        , if String.length c = 1 && Char.is_alpha c.[0]
          then "Key" ^ String.uppercase c
          else if String.length c = 1 && Char.is_digit c.[0]
          then "Digit" ^ c
          else "" )
    in
    { key; code; ctrl = k.ctrl; alt = k.alt; shift = k.shift; meta = false }
  in
  let missing =
    List.concat_map Keymap.bindings ~f:(fun b -> b.keys)
    |> List.filter_map ~f:(fun k ->
      match Key_of_dom.key (of_key k) with
      | Some k' when Key.equal k k' -> None
      | Some k' ->
        Some (sprintf "%s -> %s" (Key.to_string k) (Key.to_string k'))
      | None -> Some (sprintf "%s -> (none)" (Key.to_string k)))
  in
  print_s [%sexp (missing : string list)];
  [%expect {| () |}]
;;

let html node =
  print_endline
    (Node_helpers.to_string_html (Node_helpers.unsafe_convert_exn node))
;;

let%expect_test "dom_of_screen: styles, links and the cursor cell" =
  let span ?(style = Style.plain) text = { Content.Span.text; style } in
  let screen : Screen.t =
    { width = 20
    ; height = 4
    ; cursor = Some (1, 4)
    ; lines =
        [ [ span ~style:(Style.bold (Style.fg Cyan)) "> "
          ; span "hello "
          ; span ~style:(Style.link Style.plain "https://x.test") "docs"
          ]
        ; [ span "> "; span ~style:(Style.fg Red) "wörld" ]
        ; []
        ; [ span
              ~style:(Style.dim (Style.invert (Style.strike Style.plain)))
              "x"
          ]
        ]
    }
  in
  html (Dom_of_screen.screen screen);
  [%expect
    {|
    <pre class="screen">
      <div class="line">
        <span class="bold fg-cyan"> >  </span>
        <span> hello  </span>
        <a href="https://x.test" target="_blank" rel="noopener"> docs </a>
      </div>
      <div class="line">
        <span> >  </span>
        <span class="fg-red"> wö </span>
        <span class="cursor fg-red"> r </span>
        <span class="fg-red"> ld </span>
      </div>
      <div class="line">   </div>
      <div class="line">
        <span class="dim invert strike"> x </span>
      </div>
    </pre>
    |}];
  (* At the start of a span, past the end of the line, on an empty line and
     below the last line. *)
  let at cursor =
    html (Dom_of_screen.screen { screen with cursor = Some cursor })
  in
  at (1, 2);
  [%expect
    {|
    <pre class="screen">
      <div class="line">
        <span class="bold fg-cyan"> >  </span>
        <span> hello  </span>
        <a href="https://x.test" target="_blank" rel="noopener"> docs </a>
      </div>
      <div class="line">
        <span> >  </span>
        <span class="cursor fg-red"> w </span>
        <span class="fg-red"> örld </span>
      </div>
      <div class="line">   </div>
      <div class="line">
        <span class="dim invert strike"> x </span>
      </div>
    </pre>
    |}];
  at (1, 9);
  [%expect
    {|
    <pre class="screen">
      <div class="line">
        <span class="bold fg-cyan"> >  </span>
        <span> hello  </span>
        <a href="https://x.test" target="_blank" rel="noopener"> docs </a>
      </div>
      <div class="line">
        <span> >  </span>
        <span class="fg-red"> wörld </span>
        <span>    </span>
        <span class="cursor">   </span>
      </div>
      <div class="line">   </div>
      <div class="line">
        <span class="dim invert strike"> x </span>
      </div>
    </pre>
    |}];
  at (2, 0);
  [%expect
    {|
    <pre class="screen">
      <div class="line">
        <span class="bold fg-cyan"> >  </span>
        <span> hello  </span>
        <a href="https://x.test" target="_blank" rel="noopener"> docs </a>
      </div>
      <div class="line">
        <span> >  </span>
        <span class="fg-red"> wörld </span>
      </div>
      <div class="line">
        <span class="cursor">   </span>
      </div>
      <div class="line">
        <span class="dim invert strike"> x </span>
      </div>
    </pre>
    |}];
  at (4, 0);
  [%expect
    {|
    <pre class="screen">
      <div class="line">
        <span class="bold fg-cyan"> >  </span>
        <span> hello  </span>
        <a href="https://x.test" target="_blank" rel="noopener"> docs </a>
      </div>
      <div class="line">
        <span> >  </span>
        <span class="fg-red"> wörld </span>
      </div>
      <div class="line">   </div>
      <div class="line">
        <span class="dim invert strike"> x </span>
      </div>
      <div class="line">
        <span class="cursor">   </span>
      </div>
    </pre>
    |}]
;;

let state_json =
  {|{"session_id":"abc123","session_path":"/home/u/.prigh/sessions/1.jsonl","session_name":null,"cwd":"/work","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":128000,"supports_thinking":true,"cost":{"input":10,"output":50,"cache_read":1}},"thinking":"off","running":false,"message_count":2,"usage":{"input":1200,"output":300,"cache_read":0},"cost_usd":0.0123,"context_tokens":1500,"active_host":"backend","hosts":[{"id":"backend","name":"srv","cwd":"/work"}]}|}
;;

(* The DOM's text is the pure renderer's text: the web view cannot drift from
   [Screen.to_plain]. ([inner_text] collapses whitespace and separates spans
   with spaces, so compare with spaces removed.) *)
let%expect_test "dom_of_screen: a rendered app screen round-trips to plain text"
  =
  let model, _ = App.update App.init (Resize { width = 60; height = 8 }) in
  let model, _ = App.update model Start in
  let model, _ =
    App.update
      model
      (Reply
         ( Initial_state
         , Ok (Or_error.ok_exn (Prigh_protocol.Json.parse state_json)) ))
  in
  let model, _ = App.update model (Key (Key.char 'h')) in
  let model, _ = App.update model (Key (Key.char 'i')) in
  let screen = Render.screen model in
  let node = Node_helpers.unsafe_convert_exn (Dom_of_screen.screen screen) in
  let collapse s =
    String.split_lines s
    |> List.map ~f:(String.filter ~f:(Char.( <> ) ' '))
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:"\n"
  in
  let dom_text =
    List.map
      (Node_helpers.select node ~selector:".line")
      ~f:Node_helpers.inner_text
    |> String.concat ~sep:"\n"
    |> collapse
  in
  let plain = Screen.to_plain screen in
  print_endline plain;
  printf
    "cursor: %s\n"
    (Sexp.to_string [%sexp (screen.cursor : (int * int) option)]);
  let cursor_line =
    List.findi (Node_helpers.select node ~selector:".line") ~f:(fun _ line ->
      Option.is_some (Node_helpers.select_first line ~selector:".cursor"))
  in
  printf
    "cursor row in DOM: %s\n"
    (Sexp.to_string [%sexp (Option.map cursor_line ~f:fst : int option)]);
  if String.equal dom_text (collapse plain)
  then print_endline "DOM text matches the plain screen"
  else print_endline dom_text;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    ────────────────────────────────────────────────────────────
    > hi
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    cursor: ((6 4))
    cursor row in DOM: (6)
    DOM text matches the plain screen
    |}]
;;

module Touch = Prigh_ui_web_app.Touch

let%expect_test "touch: taps focus, swipes scroll by whole steps and carry the \
                 remainder"
  =
  let step = 30. in
  let run label points =
    let g = Touch.start ~x:100. ~y:300. in
    let g, steps =
      List.fold points ~init:(g, []) ~f:(fun (g, acc) (x, y) ->
        let g, n = Touch.move g ~x ~y ~step in
        g, n :: acc)
    in
    printf
      "%-24s steps=%s -> %s\n"
      label
      (Sexp.to_string [%sexp (List.rev steps : int list)])
      (match Touch.finish g with
       | `Tap -> "tap"
       | `Swipe -> "swipe")
  in
  run "no movement" [];
  run "jitter under slop" [ 104., 296.; 99., 302. ];
  run "one row up" [ 100., 270. ];
  run
    "up in small increments"
    [ 100., 290.; 100., 280.; 100., 268.; 100., 240. ];
  run "down then up" [ 100., 360.; 100., 300. ];
  run "horizontal only" [ 150., 300. ];
  run "fast fling" [ 100., 100. ];
  [%expect
    {|
    no movement              steps=() -> tap
    jitter under slop        steps=(0 0) -> tap
    one row up               steps=(1) -> swipe
    up in small increments   steps=(0 0 1 1) -> swipe
    down then up             steps=(-2 2) -> swipe
    horizontal only          steps=(0) -> swipe
    fast fling               steps=(6) -> swipe
    |}]
;;

let%expect_test "key_of_dom: text committed by a virtual keyboard becomes keys" =
  List.iter [ "a"; "héllo"; "ok\n"; "🙂"; "" ] ~f:(fun text ->
    printf
      "%S -> %s\n"
      text
      (String.concat
         ~sep:" "
         (List.map (Key_of_dom.keys_of_text text) ~f:Key.to_string)));
  [%expect
    {|
    "a" -> A
    "h\195\169llo" -> H é L L O
    "ok\n" -> O K Enter
    "\240\159\153\130" -> 🙂
    "" ->
    |}]
;;
