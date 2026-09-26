open! Core
open! Expect_test_helpers_core
open Prigh_ui
module Key_of_dom = Prigh_ui_web.Key_of_dom
module Dom_of_screen = Prigh_ui_web.Dom_of_screen
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

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
  href "?token=sekrit%26x%3Dy&session=abc&name=laptop";
  [%expect
    {|
    ws://127.0.0.1:7777/ws
    ws://127.0.0.1:7777/ws
    ws://server.example:9000/ws
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2
    /ui/?backend=ws%3A%2F%2Fserver.example%3A9000%2Fws%3Fx%3D1%26y%3D2&token=sekrit%26x%3Dy&session=abc&name=laptop
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
