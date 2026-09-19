open! Core
open! Expect_test_helpers_core
open Prigh_ui

let%expect_test "text width and wrapping" =
  let show s = printf "%S -> %d\n" s (Text_width.string s) in
  show "hello";
  show "héllo";
  show "日本語";
  show "🐹x";
  [%expect
    {|
    "hello" -> 5
    "h\195\169llo" -> 5
    "\230\151\165\230\156\172\232\170\158" -> 6
    "\240\159\144\185x" -> 3
    |}];
  let wrap s width =
    let lines = Content.Line.wrap (Content.Line.of_string s) ~width in
    List.iter lines ~f:(fun l -> printf "|%s|\n" (Content.Line.to_plain l))
  in
  wrap "the quick brown fox jumps over the lazy dog" 12;
  print_endline "--";
  wrap "supercalifragilisticexpialidocious yes" 10;
  print_endline "--";
  wrap "日本語のテキストを折り返す" 8;
  print_endline "--";
  wrap "" 5;
  [%expect
    {|
    |the quick |
    |brown fox |
    |jumps over |
    |the lazy dog|
    --
    |supercalif|
    |ragilistic|
    |expialidoc|
    |ious yes|
    --
    |日本語の|
    |テキスト|
    |を折り返|
    |す|
    --
    ||
    |}];
  print_endline (Text_width.truncate "a long line of text" ~width:8);
  print_endline
    (Content.Line.to_plain
       (Content.Line.truncate
          [ { text = "abc"; style = Style.plain }
          ; { text = "defgh"; style = Style.plain }
          ]
          ~width:6));
  [%expect {|
    a long …
    abcde…
    |}]
;;

let%expect_test "editor" =
  let show e =
    let p = Editor.position e in
    printf "%S cursor=%d:%d\n" (Editor.text e) p.line p.col
  in
  let e = Editor.insert Editor.empty "hello world" in
  show e;
  let e = Editor.kill_word e in
  show e;
  let e = Editor.insert e "there\nsecond" in
  show e;
  let e = Editor.home e |> Editor.left in
  show e;
  let e = Editor.backspace e in
  show e;
  let e = Editor.newline e in
  show e;
  let e = Option.value_exn (Editor.up e) in
  show e;
  let e = Editor.end_ e |> Editor.delete in
  show e;
  let e = Editor.insert e "é日" |> Editor.left in
  show e;
  let e = Editor.kill_to_end e in
  show e;
  let e = Editor.kill_line e in
  show e;
  [%expect
    {|
    "hello world" cursor=0:11
    "hello " cursor=0:6
    "hello there\nsecond" cursor=1:6
    "hello there\nsecond" cursor=0:11
    "hello ther\nsecond" cursor=0:10
    "hello ther\n\nsecond" cursor=1:0
    "hello ther\n\nsecond" cursor=0:0
    "hello ther\nsecond" cursor=0:10
    "hello ther\195\169\230\151\165\nsecond" cursor=0:11
    "hello ther\195\169\nsecond" cursor=0:11
    "\nsecond" cursor=0:0
    |}];
  (* history *)
  let _, e = Editor.submit (Editor.insert Editor.empty "first") in
  let _, e = Editor.submit (Editor.insert e "second") in
  let _, e = Editor.submit ~secret:true (Editor.insert e "secret") in
  let e = Editor.insert e "draft" in
  let e = Option.value_exn (Editor.history_prev e) in
  show e;
  let e = Option.value_exn (Editor.history_prev e) in
  show e;
  print_s [%sexp (Option.is_none (Editor.history_prev e) : bool)];
  let e = Option.value_exn (Editor.history_next e) in
  show e;
  let e = Option.value_exn (Editor.history_next e) in
  show e;
  [%expect
    {|
    "second" cursor=0:6
    "first" cursor=0:5
    true
    "second" cursor=0:6
    "draft" cursor=0:5
    |}]
;;

let%expect_test "fuzzy ranking" =
  let candidates =
    [ "Claude Fable 5"
    ; "Claude Fable 5.1"
    ; "GPT-5.5"
    ; "o4-mini"
    ; "DeepSeek V4.1 Flash"
    ; "gpt-5.5-fast"
    ]
  in
  List.iter
    [ "fable"; "fable 5.1"; "gpt"; "fl"; "5.5"; "zzz"; "" ]
    ~f:(fun query ->
      printf
        "%-10S -> %s\n"
        query
        (String.concat ~sep:" | " (Fuzzy.rank ~query candidates ~key:Fn.id)));
  [%expect
    {|
    "fable"    -> Claude Fable 5 | Claude Fable 5.1
    "fable 5.1" -> Claude Fable 5.1
    "gpt"      -> GPT-5.5 | gpt-5.5-fast
    "fl"       -> DeepSeek V4.1 Flash | Claude Fable 5 | Claude Fable 5.1
    "5.5"      -> GPT-5.5 | gpt-5.5-fast
    "zzz"      ->
    ""         -> Claude Fable 5 | Claude Fable 5.1 | GPT-5.5 | o4-mini | DeepSeek V4.1 Flash | gpt-5.5-fast
    |}]
;;

let%expect_test "fuzzy ranking: command names" =
  let names = List.map Commands.all ~f:(fun (c : Commands.Spec.t) -> c.name) in
  List.iter [ "mo"; "lo"; "s"; "sw"; "xyz" ] ~f:(fun query ->
    printf
      "%-4S -> %s\n"
      query
      (String.concat ~sep:" " (Fuzzy.rank ~query names ~key:Fn.id)));
  [%expect
    {|
    "mo" -> model
    "lo" -> login logout
    "s"  -> state switch sessions agents verbosity
    "sw" -> switch
    "xyz" ->
    |}]
;;

let%expect_test "picker" =
  let items =
    List.map [ "alpha"; "beta"; "gamma"; "delta" ] ~f:(fun l ->
      Picker.Item.create ~id:l ~marked:(String.equal l "gamma") l)
  in
  let p = Picker.create ~title:"T" items in
  let show p =
    printf
      "query=%S selected=%d visible=[%s]\n"
      (Picker.query p)
      (Picker.selected p)
      (String.concat
         ~sep:" "
         (List.map (Picker.visible p) ~f:(fun i -> i.label)))
  in
  show p;
  let step p intent =
    match Picker.handle p intent ~page:2 with
    | Continue p ->
      show p;
      p
    | Selected item ->
      printf "selected %s\n" item.id;
      p
    | Cancelled ->
      print_endline "cancelled";
      p
  in
  let p = step p Down in
  let p = step p Down in
  let p = step p Up in
  let p = step p (Insert "a") in
  let p = step p Down in
  let _ = step p Submit in
  let p = step p (Insert "zz") in
  let _ = step p Submit in
  let p = step p Backspace in
  let p = step p Backspace in
  let p = step p Kill_line in
  let p = step p Page_down in
  let _ = step p Cancel in
  ignore p;
  [%expect
    {|
    query="" selected=2 visible=[alpha beta gamma delta]
    query="" selected=3 visible=[alpha beta gamma delta]
    query="" selected=3 visible=[alpha beta gamma delta]
    query="" selected=2 visible=[alpha beta gamma delta]
    query="a" selected=0 visible=[alpha beta gamma delta]
    query="a" selected=1 visible=[alpha beta gamma delta]
    selected beta
    query="azz" selected=0 visible=[]
    cancelled
    query="az" selected=0 visible=[]
    query="a" selected=0 visible=[alpha beta gamma delta]
    query="" selected=2 visible=[alpha beta gamma delta]
    query="" selected=3 visible=[alpha beta gamma delta]
    cancelled
    |}]
;;

let%expect_test "commands" =
  List.iter
    [ "/model fable 5.1"
    ; "/help"
    ; "/"
    ; "  /switch  /tmp/a b.jsonl "
    ; "hello"
    ; "/x"
    ]
    ~f:(fun s -> print_s [%sexp (Commands.parse s : Commands.Parsed.t option)]);
  [%expect
    {|
    (((name model) (args (fable 5.1)) (rest "fable 5.1")))
    (((name help) (args ()) (rest "")))
    (((name "") (args ()) (rest "")))
    (((name switch) (args (/tmp/a b.jsonl)) (rest "/tmp/a b.jsonl")))
    ()
    (((name x) (args ()) (rest "")))
    |}];
  List.iter [ "/mo"; "/s"; "/se"; "/"; "/zz"; "/model x"; "x" ] ~f:(fun s ->
    print_s [%message s (Commands.complete s : Commands.Completion.t)]);
  [%expect
    {|
    (/mo ("Commands.complete s" (Unique "/model ")))
    (/s (
      "Commands.complete s" (
        Candidates (
          ((name sessions)
           (args "")
           (help "pick a saved session to switch to")
           (argument ()))
          ((name switch)
           (args [path])
           (help "switch to a saved session")
           (argument (Sessions)))
          ((name state)
           (args "")
           (help "show session state")
           (argument ()))))))
    (/se ("Commands.complete s" (Unique "/sessions ")))
    (/ (
      "Commands.complete s" (
        Candidates (
          ((name help)
           (args "")
           (help "show commands and keys")
           (argument ()))
          ((name model)
           (args [name|id|provider/id])
           (help "pick or switch the model")
           (argument (Model)))
          ((name login)
           (args "[provider] [api_key|oauth]")
           (help "log in to a provider")
           (argument (Login)))
          ((name logout)
           (args [provider])
           (help "remove a provider's stored credential")
           (argument (Logout)))
          ((name thinking)
           (args [off|on|low|high|max])
           (help "pick or set the thinking level")
           (argument (Thinking)))
          ((name verbosity)
           (args [quiet|normal|verbose])
           (help "set the transcript verbosity")
           (argument (Verbosity)))
          ((name auth)
           (args "")
           (help "show which providers are configured")
           (argument ()))
          ((name compact)
           (args "")
           (help "summarise older messages to free context")
           (argument ()))
          ((name new)
           (args "")
           (help "start a new session")
           (argument ()))
          ((name sessions)
           (args "")
           (help "pick a saved session to switch to")
           (argument ()))
          ((name agents)
           (args "")
           (help "focus a subagent")
           (argument ()))
          ((name switch)
           (args [path])
           (help "switch to a saved session")
           (argument (Sessions)))
          ((name cd)
           (args [path])
           (help "change the working directory")
           (argument (Path)))
          ((name fork)
           (args "")
           (help "fork the current session")
           (argument ()))
          ((name abort)
           (args "")
           (help "abort the current run")
           (argument ()))
          ((name state)
           (args "")
           (help "show session state")
           (argument ()))
          ((name clear)
           (args "")
           (help "clear the transcript")
           (argument ()))
          ((name quit)
           (args "")
           (help exit)
           (argument ()))))))
    (/zz ("Commands.complete s" Nothing))
    ("/model x" ("Commands.complete s" Nothing))
    (x ("Commands.complete s" Nothing))
    |}];
  List.iter [ "modle"; "hlep"; "sess"; "zzzz" ] ~f:(fun s ->
    print_s
      [%message
        s (Option.map (Commands.closest s) ~f:(fun c -> c.name) : string option)]);
  [%expect
    {|
    (modle ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (model)))
    (hlep ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (help)))
    (sess ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (sessions)))
    (zzzz ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" ()))
    |}]
;;

let%expect_test "model matching" =
  let m ?(provider = "anthropic") id name : Prigh_protocol.Model.t =
    { id
    ; provider
    ; key = provider ^ "/" ^ id
    ; name
    ; context_window = 1000
    ; max_output = 100
    ; supports_thinking = true
    ; cost = { input = 1.; output = 2.; cache_read = 0.1 }
    }
  in
  let models =
    [ m "claude-fable-5" "Claude Fable 5"
    ; m "claude-fable-5-1" "Claude Fable 5.1"
    ; m ~provider:"openai" "gpt-5.5" "GPT-5.5"
    ; m ~provider:"openai-codex" "gpt-5.5" "GPT-5.5"
    ; m ~provider:"deepseek" "deepseek-flash" "DeepSeek V4.1 Flash"
    ]
  in
  let show q =
    let r = Model_match.resolve models q in
    let names l = List.map l ~f:(fun (x : Prigh_protocol.Model.t) -> x.key) in
    printf
      "%-28S -> %s\n"
      q
      (match r with
       | Found x -> "found " ^ x.key
       | Ambiguous l -> "ambiguous " ^ String.concat ~sep:", " (names l)
       | Not_found l ->
         "not found; did you mean " ^ String.concat ~sep:", " (names l))
  in
  List.iter
    [ "anthropic/claude-fable-5-1"
    ; "claude fable 5.1"
    ; "CLAUDE-FABLE-5"
    ; "claude-fable"
    ; "gpt-5.5"
    ; "openai/gpt-5.5"
    ; "deep"
    ; "flash"
    ; "claud fabel 5"
    ; "zzz"
    ]
    ~f:show;
  [%expect
    {|
    "anthropic/claude-fable-5-1" -> found anthropic/claude-fable-5-1
    "claude fable 5.1"           -> found anthropic/claude-fable-5-1
    "CLAUDE-FABLE-5"             -> found anthropic/claude-fable-5
    "claude-fable"               -> ambiguous anthropic/claude-fable-5, anthropic/claude-fable-5-1
    "gpt-5.5"                    -> ambiguous openai/gpt-5.5, openai-codex/gpt-5.5
    "openai/gpt-5.5"             -> found openai/gpt-5.5
    "deep"                       -> found deepseek/deepseek-flash
    "flash"                      -> found deepseek/deepseek-flash
    "claud fabel 5"              -> ambiguous anthropic/claude-fable-5, anthropic/claude-fable-5-1
    "zzz"                        -> not found; did you mean openai/gpt-5.5, openai-codex/gpt-5.5, anthropic/claude-fable-5
    |}]
;;

let%expect_test "keymap: every binding resolves to its intent and is documented"
  =
  List.iter Keymap.bindings ~f:(fun b ->
    List.iter b.keys ~f:(fun key ->
      let intent = Keymap.lookup key in
      require
        (Option.value_map intent ~default:false ~f:(Intent.equal b.intent));
      printf
        "%-16s %s\n"
        (Key.to_string key)
        (Sexp.to_string (Intent.sexp_of_t b.intent))));
  print_s [%sexp (Keymap.lookup (Key.char 'x') : Intent.t option)];
  print_s [%sexp (Keymap.lookup (Key.alt (Char "5")) : Intent.t option)];
  print_s [%sexp (Keymap.lookup (Key.plain (Char "é")) : Intent.t option)];
  print_s [%sexp (Keymap.lookup (Key.plain (Function 5)) : Intent.t option)];
  print_s [%sexp (Keymap.lookup (Key.ctrl 'q') : Intent.t option)];
  [%expect
    {|
    Enter            Submit
    Alt+Enter        Newline
    Ctrl+J           Newline
    Esc              Cancel
    Tab              Complete
    Up               Up
    Down             Down
    Left             Left
    Right            Right
    Home             Home
    Ctrl+A           Home
    End              End
    Ctrl+E           End
    PageUp           Page_up
    PageDown         Page_down
    Backspace        Backspace
    Ctrl+H           Backspace
    Delete           Delete
    Ctrl+K           Kill_to_end
    Ctrl+U           Kill_line
    Ctrl+W           Kill_word
    Ctrl+L           Clear_screen
    Ctrl+O           Cycle_verbosity
    Shift+Tab        Next_agent
    Alt+1            (Focus_agent 1)
    Ctrl+C           Interrupt
    Ctrl+D           Force_quit
    ((Insert x))
    ((Focus_agent 5))
    ((Insert "\195\169"))
    ()
    ()
    |}];
  print_endline (Content.to_plain Keymap.help);
  print_endline (Content.to_plain Commands.help);
  [%expect
    {|
    Enter               send the prompt / accept the highlighted item
    Alt+Enter / Ctrl+J  insert a newline in the editor
    Esc                 close the dialog, or abort the running turn
    Tab                 complete a slash command / open the command picker
    Up                  move up (editor line, history, or list row)
    Down                move down (editor line, history, or list row)
    Left                move the cursor left
    Right               move the cursor right
    Home / Ctrl+A       start of line
    End / Ctrl+E        end of line
    PageUp              scroll the transcript / list up a page
    PageDown            scroll the transcript / list down a page
    Backspace / Ctrl+H  delete the character before the cursor
    Delete              delete the character under the cursor
    Ctrl+K              delete to end of line
    Ctrl+U              delete the whole line
    Ctrl+W              delete the word before the cursor
    Ctrl+L              clear the transcript
    Ctrl+O              cycle transcript verbosity (quiet / normal / verbose)
    Shift+Tab           cycle focus: main → agent 1 → … → main
    Alt+1               focus agent N (Alt+1…9)
    Ctrl+C              clear the editor, then (again) quit
    Ctrl+D              quit
    /help                              show commands and keys
    /model [name|id|provider/id]       pick or switch the model
    /login [provider] [api_key|oauth]  log in to a provider
    /logout [provider]                 remove a provider's stored credential
    /thinking [off|on|low|high|max]    pick or set the thinking level
    /verbosity [quiet|normal|verbose]  set the transcript verbosity
    /auth                              show which providers are configured
    /compact                           summarise older messages to free context
    /new                               start a new session
    /sessions                          pick a saved session to switch to
    /agents                            focus a subagent
    /switch [path]                     switch to a saved session
    /cd [path]                         change the working directory
    /fork                              fork the current session
    /abort                             abort the current run
    /state                             show session state
    /clear                             clear the transcript
    /quit                              exit
    |}]
;;

let%expect_test "markdown" =
  print_endline
    (Content.to_plain
       (Markdown.render
          "# Title\n\
           Some `code` and **bold** text.\n\
           - item one\n\
          \  - nested\n\
           ```ocaml\n\
           let x = 1\n\
           ```\n\
           plain"));
  print_s [%sexp (Markdown.render "a `b` **c**" : Content.t)];
  [%expect
    {|
    Title
    Some code and bold text.
    • item one
      • nested
    ``` ocaml
      let x = 1
    ```
    plain
    ((
      ((text "a ")
       (style (
         (fg        Default)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false))))
      ((text b)
       (style (
         (fg        Cyan)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false))))
      ((text " ")
       (style (
         (fg        Default)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false))))
      ((text c)
       (style (
         (fg        Default)
         (bold      true)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false))))))
    |}]
;;

let%expect_test "live tool tail keeps the last five lines at each verbosity" =
  let call : Prigh_protocol.Tool_call.t =
    { id = "c1"; name = "bash"; arguments = {|{"command":"seq 7"}|} }
  in
  let t = Transcript.add_tool Transcript.empty call in
  let t =
    List.fold
      (List.init 7 ~f:(fun i -> sprintf "line %d\n" i))
      ~init:t
      ~f:(fun t chunk -> Transcript.append_tool_output t ~call_id:"c1" chunk)
  in
  let tool =
    List.find_exn (Transcript.items t) ~f:(function
      | Transcript.Item.Tool _ -> true
      | _ -> false)
  in
  List.iter [ Verbosity.Quiet; Normal; Verbose ] ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    print_endline (Content.to_plain (Transcript.render_item tool ~verbosity)));
  [%expect
    {|
    == quiet ==
    ⚙ bash seq 7 …
    == normal ==
    ⚙ bash command=seq 7
      line 6
    == verbose ==
    ⚙ bash
      {
        command: "seq 7"
      }
      line 2
      line 3
      line 4
      line 5
      line 6
    |}]
;;
