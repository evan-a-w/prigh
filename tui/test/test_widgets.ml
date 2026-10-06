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
  [%expect
    {|
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
  let e = Editor.kill_to_start e in
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

let%expect_test "editor: word navigation (ascii, unicode, punctuation)" =
  let walk label e ops =
    printf "== %s ==\n" label;
    ignore
      (List.fold ops ~init:e ~f:(fun e (name, f) ->
         let e = f e in
         let p = Editor.position e in
         printf "%-8s %S cursor=%d:%d\n" name (Editor.text e) p.line p.col;
         e))
  in
  walk
    "foo.bar(baz)"
    (Editor.set_text Editor.empty "foo.bar(baz)")
    [ "left", Editor.word_left
    ; "left", Editor.word_left
    ; "left", Editor.word_left
    ; "left", Editor.word_left
    ; "left", Editor.word_left
    ; "left", Editor.word_left
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ];
  walk
    "héllo wörld"
    (Editor.set_text Editor.empty "héllo wörld")
    [ "left", Editor.word_left
    ; "left", Editor.word_left
    ; "right", Editor.word_right
    ; "right", Editor.word_right
    ];
  walk
    "delete forward"
    (Editor.home (Editor.set_text Editor.empty "foo.bar(baz)"))
    [ "del word", Editor.delete_word_forward
    ; "del word", Editor.delete_word_forward
    ];
  [%expect
    {|
    == foo.bar(baz) ==
    left     "foo.bar(baz)" cursor=0:11
    left     "foo.bar(baz)" cursor=0:8
    left     "foo.bar(baz)" cursor=0:7
    left     "foo.bar(baz)" cursor=0:4
    left     "foo.bar(baz)" cursor=0:3
    left     "foo.bar(baz)" cursor=0:0
    right    "foo.bar(baz)" cursor=0:3
    right    "foo.bar(baz)" cursor=0:4
    right    "foo.bar(baz)" cursor=0:7
    right    "foo.bar(baz)" cursor=0:8
    right    "foo.bar(baz)" cursor=0:11
    right    "foo.bar(baz)" cursor=0:12
    == héllo wörld ==
    left     "h\195\169llo w\195\182rld" cursor=0:6
    left     "h\195\169llo w\195\182rld" cursor=0:0
    right    "h\195\169llo w\195\182rld" cursor=0:5
    right    "h\195\169llo w\195\182rld" cursor=0:11
    == delete forward ==
    del word ".bar(baz)" cursor=0:0
    del word "bar(baz)" cursor=0:0
    |}]
;;

let%expect_test "editor: kill ring, yank and yank-pop" =
  let show e =
    let p = Editor.position e in
    printf
      "%S cursor=%d:%d ring=[%s]\n"
      (Editor.text e)
      p.line
      p.col
      (String.concat ~sep:"|" (Editor.kill_ring e))
  in
  let e = Editor.set_text Editor.empty "alpha beta gamma" in
  let e = Editor.kill_word e in
  show e;
  let e = Editor.kill_word e in
  show e;
  let e = Editor.yank e in
  show e;
  let e = Editor.yank_pop e in
  show e;
  let e = Editor.yank_pop e in
  show e;
  let e = Editor.set_text Editor.empty "keep this" in
  let e = Editor.goto e { line = 0; col = 5 } in
  let e = Editor.kill_to_end e in
  show e;
  let e = Editor.goto e { line = 0; col = 4 } in
  let e = Editor.kill_to_start e in
  show e;
  [%expect
    {|
    "alpha beta " cursor=0:11 ring=[gamma]
    "alpha " cursor=0:6 ring=[beta |gamma]
    "alpha beta " cursor=0:11 ring=[beta |gamma]
    "alpha gamma" cursor=0:11 ring=[beta |gamma]
    "alpha beta " cursor=0:11 ring=[beta |gamma]
    "keep " cursor=0:5 ring=[this]
    " " cursor=0:0 ring=[keep|this]
    |}]
;;

let%expect_test "editor: undo grouping" =
  let show label e =
    let p = Editor.position e in
    printf "%-10s %S cursor=%d:%d\n" label (Editor.text e) p.line p.col
  in
  let e = Editor.empty in
  let e = Editor.insert e "a" in
  let e = Editor.insert e "b" in
  let e = Editor.insert e "c" in
  let e = Editor.insert e " " in
  let e = Editor.insert e "d" in
  show "typed" e;
  let e = Editor.undo e in
  show "undo1" e;
  let e = Editor.undo e in
  show "undo2" e;
  let e = Editor.undo e in
  show "undo3" e;
  let e = Editor.insert e "hello world" in
  let e = Editor.kill_word e in
  show "kill" e;
  let e = Editor.undo e in
  show "undo" e;
  let e = Editor.insert e "XY" in
  show "insert" e;
  let e = Editor.undo e in
  show "undo" e;
  [%expect
    {|
    typed      "abc d" cursor=0:5
    undo1      "abc " cursor=0:4
    undo2      "abc" cursor=0:3
    undo3      "" cursor=0:0
    kill       "hello " cursor=0:6
    undo       "hello world" cursor=0:11
    insert     "hello worldXY" cursor=0:13
    undo       "hello world" cursor=0:11
    |}]
;;

let%expect_test "editor: paste chips" =
  let show label e =
    print_s [%sexp (Editor.chips e : Editor.Chip.t list)];
    printf
      "%-8s %S cursor=%d:%d\n"
      label
      (Editor.text e)
      (Editor.position e).line
      (Editor.position e).col
  in
  let e = Editor.insert_paste Editor.empty "l1\nl2\nl3\nl4" in
  show "4lines" e;
  let e = Editor.insert_paste Editor.empty "l1\nl2\nl3" in
  show "3lines" e;
  let e = Editor.insert_paste Editor.empty "a\nb\nc\nd" in
  let e = Option.value_exn (Editor.up e) in
  show "inside" e;
  let e = Editor.insert e "X" in
  show "edit" e;
  [%expect
    {|
    ((
      (start ((line 0) (col 0)))
      (stop  ((line 3) (col 2)))))
    4lines   "l1\nl2\nl3\nl4" cursor=3:2
    ()
    3lines   "l1\nl2\nl3" cursor=2:2
    ((
      (start ((line 0) (col 0)))
      (stop  ((line 3) (col 1)))))
    inside   "a\nb\nc\nd" cursor=2:1
    ()
    edit     "a\nb\ncX\nd" cursor=2:2
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
    "mo" -> model scoped-models import
    "lo" -> login logout clone
    "s"  -> state switch skills skill: setusr session signout sessions scoped-models jobs host agents hotkeys verbosity
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
  let p = step p Kill_to_start in
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
    query="azz" selected=0 visible=[]
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
  List.iter [ "modle"; "hlep"; "sess"; "zzzz" ] ~f:(fun s ->
    print_s
      [%message
        s (Option.map (Commands.closest s) ~f:(fun c -> c.name) : string option)]);
  [%expect
    {|
    (modle ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (model)))
    (hlep ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (help)))
    (sess ("Option.map (Commands.closest s) ~f:(fun c -> c.name)" (session)))
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
  (* [Paste] is produced by the term layer's bracketed-paste handling and has no
     key, so there is nothing for the keymap coverage check to assert. *)
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
    Alt+Enter        Queue_follow_up
    Ctrl+J           Newline
    Alt+J            Newline
    Esc              Cancel
    Tab              Complete
    Up               Up
    Down             Down
    Alt+Up           Dequeue
    Left             Left
    Right            Right
    Alt+B            Word_left
    Ctrl+Left        Word_left
    Alt+F            Word_right
    Ctrl+Right       Word_right
    Alt+D            Delete_word_forward
    Home             Home
    Ctrl+A           Home
    End              End
    Ctrl+E           End
    PageUp           Page_up
    PageDown         Page_down
    Ctrl+Up          Prev_user_message
    Ctrl+Down        Next_user_message
    Backspace        Backspace
    Ctrl+H           Backspace
    Delete           Delete
    Ctrl+K           Kill_to_end
    Ctrl+U           Kill_to_start
    Ctrl+W           Kill_word
    Alt+Backspace    Kill_word
    Ctrl+Y           Yank
    Alt+Y            Yank_pop
    Ctrl+_           Undo
    Ctrl+O           Cycle_verbosity
    Ctrl+R           Path_complete
    Ctrl+F           Search
    Ctrl+G           Edit_externally
    Ctrl+L           Model_picker
    Ctrl+P           Next_model
    Alt+P            Prev_model
    Ctrl+T           Next_thinking
    Ctrl+N           Picker_toggle_filter
    Ctrl+X           Copy_last
    Ctrl+Z           Suspend
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
    Enter                   send the prompt / accept the highlighted item
    Alt+Enter               queue a follow-up to run after the current turn
    Ctrl+J / Alt+J          insert a newline
    Esc                     close the dialog, or abort the running turn
    Tab                     complete a slash command / open the command picker
    Up                      move up (editor line, history, or list row)
    Down                    move down (editor line, history, or list row)
    Alt+Up                  pop the last queued steer/follow-up back into the editor
    Left                    move the cursor left
    Right                   move the cursor right
    Alt+B / Ctrl+Left       move the cursor back one word
    Alt+F / Ctrl+Right      move the cursor forward one word
    Alt+D                   delete the next word
    Home / Ctrl+A           start of line
    End / Ctrl+E            end of line
    PageUp                  scroll the transcript / list up a page
    PageDown                scroll the transcript / list down a page
    Ctrl+Up                 jump to the previous user message
    Ctrl+Down               jump to the next user message
    Backspace / Ctrl+H      delete the character before the cursor
    Delete                  delete the character under the cursor
    Ctrl+K                  delete to the end of the line
    Ctrl+U                  delete to the start of the line
    Ctrl+W / Alt+Backspace  delete the word before the cursor
    Ctrl+Y                  paste the most recent kill
    Alt+Y                   replace the last yank with an older kill
    Ctrl+_                  undo the last edit
    Ctrl+O                  cycle transcript verbosity
    Ctrl+R                  complete a file path at the cursor
    Ctrl+F                  search the transcript
    Ctrl+G                  edit the prompt in $EDITOR
    Ctrl+L                  pick a model
    Ctrl+P                  cycle to the next scoped model (Shift+Ctrl+P is unavailable; Alt+P goes back)
    Alt+P                   cycle to the previous scoped model
    Ctrl+T                  cycle the thinking level
    Ctrl+N                  picker: toggle the named-only / logged-in-only filter
    Ctrl+X                  copy the last assistant message
    Ctrl+Z                  suspend to the shell
    Shift+Tab               cycle focus: main → agent 1 → … → main
    Alt+1                   focus agent N (Alt+1…9)
    Ctrl+C                  clear the editor or abort the turn, then (again) quit
    Ctrl+D                  quit
    /help                              show commands and keys
    /hotkeys                           show keyboard shortcuts
    /model [name|id|provider/id]       pick or switch the model
    /scoped-models                     pick the models Ctrl+P cycles through
    /change_default                    save the current model and thinking level as the default for new sessions
    /fallback [off|model...]           show or set the models that take over, in order, when a model's usage runs out
    /default-dir [off|path]            show or set the directory new sessions start in
    /login [provider] [api_key|oauth]  log in to a provider
    /logout [provider]                 remove a provider's stored credential
    /thinking [off|low|on|high|max]    pick or set the thinking level
    /verbosity [quiet|normal|verbose]  set the transcript verbosity
    /confirm [on|off]                  ask before destructive tools
    /auth                              show which providers are configured
    /compact [instructions]            summarise older messages to free context
    /new                               start a new session
    /name [text]                       set the session name
    /session                           show session statistics
    /sessions                          pick a saved session (Ctrl+N named, Ctrl+D delete)
    /agents [cancel <n>]               focus a subagent (Ctrl+D cancels one), or cancel subagent n
    /jobs [id|kill <id>]               list background jobs (Enter shows output, Ctrl+D kills), or show/kill one
    /host [name|backend]               pick where tools run (this frontend, another one, or the backend) and the directory there
    /switch [path]                     switch to a saved session
    /cd [path]                         change the working directory
    /fork                              fork at a previous user message
    /rewind                            rewind the head to a previous user message
    /tree                              show the session tree and switch head
    /clone                             clone the current session
    /export [path]                     export the transcript (markdown or .jsonl)
    /import [path]                     import a session from a JSONL file
    /abort                             abort the current run
    /btw <question>                    ask a side question without interrupting the turn (not added to the conversation)
    /skills                            pick a skill to run (Enter puts /skill:NAME in the editor)
    /skill:NAME [args]                 run a skill, with what follows as its arguments
    /mcp [reconnect]                   list MCP servers (Enter approves one or lists its tools); reconnect restarts failed ones
    /retry-backend-connection          reconnect to the backend now instead of waiting for the next retry
    /state                             show session state
    /clear                             clear the transcript
    /signout                           sign out to log in as another user (browser only)
    /setusr [user]                     act as another user (superusers only); without a user, list them
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
    ── ocaml ──
    let x = 1
    plain
    ((
      ((text "a ")
       (style (
         (fg        Default)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false)
         (strike    false)
         (link ()))))
      ((text b)
       (style (
         (fg        Cyan)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false)
         (strike    false)
         (link ()))))
      ((text " ")
       (style (
         (fg        Default)
         (bold      false)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false)
         (strike    false)
         (link ()))))
      ((text c)
       (style (
         (fg        Default)
         (bold      true)
         (dim       false)
         (italic    false)
         (underline false)
         (invert    false)
         (strike    false)
         (link ()))))))
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
    … bash $ seq 7
    == normal ==
    … bash $ seq 7
        line 6
    == verbose ==
    … bash $ seq 7
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

let%expect_test "bash timeout is merged into the tool line at each verbosity" =
  let call : Prigh_protocol.Tool_call.t =
    { id = "c1"; name = "bash"; arguments = {|{"command":"sleep 999"}|} }
  in
  let result : Prigh_protocol.Message.Tool_result.t =
    { tool_call_id = "c1"
    ; tool_name = "bash"
    ; text = "partial output\n[timed out after 120s]"
    ; is_error = true
    ; images = []
    ; at = None
    }
  in
  let item =
    Transcript.Item.Tool
      { call; result = Some result; live_tail = None; subagent = None }
  in
  List.iter [ Verbosity.Quiet; Normal; Verbose ] ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    print_endline (Content.to_plain (Transcript.render_item item ~verbosity)));
  [%expect
    {|
    == quiet ==
    ✕ bash $ sleep 999  timed out after 120s
        partial output
    == normal ==
    ✕ bash $ sleep 999  timed out after 120s
        partial output
    == verbose ==
    ✕ bash $ sleep 999  timed out after 120s
        {
          command: "sleep 999"
        }
        partial output
    |}]
;;

let markdown_fixture =
  {md|
# Heading one
## Heading two
### Heading three

Some `code`, **bold**, *italic* and ~~struck~~ text.
A [link](https://example.com) here.

- top one
  - nested two
    1. ordered three
- top two

> quoted line

---

| name  | value | extra                       |
| ----- | ----- | --------------------------- |
| alpha | 1     | short                       |
| beta  | 2     | a much much much longer value |

```ocaml
let x = 1
let y = 2
```

plain tail
|md}
;;

let%expect_test "markdown: every construct at width 40 and 80" =
  List.iter [ 40; 80 ] ~f:(fun width ->
    printf "== width %d ==\n" width;
    let content = Markdown.render ~width markdown_fixture in
    print_endline (Content.to_plain content);
    print_endline "--- styled";
    let screen : Screen.t =
      { lines = content; cursor = None; width; height = List.length content }
    in
    print_endline (Screen.to_styled screen));
  [%expect
    {|
    == width 40 ==

    Heading one
    Heading two
    Heading three

    Some code, bold, italic and struck text.
    A link here.

    • top one
      • nested two
        1. ordered three
    • top two

    ▎ quoted line

    ────────────────────────────────────────

    name  │ value │ extra
    ──────┼───────┼─────────────────────────
    alpha │ 1     │ short
    beta  │ 2     │ a much much much longer…

    ── ocaml ──
    let x = 1
    let y = 2

    plain tail
    --- styled

    [cyan][bold]Heading one[/]
    [bold]Heading two[/]
    [bold][dim]Heading three[/]

    Some [cyan]code[/], [bold]bold[/], [italic]italic[/] and [dim][strike]struck[/] text.
    A [link=https://example.com]link[/] here.

    • top one
      • nested two
        1. ordered three
    • top two

    [dim]▎ [/]quoted line

    [dim]────────────────────────────────────────[/]

    [bold]name [/][dim] │ [/][bold]value[/][dim] │ [/][bold]extra                   [/]
    [dim]─────[/][dim]─┼─[/][dim]─────[/][dim]─┼─[/][dim]────────────────────────[/]
    alpha[dim] │ [/]1    [dim] │ [/]short
    beta [dim] │ [/]2    [dim] │ [/]a much much much longer…

    [dim]── ocaml ──[/]
    [gray]let x = 1[/]
    [gray]let y = 2[/]

    plain tail
    == width 80 ==

    Heading one
    Heading two
    Heading three

    Some code, bold, italic and struck text.
    A link here.

    • top one
      • nested two
        1. ordered three
    • top two

    ▎ quoted line

    ────────────────────────────────────────────────────────────────────────────────

    name  │ value │ extra
    ──────┼───────┼──────────────────────────────
    alpha │ 1     │ short
    beta  │ 2     │ a much much much longer value

    ── ocaml ──
    let x = 1
    let y = 2

    plain tail
    --- styled

    [cyan][bold]Heading one[/]
    [bold]Heading two[/]
    [bold][dim]Heading three[/]

    Some [cyan]code[/], [bold]bold[/], [italic]italic[/] and [dim][strike]struck[/] text.
    A [link=https://example.com]link[/] here.

    • top one
      • nested two
        1. ordered three
    • top two

    [dim]▎ [/]quoted line

    [dim]────────────────────────────────────────────────────────────────────────────────[/]

    [bold]name [/][dim] │ [/][bold]value[/][dim] │ [/][bold]extra                        [/]
    [dim]─────[/][dim]─┼─[/][dim]─────[/][dim]─┼─[/][dim]─────────────────────────────[/]
    alpha[dim] │ [/]1    [dim] │ [/]short
    beta [dim] │ [/]2    [dim] │ [/]a much much much longer value

    [dim]── ocaml ──[/]
    [gray]let x = 1[/]
    [gray]let y = 2[/]

    plain tail
    |}]
;;

let edit_result text : Prigh_protocol.Message.Tool_result.t =
  { tool_call_id = "c1"
  ; tool_name = "edit"
  ; text
  ; is_error = false
  ; images = []
  ; at = None
  }
;;

let%expect_test "diff colouring at Normal and Verbose through to_styled" =
  let call : Prigh_protocol.Tool_call.t =
    { id = "c1"; name = "edit"; arguments = "{}" }
  in
  let text =
    "--- a/f.ml\n\
     +++ b/f.ml\n\
     @@ -1,3 +1,3 @@\n\
     -old line\n\
     +new line\n\
    \  context\n\
    \  more context\n"
  in
  let item =
    Transcript.Item.Tool
      { call
      ; result = Some (edit_result text)
      ; live_tail = None
      ; subagent = None
      }
  in
  List.iter [ Verbosity.Normal; Verbosity.Verbose ] ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    print_endline (Content.to_styled (Transcript.render_item item ~verbosity)));
  [%expect
    {|
    == normal ==
    [green]✓ [/][bold]edit[/]  [green]+1[/] [red]−1[/]
        [cyan]@@ -1,3 +1,3 @@[/]
        [red]-old line[/]
        [green]+new line[/]
        [gray]  context[/]
        [gray]  more context[/]
    == verbose ==
    [green]✓ [/][bold]edit[/]  [green]+1[/] [red]−1[/]
        [dim]{}[/]
        [cyan]@@ -1,3 +1,3 @@[/]
        [red]-old line[/]
        [green]+new line[/]
        [gray]  context[/]
        [gray]  more context[/]
    |}]
;;

let tool_result ?(is_error = false) text : Prigh_protocol.Message.Tool_result.t =
  { tool_call_id = "c1"
  ; tool_name = "t"
  ; text
  ; is_error
  ; images = []
  ; at = None
  }
;;

let tool ?result ?live_tail name arguments =
  Transcript.Item.Tool
    { call = { id = "c1"; name; arguments }
    ; result
    ; live_tail
    ; subagent = None
    }
;;

let show_tool ?width ?(styled = false) ?result ?live_tail name arguments =
  let item = tool ?result ?live_tail name arguments in
  List.iter Verbosity.all ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    let content = Transcript.render_item ?width item ~verbosity in
    print_endline
      (if styled then Content.to_styled content else Content.to_plain content))
;;

let numbered n = String.concat ~sep:"\n" (List.init n ~f:(sprintf "line %d"))

let%expect_test "tools: read shows the path and how much it read" =
  show_tool
    ~styled:true
    "read"
    {|{"path":"src/retry.ml"}|}
    ~result:(tool_result (numbered 5));
  [%expect
    {|
    == quiet ==
    [green]✓ [/][bold]read[/] src/retry.ml  [dim]5 lines[/]
    == normal ==
    [green]✓ [/][bold]read[/] src/retry.ml  [dim]5 lines[/]
    == verbose ==
    [green]✓ [/][bold]read[/] src/retry.ml  [dim]5 lines[/]
        [dim]{[/]
        [dim]  path: "src/retry.ml"[/]
        [dim]}[/]
        [gray]line 0[/]
        [gray]line 1[/]
        [gray]line 2[/]
        [gray]line 3[/]
        [gray]line 4[/]
    |}];
  show_tool
    "read"
    {|{"path":"a.ml","offset":10,"limit":20}|}
    ~result:(tool_result (numbered 20));
  [%expect
    {|
    == quiet ==
    ✓ read a.ml  lines 10–29 20 lines
    == normal ==
    ✓ read a.ml  lines 10–29 20 lines
    == verbose ==
    ✓ read a.ml  lines 10–29 20 lines
        {
          path: "a.ml"
          offset: 10
          limit: 20
        }
        line 0
        line 1
        line 2
        line 3
        line 4
        line 5
        line 6
        line 7
        line 8
        line 9
        line 10
        line 11
        line 12
        line 13
        line 14
        line 15
        line 16
        line 17
        line 18
        line 19
    |}];
  show_tool
    ~styled:true
    "read"
    {|{"path":"nope.ml"}|}
    ~result:(tool_result ~is_error:true "no such file: nope.ml");
  [%expect
    {|
    == quiet ==
    [red]✕ [/][bold]read[/] nope.ml
        [red]no such file: nope.ml[/]
    == normal ==
    [red]✕ [/][bold]read[/] nope.ml
        [red]no such file: nope.ml[/]
    == verbose ==
    [red]✕ [/][bold]read[/] nope.ml
        [dim]{[/]
        [dim]  path: "nope.ml"[/]
        [dim]}[/]
        [red]no such file: nope.ml[/]
    |}]
;;

let%expect_test "tools: write counts lines; edit counts its diff" =
  show_tool
    "write"
    {|{"path":"notes.md","content":"a\nb\nc\n"}|}
    ~result:(tool_result "overwrote notes.md");
  [%expect
    {|
    == quiet ==
    ✓ write notes.md  3 lines overwrote
    == normal ==
    ✓ write notes.md  3 lines overwrote
    == verbose ==
    ✓ write notes.md  3 lines overwrote
        {
          path: "notes.md"
          content: "a\nb\nc\n"
        }
        a
        b
        c
    |}];
  let diff =
    "--- a/f.ml\n+++ b/f.ml\n@@ -1,6 +1,6 @@\n"
    ^ String.concat
        ~sep:"\n"
        (List.init 6 ~f:(fun i -> sprintf "-old %d\n+new %d" i i))
  in
  show_tool "edit" {|{"path":"f.ml"}|} ~result:(tool_result diff);
  [%expect
    {|
    == quiet ==
    ✓ edit f.ml  +6 −6
    == normal ==
    ✓ edit f.ml  +6 −6
        @@ -1,6 +1,6 @@
        -old 0
        +new 0
        -old 1
        +new 1
        -old 2
        +new 2
        -old 3
        … 5 more lines
    == verbose ==
    ✓ edit f.ml  +6 −6
        {
          path: "f.ml"
        }
        @@ -1,6 +1,6 @@
        -old 0
        +new 0
        -old 1
        +new 1
        -old 2
        +new 2
        -old 3
        +new 3
        -old 4
        +new 4
        -old 5
        +new 5
    |}];
  show_tool
    "edit"
    {|{"path":"f.ml"}|}
    ~result:(tool_result ~is_error:true "old_text not found in f.ml");
  [%expect
    {|
    == quiet ==
    ✕ edit f.ml
        old_text not found in f.ml
    == normal ==
    ✕ edit f.ml
        old_text not found in f.ml
    == verbose ==
    ✕ edit f.ml
        {
          path: "f.ml"
        }
        old_text not found in f.ml
    |}]
;;

let%expect_test "tools: bash shows the command, its outcome as a chip" =
  show_tool "bash" {|{"command":"seq 20"}|} ~result:(tool_result (numbered 20));
  [%expect
    {|
    == quiet ==
    ✓ bash $ seq 20  20 lines
    == normal ==
    ✓ bash $ seq 20
        line 0
        line 1
        line 2
        line 3
        line 4
        … 12 more lines
        line 17
        line 18
        line 19
    == verbose ==
    ✓ bash $ seq 20
        {
          command: "seq 20"
        }
        line 0
        line 1
        line 2
        line 3
        line 4
        line 5
        line 6
        line 7
        line 8
        line 9
        line 10
        line 11
        line 12
        line 13
        line 14
        line 15
        line 16
        line 17
        line 18
        line 19
    |}];
  show_tool
    ~styled:true
    "bash"
    {|{"command":"make test\nmake lint"}|}
    ~result:(tool_result ~is_error:true "test.ml:3: failure\n[exit code 2]");
  [%expect
    {|
    == quiet ==
    [red]✕ [/][bold]bash[/] $ make test …  [red]exit code 2[/]
        [red]test.ml:3: failure[/]
    == normal ==
    [red]✕ [/][bold]bash[/] $ make test …  [red]exit code 2[/]
        [red]test.ml:3: failure[/]
    == verbose ==
    [red]✕ [/][bold]bash[/] $ make test …  [red]exit code 2[/]
        [dim]{[/]
        [dim]  command: "make test\nmake lint"[/]
        [dim]}[/]
        [red]test.ml:3: failure[/]
    |}];
  show_tool
    "bash"
    {|{"command":"npm run dev","background":true}|}
    ~result:(tool_result "started job j1: npm run dev");
  [%expect
    {|
    == quiet ==
    ✓ bash $ npm run dev  job j1
    == normal ==
    ✓ bash $ npm run dev  job j1
    == verbose ==
    ✓ bash $ npm run dev  job j1
        {
          command: "npm run dev"
          background: true
        }
    |}];
  (* Aborting cancels the call: what it printed is kept, but it did not fail. *)
  show_tool
    ~styled:true
    "bash"
    {|{"command":"sleep 9"}|}
    ~result:(tool_result ~is_error:true "partial\n[cancelled]");
  [%expect
    {|
    == quiet ==
    [dim]■ [/][bold]bash[/] $ sleep 9  [dim]1 line[/] [dim]cancelled[/]
    == normal ==
    [dim]■ [/][bold]bash[/] $ sleep 9  [dim]cancelled[/]
        [gray]partial[/]
    == verbose ==
    [dim]■ [/][bold]bash[/] $ sleep 9  [dim]cancelled[/]
        [dim]{[/]
        [dim]  command: "sleep 9"[/]
        [dim]}[/]
        [gray]partial[/]
    |}];
  show_tool
    ~styled:true
    "bash"
    {|{"command":"make"}|}
    ~live_tail:"cc a.c\ncc b.c\n";
  [%expect
    {|
    == quiet ==
    [yellow]… [/][bold]bash[/] $ make
    == normal ==
    [yellow]… [/][bold]bash[/] $ make
        [gray]cc b.c[/]
    == verbose ==
    [yellow]… [/][bold]bash[/] $ make
        [dim]{[/]
        [dim]  command: "make"[/]
        [dim]}[/]
        [gray]cc a.c[/]
        [gray]cc b.c[/]
    |}]
;;

let%expect_test "tools: search flags, other tools, and narrow terminals" =
  show_tool
    "grep"
    {|{"pattern":"retry","path":"src","glob":"*.ml","ignore_case":true}|}
    ~result:(tool_result "src/a.ml:1:retry\nsrc/b.ml:4:Retry");
  [%expect
    {|
    == quiet ==
    ✓ grep retry  in src *.ml ignore case 2 lines
    == normal ==
    ✓ grep retry  in src *.ml ignore case
        src/a.ml:1:retry
        src/b.ml:4:Retry
    == verbose ==
    ✓ grep retry  in src *.ml ignore case
        {
          pattern: "retry"
          path: "src"
          glob: "*.ml"
          ignore_case: true
        }
        src/a.ml:1:retry
        src/b.ml:4:Retry
    |}];
  show_tool "web_search" {|{"limit":3,"query":"ocaml eio"}|};
  [%expect
    {|
    == quiet ==
    … web_search ocaml eio
    == normal ==
    … web_search ocaml eio
    == verbose ==
    … web_search ocaml eio
        {
          limit: 3
          query: "ocaml eio"
        }
    |}];
  (* The argument gives way so that the outcome stays on the line. *)
  show_tool
    ~width:40
    "bash"
    {|{"command":"dune build @runtest --force 2>&1 | tail -n 40"}|}
    ~result:
      (tool_result
         ~is_error:true
         "Error: a long error message that has to wrap\n[exit code 1]");
  [%expect
    {|
    == quiet ==
    ✕ bash $ dune build @runte…  exit code 1
        Error: a long error message that has
        to wrap
    == normal ==
    ✕ bash $ dune build @runte…  exit code 1
        Error: a long error message that has
        to wrap
    == verbose ==
    ✕ bash $ dune build @runte…  exit code 1
        {
          command: "dune build @runtest
        --force 2>&1 | tail -n 40"
        }
        Error: a long error message that has
        to wrap
    |}]
;;

let%expect_test "turns: the prompt opens a turn; the agent's steps hang below" =
  let user text : Prigh_protocol.Message.User.t =
    { text; images = []; at = None }
  in
  let t =
    List.fold
      [ Transcript.Item.User (user "first question")
      ; Assistant { text = "first answer"; final = true }
      ; User (user "make retries back off exponentially, and keep the cap")
      ; Thinking "one\ntwo\nthree\nfour"
      ; Assistant { text = "I'll read the loop first."; final = false }
      ; tool
          "read"
          {|{"path":"src/retry.ml"}|}
          ~result:(tool_result (numbered 5))
      ; Notice (Warn, "[aborted]")
      ]
      ~init:Transcript.empty
      ~f:Transcript.add
  in
  List.iter Verbosity.all ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    print_endline
      (Content.to_plain (Transcript.render_all t ~width:30 ~verbosity));
    print_s
      [%sexp (Transcript.user_message_lines t ~width:30 ~verbosity : int list)]);
  [%expect
    {|
    == quiet ==

    ▌ first question
      first answer

    ▌ make retries back off
    ▌ exponentially, and keep the
    ▌ cap
      I'll read the loop first.
    ✓ read src/retry.ml  5 lines
      [aborted]
    (1 4)
    == normal ==

    ▌ first question
      first answer

    ▌ make retries back off
    ▌ exponentially, and keep the
    ▌ cap
    ┆ one
    ┆ two
    ┆ three
    ┆ …
      I'll read the loop first.
    ✓ read src/retry.ml  5 lines
      [aborted]
    (1 4)
    == verbose ==

    ▌ first question
      first answer

    ▌ make retries back off
    ▌ exponentially, and keep the
    ▌ cap
    ┆ one
    ┆ two
    ┆ three
    ┆ four
      I'll read the loop first.
    ✓ read src/retry.ml  5 lines
        {
          path: "src/retry.ml"
        }
        line 0
        line 1
        line 2
        line 3
        line 4
      [aborted]
    (1 4)
    |}];
  print_endline
    (Content.to_styled (Transcript.render_all t ~width:30 ~verbosity:Normal));
  [%expect
    {|
    [yellow]▌ [/][bold]first question[/]
      first answer

    [yellow]▌ [/][bold]make [/][bold]retries [/][bold]back [/][bold]off [/]
    [yellow]▌ [/][bold]exponentially, [/][bold]and [/][bold]keep [/][bold]the [/]
    [yellow]▌ [/][bold]cap[/]
    [dim]┆ [/][dim][italic]one[/]
    [dim]┆ [/][dim][italic]two[/]
    [dim]┆ [/][dim][italic]three[/]
    [dim]┆ [/][dim][italic]…[/]
      I'll read the loop first.
    [green]✓ [/][bold]read[/] src/retry.ml  [dim]5 lines[/]
      [yellow][bold][aborted][/]
    |}]
;;

let%expect_test "content: highlight inverts matching spans" =
  let line = Content.Line.of_string "Earlier question, earlier answer" in
  List.iter [ "earlier"; "question"; "zzz" ] ~f:(fun needle ->
    printf "== %s ==\n" needle;
    print_endline (Content.to_styled [ Content.Line.highlight line ~needle ]));
  [%expect
    {|
    == earlier ==
    [invert]Earlier[/] question, [invert]earlier[/] answer
    == question ==
    Earlier [invert]question[/], earlier answer
    == zzz ==
    Earlier question, earlier answer
    |}]
;;

let%expect_test "markdown never raises on malformed input" =
  let inputs =
    [ "```"
    ; "```ocaml\nlet x = 1"
    ; "| a | b |\n| not a separator |"
    ; "#"
    ; "- "
    ; "[text]("
    ; "**unterminated"
    ; ">"
    ; "~~~"
    ; ""
    ]
  in
  List.iter inputs ~f:(fun input ->
    printf "== %S ==\n" input;
    print_endline (Content.to_plain (Markdown.render input)));
  [%expect
    {|
    == "```" ==
    ──
    == "```ocaml\nlet x = 1" ==
    ── ocaml ──
    let x = 1
    == "| a | b |\n| not a separator |" ==
    | a | b |
    | not a separator |
    == "#" ==

    == "- " ==
    •
    == "[text](" ==
    [text](
    == "**unterminated" ==
    **unterminated
    == ">" ==
    ▎
    == "~~~" ==
    ~~~
    == "" ==
    |}]
;;

let%expect_test
    "autocomplete: Enter accepts arguments only after a filter or navigation; \
     /cd keeps directories only"
  =
  let models =
    Or_error.ok_exn
      (Prigh_protocol.Json.parse Fixtures.models_json
       |> Or_error.bind ~f:(function
         | `Array items ->
           Or_error.all (List.map items ~f:Prigh_protocol.Model.of_json)
         | _ -> Or_error.error_string "array"))
  in
  let compute ?skills line =
    Autocomplete.compute
      ~line
      ~col:(String.length line)
      ~line_index:0
      ~models
      ~auth:[]
      ~sessions:None
      ~skills
      ~logged_in:(fun _ -> true)
      ~default_dir_host:None
  in
  let show name ac =
    printf
      "%-12s %-10s prefix=%S navigated=%b enter_accepts=%b items=%d\n"
      name
      (match Autocomplete.source ac with
       | Command -> "command"
       | Argument spec -> "arg:" ^ spec.name
       | Path -> "path"
       | Directory { host } ->
         "dir" ^ Option.value_map host ~default:"" ~f:(fun h -> "@" ^ h)
       | Skill -> "skill")
      (Autocomplete.prefix ac)
      (Autocomplete.navigated ac)
      (Autocomplete.accepts_on_enter ac)
      (List.length (Autocomplete.items ac))
  in
  let ac ?skills line = Option.value_exn (compute ?skills line) in
  show "/mo" (ac "/mo");
  show "/model " (ac "/model ");
  show "/model down" (Autocomplete.down (ac "/model "));
  show "/model gp" (ac "/model gp");
  show "@" (ac "@");
  show "@ down" (Autocomplete.down (ac "@"));
  show "/cd sr" (ac "/cd sr");
  show "/export s" (ac "/export s");
  show "host prompt" (Autocomplete.directory ~host:"client-1" ~text:"~/de");
  [%expect
    {|
    /mo          command    prefix="mo" navigated=false enter_accepts=true items=3
    /model       arg:model  prefix="" navigated=false enter_accepts=false items=4
    /model down  arg:model  prefix="" navigated=true enter_accepts=true items=4
    /model gp    arg:model  prefix="gp" navigated=false enter_accepts=true items=1
    @            path       prefix="" navigated=false enter_accepts=false items=0
    @ down       path       prefix="" navigated=true enter_accepts=true items=0
    /cd sr       dir        prefix="sr" navigated=false enter_accepts=true items=0
    /export s    path       prefix="s" navigated=false enter_accepts=true items=0
    host prompt  dir@client-1 prefix="~/de" navigated=false enter_accepts=true items=0
    |}];
  (* Accepting a directory in the host prompt replaces the whole line. *)
  let dirs = Autocomplete.directory ~host:"client-1" ~text:"~/de" in
  let dirs =
    Autocomplete.set_items dirs [ Picker.Item.create ~id:"~/dev/" "~/dev/" ]
  in
  print_endline (Autocomplete.accept dirs ~editor_text:"~/de");
  [%expect {| ~/dev/ |}]
;;

let%expect_test "autocomplete: /skill: completes skill names" =
  let skill name description : Prigh_protocol.Skill.t =
    { name
    ; description
    ; path = sprintf "/p/.claude/skills/%s/SKILL.md" name
    ; model_invocable = true
    }
  in
  let skills =
    [ skill "frontend-design" "Build distinctive UIs"
    ; skill "release" "Cut a release"
    ; skill "review" "Review a diff"
    ]
  in
  let compute ?skills line =
    Autocomplete.compute
      ~line
      ~col:(String.length line)
      ~line_index:0
      ~models:[]
      ~auth:[]
      ~sessions:None
      ~skills
      ~logged_in:(fun _ -> true)
      ~default_dir_host:None
  in
  let show line ?skills () =
    match compute ?skills line with
    | None -> printf "%-14s none\n" line
    | Some ac ->
      printf
        "%-14s %s prefix=%S enter_accepts=%b items=[%s] accept=%S\n"
        line
        (match Autocomplete.source ac with
         | Argument spec -> "arg:" ^ spec.name
         | source -> Sexp.to_string [%sexp (source : Autocomplete.Source.t)])
        (Autocomplete.prefix ac)
        (Autocomplete.accepts_on_enter ac)
        (String.concat
           ~sep:" "
           (List.map (Autocomplete.items ac) ~f:(fun i -> i.Picker.Item.id)))
        (Autocomplete.accept ac ~editor_text:line)
  in
  show "/sk" ();
  show "/skill:" ();
  show "/skill:" ~skills ();
  show "/skill:re" ~skills ();
  show "/skill:fd" ~skills ();
  show "/skill:zzz" ~skills ();
  show "/skill:review x" ~skills ();
  show "/skill:" ~skills:[] ();
  show "/mcp " ();
  [%expect
    {|
    /sk            Command prefix="sk" enter_accepts=true items=[skills skill:] accept="/skills "
    /skill:        Skill prefix="" enter_accepts=false items=[] accept="/skill:"
    /skill:        Skill prefix="" enter_accepts=false items=[frontend-design release review] accept="/skill:frontend-design "
    /skill:re      Skill prefix="re" enter_accepts=true items=[review release frontend-design] accept="/skill:review "
    /skill:fd      Skill prefix="fd" enter_accepts=true items=[frontend-design] accept="/skill:frontend-design "
    /skill:zzz     none
    /skill:review x none
    /skill:        Skill prefix="" enter_accepts=false items=[] accept="/skill:"
    /mcp           arg:mcp prefix="" enter_accepts=false items=[reconnect] accept="/mcp reconnect"
    |}]
;;

let%expect_test
    "skill messages: parsed back into name, location, body and arguments"
  =
  let head =
    "<skill name=\"review\" location=\"/p/.prigh/skills/review/SKILL.md\">\n"
  in
  List.iter
    [ head ^ "References are relative to /p.\n\nRead the diff.\n</skill>"
    ; head ^ "Read the diff.\n</skill>\n\nfocus on app.ml\n\nand tests"
    ; head ^ "Quote:\n</skill>\nnot the end\n</skill>\n\nargs"
    ; head
      ^ "Write </skill> in HTML.\n</skill>\n\nwhy does\n</skill>\nclose it?"
    ; head ^ "</skill>"
    ; head ^ "  Read the diff.\n\n</skill>\n\n  focus \n"
    ; "<skill name=\"x\" location=\"/p/\"q\">/SKILL.md\">\nbody\n</skill>"
    ; head ^ "no closing tag"
    ; "<skill name=\"x\">\nbody\n</skill>"
    ; "look at " ^ head ^ "body\n</skill>"
    ; "<skill name=\"x"
    ; "/skill:review focus"
    ]
    ~f:(fun text ->
      match Skill_message.parse text with
      | None -> print_endline "none"
      | Some t ->
        print_s
          [%message
            (t : Skill_message.t)
              ~invocation:(Skill_message.invocation t : string)]);
  [%expect
    {|
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body "References are relative to /p.\n\nRead the diff.")
       (args "")))
     (invocation /skill:review))
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body     "Read the diff.")
       (args     "focus on app.ml\n\nand tests")))
     (invocation "/skill:review focus on app.ml\n\nand tests"))
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body     "Quote:\n</skill>\nnot the end")
       (args     args)))
     (invocation "/skill:review args"))
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body     "Write </skill> in HTML.")
       (args     "why does\n</skill>\nclose it?")))
     (invocation "/skill:review why does\n</skill>\nclose it?"))
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body     "")
       (args     "")))
     (invocation /skill:review))
    ((t (
       (name     review)
       (location /p/.prigh/skills/review/SKILL.md)
       (body     "Read the diff.")
       (args     focus)))
     (invocation "/skill:review focus"))
    ((t (
       (name     x)
       (location "/p/\"q\">/SKILL.md")
       (body     body)
       (args     "")))
     (invocation /skill:x))
    none
    none
    none
    none
    none
    |}]
;;

let%expect_test
    "autocomplete: /fallback completes each word with a model not listed yet; \
     /default-dir lists directories on its host"
  =
  let models =
    Or_error.ok_exn
      (Prigh_protocol.Json.parse Fixtures.models_json
       |> Or_error.bind ~f:(function
         | `Array items ->
           Or_error.all (List.map items ~f:Prigh_protocol.Model.of_json)
         | _ -> Or_error.error_string "array"))
  in
  let show ?col ?default_dir_host line =
    match
      Autocomplete.compute
        ~line
        ~col:(Option.value col ~default:(String.length line))
        ~line_index:0
        ~models
        ~auth:[]
        ~sessions:None
        ~skills:None
        ~logged_in:(fun _ -> true)
        ~default_dir_host
    with
    | None -> printf "%S none\n" line
    | Some ac ->
      printf
        "%S %s prefix=%S enter_accepts=%b\n  items=[%s]\n  accept=%S\n"
        line
        (match Autocomplete.source ac with
         | Argument spec -> "arg:" ^ spec.name
         | source -> Sexp.to_string [%sexp (source : Autocomplete.Source.t)])
        (Autocomplete.prefix ac)
        (Autocomplete.accepts_on_enter ac)
        (String.concat
           ~sep:" "
           (List.map (Autocomplete.items ac) ~f:(fun i -> i.Picker.Item.id)))
        (Autocomplete.accept ac ~editor_text:line)
  in
  show "/fallback ";
  show "/fallback fab";
  show "/fallback anthropic/claude-fable-5 gp";
  show "/fallback anthropic/claude-fable-5 openai/gpt-5.5 ";
  (* The cursor in the first word of three: the word is replaced, the rest
     kept. *)
  show
    ~col:(String.length "/fallback dee")
    "/fallback deep openai/gpt-5.5 anthropic/claude-fable-5";
  show "/fallback off";
  show "/fallback zzzz";
  show "/default-dir ~/pro";
  show ~default_dir_host:"backend" "/default-dir ~/pro";
  [%expect
    {|
    "/fallback " arg:fallback prefix="" enter_accepts=false
      items=[off anthropic/claude-fable-5 anthropic/claude-fable-5-1 openai/gpt-5.5 deepseek/deepseek-flash]
      accept="/fallback off"
    "/fallback fab" arg:fallback prefix="fab" enter_accepts=true
      items=[anthropic/claude-fable-5 anthropic/claude-fable-5-1]
      accept="/fallback anthropic/claude-fable-5"
    "/fallback anthropic/claude-fable-5 gp" arg:fallback prefix="gp" enter_accepts=true
      items=[openai/gpt-5.5]
      accept="/fallback anthropic/claude-fable-5 openai/gpt-5.5"
    "/fallback anthropic/claude-fable-5 openai/gpt-5.5 " arg:fallback prefix="" enter_accepts=false
      items=[anthropic/claude-fable-5-1 deepseek/deepseek-flash]
      accept="/fallback anthropic/claude-fable-5 openai/gpt-5.5 anthropic/claude-fable-5-1"
    "/fallback deep openai/gpt-5.5 anthropic/claude-fable-5" arg:fallback prefix="deep" enter_accepts=true
      items=[deepseek/deepseek-flash anthropic/claude-fable-5-1]
      accept="/fallback deepseek/deepseek-flash openai/gpt-5.5 anthropic/claude-fable-5"
    "/fallback off" arg:fallback prefix="off" enter_accepts=true
      items=[off]
      accept="/fallback off"
    "/fallback zzzz" none
    "/default-dir ~/pro" (Directory(host())) prefix="~/pro" enter_accepts=true
      items=[]
      accept="/default-dir ~/pro"
    "/default-dir ~/pro" (Directory(host(backend))) prefix="~/pro" enter_accepts=true
      items=[]
      accept="/default-dir ~/pro"
    |}]
;;

let%expect_test "hand-over messages: parsed back into from, to and the error" =
  List.iter
    [ "[prigh: openai-codex/gpt-6-sol cannot continue (HTTP 429: The usage \
       limit has been reached (usage limit reached)), so \
       anthropic/claude-opus-5-5 takes over this conversation from here. Carry \
       on with the task where it left off.]"
    ; "[prigh: a/b cannot continue (no, so what), so c/d takes over this \
       conversation from here. Carry on with the task where it left off.]"
    ; "[prigh: a/x cannot continue (HTTP 429: {\"error\": \"usage limit \
       reached (plan: pro), try later\"}), so b/y takes over this conversation \
       from here. Carry on with the task where it left off.]"
    ; "[prigh: a/x cannot continue (quota (daily), so wait; b/y takes over \
       this conversation), so b/y takes over this conversation from here. \
       Carry on with the task where it left off.]"
    ; "[prigh: something else]"
    ; "[prigh: a/b cannot continue (x), so c/d takes over this conversation \
       from here."
    ; "[prigh: a/b cannot continue (x) and c/d takes over this conversation.]"
    ; "please: [prigh: a/b cannot continue (x), so c/d takes over this \
       conversation.]"
    ; "[prigh: a/b cannot continue (x), so c d takes over this conversation.]"
    ; "[prigh: a b cannot continue (x), so c/d takes over this conversation.]"
    ; "[prigh:  cannot continue (x), so c/d takes over this conversation.]"
    ]
    ~f:(fun text ->
      match Handover_message.parse text with
      | None -> print_endline "none"
      | Some t ->
        print_s [%sexp (t : Handover_message.t)];
        print_endline (Handover_message.summary t));
  [%expect
    {|
    ((from openai-codex/gpt-6-sol)
     (to_  anthropic/claude-opus-5-5)
     (error "HTTP 429: The usage limit has been reached (usage limit reached)"))
    ↪ handed over from openai-codex/gpt-6-sol to anthropic/claude-opus-5-5
    ((from  a/b)
     (to_   c/d)
     (error "no, so what"))
    ↪ handed over from a/b to c/d
    ((from a/x)
     (to_  b/y)
     (error
      "HTTP 429: {\"error\": \"usage limit reached (plan: pro), try later\"}"))
    ↪ handed over from a/x to b/y
    ((from a/x)
     (to_  b/y)
     (error "quota (daily), so wait; b/y takes over this conversation"))
    ↪ handed over from a/x to b/y
    none
    none
    none
    none
    none
    none
    none
    |}]
;;
