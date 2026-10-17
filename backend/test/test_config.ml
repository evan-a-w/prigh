open! Core
open! Prigh
open Tool_test_helpers

let config_path t = Filename.concat t.dir ".prigh/config.json"

let%expect_test "a missing file yields the default" =
  with_sandbox
  @@ fun t ->
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (max_budget_usd ()) (goal_max_continuations 20) (reminders ())
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)
      (auto_background_seconds 30)))
    |}]
;;

let%expect_test "save then load round trips" =
  with_sandbox
  @@ fun t ->
  let config =
    { Config.scoped_models = [ "anthropic/claude-fable-5"; "deepseek-v4-pro" ]
    ; approval_mode = Ask
    ; permissions = Approval.Rules.empty
    ; default_model = Some "anthropic/claude-fable-5"
    ; default_thinking = Some (On (Some High))
    ; fallback_models = []
    ; default_cwd = None
    ; max_budget_usd = None
    ; goal_max_continuations = 20
    ; reminders = []
    ; resume_on_limit_reset = true
    ; archive_after_days = 7.
    ; notify = Bell
    ; auto_background_seconds = 30.
    }
  in
  print_s [%sexp (Config.save ~home:t.dir config : unit Or_error.t)];
  print_endline (In_channel.read_all (config_path t));
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  [%expect
    {|
    (Ok ())
    {
      "scoped_models": [
        "anthropic/claude-fable-5",
        "deepseek-v4-pro"
      ],
      "approval_mode": "ask",
      "default_model": "anthropic/claude-fable-5",
      "default_thinking": "high",
      "fallback_models": [],
      "default_cwd": null,
      "max_budget_usd": null,
      "goal_max_continuations": 20,
      "resume_on_limit_reset": true,
      "archive_after_days": 7,
      "notify": "bell",
      "auto_background_seconds": 30
    }
    (Ok
     ((scoped_models (anthropic/claude-fable-5 deepseek-v4-pro))
      (approval_mode Ask) (permissions ((allow ()) (ask ()) (deny ())))
      (default_model (anthropic/claude-fable-5)) (default_thinking ((On (High))))
      (fallback_models ()) (default_cwd ()) (max_budget_usd ())
      (goal_max_continuations 20) (reminders ()) (resume_on_limit_reset true)
      (archive_after_days 7) (notify Bell) (auto_background_seconds 30)))
    |}]
;;

let%expect_test "unknown fields are ignored" =
  with_sandbox
  @@ fun t ->
  write
    t
    ".prigh/config.json"
    {|{"scoped_models": ["x"], "confirm_tools": true, "future": {"a": 1}}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((scoped_models (x)) (approval_mode Ask)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (max_budget_usd ()) (goal_max_continuations 20) (reminders ())
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)
      (auto_background_seconds 30)))
    |}]
;;

let%expect_test "malformed JSON and wrong types are errors" =
  with_sandbox
  @@ fun t ->
  write t ".prigh/config.json" "{not json";
  print_endline
    (mask
       t
       (Sexp.to_string_hum
          [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)]));
  write t ".prigh/config.json" {|{"scoped_models": "x"}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  write t ".prigh/config.json" {|{"confirm_tools": 1}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  write t ".prigh/config.json" {|{"default_model": 1}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  write t ".prigh/config.json" {|{"default_thinking": "lots"}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  write t ".prigh/config.json" {|{"default_thinking": true}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  [%expect
    {|
    (Error
     "$DIR/.prigh/config.json is not valid JSON (json > object: char '}')")
    (Error "config.scoped_models must be an array of strings")
    (Error "config.confirm_tools must be a boolean")
    (Error "config.default_model must be a string")
    (Error
     (config.default_thinking "thinking must be one of: off, on, low, high, max"))
    (Error "config.default_thinking must be a string")
    |}]
;;

let%expect_test
    "problems: wrong types and unknown settings say where and what to do"
  =
  with_sandbox
  @@ fun t ->
  let show contents =
    write t ".prigh/config.json" contents;
    List.iter (Config.problems ~home:t.dir) ~f:(fun p ->
      print_endline (mask t p))
  in
  show {|{"scoped_models": [], "providers": {}, "default_cwd": null}|};
  [%expect {| |}];
  show {|{"confirm_tools": "yes", "providrs": {}, "colour": "blue"}|};
  [%expect
    {|
    config.confirm_tools must be a boolean (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    unknown setting "providrs" in $DIR/.prigh/config.json is ignored; did you mean "providers"?
    unknown setting "colour" in $DIR/.prigh/config.json is ignored (known: providers, confirm_tools, scoped_models, approval_mode, permissions, default_model, default_thinking, fallback_models, default_cwd, max_budget_usd, goal_max_continuations, reminders, resume_on_limit_reset, archive_after_days, notify, auto_background_seconds)
    |}];
  show {|{"archive_after_days": "a week"}|};
  show {|{"archive_after_days": -1}|};
  show {|{"archive_after_days": 2.5}|};
  [%expect
    {|
    config.archive_after_days must be a number of days (0: never) (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    config.archive_after_days must be a number of days (0: never) (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    |}];
  (* Unparsable files are [read_fields]' error, reported once elsewhere. *)
  show "{not json";
  [%expect {| |}]
;;

let%expect_test "null defaults are unset" =
  with_sandbox
  @@ fun t ->
  write
    t
    ".prigh/config.json"
    {|{"default_model": null, "default_thinking": null}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  write
    t
    ".prigh/config.json"
    {|{"default_model": "deepseek/deepseek-flash", "default_thinking": "on"}|};
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ()))) (default_model ())
      (default_thinking ()) (fallback_models ()) (default_cwd ())
      (max_budget_usd ()) (goal_max_continuations 20) (reminders ())
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)
      (auto_background_seconds 30)))
    (Ok
     ((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ())))
      (default_model (deepseek/deepseek-flash)) (default_thinking ((On ())))
      (fallback_models ()) (default_cwd ()) (max_budget_usd ())
      (goal_max_continuations 20) (reminders ()) (resume_on_limit_reset true)
      (archive_after_days 7) (notify Bell) (auto_background_seconds 30)))
    |}]
;;

let%expect_test "notify: off, bell or desktop" =
  List.iter
    [ {|{"notify": "desktop"}|}
    ; {|{"notify": "off"}|}
    ; {|{"notify": "loud"}|}
    ; {|{"notify": 1}|}
    ]
    ~f:(fun json ->
      print_s
        [%sexp
          (Or_error.map
             (Config.of_json (Jsonaf.of_string json))
             ~f:(fun c -> c.notify)
           : Config.Notify.t Or_error.t)]);
  [%expect
    {|
    (Ok Desktop)
    (Ok Off)
    (Error
     "config.notify must be \"off\", \"bell\" or \"desktop\", not \"loud\"")
    (Error "config.notify must be \"off\", \"bell\" or \"desktop\"")
    |}]
;;

let%expect_test "reminders: parsed, round-tripped, kept by save as written" =
  with_sandbox
  @@ fun t ->
  write
    t
    ".prigh/config.json"
    {|{"reminders": [
        {"name": "keep-going", "text": "Don't stop until every test passes.", "on": "stop", "max_repeats": 5},
        {"text": "Check disk usage (df -h).\nThen clean up.", "every_turns": 40},
        {"text": "x", "on": "stop"}
      ]}|};
  let config = Or_error.ok_exn (Config.load ~home:t.dir) in
  print_s [%sexp (config.reminders : Reminder.t list)];
  List.iter config.reminders ~f:(fun r ->
    print_endline (Reminder.label r);
    print_endline (Reminder.message_text r ~repeat:(Some 1)));
  let json = Config.to_json config in
  print_endline
    (Jsonaf.to_string (Option.value_exn (Jsonaf.member "reminders" json)));
  print_s
    [%sexp
      (Or_error.map (Config.of_json json) ~f:(fun c ->
         List.equal Reminder.equal c.reminders config.reminders)
       : bool Or_error.t)];
  (* Saving another setting leaves them as the user wrote them. *)
  let before = In_channel.read_all (config_path t) in
  Or_error.ok_exn
    (Config.save ~home:t.dir { config with reminders = []; notify = Off });
  let fields = Or_error.ok_exn (Config.read_fields ~home:t.dir) in
  print_s
    [%sexp
      (Jsonaf.exactly_equal
         (List.Assoc.find_exn fields ~equal:String.equal "reminders")
         (Option.value_exn
            (Jsonaf.member "reminders" (Jsonaf.of_string before)))
       : bool)];
  [%expect
    {|
    (((name (keep-going)) (text "Don't stop until every test passes.")
      (trigger (On_stop (max_repeats 5))))
     ((name ()) (text  "Check disk usage (df -h).\
                      \nThen clean up.")
      (trigger (Every_turns 40)))
     ((name ()) (text x) (trigger (On_stop (max_repeats 3)))))
    keep-going
    [prigh reminder: keep-going, as you ended your turn (1 of 5)] Don't stop until every test passes.
    Check disk usage (df -h).
    [prigh reminder, every 40 turns] Check disk usage (df -h).
    Then clean up.
    x
    [prigh reminder, as you ended your turn (1 of 3)] x
    [{"name":"keep-going","text":"Don't stop until every test passes.","on":"stop","max_repeats":5},{"text":"Check disk usage (df -h).\nThen clean up.","every_turns":40},{"text":"x","on":"stop","max_repeats":3}]
    (Ok true)
    true
    |}]
;;

let%expect_test "reminders: errors say which one and what is wrong" =
  List.iter
    [ {|{"reminders": {"text": "x"}}|}
    ; {|{"reminders": ["x"]}|}
    ; {|{"reminders": [{"text": "x", "on": "stop"}, {"text": "y"}]}|}
    ; {|{"reminders": [{"text": "  ", "on": "stop"}]}|}
    ; {|{"reminders": [{"on": "stop"}]}|}
    ; {|{"reminders": [{"text": "x", "on": "start"}]}|}
    ; {|{"reminders": [{"text": "x", "on": "stop", "every_turns": 3}]}|}
    ; {|{"reminders": [{"text": "x", "every_turns": 0}]}|}
    ; {|{"reminders": [{"text": "x", "every_turns": 2.5}]}|}
    ; {|{"reminders": [{"text": "x", "every_turns": 3, "max_repeats": 2}]}|}
    ; {|{"reminders": [{"text": "x", "on": "stop", "max_repeats": 0}]}|}
    ; {|{"reminders": [{"text": "x", "on": "stop", "evry_turns": 3}]}|}
    ; {|{"reminders": [{"text": "x", "on": "stop", "colour": "red"}]}|}
    ; {|{"reminders": [{"name": 1, "text": "x", "on": "stop"}]}|}
    ]
    ~f:(fun json ->
      print_s
        [%sexp
          (Or_error.map
             (Config.of_json (Jsonaf.of_string json))
             ~f:(fun c -> c.reminders)
           : Reminder.t list Or_error.t)]);
  [%expect
    {|
    (Error
     "config.reminders must be an array of reminders, e.g. [{\"text\": \"...\", \"on\": \"stop\"}]")
    (Error
     "config.reminders[0]: must be an object such as {\"text\": \"...\", \"on\": \"stop\"}")
    (Error
     "config.reminders[1]: needs \"on\": \"stop\" (when the agent ends its turn) or \"every_turns\": N")
    (Error
     "config.reminders[0]: \"text\" must be a non-empty string: what to remind the agent of")
    (Error
     "config.reminders[0]: \"text\" must be a non-empty string: what to remind the agent of")
    (Error "config.reminders[0]: \"on\" must be \"stop\"")
    (Error
     "config.reminders[0]: has both \"on\": \"stop\" and \"every_turns\"; keep one")
    (Error
     "config.reminders[0]: \"every_turns\" must be a whole number, 1 or more")
    (Error
     "config.reminders[0]: \"every_turns\" must be a whole number, 1 or more")
    (Error
     "config.reminders[0]: \"max_repeats\" only goes with \"on\": \"stop\", not with \"every_turns\"")
    (Error
     "config.reminders[0]: \"max_repeats\" must be a whole number, 1 or more")
    (Error
     "config.reminders[0]: unknown key \"evry_turns\"; did you mean \"every_turns\"?")
    (Error
     "config.reminders[0]: unknown key \"colour\" (known: name, text, on, every_turns, max_repeats)")
    (Error "config.reminders[0]: \"name\" must be a string")
    |}]
;;
