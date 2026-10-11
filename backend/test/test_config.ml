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
      (max_budget_usd ()) (goal_max_continuations 20)
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)))
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
    ; resume_on_limit_reset = true
    ; archive_after_days = 7.
    ; notify = Bell
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
      "notify": "bell"
    }
    (Ok
     ((scoped_models (anthropic/claude-fable-5 deepseek-v4-pro))
      (approval_mode Ask) (permissions ((allow ()) (ask ()) (deny ())))
      (default_model (anthropic/claude-fable-5)) (default_thinking ((On (High))))
      (fallback_models ()) (default_cwd ()) (max_budget_usd ())
      (goal_max_continuations 20) (resume_on_limit_reset true)
      (archive_after_days 7) (notify Bell)))
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
      (max_budget_usd ()) (goal_max_continuations 20)
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)))
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
    unknown setting "colour" in $DIR/.prigh/config.json is ignored (known: providers, confirm_tools, scoped_models, approval_mode, permissions, default_model, default_thinking, fallback_models, default_cwd, max_budget_usd, goal_max_continuations, resume_on_limit_reset, archive_after_days, notify)
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
      (max_budget_usd ()) (goal_max_continuations 20)
      (resume_on_limit_reset true) (archive_after_days 7) (notify Bell)))
    (Ok
     ((scoped_models ()) (approval_mode Auto)
      (permissions ((allow ()) (ask ()) (deny ())))
      (default_model (deepseek/deepseek-flash)) (default_thinking ((On ())))
      (fallback_models ()) (default_cwd ()) (max_budget_usd ())
      (goal_max_continuations 20) (resume_on_limit_reset true)
      (archive_after_days 7) (notify Bell)))
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
