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
     ((scoped_models ()) (confirm_tools false) (default_model ())
      (default_thinking ())))
    |}]
;;

let%expect_test "save then load round trips" =
  with_sandbox
  @@ fun t ->
  let config =
    { Config.scoped_models = [ "anthropic/claude-fable-5"; "deepseek-v4-pro" ]
    ; confirm_tools = true
    ; default_model = Some "anthropic/claude-fable-5"
    ; default_thinking = Some (On (Some High))
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
      "confirm_tools": true,
      "default_model": "anthropic/claude-fable-5",
      "default_thinking": "high"
    }
    (Ok
     ((scoped_models (anthropic/claude-fable-5 deepseek-v4-pro))
      (confirm_tools true) (default_model (anthropic/claude-fable-5))
      (default_thinking ((On (High))))))
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
     ((scoped_models (x)) (confirm_tools true) (default_model ())
      (default_thinking ())))
    |}]
;;

let%expect_test "malformed JSON and wrong types are errors" =
  with_sandbox
  @@ fun t ->
  write t ".prigh/config.json" "{not json";
  print_s [%sexp (Config.load ~home:t.dir : Config.t Or_error.t)];
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
    (Error "json > object: char '}'")
    (Error "config.scoped_models must be an array of strings")
    (Error "config.confirm_tools must be a boolean")
    (Error "config.default_model must be a string")
    (Error
     (config.default_thinking "thinking must be one of: off, on, low, high, max"))
    (Error "config.default_thinking must be a string")
    |}]
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
     ((scoped_models ()) (confirm_tools false) (default_model ())
      (default_thinking ())))
    (Ok
     ((scoped_models ()) (confirm_tools false)
      (default_model (deepseek/deepseek-flash)) (default_thinking ((On ())))))
    |}]
;;
