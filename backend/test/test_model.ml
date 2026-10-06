open! Core
open! Prigh

let show query =
  match Model.resolve query with
  | Ok m -> print_s [%sexp (query : string), (m : Model.t)]
  | Error e -> print_s [%sexp (query : string), (e : Error.t)]
;;

let%expect_test "GPT-6.1 Sol on the OpenAI API and Codex" =
  show "openai/gpt-6.1-sol";
  show "openai-codex/gpt-6.1-sol";
  show "GPT-6.1 Sol";
  show "gpt-6-sol";
  [%expect
    {|
    (openai/gpt-6.1-sol
     ((id gpt-6.1-sol) (provider Openai) (name "GPT-6.1 Sol")
      (context_window 272000) (max_output 128000) (supports_thinking true)
      (thinking_style Budget) (cost ((input 2) (output 10) (cache_read 0.1)))
      (supports_images true)))
    (openai-codex/gpt-6.1-sol
     ((id gpt-6.1-sol) (provider Openai_codex) (name "GPT-6.1 Sol")
      (context_window 272000) (max_output 128000) (supports_thinking true)
      (thinking_style Budget) (cost ((input 2) (output 10) (cache_read 0.1)))
      (supports_images true)))
    ("GPT-6.1 Sol"
     "model \"GPT-6.1 Sol\" is ambiguous; one of: openai/gpt-6.1-sol, openai-codex/gpt-6.1-sol")
    (gpt-6-sol
     ((id gpt-6-sol) (provider Openai_codex) (name "GPT-6 Sol")
      (context_window 272000) (max_output 128000) (supports_thinking true)
      (thinking_style Budget) (cost ((input 2) (output 10) (cache_read 0.2)))
      (supports_images true)))
    |}]
;;

let%expect_test "GPT-6.1 Sol cost: cached input is billed at the cache rate" =
  let m = Option.value_exn (Model.find "openai-codex/gpt-6.1-sol") in
  print_s
    [%sexp
      (Model.cost_usd
         m
         { input = 1_000_000; cache_read = 500_000; output = 100_000 }
       : float)];
  [%expect {| 2.05 |}]
;;

let%expect_test "unknown models: suggestions stay within a named provider" =
  List.iter [ "anthropic/nope"; "opus-55"; "nosuch/claude-opus" ] ~f:(fun q ->
    match Model.resolve q with
    | Ok m -> print_endline (Model.key m)
    | Error e -> print_endline (Error.to_string_hum e));
  [%expect
    {|
    unknown model "anthropic/nope"; did you mean: anthropic/claude-opus-5 (Claude Opus 5), anthropic/claude-fable-5 (Claude Fable 5), anthropic/claude-opus-4-5 (Claude Opus 4.5 (latest))
    unknown model "opus-55"; did you mean: openai/gpt-5 (GPT-5), openai/gpt-5.5 (GPT-5.5), openai-codex/gpt-5.5 (GPT-5.5)
    unknown model "nosuch/claude-opus"; did you mean: anthropic/claude-opus-5 (Claude Opus 5), anthropic/claude-opus-4-5 (Claude Opus 4.5 (latest)), anthropic/claude-opus-4-6 (Claude Opus 4.6)
    |}]
;;
