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
      (context_window 272000) (max_output 128000)
      (thinking_levels (Low Medium High Xhigh Max)) (can_disable_thinking false)
      (thinking_default ((On (Medium))))
      (thinking_style (Effort (off_is_none false)))
      (cost ((input 2) (output 10) (cache_read 0.1))) (supports_images true)
      (native_computer Openai)))
    (openai-codex/gpt-6.1-sol
     ((id gpt-6.1-sol) (provider Openai_codex) (name "GPT-6.1 Sol")
      (context_window 272000) (max_output 128000)
      (thinking_levels (Low Medium High Xhigh Max)) (can_disable_thinking false)
      (thinking_default ((On (Medium))))
      (thinking_style (Effort (off_is_none false)))
      (cost ((input 2) (output 10) (cache_read 0.1))) (supports_images true)))
    ("GPT-6.1 Sol"
     "model \"GPT-6.1 Sol\" is ambiguous; one of: openai/gpt-6.1-sol, openai-codex/gpt-6.1-sol")
    (gpt-6-sol
     ((id gpt-6-sol) (provider Openai_codex) (name "GPT-6 Sol")
      (context_window 272000) (max_output 128000)
      (thinking_levels (Low Medium High Xhigh Max)) (can_disable_thinking true)
      (thinking_default ((On (Medium))))
      (thinking_style (Effort (off_is_none true)))
      (cost ((input 2) (output 10) (cache_read 0.2))) (supports_images true)))
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

let%expect_test
    "every built-in model's thinking: options, provider's default, style"
  =
  let options m =
    String.concat
      ~sep:" "
      (List.map (Model.thinking_options m) ~f:Thinking.to_string)
  in
  List.iter Model.all ~f:(fun (m : Model.t) ->
    printf
      "%-36s %-42s %-8s %s\n"
      (Model.key m)
      (options m)
      (Option.value_map m.thinking_default ~default:"?" ~f:Thinking.to_string)
      (Sexp.to_string [%sexp (m.thinking_style : Model.Thinking_style.t)]));
  [%expect
    {|
    anthropic/claude-fable-5             default low medium high xhigh max          high     Adaptive
    anthropic/claude-fable-5-1           default low medium high xhigh max          high     Adaptive
    anthropic/claude-fable-5-2           default low medium high xhigh max          high     Adaptive
    anthropic/claude-haiku-4-5           off default low medium high xhigh max      medium   (Budget(effort false))
    anthropic/claude-haiku-4-5-20251001  off default low medium high xhigh max      medium   (Budget(effort false))
    anthropic/claude-opus-4-5            off default low medium high                high     (Budget(effort true))
    anthropic/claude-opus-4-5-20251101   off default low medium high                high     (Budget(effort true))
    anthropic/claude-opus-4-6            off default low medium high max            high     Adaptive
    anthropic/claude-opus-4-7            off default low medium high xhigh max      high     Adaptive
    anthropic/claude-opus-4-8            off default low medium high xhigh max      high     Adaptive
    anthropic/claude-opus-5              off default low medium high xhigh max      high     Adaptive
    anthropic/claude-opus-5-5            default low medium high xhigh max          medium   Adaptive
    anthropic/claude-sonnet-4-5          off default low medium high xhigh max      medium   (Budget(effort false))
    anthropic/claude-sonnet-4-5-20250929 off default low medium high xhigh max      medium   (Budget(effort false))
    anthropic/claude-sonnet-4-6          off default low medium high max            high     Adaptive
    anthropic/claude-sonnet-5            off default low medium high xhigh max      high     Adaptive
    anthropic/claude-sonnet-5-5          off default low medium high xhigh max      high     Adaptive_between_tools
    openai/gpt-4                         off                                        ?        (Effort(off_is_none false))
    openai/gpt-4-turbo                   off                                        ?        (Effort(off_is_none false))
    openai/gpt-4.1                       off                                        ?        (Effort(off_is_none false))
    openai/gpt-4.1-mini                  off                                        ?        (Effort(off_is_none false))
    openai/gpt-4.1-nano                  off                                        ?        (Effort(off_is_none false))
    openai/gpt-4o                        off                                        ?        (Effort(off_is_none false))
    openai/gpt-4o-2024-05-13             off                                        ?        (Effort(off_is_none false))
    openai/gpt-4o-2024-08-06             off                                        ?        (Effort(off_is_none false))
    openai/gpt-4o-2024-11-20             off                                        ?        (Effort(off_is_none false))
    openai/gpt-4o-mini                   off                                        ?        (Effort(off_is_none false))
    openai/gpt-5                         default minimal low medium high            medium   (Effort(off_is_none false))
    openai/gpt-5-chat-latest             off                                        ?        (Effort(off_is_none false))
    openai/gpt-5-mini                    default minimal low medium high            medium   (Effort(off_is_none false))
    openai/gpt-5-nano                    default minimal low medium high            medium   (Effort(off_is_none false))
    openai/gpt-5-pro                     default high                               high     (Effort(off_is_none false))
    openai/gpt-5.1                       off default low medium high                off      (Effort(off_is_none true))
    openai/gpt-5.2                       off default low medium high xhigh          off      (Effort(off_is_none true))
    openai/gpt-5.2-chat-latest           off                                        ?        (Effort(off_is_none false))
    openai/gpt-5.2-pro                   default medium high xhigh                  ?        (Effort(off_is_none false))
    openai/gpt-5.3-chat-latest           off                                        ?        (Effort(off_is_none false))
    openai/gpt-5.3-codex                 default low medium high xhigh              ?        (Effort(off_is_none false))
    openai/gpt-5.3-codex-spark           off                                        ?        (Effort(off_is_none false))
    openai/gpt-5.4                       off default low medium high xhigh          off      (Effort(off_is_none true))
    openai/gpt-5.4-mini                  off default low medium high xhigh          off      (Effort(off_is_none true))
    openai/gpt-5.4-nano                  off default low medium high xhigh          off      (Effort(off_is_none true))
    openai/gpt-5.4-pro                   default medium high xhigh                  medium   (Effort(off_is_none false))
    openai/gpt-5.5                       off default low medium high xhigh          medium   (Effort(off_is_none true))
    openai/gpt-5.5-pro                   default medium high xhigh                  high     (Effort(off_is_none false))
    openai/gpt-5.6-luna                  off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai/gpt-5.6-sol                   off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai/gpt-5.6-terra                 off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai/gpt-6-astra                   default low medium high xhigh max          ?        (Effort(off_is_none false))
    openai/gpt-6.1-sol                   default low medium high xhigh max          medium   (Effort(off_is_none false))
    openai/gpt-realtime-2.1              default minimal low medium high xhigh      low      (Effort(off_is_none false))
    openai/o1                            default low medium high                    medium   (Effort(off_is_none false))
    openai/o1-pro                        default low medium high                    medium   (Effort(off_is_none false))
    openai/o3                            default low medium high                    medium   (Effort(off_is_none false))
    openai/o3-mini                       default low medium high                    medium   (Effort(off_is_none false))
    openai/o3-pro                        default low medium high                    medium   (Effort(off_is_none false))
    openai/o4-mini                       default low medium high                    medium   (Effort(off_is_none false))
    openai-codex/gpt-5.3-codex-spark     off                                        ?        (Effort(off_is_none false))
    openai-codex/gpt-5.5                 off default low medium high xhigh          medium   (Effort(off_is_none true))
    openai-codex/gpt-5.6-luna            off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai-codex/gpt-5.6-sol             off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai-codex/gpt-5.6-terra           off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai-codex/gpt-6-astra             default low medium high xhigh max          ?        (Effort(off_is_none false))
    openai-codex/gpt-6-luna              off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai-codex/gpt-6-sol               off default low medium high xhigh max      medium   (Effort(off_is_none true))
    openai-codex/gpt-6.1-sol             default low medium high xhigh max          medium   (Effort(off_is_none false))
    deepseek/deepseek-flash              off default low high max                   high     (Effort(off_is_none false))
    deepseek/deepseek-v4-pro             off default low high max                   high     (Effort(off_is_none false))
    |}]
;;

let%expect_test "clamping to a model's options" =
  let models =
    [ "claude-opus-4-5"
    ; "claude-opus-4-6"
    ; "claude-opus-5-5"
    ; "gpt-5"
    ; "o3"
    ; "gpt-6.1-sol"
    ; "deepseek-flash"
    ; "gpt-4.1"
    ]
  in
  let settings : Thinking.t list =
    Off
    :: On None
    :: List.map Thinking.Level.all ~f:(fun l -> Thinking.On (Some l))
  in
  printf "%-18s" "";
  List.iter settings ~f:(fun s -> printf " %-8s" (Thinking.to_string s));
  print_endline "";
  List.iter models ~f:(fun id ->
    match Model.find id with
    | None -> printf "%s: not found\n" id
    | Some m ->
      printf "%-18s" id;
      List.iter settings ~f:(fun s ->
        printf " %-8s" (Thinking.to_string (Model.clamp_thinking m s)));
      print_endline "");
  [%expect
    {|
                       off      default  minimal  low      medium   high     xhigh    max
    claude-opus-4-5    off      default  low      low      medium   high     high     high
    claude-opus-4-6    off      default  low      low      medium   high     high     max
    claude-opus-5-5    default  default  low      low      medium   high     xhigh    max
    gpt-5              default  default  minimal  low      medium   high     high     high
    o3                 default  default  low      low      medium   high     high     high
    gpt-6.1-sol        default  default  low      low      medium   high     xhigh    max
    deepseek-flash     off      default  low      low      low      high     high     max
    gpt-4.1            off      off      off      off      off      off      off      off
    |}]
;;

let%expect_test "check_thinking: an explicit setting the model lacks is refused"
  =
  let m = Option.value_exn (Model.find "claude-opus-4-6") in
  print_s
    [%sexp (Model.check_thinking m (On (Some Xhigh)) : Thinking.t Or_error.t)];
  print_s
    [%sexp (Model.check_thinking m (On (Some Max)) : Thinking.t Or_error.t)];
  [%expect
    {|
    (Error
     "Claude Opus 4.6 does not take thinking xhigh; it takes: off, default, low, medium, high, max")
    (Ok (On (Max)))
    |}]
;;
