open! Core
open! Import

module Cost = struct
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of]
end

type t =
  { id : string
  ; provider : Provider_id.t
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of]

let key t = Provider_id.to_string t.provider ^ "/" ^ t.id

let m provider id name ~ctx ~max ~thinking ~input ~output ~cache_read =
  { id
  ; provider
  ; name
  ; context_window = ctx
  ; max_output = max
  ; supports_thinking = thinking
  ; cost = { input; output; cache_read }
  }
;;

(* Mirrors pi's model catalog for these providers. *)
let all =
  [ m
      Anthropic
      "claude-fable-5"
      "Claude Fable 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Anthropic
      "claude-fable-5-1"
      "Claude Fable 5.1"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:10.
      ~output:50.
      ~cache_read:0.25
  ; m
      Anthropic
      "claude-haiku-4-5"
      "Claude Haiku 4.5 (latest)"
      ~ctx:200000
      ~max:64000
      ~thinking:true
      ~input:1.
      ~output:5.
      ~cache_read:0.1
  ; m
      Anthropic
      "claude-haiku-4-5-20251001"
      "Claude Haiku 4.5"
      ~ctx:200000
      ~max:64000
      ~thinking:true
      ~input:1.
      ~output:5.
      ~cache_read:0.1
  ; m
      Anthropic
      "claude-opus-4-5"
      "Claude Opus 4.5 (latest)"
      ~ctx:200000
      ~max:64000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-5-20251101"
      "Claude Opus 4.5"
      ~ctx:200000
      ~max:64000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-6"
      "Claude Opus 4.6"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-7"
      "Claude Opus 4.7"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-8"
      "Claude Opus 4.8"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-5"
      "Claude Opus 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-sonnet-4-5"
      "Claude Sonnet 4.5 (latest)"
      ~ctx:1000000
      ~max:64000
      ~thinking:true
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-4-5-20250929"
      "Claude Sonnet 4.5"
      ~ctx:1000000
      ~max:64000
      ~thinking:true
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-4-6"
      "Claude Sonnet 4.6"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-5"
      "Claude Sonnet 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:true
      ~input:2.
      ~output:10.
      ~cache_read:0.2
  ; m
      Openai
      "gpt-4"
      "GPT-4"
      ~ctx:8192
      ~max:8192
      ~thinking:false
      ~input:30.
      ~output:60.
      ~cache_read:0.
  ; m
      Openai
      "gpt-4-turbo"
      "GPT-4 Turbo"
      ~ctx:128000
      ~max:4096
      ~thinking:false
      ~input:10.
      ~output:30.
      ~cache_read:0.
  ; m
      Openai
      "gpt-4.1"
      "GPT-4.1"
      ~ctx:1047576
      ~max:32768
      ~thinking:false
      ~input:2.
      ~output:8.
      ~cache_read:0.5
  ; m
      Openai
      "gpt-4.1-mini"
      "GPT-4.1 mini"
      ~ctx:1047576
      ~max:32768
      ~thinking:false
      ~input:0.4
      ~output:1.6
      ~cache_read:0.1
  ; m
      Openai
      "gpt-4.1-nano"
      "GPT-4.1 nano"
      ~ctx:1047576
      ~max:32768
      ~thinking:false
      ~input:0.1
      ~output:0.4
      ~cache_read:0.025
  ; m
      Openai
      "gpt-4o"
      "GPT-4o"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-2024-05-13"
      "GPT-4o (2024-05-13)"
      ~ctx:128000
      ~max:4096
      ~thinking:false
      ~input:5.
      ~output:15.
      ~cache_read:0.
  ; m
      Openai
      "gpt-4o-2024-08-06"
      "GPT-4o (2024-08-06)"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-2024-11-20"
      "GPT-4o (2024-11-20)"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-mini"
      "GPT-4o mini"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:0.15
      ~output:0.6
      ~cache_read:0.075
  ; m
      Openai
      "gpt-5"
      "GPT-5"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ; m
      Openai
      "gpt-5-chat-latest"
      "GPT-5 Chat Latest"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ; m
      Openai
      "gpt-5-mini"
      "GPT-5 Mini"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:0.25
      ~output:2.
      ~cache_read:0.025
  ; m
      Openai
      "gpt-5-nano"
      "GPT-5 Nano"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:0.05
      ~output:0.4
      ~cache_read:0.005
  ; m
      Openai
      "gpt-5-pro"
      "GPT-5 Pro"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:15.
      ~output:120.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.1"
      "GPT-5.1"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ; m
      Openai
      "gpt-5.2"
      "GPT-5.2"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.2-chat-latest"
      "GPT-5.2 Chat"
      ~ctx:128000
      ~max:16384
      ~thinking:true
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.2-pro"
      "GPT-5.2 Pro"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:21.
      ~output:168.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.3-chat-latest"
      "GPT-5.3 Chat (latest)"
      ~ctx:128000
      ~max:16384
      ~thinking:false
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.3-codex"
      "GPT-5.3 Codex"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.3-codex-spark"
      "GPT-5.3 Codex Spark"
      ~ctx:128000
      ~max:32000
      ~thinking:true
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.4"
      "GPT-5.4"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:2.5
      ~output:15.
      ~cache_read:0.25
  ; m
      Openai
      "gpt-5.4-mini"
      "GPT-5.4 mini"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:0.75
      ~output:4.5
      ~cache_read:0.075
  ; m
      Openai
      "gpt-5.4-nano"
      "GPT-5.4 nano"
      ~ctx:400000
      ~max:128000
      ~thinking:true
      ~input:0.2
      ~output:1.25
      ~cache_read:0.02
  ; m
      Openai
      "gpt-5.4-pro"
      "GPT-5.4 Pro"
      ~ctx:1050000
      ~max:128000
      ~thinking:true
      ~input:30.
      ~output:180.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.5"
      "GPT-5.5"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; m
      Openai
      "gpt-5.5-pro"
      "GPT-5.5 Pro"
      ~ctx:1050000
      ~max:128000
      ~thinking:true
      ~input:30.
      ~output:180.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.6-luna"
      "GPT-5.6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:0.2
      ~output:1.2
      ~cache_read:0.02
  ; m
      Openai
      "gpt-5.6-sol"
      "GPT-5.6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:4.
      ~output:20.
      ~cache_read:0.4
  ; m
      Openai
      "gpt-5.6-terra"
      "GPT-5.6 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:2.
      ~output:12.
      ~cache_read:0.2
  ; m
      Openai
      "gpt-6-astra"
      "GPT-6 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Openai
      "gpt-realtime-2.1"
      "GPT-Realtime-2.1"
      ~ctx:128000
      ~max:32000
      ~thinking:true
      ~input:4.
      ~output:24.
      ~cache_read:0.4
  ; m
      Openai
      "o1"
      "o1"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:15.
      ~output:60.
      ~cache_read:7.5
  ; m
      Openai
      "o1-pro"
      "o1-pro"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:150.
      ~output:600.
      ~cache_read:0.
  ; m
      Openai
      "o3"
      "o3"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:2.
      ~output:8.
      ~cache_read:0.5
  ; m
      Openai
      "o3-mini"
      "o3-mini"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:1.1
      ~output:4.4
      ~cache_read:0.55
  ; m
      Openai
      "o3-pro"
      "o3-pro"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:20.
      ~output:80.
      ~cache_read:0.
  ; m
      Openai
      "o4-mini"
      "o4-mini"
      ~ctx:200000
      ~max:100000
      ~thinking:true
      ~input:1.1
      ~output:4.4
      ~cache_read:0.275
  ; m
      Openai_codex
      "gpt-5.3-codex-spark"
      "GPT-5.3 Codex Spark"
      ~ctx:128000
      ~max:128000
      ~thinking:true
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai_codex
      "gpt-5.5"
      "GPT-5.5"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; m
      Openai_codex
      "gpt-5.6-luna"
      "GPT-5.6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:0.2
      ~output:1.2
      ~cache_read:0.02
  ; m
      Openai_codex
      "gpt-5.6-sol"
      "GPT-5.6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; m
      Openai_codex
      "gpt-5.6-terra"
      "GPT-5.6 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:2.
      ~output:12.
      ~cache_read:0.2
  ; m
      Openai_codex
      "gpt-6-astra"
      "GPT-6 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:true
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Deepseek
      "deepseek-flash"
      "DeepSeek V4.1 Flash"
      ~ctx:1000000
      ~max:384000
      ~thinking:true
      ~input:0.3
      ~output:1.2
      ~cache_read:0.006
  ; m
      Deepseek
      "deepseek-v4-pro"
      "DeepSeek V4 Pro"
      ~ctx:1000000
      ~max:384000
      ~thinking:true
      ~input:1.32
      ~output:3.96
      ~cache_read:0.044
  ]
;;

let find_exn k = List.find_exn all ~f:(fun t -> String.equal (key t) k)
let default = find_exn "deepseek/deepseek-flash"

let default_for : Provider_id.t -> t = function
  | Anthropic -> find_exn "anthropic/claude-opus-4-6"
  | Openai -> find_exn "openai/gpt-5.5"
  | Openai_codex -> find_exn "openai-codex/gpt-5.5"
  | Deepseek -> default
;;

let find s =
  match String.lsplit2 s ~on:'/' with
  | Some (provider, id) ->
    Option.bind (Provider_id.of_string provider) ~f:(fun provider ->
      List.find all ~f:(fun t ->
        Provider_id.equal t.provider provider && String.equal t.id id))
  | None -> List.find all ~f:(fun t -> String.equal t.id s)
;;

let cost_usd t (usage : Usage.t) =
  let per_m tokens price = Float.of_int tokens *. price /. 1e6 in
  per_m (usage.input - usage.cache_read) t.cost.input
  +. per_m usage.cache_read t.cost.cache_read
  +. per_m usage.output t.cost.output
;;
