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

let deepseek =
  [ { id = "deepseek-flash"
    ; provider = Deepseek
    ; name = "DeepSeek V4.1 Flash"
    ; context_window = 1_000_000
    ; max_output = 384_000
    ; supports_thinking = true
    ; cost = { input = 0.3; output = 1.2; cache_read = 0.006 }
    }
  ; { id = "deepseek-v4-pro"
    ; provider = Deepseek
    ; name = "DeepSeek V4 Pro"
    ; context_window = 1_000_000
    ; max_output = 384_000
    ; supports_thinking = true
    ; cost = { input = 1.32; output = 3.96; cache_read = 0.044 }
    }
  ; { id = "deepseek-chat"
    ; provider = Deepseek
    ; name = "DeepSeek Chat (legacy alias)"
    ; context_window = 128_000
    ; max_output = 8_000
    ; supports_thinking = true
    ; cost = { input = 0.28; output = 0.42; cache_read = 0.028 }
    }
  ]
;;

let claude id name ~context_window ~max_output ~input ~output ~cache_read =
  { id
  ; provider = Anthropic
  ; name
  ; context_window
  ; max_output
  ; supports_thinking = true
  ; cost = { input; output; cache_read }
  }
;;

let anthropic =
  [ claude
      "claude-opus-4-6"
      "Claude Opus 4.6"
      ~context_window:1_000_000
      ~max_output:128_000
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; claude
      "claude-sonnet-4-6"
      "Claude Sonnet 4.6"
      ~context_window:1_000_000
      ~max_output:128_000
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; claude
      "claude-opus-4-5"
      "Claude Opus 4.5"
      ~context_window:200_000
      ~max_output:64_000
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; claude
      "claude-sonnet-4-5"
      "Claude Sonnet 4.5"
      ~context_window:1_000_000
      ~max_output:64_000
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; claude
      "claude-haiku-4-5"
      "Claude Haiku 4.5"
      ~context_window:200_000
      ~max_output:64_000
      ~input:1.
      ~output:5.
      ~cache_read:0.1
  ]
;;

let gpt provider id name ~context_window ~input ~output ~cache_read =
  { id
  ; provider
  ; name
  ; context_window
  ; max_output = 128_000
  ; supports_thinking = true
  ; cost = { input; output; cache_read }
  }
;;

let openai =
  [ gpt
      Openai
      "gpt-5.5"
      "GPT-5.5"
      ~context_window:272_000
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; gpt
      Openai
      "gpt-5.4"
      "GPT-5.4"
      ~context_window:272_000
      ~input:2.5
      ~output:15.
      ~cache_read:0.25
  ; gpt
      Openai
      "gpt-5.4-mini"
      "GPT-5.4 mini"
      ~context_window:400_000
      ~input:0.75
      ~output:4.5
      ~cache_read:0.075
  ; gpt
      Openai
      "gpt-5.2"
      "GPT-5.2"
      ~context_window:400_000
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; gpt
      Openai
      "gpt-5.1"
      "GPT-5.1"
      ~context_window:400_000
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ]
;;

(* ChatGPT subscription access; prices are the API equivalents. *)
let openai_codex =
  [ gpt
      Openai_codex
      "gpt-5.5"
      "GPT-5.5 (ChatGPT)"
      ~context_window:272_000
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; gpt
      Openai_codex
      "gpt-5.3-codex-spark"
      "GPT-5.3 Codex Spark (ChatGPT)"
      ~context_window:128_000
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ]
;;

let all = deepseek @ anthropic @ openai @ openai_codex
let default = List.hd_exn deepseek

let default_for provider =
  List.find_exn all ~f:(fun t -> Provider_id.equal t.provider provider)
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
