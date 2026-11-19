open! Core
open! Import

module Cost = struct
  module Long_prompt = struct
    type t =
      { above : int
      ; input : float
      ; output : float
      ; cache_read : float
      }
    [@@deriving sexp_of]
  end

  type t =
    { input : float
    ; output : float
    ; cache_read : float
    ; long_prompt : Long_prompt.t option [@sexp.option]
    }
  [@@deriving sexp_of]
end

module Thinking_style = struct
  type t =
    | Budget of { effort : bool }
    | Adaptive
    | Adaptive_between_tools
    | Effort of { off_is_none : bool }
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; provider : Provider_id.t
  ; name : string
  ; context_window : int
  ; max_output : int
  ; thinking_levels : Thinking.Level.t list
  ; can_disable_thinking : bool
  ; thinking_default : Thinking.t option
  ; thinking_style : Thinking_style.t
  ; cost : Cost.t
  ; supports_images : bool
  ; native_computer : Native_computer.t option [@sexp.option]
  }
[@@deriving sexp_of]

let key t = Provider_id.to_string t.provider ^ "/" ^ t.id

module Spec = struct
  type t =
    { levels : Thinking.Level.t list
    ; off : bool
    ; default : Thinking.t option
    ; style : Thinking_style.t
    }
end

let m
      ?(images = true)
      ?long_prompt
      provider
      id
      name
      ~ctx
      ~max
      ~(thinking : Spec.t)
      ~input
      ~output
      ~cache_read
  =
  { id
  ; provider
  ; name
  ; context_window = ctx
  ; max_output = max
  ; thinking_levels = thinking.levels
  ; can_disable_thinking = thinking.off
  ; thinking_default = thinking.default
  ; thinking_style = thinking.style
  ; cost = { input; output; cache_read; long_prompt }
  ; supports_images = images
  ; native_computer = Native_computer.of_model provider id
  }
;;

let no_thinking =
  { Spec.levels = []
  ; off = true
  ; default = None
  ; style = Effort { off_is_none = false }
  }
;;

let level l = Some (Thinking.On (Some l))
let five : Thinking.Level.t list = [ Low; Medium; High; Xhigh; Max ]
let no_xhigh : Thinking.Level.t list = [ Low; Medium; High; Max ]
let to_xhigh : Thinking.Level.t list = [ Low; Medium; High; Xhigh ]
let pro : Thinking.Level.t list = [ Medium; High; Xhigh ]

(* Anthropic's effort docs and the per-model table of "Troubleshooting
   thinking" (November 2026): which effort levels each model takes, its
   default, and whether [thinking.type = disabled] (or, on Claude Sonnet 5.5,
   [between_tools]) is accepted. Claude Opus 5 and Haiku 5.5 reject [disabled]
   only at xhigh and max, which off never sends. *)
let claude_adaptive
      ?(off = true)
      ?(between_tools = false)
      ?(default : Thinking.Level.t = High)
      levels
  =
  { Spec.levels
  ; off
  ; default = level default
  ; style = (if between_tools then Adaptive_between_tools else Adaptive)
  }
;;

(* Extended thinking only: a [budget_tokens] per level, ours to choose
   ({!Thinking.budget_tokens}); with no level, the medium one. *)
let claude_budget =
  { Spec.levels = five
  ; off = true
  ; default = level Medium
  ; style = Budget { effort = false }
  }
;;

(* Claude Opus 4.5, "the only extended-thinking-only model that supports
   effort" (low, medium, high; default high), alongside the budget. *)
let claude_budget_effort =
  { Spec.levels = [ Low; Medium; High ]
  ; off = true
  ; default = level High
  ; style = Budget { effort = true }
  }
;;

(* Each model's page in OpenAI's API docs ("Reasoning.effort supports: ...",
   November 2026). [none]: it takes effort "none", which is how off is sent;
   otherwise it always reasons. *)
let openai ?(none = false) ?(default = level Medium) levels =
  { Spec.levels; off = none; default; style = Effort { off_is_none = none } }
;;

(* Over 272K input tokens: twice the input price and 1.5 times the
   output. *)
let openai_pro_long_prompt =
  { Cost.Long_prompt.above = 272000
  ; input = 60.
  ; output = 270.
  ; cache_read = 0.
  }
;;

let gpt5 = openai [ Minimal; Low; Medium; High ]
let o_series = openai [ Low; Medium; High ]

let gemini ?long_prompt ~default levels id name ~input ~output ~cache_read =
  m
    ?long_prompt
    Gemini
    id
    name
    ~ctx:1048576
    ~max:65536
    ~thinking:(openai ~default:(level default) levels)
    ~input
    ~output
    ~cache_read
;;

let gemini_31_pro id name =
  gemini
    ~long_prompt:{ above = 200000; input = 4.; output = 18.; cache_read = 0.4 }
    ~default:High
    [ Low; Medium; High ]
    id
    name
    ~input:2.
    ~output:12.
    ~cache_read:0.2
;;

(* DeepSeek's thinking mode guide: [thinking.type] enabled or disabled, and
   [reasoning_effort] low, high or max (default high). *)
let deepseek =
  { Spec.levels = [ Low; High; Max ]
  ; off = true
  ; default = level High
  ; style = Effort { off_is_none = false }
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
      ~thinking:(claude_adaptive ~off:false five)
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Anthropic
      "claude-fable-5-1"
      "Claude Fable 5.1"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false five)
      ~input:10.
      ~output:50.
      ~cache_read:0.25
  ; m
      Anthropic
      "claude-fable-5-2"
      "Claude Fable 5.2"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false five)
      ~input:8.
      ~output:40.
      ~cache_read:0.2
  ; m
      Anthropic
      "claude-haiku-4-5"
      "Claude Haiku 4.5 (latest)"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget
      ~input:1.
      ~output:5.
      ~cache_read:0.1
  ; m
      Anthropic
      "claude-haiku-4-5-20251001"
      "Claude Haiku 4.5"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget
      ~input:1.
      ~output:5.
      ~cache_read:0.1
  ; m
      Anthropic
      "claude-haiku-5-5"
      "Claude Haiku 5.5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~default:Medium five)
      ~input:0.1
      ~output:0.5
      ~cache_read:0.01
      ~long_prompt:
        { above = 100000; input = 0.5; output = 2.5; cache_read = 0.05 }
  ; m
      Anthropic
      "claude-mythos-5"
      "Claude Mythos 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false five)
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Anthropic
      "claude-mythos-5-1"
      "Claude Mythos 5.1"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false five)
      ~input:10.
      ~output:50.
      ~cache_read:0.25
  ; m
      Anthropic
      "claude-mythos-5-2"
      "Claude Mythos 5.2"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false five)
      ~input:8.
      ~output:40.
      ~cache_read:0.2
  ; m
      Anthropic
      "claude-opus-4-5"
      "Claude Opus 4.5 (latest)"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget_effort
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-5-20251101"
      "Claude Opus 4.5"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget_effort
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-6"
      "Claude Opus 4.6"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive no_xhigh)
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-7"
      "Claude Opus 4.7"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive five)
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-4-8"
      "Claude Opus 4.8"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive five)
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-5"
      "Claude Opus 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive five)
      ~input:5.
      ~output:25.
      ~cache_read:0.5
  ; m
      Anthropic
      "claude-opus-5-5"
      "Claude Opus 5.5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~off:false ~default:Medium five)
      ~input:4.
      ~output:20.
      ~cache_read:0.2
  ; m
      Anthropic
      "claude-sonnet-4-5"
      "Claude Sonnet 4.5 (latest)"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-4-5-20250929"
      "Claude Sonnet 4.5"
      ~ctx:200000
      ~max:64000
      ~thinking:claude_budget
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-4-6"
      "Claude Sonnet 4.6"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive no_xhigh)
      ~input:3.
      ~output:15.
      ~cache_read:0.3
  ; m
      Anthropic
      "claude-sonnet-5"
      "Claude Sonnet 5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive five)
      ~input:2.
      ~output:10.
      ~cache_read:0.2
  ; m
      Anthropic
      "claude-sonnet-5-5"
      "Claude Sonnet 5.5"
      ~ctx:1000000
      ~max:128000
      ~thinking:(claude_adaptive ~between_tools:true five)
      ~input:2.
      ~output:10.
      ~cache_read:0.2
  ; m
      Openai
      "chat-latest"
      "Chat Latest"
      ~ctx:400000
      ~max:128000
      ~thinking:no_thinking
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; m
      Openai
      "gpt-4.1"
      "GPT-4.1"
      ~ctx:1047576
      ~max:32768
      ~thinking:no_thinking
      ~input:2.
      ~output:8.
      ~cache_read:0.5
  ; m
      Openai
      "gpt-4.1-mini"
      "GPT-4.1 mini"
      ~ctx:1047576
      ~max:32768
      ~thinking:no_thinking
      ~input:0.4
      ~output:1.6
      ~cache_read:0.1
  ; m
      Openai
      "gpt-4o"
      "GPT-4o"
      ~ctx:128000
      ~max:16384
      ~thinking:no_thinking
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-2024-08-06"
      "GPT-4o (2024-08-06)"
      ~ctx:128000
      ~max:16384
      ~thinking:no_thinking
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-2024-11-20"
      "GPT-4o (2024-11-20)"
      ~ctx:128000
      ~max:16384
      ~thinking:no_thinking
      ~input:2.5
      ~output:10.
      ~cache_read:1.25
  ; m
      Openai
      "gpt-4o-mini"
      "GPT-4o mini"
      ~ctx:128000
      ~max:16384
      ~thinking:no_thinking
      ~input:0.15
      ~output:0.6
      ~cache_read:0.075
  ; m
      Openai
      "gpt-5"
      "GPT-5"
      ~ctx:400000
      ~max:128000
      ~thinking:gpt5
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ; m
      Openai
      "gpt-5-mini"
      "GPT-5 Mini"
      ~ctx:400000
      ~max:128000
      ~thinking:gpt5
      ~input:0.25
      ~output:2.
      ~cache_read:0.025
  ; m
      Openai
      "gpt-5-nano"
      "GPT-5 Nano"
      ~ctx:400000
      ~max:128000
      ~thinking:gpt5
      ~input:0.05
      ~output:0.4
      ~cache_read:0.005
  ; m
      Openai
      "gpt-5-pro"
      "GPT-5 Pro"
      ~ctx:400000
      ~max:272000
      ~thinking:(openai ~default:(level High) [ High ])
      ~input:15.
      ~output:120.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.1"
      "GPT-5.1"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~none:true ~default:(Some Off) [ Low; Medium; High ])
      ~input:1.25
      ~output:10.
      ~cache_read:0.125
  ; m
      Openai
      "gpt-5.2"
      "GPT-5.2"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~none:true ~default:(Some Off) to_xhigh)
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.2-pro"
      "GPT-5.2 Pro"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~default:None pro)
      ~input:21.
      ~output:168.
      ~cache_read:0.
  ; m
      Openai
      "gpt-5.3-codex"
      "GPT-5.3 Codex"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~default:None to_xhigh)
      ~input:1.75
      ~output:14.
      ~cache_read:0.175
  ; m
      Openai
      "gpt-5.4"
      "GPT-5.4"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true ~default:(Some Off) to_xhigh)
      ~input:2.5
      ~output:15.
      ~cache_read:0.25
  ; m
      Openai
      "gpt-5.4-mini"
      "GPT-5.4 mini"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~none:true ~default:(Some Off) to_xhigh)
      ~input:0.75
      ~output:4.5
      ~cache_read:0.075
  ; m
      Openai
      "gpt-5.4-nano"
      "GPT-5.4 nano"
      ~ctx:400000
      ~max:128000
      ~thinking:(openai ~none:true ~default:(Some Off) to_xhigh)
      ~input:0.2
      ~output:1.25
      ~cache_read:0.02
  ; m
      Openai
      "gpt-5.4-pro"
      "GPT-5.4 Pro"
      ~ctx:1050000
      ~max:128000
      ~thinking:(openai pro)
      ~input:30.
      ~output:180.
      ~cache_read:0.
      ~long_prompt:openai_pro_long_prompt
  ; m
      Openai
      "gpt-5.5"
      "GPT-5.5"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true to_xhigh)
      ~input:5.
      ~output:30.
      ~cache_read:0.5
  ; m
      Openai
      "gpt-5.5-pro"
      "GPT-5.5 Pro"
      ~ctx:1050000
      ~max:128000
      ~thinking:(openai ~default:(level High) pro)
      ~input:30.
      ~output:180.
      ~cache_read:0.
      ~long_prompt:openai_pro_long_prompt
  ; m
      Openai
      "gpt-5.6-luna"
      "GPT-5.6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:0.2
      ~output:1.2
      ~cache_read:0.02
  ; m
      Openai
      "gpt-5.6-sol"
      "GPT-5.6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:4.
      ~output:20.
      ~cache_read:0.4
  ; m
      Openai
      "gpt-5.6-terra"
      "GPT-5.6 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:2.
      ~output:12.
      ~cache_read:0.2
  ; m
      Openai
      "gpt-6-astra"
      "GPT-6 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~default:None five)
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Openai
      "gpt-6-luna"
      "GPT-6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:0.1
      ~output:0.5
      ~cache_read:0.01
  ; m
      Openai
      "gpt-6-sol"
      "GPT-6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:2.
      ~output:10.
      ~cache_read:0.2
  ; m
      Openai
      "gpt-6.1-sol"
      "GPT-6.1 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai five)
      ~input:2.
      ~output:10.
      ~cache_read:0.1
  ; m
      Openai
      "gpt-6.1-terra"
      "GPT-6.1 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:1.
      ~output:5.
      ~cache_read:0.05
  ; m
      Openai
      "gpt-6.2-astra"
      "GPT-6.2 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~default:None five)
      ~input:8.
      ~output:40.
      ~cache_read:0.4
  ; m
      Openai
      "gpt-6.2-sol"
      "GPT-6.2 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai five)
      ~input:2.
      ~output:10.
      ~cache_read:0.1
  ; m
      Openai
      "gpt-realtime-2.1"
      "GPT-Realtime-2.1"
      ~ctx:128000
      ~max:32000
      ~thinking:
        (openai ~default:(level Low) [ Minimal; Low; Medium; High; Xhigh ])
      ~input:4.
      ~output:24.
      ~cache_read:0.4
  ; m
      Openai
      "o3"
      "o3"
      ~ctx:200000
      ~max:100000
      ~thinking:o_series
      ~input:2.
      ~output:8.
      ~cache_read:0.5
  ; m
      Openai
      "o3-pro"
      "o3-pro"
      ~ctx:200000
      ~max:100000
      ~thinking:o_series
      ~input:20.
      ~output:80.
      ~cache_read:0.
  ; m
      Openai_codex
      "gpt-5.6-luna"
      "GPT-5.6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:0.2
      ~output:1.2
      ~cache_read:0.02
  ; m
      Openai_codex
      "gpt-5.6-sol"
      "GPT-5.6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:4.
      ~output:20.
      ~cache_read:0.4
  ; m
      Openai_codex
      "gpt-5.6-terra"
      "GPT-5.6 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:2.
      ~output:12.
      ~cache_read:0.2
  ; m
      Openai_codex
      "gpt-6-astra"
      "GPT-6 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~default:None five)
      ~input:10.
      ~output:50.
      ~cache_read:1.
  ; m
      Openai_codex
      "gpt-6-luna"
      "GPT-6 Luna"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:0.1
      ~output:0.5
      ~cache_read:0.01
  ; m
      Openai_codex
      "gpt-6-sol"
      "GPT-6 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:2.
      ~output:10.
      ~cache_read:0.2
  ; m
      Openai_codex
      "gpt-6.1-sol"
      "GPT-6.1 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai five)
      ~input:2.
      ~output:10.
      ~cache_read:0.1
  ; m
      Openai_codex
      "gpt-6.1-terra"
      "GPT-6.1 Terra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~none:true five)
      ~input:1.
      ~output:5.
      ~cache_read:0.05
  ; m
      Openai_codex
      "gpt-6.2-astra"
      "GPT-6.2 Astra"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai ~default:None five)
      ~input:8.
      ~output:40.
      ~cache_read:0.4
  ; m
      Openai_codex
      "gpt-6.2-sol"
      "GPT-6.2 Sol"
      ~ctx:272000
      ~max:128000
      ~thinking:(openai five)
      ~input:2.
      ~output:10.
      ~cache_read:0.1
  ; m
      Deepseek
      "deepseek-flash"
      "DeepSeek V4.1 Flash"
      ~ctx:1000000
      ~max:384000
      ~thinking:deepseek
      ~input:0.3
      ~output:1.2
      ~cache_read:0.006
  ; m
      ~images:false
      Deepseek
      "deepseek-v4-pro"
      "DeepSeek V4 Pro"
      ~ctx:1000000
      ~max:384000
      ~thinking:deepseek
      ~input:1.32
      ~output:3.96
      ~cache_read:0.044
  ; gemini
      ~default:Medium
      [ Low; Medium; High ]
      "gemini-4-carbon"
      "Gemini 4 Carbon"
      ~input:2.
      ~output:10.
      ~cache_read:0.10
  ; gemini
      ~default:Medium
      [ Low; Medium; High ]
      "gemini-4-argon"
      "Gemini 4 Argon"
      ~input:2.
      ~output:10.
      ~cache_read:0.10
  ; gemini
      ~default:High
      to_xhigh
      "gemini-4-neon"
      "Gemini 4 Neon"
      ~input:3.
      ~output:15.
      ~cache_read:0.15
  ; gemini
      ~default:High
      to_xhigh
      "gemini-4-hydrogen"
      "Gemini 4 Hydrogen"
      ~input:1.5
      ~output:7.5
      ~cache_read:0.075
  ; gemini
      ~default:Medium
      [ Low; Medium; High ]
      "gemini-3.8-flash"
      "Gemini 3.8 Flash"
      ~input:0.75
      ~output:3.75
      ~cache_read:0.075
  ; gemini
      ~default:Medium
      [ Minimal; Low; Medium; High ]
      "gemini-3.6-flash"
      "Gemini 3.6 Flash"
      ~input:0.75
      ~output:3.75
      ~cache_read:0.075
  ; gemini
      ~default:Minimal
      [ Minimal; Low; Medium; High ]
      "gemini-3.5-flash-lite"
      "Gemini 3.5 Flash-Lite"
      ~input:0.30
      ~output:2.50
      ~cache_read:0.03
  ; gemini
      ~default:Minimal
      [ Minimal; Low; Medium; High ]
      "gemini-3.1-flash-lite"
      "Gemini 3.1 Flash-Lite"
      ~input:0.25
      ~output:1.50
      ~cache_read:0.025
  ; gemini_31_pro "gemini-3.1-pro-preview" "Gemini 3.1 Pro Preview"
  ; gemini_31_pro
      "gemini-3.1-pro-preview-customtools"
      "Gemini 3.1 Pro Preview (Custom Tools)"
  ; gemini
      ~default:High
      [ Minimal; Low; Medium; High ]
      "gemini-3-flash-preview"
      "Gemini 3 Flash Preview"
      ~input:0.50
      ~output:3.
      ~cache_read:0.05
  ]
;;

let find_exn k = List.find_exn all ~f:(fun t -> String.equal (key t) k)
let default = find_exn "deepseek/deepseek-flash"

let default_for : Provider_id.t -> t option = function
  | Anthropic -> Some (find_exn "anthropic/claude-opus-4-6")
  | Openai -> Some (find_exn "openai/gpt-5.5")
  | Openai_codex -> Some (find_exn "openai-codex/gpt-6.1-sol")
  | Deepseek -> Some default
  | Gemini -> Some (find_exn "gemini/gemini-4-hydrogen")
  | Custom _ -> None
;;

let key_provider key = Option.map (String.lsplit2 key ~on:'/') ~f:fst

(* Ids may contain '/' (OpenRouter's [vendor/model]): only a known provider
   name before the first '/' makes it a key. *)
let find_in models s =
  let by_id id = List.find models ~f:(fun t -> String.equal t.id id) in
  match String.lsplit2 s ~on:'/' with
  | Some (provider, id) ->
    (match
       List.find models ~f:(fun t ->
         String.equal (Provider_id.to_string t.provider) provider
         && String.equal t.id id)
     with
     | Some t -> Some t
     | None -> by_id s)
  | None -> by_id s
;;

let find = find_in all
let names t = [ key t; t.id; t.name ]

let closest_in models query =
  let q = String.lowercase (String.strip query) in
  (* [anthropic/nope]: that provider's models, not every close id. *)
  let candidates =
    match
      Option.bind (key_provider q) ~f:(fun provider ->
        match
          List.filter models ~f:(fun t ->
            String.equal (Provider_id.to_string t.provider) provider)
        with
        | [] -> None
        | some -> Some some)
    with
    | Some some -> some
    | None -> models
  in
  let scored =
    List.map candidates ~f:(fun t ->
      ( List.min_elt
          (List.map (names t) ~f:(fun n -> Edit_distance.caseless n q))
          ~compare:Int.compare
        |> Option.value ~default:Int.max_value
      , t ))
    |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  match scored with
  | [] -> []
  | (best, _) :: _ ->
    List.take_while scored ~f:(fun (d, _) -> d <= best + 2)
    |> Fn.flip List.take 3
    |> List.map ~f:snd
;;

let resolve_in models query =
  let q = String.lowercase (String.strip query) in
  let matches ~f = List.filter models ~f:(fun t -> List.exists (names t) ~f) in
  let keys ts = String.concat ~sep:", " (List.map ts ~f:key) in
  match find_in models query with
  | Some t -> Ok t
  | None ->
    (match matches ~f:(fun n -> String.equal (String.lowercase n) q) with
     | [ t ] -> Ok t
     | _ :: _ as many ->
       Or_error.errorf "model %S is ambiguous; one of: %s" query (keys many)
     | [] ->
       (match
          matches ~f:(fun n -> String.is_prefix (String.lowercase n) ~prefix:q)
        with
        | [ t ] -> Ok t
        | _ :: _ as many ->
          Or_error.errorf "model %S is ambiguous; one of: %s" query (keys many)
        | [] ->
          Or_error.errorf
            "unknown model %S; did you mean: %s"
            query
            (String.concat
               ~sep:", "
               (List.map (closest_in models query) ~f:(fun t ->
                  sprintf "%s (%s)" (key t) t.name)))))
;;

let resolve = resolve_in all

let written_by model_key provider =
  match find model_key with
  | Some m -> Provider_id.equal m.provider provider
  | None ->
    Option.equal
      String.equal
      (key_provider model_key)
      (Some (Provider_id.to_string provider))
;;

let cost_usd t (usage : Usage.t) =
  let per_m tokens price = Float.of_int tokens *. price /. 1e6 in
  let input, output, cache_read =
    match t.cost.long_prompt with
    | Some long when usage.input > long.above ->
      long.input, long.output, long.cache_read
    | Some _ | None -> t.cost.input, t.cost.output, t.cost.cache_read
  in
  per_m (usage.input - usage.cache_read) input
  +. per_m usage.cache_read cache_read
  +. per_m usage.output output
;;

(* As of the preserved thinking docs (November 2026). *)
let binds_thinking_to_prefix t =
  match t.provider with
  | Anthropic ->
    List.exists
      [ "claude-fable-5-1"
      ; "claude-mythos-5-1"
      ; "claude-fable-5-2"
      ; "claude-mythos-5-2"
      ; "claude-opus-5-5"
      ; "claude-sonnet-5-5"
      ; "claude-haiku-5-5"
      ]
      ~f:(fun family ->
        String.equal t.id family || String.is_prefix t.id ~prefix:(family ^ "-"))
  | Openai | Openai_codex | Deepseek | Gemini | Custom _ -> false
;;

let supports_thinking t = not (List.is_empty t.thinking_levels)

let thinking_options t : Thinking.t list =
  if supports_thinking t
  then
    (if t.can_disable_thinking then [ Thinking.Off ] else [])
    @ [ Thinking.On None ]
    @ List.map t.thinking_levels ~f:(fun level -> Thinking.On (Some level))
  else [ Off ]
;;

let clamp_thinking t thinking =
  Thinking.clamp
    thinking
    ~levels:t.thinking_levels
    ~can_disable:t.can_disable_thinking
;;

let check_thinking t thinking =
  if List.mem (thinking_options t) thinking ~equal:Thinking.equal
  then Ok thinking
  else
    Or_error.errorf
      "%s does not take thinking %s; it takes: %s"
      t.name
      (Thinking.to_string thinking)
      (String.concat
         ~sep:", "
         (List.map (thinking_options t) ~f:Thinking.to_string))
;;
