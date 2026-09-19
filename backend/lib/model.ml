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
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of]

let all =
  [ { id = "deepseek-flash"
    ; name = "DeepSeek V4.1 Flash"
    ; context_window = 1_000_000
    ; max_output = 384_000
    ; supports_thinking = true
    ; cost = { input = 0.3; output = 1.2; cache_read = 0.006 }
    }
  ; { id = "deepseek-v4-pro"
    ; name = "DeepSeek V4 Pro"
    ; context_window = 1_000_000
    ; max_output = 384_000
    ; supports_thinking = true
    ; cost = { input = 1.32; output = 3.96; cache_read = 0.044 }
    }
  ; { id = "deepseek-chat"
    ; name = "DeepSeek Chat (legacy alias)"
    ; context_window = 128_000
    ; max_output = 8_000
    ; supports_thinking = true
    ; cost = { input = 0.28; output = 0.42; cache_read = 0.028 }
    }
  ]
;;

let default = List.hd_exn all
let find id = List.find all ~f:(fun t -> String.equal t.id id)

let cost_usd t (usage : Usage.t) =
  let per_m tokens price = Float.of_int tokens *. price /. 1e6 in
  per_m (usage.input - usage.cache_read) t.cost.input
  +. per_m usage.cache_read t.cost.cache_read
  +. per_m usage.output t.cost.output
;;
