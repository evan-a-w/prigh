open! Core

(** DeepSeek API key: [DEEPSEEK_API_KEY] in the environment, else the
    ["deepseek"] key of [~/.config/prigh/auth.json]. *)
val deepseek_api_key
  :  ?env:(string -> string option)
  -> ?auth_file:string
  -> unit
  -> string Or_error.t
