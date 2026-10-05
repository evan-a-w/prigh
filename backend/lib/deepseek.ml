open! Core
open! Import

let default_base_url = "https://api.deepseek.com"

let create ~env ?(base_url = default_base_url) ?timeout ~api_key () =
  Openai_chat.create
    ~env
    ?timeout
    ~name:"deepseek"
    ~url:(base_url ^ "/chat/completions")
    ~headers:[ "Authorization", "Bearer " ^ api_key ]
    ~quirks:Openai_chat.Quirks.deepseek
    ()
;;

module For_testing = struct
  let request_body =
    Openai_chat.For_testing.request_body ~quirks:Openai_chat.Quirks.deepseek
  ;;

  module Chunk = Openai_chat.For_testing.Chunk

  let parse_chunk = Openai_chat.For_testing.parse_chunk
end
