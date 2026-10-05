open! Core
open! Import

module Status = struct
  type t =
    | Ready of Mcp_client.Tool.t list
    | Failed of string
    | Needs_approval
  [@@deriving sexp_of]
end

module Server_status = struct
  type t =
    { server : Mcp_config.Server.t
    ; status : Status.t
    }
  [@@deriving sexp_of]
end

type t = unit

let create ~env:_ ~sw:_ () = ()
let servers () ?reconnect:_ ~cwd:_ ~home:_ () = [], []
let call () ~cancel:_ ~source:_ ~server:_ ~home:_ ~tool:_ ~arguments:_ = Tool_result.error "stub"
let close () = ()
