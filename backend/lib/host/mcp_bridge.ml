open! Core
open! Import

let socket_env = "PRIGH_MCP_SOCKET"

let relay ~stdin ~stdout socket =
  Eio.Fiber.first
    (fun () -> Eio.Flow.copy socket stdout)
    (fun () ->
       Eio.Flow.copy stdin socket;
       Eio.Flow.shutdown socket `Send;
       Eio.Fiber.await_cancel ())
;;

let run env ~socket =
  match socket with
  | None ->
    Or_error.errorf
      "$%s is not set: prigh mcp-bridge is started by a harness prigh runs \
       (Codex, an ACP agent), not by hand"
      socket_env
  | Some path ->
    Eio.Switch.run (fun sw ->
      match Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix path) with
      | exception _ ->
        Or_error.errorf
          "cannot reach prigh's tools at %s: the session that started this \
           harness is gone"
          path
      | socket ->
        relay
          ~stdin:(Eio.Stdenv.stdin env)
          ~stdout:(Eio.Stdenv.stdout env)
          socket;
        Ok ())
;;
