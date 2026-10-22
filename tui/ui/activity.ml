open! Core
open! Import

type t =
  | Idle
  | Working
  | Waiting of P.Retry.t option
  | Retrying of P.Retry.t
  | Thinking
  | Writing
  | Running_tools
[@@deriving sexp_of, equal]

let of_running running = if running then Working else Idle

let set_running t running =
  match t, running with
  | _, false -> Idle
  | Idle, true -> Working
  | t, true -> t
;;

let apply t (event : P.Event.t) =
  match event with
  | Agent_start | Turn_start -> Working
  | Agent_end _ -> Idle
  | Retry retry -> Retrying retry
  | Message_start (Assistant _) ->
    (match t with
     | Retrying retry -> Waiting (Some retry)
     | _ -> Waiting None)
  | Message_update { delta; _ } ->
    (match delta with
     | Thinking_delta _ | Thinking_signature -> Thinking
     | Text_delta _
     | Tool_call_start _
     | Tool_call_delta _
     | Server_tool_result _ -> Writing
     | Server_tool_use _ -> Running_tools)
  | Message_end (Assistant a) ->
    if
      List.exists a.content ~f:(function
        | Tool_call _ -> true
        | Text _ | Thinking _ | Server_tool _ -> false)
    then Running_tools
    else Working
  | Tool_start _ -> Running_tools
  | State s -> set_running t s.running
  | _ -> t
;;

let one_line ~max text =
  let text =
    String.concat_map text ~f:(function
      | '\n' | '\r' | '\t' -> " "
      | c -> String.of_char c)
    |> String.strip
  in
  if String.length text <= max then text else String.prefix text (max - 1) ^ "…"
;;

let seconds ms =
  if ms < 1000 then sprintf "%dms" ms else sprintf "%ds" ((ms + 500) / 1000)
;;

let label = function
  | Idle -> None
  | Working -> Some "working"
  | Waiting None -> Some "waiting for the model"
  | Waiting (Some { attempt; max_attempts; error; delay_ms = _ }) ->
    Some
      (sprintf
         "waiting for the model (attempt %d/%d, after: %s)"
         attempt
         max_attempts
         (one_line ~max:80 error))
  | Retrying { attempt; max_attempts; error; delay_ms } ->
    Some
      (sprintf
         "retrying in %s (attempt %d/%d): %s"
         (seconds delay_ms)
         attempt
         max_attempts
         (one_line ~max:80 error))
  | Thinking -> Some "thinking"
  | Writing -> Some "writing"
  | Running_tools -> Some "running tools"
;;

let brief = function
  | Waiting (Some { attempt; max_attempts; _ }) ->
    Some (sprintf "waiting for the model (attempt %d/%d)" attempt max_attempts)
  | Retrying { delay_ms; _ } ->
    Some (sprintf "retrying in %s" (seconds delay_ms))
  | t -> label t
;;

let retry = function
  | Waiting retry -> retry
  | Retrying retry -> Some retry
  | Idle | Working | Thinking | Writing | Running_tools -> None
;;
