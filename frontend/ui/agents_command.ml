open! Core
open! Import

module Action = struct
  type t =
    | Pick
    | Cancel of string
    | Message of
        { agent : string
        ; kind : Queued_message.Kind.t
        ; text : string
        }
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage =
  "/agents [cancel <n|id> | steer <n|id> TEXT | follow-up <n|id> TEXT]"
;;

let subcommands =
  [ "cancel", "cancel subagent n"
  ; "steer", "tell subagent n something at its next step"
  ; "follow-up", "tell subagent n something when it would report"
  ]
;;

let split_word s =
  let s = String.lstrip s in
  match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> s, ""
  | Some i -> String.prefix s i, String.lstrip (String.drop_prefix s i)
;;

let message (kind : Queued_message.Kind.t) after : Action.t =
  let agent, text = split_word after in
  if String.is_empty agent || String.is_empty (String.strip text)
  then
    Error
      (sprintf "usage: /agents %s <n|id> TEXT" (Queued_message.Kind.name kind))
  else Message { agent; kind; text }
;;

let run rest : Action.t =
  let word, after = split_word rest in
  match word with
  | "" -> Pick
  | "cancel" ->
    (match split_word after with
     | agent, "" when not (String.is_empty agent) -> Cancel agent
     | _ -> Error "usage: /agents cancel <n|id>")
  | "steer" -> message Steer after
  | "follow-up" | "follow_up" | "followup" -> message Follow_up after
  | word ->
    (match
       List.find subcommands ~f:(fun (s, _) ->
         Edit_distance.distance s word <= 2)
     with
     | Some (close, _) ->
       Error
         (sprintf
            "unknown /agents %S; did you mean %s? usage: %s"
            word
            close
            usage)
     | None -> Error ("usage: " ^ usage))
;;

let find (agents : Agent_view.t list) arg =
  match Int.of_string_opt arg with
  | Some n when n >= 1 -> List.nth agents (n - 1)
  | Some _ -> None
  | None -> Agent_view.find agents arg
;;

let unknown (agents : Agent_view.t list) arg =
  match agents with
  | [] -> "no subagents: the main agent starts them when it delegates work"
  | agents ->
    sprintf
      "no subagent %s; known: %s"
      arg
      (String.concat
         ~sep:", "
         (List.mapi agents ~f:(fun i (a : Agent_view.t) ->
            sprintf "%d (%s)" (i + 1) a.id)))
;;

let complete ~(agents : Agent_view.t list) rest =
  match String.lfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> Some (0, subcommands)
  | Some i ->
    let word = String.prefix rest i in
    let name = String.lstrip (String.drop_prefix rest i) in
    if
      List.Assoc.mem subcommands word ~equal:String.equal
      && not (String.exists name ~f:Char.is_whitespace)
    then
      Some
        ( String.length rest - String.length name
        , List.mapi agents ~f:(fun i (a : Agent_view.t) ->
            ( Int.to_string (i + 1)
            , sprintf
                "%s · %s"
                a.id
                (String.concat ~sep:" " (String.split_lines a.task)) )) )
    else None
;;
