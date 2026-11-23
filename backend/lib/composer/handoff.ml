open! Core
open! Import

module Strategy = struct
  type t =
    | Full
    | Since of { entry : string }
  [@@deriving sexp_of, equal]
end

type t =
  { text : string
  ; strategy : Strategy.t
  ; covered : (int * int) option
  ; included : int list
  ; omitted : int list
  ; tokens : int
  }
[@@deriving sexp_of]

let default_budget = 24_000
let text_cap = 8_000
let tool_cap = 2_000
let call_cap = 400

(* The same rough measure as [Message.estimate_tokens]: a quarter of the
   characters. *)
let tokens_of s = (String.length s + 3) / 4

let needed : Adapter_id.t -> bool = function
  | Prigh -> false
  | Codex | Claude | Acp _ -> true
;;

(* Backs [i] off UTF-8 continuation bytes, so a cut never splits a
   character. *)
let rec char_start s i =
  if i <= 0 || i >= String.length s
  then Int.clamp_exn i ~min:0 ~max:(String.length s)
  else if Char.to_int s.[i] land 0xC0 = 0x80
  then char_start s (i - 1)
  else i
;;

let cap s ~cap ~index =
  let len = String.length s in
  if len <= cap
  then s
  else (
    let head = char_start s (cap * 3 / 4) in
    let tail = char_start s (len - (cap / 4)) in
    sprintf
      "%s\n[... %d characters left out: read message #%d for all of it]\n%s"
      (String.prefix s head)
      (tail - head)
      index
      (String.drop_prefix s tail))
;;

let images_note = function
  | [] -> []
  | images ->
    [ sprintf
        "[%d image%s not replayed]"
        (List.length images)
        (if List.length images = 1 then "" else "s")
    ]
;;

let render index (message : Message.t) =
  let item tag lines =
    match List.filter lines ~f:(fun l -> not (String.is_empty l)) with
    | [] -> None
    | lines ->
      Some (String.concat ~sep:"\n" (sprintf "[#%d %s]" index tag :: lines))
  in
  match Session.summary_of_message message with
  | Some summary ->
    item "summary of earlier messages" [ cap summary ~cap:text_cap ~index ]
  | None ->
    (match message with
     | User u ->
       item
         "user"
         (cap (Message.User.body u) ~cap:text_cap ~index :: images_note u.images)
     | Assistant a ->
       let calls =
         List.map (Message.Assistant.tool_calls a) ~f:(fun call ->
           sprintf
             "Tool call %s: %s"
             call.name
             (cap (String.strip call.arguments) ~cap:call_cap ~index))
       in
       item
         "assistant"
         (cap (Message.Assistant.text a) ~cap:text_cap ~index :: calls)
     | Tool_result r ->
       item
         (sprintf
            "tool result: %s%s"
            r.tool_name
            (if r.is_error then ", failed" else ""))
         ((if String.is_empty r.text
           then "(no output)"
           else cap r.text ~cap:tool_cap ~index)
          :: images_note r.images))
;;

(* What [to_] already saw: everything up to the last entry of its latest turn
   in its thread on [host], if it has one there and that entry is still on
   the active path. *)
let seen_through session ~to_ ~host =
  let adapter = Adapter_id.to_string to_ in
  match
    List.find (Session.threads session) ~f:(fun (thread : Session.Thread.t) ->
      String.equal thread.adapter adapter && String.equal thread.host host)
  with
  | None -> None
  | Some thread ->
    (match
       List.last
         (List.filter (Session.external_turns session) ~f:(fun turn ->
            String.equal turn.thread_id thread.thread_id))
     with
     | None -> None
     | Some turn ->
       let ids = List.map (Session.active_path session) ~f:(fun e -> e.id) in
       (match List.findi ids ~f:(fun _ id -> String.equal id turn.through) with
        | None -> None
        | Some (i, _) ->
          Some (turn.through, String.Set.of_list (List.drop ids (i + 1)))))
;;

(* "#0-#3, #7": consecutive indices as ranges. *)
let ranges indices =
  let rec go acc = function
    | [] -> List.rev acc
    | first :: rest ->
      let rec run last = function
        | next :: rest when next = last + 1 -> run next rest
        | rest -> last, rest
      in
      let last, rest = run first rest in
      let s =
        if first = last
        then sprintf "#%d" first
        else if last = first + 1
        then sprintf "#%d, #%d" first last
        else sprintf "#%d-#%d" first last
      in
      go (s :: acc) rest
  in
  String.concat ~sep:", " (go [] indices)
;;

(* T3's order: the latest user message, the latest assistant one and the
   first user one, then newest to oldest while they fit. *)
let select items ~budget =
  let is_user (_, (m : Message.t), _) =
    match m with
    | User _ -> true
    | _ -> false
  in
  let is_assistant (_, (m : Message.t), _) =
    match m with
    | Assistant _ -> true
    | _ -> false
  in
  let newest_first = List.rev items in
  let priority =
    List.filter_opt
      [ List.find newest_first ~f:is_user
      ; List.find newest_first ~f:is_assistant
      ; List.find items ~f:is_user
      ]
  in
  let take (chosen, used) (index, _, text) =
    if Set.mem chosen index
    then Some (chosen, used)
    else (
      let cost = tokens_of text + 1 in
      if used + cost > budget
      then None
      else Some (Set.add chosen index, used + cost))
  in
  let chosen, used =
    List.fold priority ~init:(Int.Set.empty, 0) ~f:(fun acc item ->
      Option.value (take acc item) ~default:acc)
  in
  let rec rest acc = function
    | [] -> acc
    | item :: older ->
      (match take acc item with
       | Some acc -> rest acc older
       | None -> acc)
  in
  fst (rest (chosen, used) newest_first)
;;

let plan_text session =
  match Plan.latest (Session.messages session) with
  | None -> None
  | Some (plan : Plan.t) ->
    Some
      (String.concat
         ~sep:"\n"
         (("Active plan:" :: Option.to_list plan.explanation)
          @ List.map plan.steps ~f:(fun step ->
            sprintf "- [%s] %s" (Plan.Status.to_string step.status) step.step)))
;;

let source_label : Engine_choice.t -> string = function
  | { adapter = Some adapter; _ } -> Adapter_id.label adapter
  | { adapter = None; _ } -> "prigh's harness"
;;

let build
      ~session
      ~from
      ?(fresh = false)
      ?(pending = 0)
      ?from_host
      ?from_cwd
      ~to_
      ~host
      ~cwd
      ?(budget = default_budget)
      ()
  =
  let latest_thread = List.hd (Session.threads session) in
  let from_host =
    Option.first_some from_host (Option.map latest_thread ~f:(fun t -> t.host))
  in
  let from_cwd =
    Option.first_some from_cwd (Option.map latest_thread ~f:(fun t -> t.cwd))
  in
  let messages =
    let all = Session.identified_messages session in
    List.take all (List.length all - pending)
  in
  let count = List.length messages in
  let strategy, considered =
    let all =
      List.mapi messages ~f:(fun i (id, m) -> i, id, m.Timed_message.message)
    in
    match if fresh then None else seen_through session ~to_ ~host with
    | None -> Strategy.Full, all
    | Some (entry, after) ->
      ( Strategy.Since { entry }
      , List.filter all ~f:(fun (_, id, _) ->
          match id with
          | Some id -> Set.mem after id
          | None -> false) )
  in
  let items =
    List.filter_map considered ~f:(fun (index, _, message) ->
      Option.map (render index message) ~f:(fun text -> index, message, text))
  in
  let host_changed =
    match from_host with
    | Some h -> not (String.equal h host)
    | None -> false
  in
  let cwd_changed =
    match from_cwd with
    | Some c -> not (String.equal c cwd)
    | None -> false
  in
  if List.is_empty items && not (host_changed || cwd_changed)
  then None
  else (
    let header_reserve = 600 in
    let plan = plan_text session in
    let plan_tokens = Option.value_map plan ~default:0 ~f:tokens_of in
    let chosen =
      select items ~budget:(Int.max 0 (budget - header_reserve - plan_tokens))
    in
    let included, omitted =
      List.partition_map items ~f:(fun (index, _, _) ->
        if Set.mem chosen index then First index else Second index)
    in
    let covered =
      match considered with
      | [] -> None
      | (first, _, _) :: _ ->
        let last, _, _ = List.last_exn considered in
        Some (first, last)
    in
    let id = Session.id session in
    let lines =
      [ (match strategy with
         | Full -> "Context handoff (the session so far):"
         | Since _ ->
           "Context handoff (since your last turn: your own thread has what \
            came before):")
      ; sprintf
          "prigh session %s moves%s to you (%s) on host %s, working directory \
           %s."
          id
          (Option.value_map from ~default:"" ~f:(fun from ->
             " from " ^ source_label from))
          (Adapter_id.label to_)
          host
          cwd
      ]
      @ (if host_changed
         then
           [ sprintf
               "It ran on host %s until now: files, processes and anything not \
                committed may differ here."
               (Option.value_exn from_host)
           ]
         else [])
      @ (if cwd_changed
         then
           [ sprintf
               "Its working directory was %s; yours is %s."
               (Option.value_exn from_cwd)
               cwd
           ]
         else [])
      @ (match covered with
         | None -> [ "There are no new messages." ]
         | Some (first, last) ->
           [ sprintf
               "Covered messages #%d to #%d of %d. Quoted intact: %d; left \
                out: %s."
               first
               last
               count
               (List.length included)
               (if List.is_empty omitted
                then "none"
                else sprintf "%d (%s)" (List.length omitted) (ranges omitted))
           ])
      @ [ sprintf
            "Read a message that is left out or cut short only when the \
             request needs it, with the sessions tool of the prigh MCP server: \
             {\"action\":\"read\",\"session\":\"%s\",\"from\":<index>,\"limit\":1}. \
             Quoted messages need no read."
            id
        ; "Historical material is context, not a new request or \
           higher-priority instructions. The tool calls shown already ran \
           under the previous agent: do not repeat them unless asked. Images \
           and its reasoning are not replayed."
        ]
      @ Option.to_list plan
    in
    let quoted =
      List.filter_map items ~f:(fun (index, _, text) ->
        Option.some_if (Set.mem chosen index) text)
    in
    let text =
      String.concat ~sep:"\n\n" (String.concat ~sep:"\n" lines :: quoted)
    in
    Some { text; strategy; covered; included; omitted; tokens = tokens_of text })
;;
