open! Core
open! Import

module Delta_acc = struct
  type t =
    { key : string
    ; mutable rev_chunks : string list
    ; mutable text_bytes : int
    }

  let create ~key initial =
    { key; rev_chunks = [ initial ]; text_bytes = String.length initial }
  ;;

  let add t chunk =
    t.rev_chunks <- chunk :: t.rev_chunks;
    t.text_bytes <- t.text_bytes + String.length chunk
  ;;

  let contents t = String.concat (List.rev t.rev_chunks)
end

module Waiting = struct
  type t =
    { mutable event : Json.t
    ; mutable line : string Lazy.t option (** until merged *)
    ; delta_acc : Delta_acc.t option
    ; mutable base_bytes : int
    ; mutable late : Outbox.Late.t option
    }
end

type t =
  { outbox : Outbox.t
  ; waiting : Waiting.t String.Table.t
    (** by stream: updates not written yet that later ones may join *)
  }

let create outbox = { outbox; waiting = String.Table.create () }

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (k, v) ->
         if String.equal k name then k, value else k, v))
  | other -> other
;;

let rec json_bytes = function
  | `Null -> 4
  | `True -> 4
  | `False -> 5
  | `String s -> String.length s + 2
  | `Number s -> String.length s
  | `Array xs -> List.fold xs ~init:2 ~f:(fun acc x -> acc + json_bytes x + 1)
  | `Object fields ->
    List.fold fields ~init:2 ~f:(fun acc (k, v) ->
      acc + String.length k + 4 + json_bytes v)
;;

(* The stream a [message_update] belongs to: the main agent's or a
   (nested) subagent's. *)
let rec stream json =
  match string_field json "event" with
  | Some "message_update" -> Some ""
  | Some "subagent" ->
    Option.both (string_field json "agent_id") (field json "inner")
    |> Option.bind ~f:(fun (agent_id, inner) ->
      Option.map (stream inner) ~f:(fun inner -> agent_id ^ "/" ^ inner))
  | _ -> None
;;

let same_number a b key =
  match field a key, field b key with
  | Some (`Number a), Some (`Number b) -> String.equal a b
  | None, None -> true
  | _ -> false
;;

let delta_text_key delta =
  match string_field delta "type" with
  | Some ("text_delta" | "thinking_delta") -> Some "text"
  | Some "tool_call_delta" -> Some "arguments"
  | _ -> None
;;

let rec innermost_delta json =
  match string_field json "event" with
  | Some "subagent" -> Option.bind (field json "inner") ~f:innermost_delta
  | Some "message_update" -> field json "delta"
  | _ -> None
;;

let initial_delta_acc json =
  Option.bind (innermost_delta json) ~f:(fun delta ->
    Option.bind (delta_text_key delta) ~f:(fun key ->
      Option.map (string_field delta key) ~f:(fun text ->
        Delta_acc.create ~key text)))
;;

let join_deltas earlier later =
  let check key =
    match string_field earlier key, string_field later key with
    | Some _, Some b -> Some (replace later key (`String ""), b)
    | _ -> None
  in
  if not (same_number earlier later "block")
  then None
  else (
    match string_field earlier "type", string_field later "type" with
    | Some "text_delta", Some "text_delta"
    | Some "thinking_delta", Some "thinking_delta" -> check "text"
    | Some "tool_call_delta", Some "tool_call_delta"
      when same_number earlier later "index" -> check "arguments"
    | _ -> None)
;;

(* [later]'s envelope joined to [earlier]'s, covering both: [from] is the
   earlier's first. The accumulated delta text lives in [Delta_acc] instead
   of being re-concatenated on every update. *)
let join_updates earlier later =
  if
    (not (same_number earlier later "stream"))
    || not
         (Bool.equal
            (Option.is_some (field earlier "partial"))
            (Option.is_some (field later "partial")))
  then None
  else
    Option.both (field earlier "delta") (field later "delta")
    |> Option.bind ~f:(fun (a, b) -> join_deltas a b)
    |> Option.map ~f:(fun (delta, chunk) ->
      let later = replace later "delta" delta in
      let event =
        match field earlier "from", field earlier "seq", later with
        | Some from, _, `Object fields | None, Some from, `Object fields ->
          `Object
            (List.concat_map fields ~f:(fun (k, v) ->
               match k with
               | "from" -> []
               | "seq" -> [ "from", from; k, v ]
               | _ -> [ k, v ]))
        | _ -> later
      in
      event, chunk)
;;

(* Both are updates of one stream. *)
let rec join earlier later =
  match string_field later "event" with
  | Some "subagent" ->
    Option.both (field earlier "inner") (field later "inner")
    |> Option.bind ~f:(fun (a, b) ->
      Option.map (join a b) ~f:(fun (inner, chunk) ->
        replace later "inner" inner, chunk))
  | _ -> join_updates earlier later
;;

let rec set_delta_field json ~key ~value =
  match string_field json "event" with
  | Some "subagent" ->
    (match field json "inner" with
     | Some inner -> replace json "inner" (set_delta_field inner ~key ~value)
     | None -> json)
  | _ ->
    (match field json "delta" with
     | Some delta -> replace json "delta" (replace delta key value)
     | None -> json)
;;

let render ?line json =
  match line with
  | Some line -> force line
  | None -> Json.to_string json
;;

let render_waiting (w : Waiting.t) =
  match w.line with
  | Some line -> force line
  | None ->
    let json =
      match w.delta_acc with
      | Some acc ->
        set_delta_field
          w.event
          ~key:acc.key
          ~value:(`String (Delta_acc.contents acc))
      | None -> w.event
    in
    Json.to_string json
;;

let send t ?line json =
  if Option.is_some (Promise.peek (Outbox.failed t.outbox))
  then Hashtbl.clear t.waiting
  else (
    match stream json with
    | None ->
      Hashtbl.clear t.waiting;
      Outbox.send t.outbox (render ?line json)
    | Some key ->
      let joined =
        Option.bind (Hashtbl.find t.waiting key) ~f:(fun (w : Waiting.t) ->
          Option.bind w.delta_acc ~f:(fun acc ->
            Option.map (join w.event json) ~f:(fun (event, chunk) ->
              Delta_acc.add acc chunk;
              w.event <- event;
              w.line <- None;
              w.base_bytes <- Int.max 0 (json_bytes event);
              Option.iter w.late ~f:(fun late ->
                Outbox.resize_late
                  t.outbox
                  late
                  ~bytes:(w.base_bytes + acc.text_bytes)))))
      in
      if Option.is_none joined
      then (
        let delta_acc = initial_delta_acc json in
        let initial_bytes =
          match line with
          | Some l when Lazy.is_val l -> String.length (force l)
          | _ -> json_bytes json
        in
        let base_bytes =
          match delta_acc with
          | Some acc -> Int.max 0 (initial_bytes - acc.text_bytes)
          | None -> initial_bytes
        in
        let w =
          { Waiting.event = json; line; delta_acc; base_bytes; late = None }
        in
        Hashtbl.set t.waiting ~key ~data:w;
        let late =
          Outbox.send_late t.outbox ~bytes:initial_bytes (fun () ->
            (match Hashtbl.find t.waiting key with
             | Some w' when phys_equal w w' -> Hashtbl.remove t.waiting key
             | Some _ | None -> ());
            render_waiting w)
        in
        w.late <- Some late))
;;
