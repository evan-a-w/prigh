open! Core
open! Import

module Waiting = struct
  type t =
    { mutable event : Json.t
    ; mutable line : string Lazy.t option (** until merged *)
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

let join_deltas earlier later =
  let concat key =
    match string_field earlier key, string_field later key with
    | Some a, Some b -> Some (replace later key (`String (a ^ b)))
    | _ -> None
  in
  if not (same_number earlier later "block")
  then None
  else (
    match string_field earlier "type", string_field later "type" with
    | Some "text_delta", Some "text_delta"
    | Some "thinking_delta", Some "thinking_delta" -> concat "text"
    | Some "tool_call_delta", Some "tool_call_delta"
      when same_number earlier later "index" -> concat "arguments"
    | _ -> None)
;;

(* [later]'s delta joined to [earlier]'s, covering both: [from] is the
   earlier's first. *)
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
    |> Option.map ~f:(fun delta ->
      let later = replace later "delta" delta in
      match field earlier "from", field earlier "seq", later with
      | Some from, _, `Object fields | None, Some from, `Object fields ->
        `Object
          (List.concat_map fields ~f:(fun (k, v) ->
             match k with
             | "from" -> []
             | "seq" -> [ "from", from; k, v ]
             | _ -> [ k, v ]))
      | _ -> later)
;;

(* Both are updates of one stream. *)
let rec join earlier later =
  match string_field later "event" with
  | Some "subagent" ->
    Option.both (field earlier "inner") (field later "inner")
    |> Option.bind ~f:(fun (a, b) ->
      Option.map (join a b) ~f:(replace later "inner"))
  | _ -> join_updates earlier later
;;

let render ?line json =
  match line with
  | Some line -> force line
  | None -> Json.to_string json
;;

let send t ?line json =
  match stream json with
  | None ->
    Hashtbl.clear t.waiting;
    Outbox.send t.outbox (render ?line json)
  | Some key ->
    let joined =
      Option.bind (Hashtbl.find t.waiting key) ~f:(fun (w : Waiting.t) ->
        Option.map (join w.event json) ~f:(fun event ->
          w.event <- event;
          w.line <- None))
    in
    if Option.is_none joined
    then (
      let w = { Waiting.event = json; line } in
      Hashtbl.set t.waiting ~key ~data:w;
      Outbox.send_late t.outbox (fun () ->
        (match Hashtbl.find t.waiting key with
         | Some w' when phys_equal w w' -> Hashtbl.remove t.waiting key
         | Some _ | None -> ());
        render ?line:w.line w.event))
;;
