open! Core
open! Import

module Waiting = struct
  type t = { mutable event : Json.t }
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

let join_deltas earlier later =
  let concat key =
    match string_field earlier key, string_field later key with
    | Some a, Some b -> Some (replace later key (`String (a ^ b)))
    | _ -> None
  in
  match string_field earlier "type", string_field later "type" with
  | Some "text_delta", Some "text_delta"
  | Some "thinking_delta", Some "thinking_delta" -> concat "text"
  | Some "tool_call_delta", Some "tool_call_delta" ->
    (match field earlier "index", field later "index" with
     | Some (`Number a), Some (`Number b) when String.equal a b ->
       concat "arguments"
     | _ -> None)
  | _ -> None
;;

(* Both are updates of one stream. *)
let rec join earlier later =
  match string_field later "event" with
  | Some "subagent" ->
    Option.both (field earlier "inner") (field later "inner")
    |> Option.bind ~f:(fun (a, b) ->
      Option.map (join a b) ~f:(replace later "inner"))
  | _ ->
    Option.both (field earlier "delta") (field later "delta")
    |> Option.bind ~f:(fun (a, b) ->
      Option.map (join_deltas a b) ~f:(replace later "delta"))
;;

let send t json =
  match stream json with
  | None ->
    Hashtbl.clear t.waiting;
    Outbox.send t.outbox (Json.to_string json)
  | Some key ->
    let joined =
      Option.bind (Hashtbl.find t.waiting key) ~f:(fun (w : Waiting.t) ->
        Option.map (join w.event json) ~f:(fun event -> w.event <- event))
    in
    if Option.is_none joined
    then (
      let w = { Waiting.event = json } in
      Hashtbl.set t.waiting ~key ~data:w;
      Outbox.send_late t.outbox (fun () ->
        (match Hashtbl.find t.waiting key with
         | Some w' when phys_equal w w' -> Hashtbl.remove t.waiting key
         | Some _ | None -> ());
        Json.to_string w.event))
;;
