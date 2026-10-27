open! Core
open! Import

module Role = struct
  type t =
    | User
    | Assistant
    | Tool_result
  [@@deriving sexp_of]

  let name = function
    | User -> "user"
    | Assistant -> "assistant"
    | Tool_result -> "tool_result"
  ;;
end

module Effect = struct
  type t =
    { messages : int
    ; tokens : int
    }
  [@@deriving sexp_of]
end

type t =
  { message : string
  ; index : int
  ; role : Role.t
  ; text : string
  ; at : float option
  ; trim_before : Effect.t option
  ; delete : Effect.t option
  ; trim_after : Effect.t option
  }
[@@deriving sexp_of]

let tool_call_ids items =
  List.concat_map items ~f:(fun (_, (m : Timed_message.t)) ->
    match m.message with
    | Assistant a ->
      List.map (Message.Assistant.tool_calls a) ~f:(fun c -> c.id)
    | User _ | Tool_result _ -> [])
;;

let splits_a_call ~dropped ~kept =
  let calls = String.Hash_set.of_list (tool_call_ids dropped) in
  List.exists kept ~f:(fun (_, (m : Timed_message.t)) ->
    match m.message with
    | Tool_result r -> Hash_set.mem calls r.tool_call_id
    | User _ | Assistant _ -> false)
;;

let is_id id (entry, _) = Option.equal String.equal entry (Some id)

let deleted_with context ~message =
  match List.drop_while context ~f:(fun item -> not (is_id message item)) with
  | [] -> String.Set.empty
  | item :: rest ->
    let calls = String.Hash_set.of_list (tool_call_ids [ item ]) in
    List.take_while rest ~f:(fun (_, (m : Timed_message.t)) ->
      match m.message with
      | Assistant _ -> false
      | User _ | Tool_result _ -> true)
    |> List.filter_map ~f:(fun (entry, (m : Timed_message.t)) ->
      match m.message with
      | Tool_result r when Hash_set.mem calls r.tool_call_id -> entry
      | Tool_result _ | User _ | Assistant _ -> None)
    |> List.cons message
    |> String.Set.of_list
;;

let trim_after_error context ~after =
  match List.findi context ~f:(fun _ item -> is_id after item) with
  | None ->
    Some
      (sprintf
         "message %s is not in the model's context: trim after one of the \
          messages that /trim after lists"
         after)
  | Some (i, _) ->
    let kept = List.take context (i + 1) in
    let dropped = List.drop context (i + 1) in
    if List.is_empty dropped
    then
      Some "nothing to trim: that message is the newest in the model's context"
    else if List.exists dropped ~f:(fun (id, _) -> Option.is_none id)
    then
      Some
        "trimming there would cut into a compaction's summary: pick a message \
         after it"
    else if splits_a_call ~dropped:kept ~kept:dropped
    then
      Some
        "trimming there would part a tool call from its result: pick the \
         message with its result, or a later one"
    else None
;;

let text (message : Message.t) =
  match message with
  | User u -> Outline.cut (Message.User.body u)
  | Assistant a ->
    let text = String.strip (Message.Assistant.text a) in
    if not (String.is_empty text)
    then Outline.cut text
    else (
      match Message.Assistant.tool_calls a with
      | [] -> Outline.cut (Message.Assistant.thinking a)
      | calls ->
        Outline.cut
          (String.concat
             ~sep:", "
             (List.map calls ~f:(fun (c : Content.Tool_call.t) -> c.name))))
  | Tool_result r -> r.tool_name
;;

(* [split.(i)]: trimming after the [i]th message would part a call made at
   or before it from a result after it. *)
let splits context =
  let items = Array.of_list context in
  let n = Array.length items in
  let call_at = String.Table.create () in
  let depth = Array.create ~len:(n + 1) 0 in
  Array.iteri items ~f:(fun j (_, (m : Timed_message.t)) ->
    match m.message with
    | Assistant a ->
      List.iter (Message.Assistant.tool_calls a) ~f:(fun c ->
        Hashtbl.set call_at ~key:c.id ~data:j)
    | Tool_result r ->
      Option.iter (Hashtbl.find call_at r.tool_call_id) ~f:(fun c ->
        depth.(c) <- depth.(c) + 1;
        depth.(j) <- depth.(j) - 1)
    | User _ -> ());
  let split = Array.create ~len:n false in
  let (_ : int) =
    Array.foldi split ~init:0 ~f:(fun i open_ _ ->
      let open_ = open_ + depth.(i) in
      split.(i) <- open_ > 0;
      open_)
  in
  split
;;

let create context =
  let items = Array.of_list context in
  let n = Array.length items in
  let tokens =
    Array.map items ~f:(fun (_, (m : Timed_message.t)) ->
      Message.estimate_tokens [ m.message ])
  in
  let tokens_after = Array.create ~len:(n + 1) 0 in
  for i = n - 1 downto 0 do
    tokens_after.(i) <- tokens_after.(i + 1) + tokens.(i)
  done;
  let tokens_before i = tokens_after.(0) - tokens_after.(i) in
  let last_summary =
    Array.foldi items ~init:(-1) ~f:(fun i acc (id, _) ->
      if Option.is_none id then i else acc)
  in
  let split = splits context in
  List.filter_mapi context ~f:(fun index (entry, (m : Timed_message.t)) ->
    Option.map entry ~f:(fun message ->
      let role : Role.t =
        match m.message with
        | User _ -> User
        | Assistant _ -> Assistant
        | Tool_result _ -> Tool_result
      in
      let trim_before =
        match role with
        | Tool_result -> None
        | User | Assistant ->
          if index = 0 || split.(index - 1)
          then None
          else Some { Effect.messages = index; tokens = tokens_before index }
      in
      let delete =
        match role with
        | Tool_result -> None
        | User -> Some { Effect.messages = 1; tokens = tokens.(index) }
        | Assistant ->
          let calls =
            String.Hash_set.of_list (tool_call_ids [ items.(index) ])
          in
          let rec results i acc =
            if i >= n
            then acc
            else (
              match (snd items.(i)).message with
              | Assistant _ -> acc
              | Tool_result r when Hash_set.mem calls r.tool_call_id ->
                results (i + 1) (i :: acc)
              | Tool_result _ | User _ -> results (i + 1) acc)
          in
          let deleted = index :: results (index + 1) [] in
          Some
            { Effect.messages = List.length deleted
            ; tokens = List.sum (module Int) deleted ~f:(fun i -> tokens.(i))
            }
      in
      let trim_after =
        if index = n - 1 || index < last_summary || split.(index)
        then None
        else
          Some
            { Effect.messages = n - index - 1; tokens = tokens_after.(index + 1) }
      in
      { message
      ; index
      ; role
      ; text = text m.message
      ; at = m.at
      ; trim_before
      ; delete
      ; trim_after
      }))
;;

let to_json points =
  let int n = `Number (Int.to_string n) in
  let effect name =
    Option.value_map ~default:[] ~f:(fun { Effect.messages; tokens } ->
      [ name, `Object [ "messages", int messages; "tokens", int tokens ] ])
  in
  `Object
    [ ( "points"
      , `Array
          (List.map points ~f:(fun t ->
             `Object
               ([ "message", `String t.message
                ; "index", int t.index
                ; "role", `String (Role.name t.role)
                ; "text", `String t.text
                ]
                @ Option.value_map t.at ~default:[] ~f:(fun at ->
                  [ "at", int (Float.iround_nearest_exn (at *. 1000.)) ])
                @ effect "trim_before" t.trim_before
                @ effect "delete" t.delete
                @ effect "trim_after" t.trim_after))) )
    ]
;;
