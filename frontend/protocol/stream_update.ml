open! Core

type t =
  { stream : int
  ; from : int
  ; seq : int
  ; block : int
  ; delta : Delta.t
  ; partial : Message.Assistant.t option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let int_default name ~default =
    match Json.field j name with
    | None -> Ok default
    | Some _ -> Json.int_field j name
  in
  let%bind stream = int_default "stream" ~default:0 in
  let%bind seq = int_default "seq" ~default:0 in
  let%bind from = int_default "from" ~default:seq in
  let%bind delta_json = Json.object_field j "delta" in
  let%bind delta = Delta.of_json delta_json in
  let%bind block =
    match Json.field delta_json "block" with
    | None -> Ok (-1)
    | Some _ -> Json.int_field delta_json "block"
  in
  let%map partial =
    match Json.field j "partial" with
    | None -> Ok None
    | Some p -> Message.Assistant.of_json p >>| Option.some
  in
  { stream; from; seq; block; delta; partial }
;;

module Position = struct
  type t =
    { stream : int
    ; seq : int
    }
  [@@deriving sexp_of, equal]
end

module Applied = struct
  type t =
    | Updated of Position.t * Message.Assistant.t
    | Unchanged
    | Gap
  [@@deriving sexp_of]
end

(* [content] with [delta] applied to its block [block] (a new block when it
   is the next one); [None] when it does not fit. *)
let apply_delta (content : Content.t list) ~block (delta : Delta.t) =
  let n = List.length content in
  let add (c : Content.t) = Option.some_if (block = n) (content @ [ c ]) in
  let change f =
    if block < 0 || block >= n
    then None
    else
      Option.map
        (f (List.nth_exn content block))
        ~f:(fun (c : Content.t) ->
          List.mapi content ~f:(fun i old -> if i = block then c else old))
  in
  let add_or_change c f = if block = n then add c else change f in
  match delta with
  | Text_delta s ->
    add_or_change (Text s) (function
      | Text t -> Some (Text (t ^ s))
      | Thinking _ | Tool_call _ | Server_tool _ -> None)
  | Thinking_delta s ->
    add_or_change (Thinking s) (function
      | Thinking t -> Some (Thinking (t ^ s))
      | Text _ | Tool_call _ | Server_tool _ -> None)
  | Thinking_signature ->
    add_or_change (Thinking "") (function
      | Thinking _ as c -> Some c
      | Text _ | Tool_call _ | Server_tool _ -> None)
  | Tool_call_start { index = _; id; name } ->
    add (Tool_call { id; name; arguments = "" })
  | Tool_call_delta { index = _; arguments } ->
    (* For a call that never started the backend changed nothing. *)
    Some
      (Option.value
         (change (function
            | Tool_call c ->
              Some (Tool_call { c with arguments = c.arguments ^ arguments })
            | Text _ | Thinking _ | Server_tool _ -> None))
         ~default:content)
  | Server_tool_use { id; name; input } ->
    add_or_change
      (Server_tool { id; name; input; output = None; is_error = false })
      (function
        | Server_tool s when String.equal s.id id ->
          Some (Server_tool { s with name; input })
        | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
  | Server_tool_result { id; output; is_error } ->
    add_or_change
      (Server_tool
         { id; name = ""; input = "{}"; output = Some output; is_error })
      (function
        | Server_tool s when String.equal s.id id ->
          Some (Server_tool { s with output = Some output; is_error })
        | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
;;

let apply t ~(at : Position.t option) (message : Message.Assistant.t)
  : Applied.t
  =
  let position = { Position.stream = t.stream; seq = t.seq } in
  match t.partial with
  | Some partial -> Updated (position, partial)
  | None ->
    let base =
      match at with
      | _ when t.from = 1 -> Some { message with content = [] }
      | Some at when at.stream = t.stream && t.from = at.seq + 1 -> Some message
      | Some _ | None -> None
    in
    (match at, base with
     | Some at, _ when at.stream = t.stream && t.seq <= at.seq -> Unchanged
     | _, None -> Gap
     | _, Some base ->
       (match apply_delta base.content ~block:t.block t.delta with
        | Some content -> Updated (position, { base with content })
        | None -> Gap))
;;
