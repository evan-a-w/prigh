open! Core
open! Import

let int_ i = `Number (Int.to_string i)

module Elided = struct
  type t =
    { at : int
    ; lines : int
    ; bytes : int
    }
  [@@deriving sexp_of]

  let to_json { at; lines; bytes } =
    `Object [ "at", int_ at; "lines", int_ lines; "bytes", int_ bytes ]
  ;;

  let line_count text =
    let text = String.rstrip text ~drop:(Char.equal '\n') in
    if String.is_empty text
    then 0
    else 1 + String.count text ~f:(Char.equal '\n')
  ;;

  (* Back to the start of a UTF-8 sequence. *)
  let rec boundary s i =
    if i > 0 && i < String.length s && Char.to_int s.[i] land 0xC0 = 0x80
    then boundary s (i - 1)
    else i
  ;;

  (* The end of the first [keep] lines, at most [max] bytes in. *)
  let head_end text ~keep ~max =
    let rec go pos n =
      if n = 0
      then pos
      else (
        match String.index_from text pos '\n' with
        | Some i when i + 1 <= max -> go (i + 1) (n - 1)
        | Some _ | None -> if pos > 0 then pos else boundary text max)
    in
    go 0 keep
  ;;

  (* The start of the last [keep] lines (a trailing newline ends the last
     one), at most [max] bytes from the end. *)
  let tail_start text ~keep ~max =
    let len = String.length text in
    let limit = len - max in
    let rec go pos n =
      if n = 0
      then pos
      else (
        match
          if pos < 2 then None else String.rindex_from text (pos - 2) '\n'
        with
        | Some i when i + 1 >= limit -> go (i + 1) (n - 1)
        | Some _ | None -> if pos < len then pos else boundary text limit)
    in
    if len < 2 then len else go len keep
  ;;

  let cut ?(above = 8192) ?(keep = 20) text =
    let bytes = String.length text in
    if bytes <= above
    then None
    else (
      let max = above / 10 in
      let at = head_end text ~keep ~max in
      let from = Int.max at (tail_start text ~keep ~max) in
      Some
        ( String.prefix text at ^ String.drop_prefix text from
        , { at; lines = line_count text; bytes } ))
  ;;
end

type t =
  { messages : Json.t list
  ; older : int
  ; before : string option
  }

let message ?elide_above ({ message; at } : Timed_message.t) =
  match message with
  | Tool_result r ->
    (match Elided.cut ?above:elide_above r.text with
     | None -> Rpc_json.timed_message { message; at }
     | Some (text, elided) ->
       (match
          Rpc_json.timed_message { message = Tool_result { r with text }; at }
        with
        | `Object fields ->
          `Object (fields @ [ "elided", Elided.to_json elided ])
        | json -> json))
  | User _ | Assistant _ -> Rpc_json.timed_message { message; at }
;;

let create ?elide_above ?from messages ~limit ~before =
  let all = Array.of_list messages in
  let index id =
    match
      Array.findi all ~f:(fun _ (entry, _) ->
        Option.equal String.equal entry (Some id))
    with
    | Some (i, _) -> Ok i
    | None ->
      Or_error.errorf
        "message %s is not in the session any more: load its latest messages \
         again"
        id
  in
  let open Or_error.Let_syntax in
  let%bind stop =
    match before with
    | None -> Ok (Array.length all)
    | Some id -> index id
  in
  let%map limit =
    match from with
    | None -> Ok limit
    | Some id ->
      let%map from = index id in
      stop - from
  in
  let rec start i =
    if i <= 0
    then 0
    else (
      match all.(i) with
      | None, _ | _, { Timed_message.message = Tool_result _; _ } ->
        start (i - 1)
      | Some _, { message = User _ | Assistant _; _ } -> i)
  in
  let start = start (Int.max 0 (stop - Int.max 1 limit)) in
  { messages =
      Array.sub all ~pos:start ~len:(stop - start)
      |> Array.to_list
      |> List.map ~f:(fun (_, m) -> message ?elide_above m)
  ; older = start
  ; before = (if start > 0 then fst all.(start) else None)
  }
;;

let to_json { messages; older; before } =
  `Object
    [ "messages", `Array messages
    ; "older", int_ older
    ; "before", Option.value_map before ~default:`Null ~f:(fun s -> `String s)
    ]
;;

let find_output messages ~call_id =
  List.find_map
    (List.rev messages)
    ~f:(fun ({ message; _ } : Timed_message.t) ->
      match message with
      | Tool_result r when String.equal r.tool_call_id call_id -> Some r
      | Tool_result _ | User _ | Assistant _ -> None)
;;
