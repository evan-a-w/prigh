open! Core
open! Import

let keep_turns = 10
let min_bytes = 2048

let size bytes =
  if bytes < 1024
  then sprintf "%d bytes" bytes
  else sprintf "%d KB" ((bytes + 512) / 1024)
;;

let lines s =
  match String.count s ~f:(Char.equal '\n') with
  | n when String.is_empty s || String.is_suffix s ~suffix:"\n" -> n
  | n -> n + 1
;;

let images n = if n = 1 then "1 image" else sprintf "%d images" n

let mask_result ~min_bytes (r : Message.Tool_result.t) =
  if r.is_error
  then r
  else if String.length r.text >= min_bytes
  then
    { r with
      text =
        sprintf
          "[%s output omitted to save context: %d lines, %s%s; run the tool \
           again if you need it]"
          r.tool_name
          (lines r.text)
          (size (String.length r.text))
          (match r.images with
           | [] -> ""
           | l -> ", " ^ images (List.length l))
    ; images = []
    }
  else (
    match r.images with
    | [] -> r
    | l ->
      { r with
        text =
          sprintf
            "%s\n[%s omitted to save context]"
            r.text
            (images (List.length l))
      ; images = []
      })
;;

let mask_call ~min_bytes (c : Content.Tool_call.t) =
  let min_field_bytes = min_bytes / 2 in
  if String.length c.arguments < min_bytes
  then c
  else (
    match Json.parse c.arguments with
    | Error _ -> c
    | Ok json ->
      let changed = ref false in
      let rec go : Json.t -> Json.t = function
        | `String s when String.length s >= min_field_bytes ->
          changed := true;
          `String
            (sprintf
               "[omitted to save context: %d lines, %s]"
               (lines s)
               (size (String.length s)))
        | `Object fields -> `Object (List.map fields ~f:(fun (k, v) -> k, go v))
        | `Array items -> `Array (List.map items ~f:go)
        | other -> other
      in
      let json = go json in
      if !changed then { c with arguments = Json.to_string json } else c)
;;

let mask ?(min_bytes = min_bytes) (m : Message.t) : Message.t =
  match m with
  | User _ -> m
  | Tool_result r ->
    let masked = mask_result ~min_bytes r in
    if phys_equal masked r then m else Tool_result masked
  | Assistant a ->
    let changed = ref false in
    let content =
      List.map a.content ~f:(function
        | Tool_call c ->
          let masked = mask_call ~min_bytes c in
          if not (phys_equal masked c) then changed := true;
          Content.Tool_call masked
        | (Text _ | Thinking _ | Server_tool _) as c -> c)
    in
    if !changed then Assistant { a with content } else m
;;

let tokens m = Message.estimate_tokens [ m ]

module Plan = struct
  type t =
    { through : int
    ; masked : int
    ; tokens_saved : int
    }
  [@@deriving sexp_of]
end

let plan ?(keep_turns = keep_turns) ?min_bytes messages =
  let assistants =
    List.filter_mapi messages ~f:(fun i (m : Message.t) ->
      match m with
      | Assistant _ -> Some i
      | User _ | Tool_result _ -> None)
  in
  match List.nth (List.rev assistants) (keep_turns - 1) with
  | None -> None
  | Some first_kept ->
    let plan =
      List.foldi
        (List.take messages first_kept)
        ~init:{ Plan.through = -1; masked = 0; tokens_saved = 0 }
        ~f:(fun i (plan : Plan.t) m ->
          let masked = mask ?min_bytes m in
          if phys_equal masked m
          then plan
          else
            { through = i
            ; masked = plan.masked + 1
            ; tokens_saved = plan.tokens_saved + tokens m - tokens masked
            })
    in
    Option.some_if (plan.masked > 0) plan
;;

let apply ?min_bytes ~through messages =
  List.mapi messages ~f:(fun i m ->
    if i <= through then mask ?min_bytes m else m)
;;
