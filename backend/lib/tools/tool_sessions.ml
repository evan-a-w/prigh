open! Core
open! Import

let name = "sessions"
let default_search_limit = 8
let max_search_limit = 20
let snippets_per_session = 2
let matches_in_session = 10
let outline_points = 40
let default_read_limit = 10
let max_read_limit = 30
let read_budget = 12_000
let text_cut = 1500
let result_cut = 300

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description =
      "Search and read other saved sessions, only when the user refers to one \
       or asks, or the task plainly depends on work done in another session: \
       not routinely, and never to start a task. search finds sessions whose \
       title, directory or transcript has all the words of query, with their \
       ids and snippets; with session, the matching messages of that one. \
       outline lists a session's prompts with their message indices. read \
       shows messages from an index, cut short. Read only what you need; for \
       one specific question about a session, ask_session costs less."
  ; parameters =
      Tool_args.schema
        ~required:[ "action" ]
        [ "action", `String, "search, outline or read"
        ; "query", `String, "search: words, all of which must match"
        ; ( "session"
          , `String
          , "outline, read: the session's id or a unique prefix of it; search: \
             only inside this one" )
        ; ( "from"
          , `Integer
          , "read: the first message's index (default: the last ones)" )
        ; ( "before"
          , `Integer
          , "outline: only prompts before this index (default: the last ones)" )
        ; ( "limit"
          , `Integer
          , sprintf
              "search: sessions (default %d, at most %d); read: messages \
               (default %d, at most %d)"
              default_search_limit
              max_search_limit
              default_read_limit
              max_read_limit )
        ]
  }
;;

let find ~dir key =
  let key = String.strip key in
  if String.mem key '/' || String.is_suffix key ~suffix:".jsonl"
  then
    Or_error.errorf
      "%S is a path: give the session's id (action search of the sessions tool \
       finds it)"
      key
  else (
    match Session.find_summary (Session.list ~dir) key with
    | Ok _ as ok -> ok
    | Error e ->
      Or_error.errorf
        "%s; action search of the sessions tool finds sessions and their ids"
        (Error.to_string_hum e))
;;

(* The start of the character that byte [i] of [s] is in. *)
let rec boundary s i =
  if i > 0 && i < String.length s && Char.to_int s.[i] land 0xC0 = 0x80
  then boundary s (i - 1)
  else i
;;

let one_line s =
  String.split_on_chars s ~on:[ ' '; '\n'; '\t'; '\r' ]
  |> List.filter ~f:(fun w -> not (String.is_empty w))
  |> String.concat ~sep:" "
;;

let cut ~max text =
  let text = String.strip text in
  if String.length text <= max
  then text
  else (
    let kept = boundary text max in
    sprintf
      "%s… [+%d chars]"
      (String.prefix text kept)
      (String.length text - kept))
;;

let short ~max text =
  let text = one_line text in
  if String.length text <= max
  then text
  else String.prefix text (boundary text max) ^ "…"
;;

let title (s : Session.Summary.t) =
  match Option.first_some s.name s.first_prompt with
  | Some title when not (String.is_empty (String.strip title)) ->
    sprintf "%S" (short ~max:80 title)
  | Some _ | None -> "(untitled)"
;;

let session_line ~current (s : Session.Summary.t) =
  String.concat
    ~sep:"  "
    ([ s.id
     ; String.prefix s.updated_at 10
     ; title s
     ; s.cwd
     ; sprintf "%d messages" s.message_count
     ]
     @ (if [%equal: bool option] s.archived (Some true)
        then [ "(archived)" ]
        else [])
     @
     if [%equal: string option] (current ()) (Some s.id)
     then [ "(this session)" ]
     else [])
;;

let words query =
  String.lowercase query
  |> String.split_on_chars ~on:[ ' '; '\n'; '\t'; '\r' ]
  |> List.filter ~f:(fun w -> not (String.is_empty w))
  |> List.dedup_and_sort ~compare:String.compare
;;

let meta (s : Session.Summary.t) =
  let repo =
    match s.repo with
    | Some (Some repo) -> [ Some repo.root; repo.branch ]
    | Some None | None -> []
  in
  List.filter_opt ([ s.name; s.description; s.first_prompt; Some s.cwd ] @ repo)
  |> String.concat ~sep:"\n"
  |> String.lowercase
;;

let has text word = String.is_substring text ~substring:word

let role : Message.t -> string = function
  | User _ -> "user"
  | Assistant _ -> "assistant"
  | Tool_result r -> "result " ^ r.tool_name
;;

let user_text (message : Message.t) (u : Message.User.t) =
  match Session.summary_of_message message with
  | Some summary -> "[summary of earlier messages] " ^ summary
  | None -> Message.User.body u
;;

let searchable (message : Message.t) =
  match message with
  | User u -> user_text message u
  | Assistant a ->
    String.concat
      ~sep:"\n"
      (Message.Assistant.text a
       :: List.map (Message.Assistant.tool_calls a) ~f:(fun call ->
         call.name ^ " " ^ call.arguments))
  | Tool_result r -> r.text
;;

module Hit = struct
  type t =
    { index : int
    ; message : Message.t
    ; text : string
    ; lower : string
    ; matched : string list
    }

  (* Best first: the most words, then the earliest. *)
  let compare a b =
    match Int.compare (List.length b.matched) (List.length a.matched) with
    | 0 -> Int.compare a.index b.index
    | c -> c
  ;;

  let line t =
    let at =
      List.filter_map t.matched ~f:(fun word ->
        String.substr_index t.lower ~pattern:word)
      |> List.min_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    let start = boundary t.text (Int.max 0 (at - 60)) in
    let stop = boundary t.text (Int.min (String.length t.text) (at + 100)) in
    sprintf
      "  [%d] %s: %s%s%s"
      t.index
      (role t.message)
      (if start > 0 then "…" else "")
      (one_line (String.sub t.text ~pos:start ~len:(stop - start)))
      (if stop < String.length t.text then "…" else "")
  ;;
end

let hits session ~words =
  List.filter_mapi
    (Session.identified_messages session)
    ~f:(fun index (_, { Timed_message.message; at = _ }) ->
      let text = searchable message in
      let lower = String.lowercase text in
      match List.filter words ~f:(has lower) with
      | [] -> None
      | matched -> Some { Hit.index; message; text; lower; matched })
;;

let load (s : Session.Summary.t) = Session.load ~yield:Fiber.yield s.path

(* Words that appear in a session's file as they are in its text: JSON
   escapes nothing of them. *)
let plain word =
  String.for_all word ~f:(fun c ->
    Char.is_alphanum c || String.mem "-_./:@#=+~" c)
;;

let search_in ~current (summary : Session.Summary.t) ~words =
  let%map.Or_error session = load summary in
  let header = session_line ~current summary in
  match List.sort (hits session ~words) ~compare:Hit.compare with
  | [] -> sprintf "%s\nNo message has any of these words." header
  | hits ->
    let shown =
      List.take hits matches_in_session
      |> List.sort ~compare:(fun (a : Hit.t) b -> Int.compare a.index b.index)
    in
    let more = List.length hits - List.length shown in
    String.concat
      ~sep:"\n"
      ((header :: List.map shown ~f:Hit.line)
       @
       if more > 0
       then [ sprintf "(%d more matching messages: add words)" more ]
       else [])
;;

let search_all ~max_scan_bytes ~dir ~current ~words ~limit =
  let summaries = Session.list ~dir in
  let by_meta, rest =
    List.partition_tf summaries ~f:(fun s ->
      let meta = meta s in
      List.for_all words ~f:(has meta))
  in
  let found = Queue.create () in
  List.iter (List.take by_meta limit) ~f:(fun s ->
    Queue.enqueue found (session_line ~current s));
  let patterns =
    List.filter words ~f:plain
    |> List.map ~f:(String.Search_pattern.create ~case_sensitive:false)
  in
  let more = "(more sessions may match: add words or raise limit)" in
  let scanned = ref 0 in
  let rec scan = function
    | [] -> None
    | _ :: _ when Queue.length found >= limit -> Some more
    | _ when !scanned >= max_scan_bytes ->
      Some
        (sprintf
           "(stopped after reading %d MiB of sessions: older ones were not \
            searched)"
           (!scanned / (1024 * 1024)))
    | (s : Session.Summary.t) :: rest ->
      Fiber.yield ();
      (match In_channel.read_all s.path with
       | exception Sys_error _ -> ()
       | raw ->
         scanned := !scanned + String.length raw;
         if
           List.for_all patterns ~f:(fun pattern ->
             Option.is_some (String.Search_pattern.index pattern ~in_:raw))
         then (
           match load s with
           | Error _ -> ()
           | Ok session ->
             let hits = hits session ~words in
             let meta = meta s in
             if
               List.for_all words ~f:(fun word ->
                 has meta word
                 || List.exists hits ~f:(fun (hit : Hit.t) ->
                   List.mem hit.matched word ~equal:String.equal))
             then
               Queue.enqueue
                 found
                 (String.concat
                    ~sep:"\n"
                    (session_line ~current s
                     :: (List.sort hits ~compare:Hit.compare
                         |> Fn.flip List.take snippets_per_session
                         |> List.map ~f:Hit.line)))));
      scan rest
  in
  let stopped = if List.length by_meta > limit then Some more else scan rest in
  match Queue.to_list found, stopped with
  | [], None ->
    "No session matches: all the words must match, so try fewer or other words."
  | found, stopped -> String.concat ~sep:"\n" (found @ Option.to_list stopped)
;;

let time at =
  String.prefix
    (Time_float.to_string_utc
       (Time_float.of_span_since_epoch (Time_float.Span.of_sec at)))
    16
;;

let outline ~current (summary : Session.Summary.t) ~before =
  let%map.Or_error session = load summary in
  let points =
    Outline.create (Session.identified_messages session)
    |> List.filter ~f:(fun (p : Outline.Point.t) ->
      Option.for_all before ~f:(fun before -> p.index < before))
  in
  let earlier = Int.max 0 (List.length points - outline_points) in
  let shown = List.drop points earlier in
  let line ({ index; kind; text; at } : Outline.Point.t) =
    let kind =
      match kind with
      | User -> "user"
      | Compaction -> "summary of earlier messages"
      | Error -> "error"
    in
    String.concat
      (List.filter_opt
         [ Some (sprintf "[%d] %s" index kind)
         ; Option.map at ~f:(fun at -> " " ^ time at)
         ; Option.some_if
             (not (String.is_empty text))
             (": " ^ short ~max:120 text)
         ])
  in
  String.concat
    ~sep:"\n"
    ((session_line ~current summary
      ::
      (match shown with
       | first :: _ when earlier > 0 ->
         [ sprintf
             "(%d earlier prompts: outline with before=%d)"
             earlier
             first.index
         ]
       | _ -> []))
     @ List.map shown ~f:line)
;;

let call_line (call : Content.Tool_call.t) =
  let arg =
    match Content.Tool_call.parse_arguments call with
    | Error _ -> None
    | Ok args ->
      List.find_map
        [ "command"
        ; "path"
        ; "pattern"
        ; "task"
        ; "url"
        ; "query"
        ; "question"
        ; "action"
        ]
        ~f:(fun key ->
          match Json.member key args with
          | Some (`String s) -> Some s
          | _ -> None)
  in
  sprintf
    "  -> %s%s"
    call.name
    (Option.value_map arg ~default:"" ~f:(fun arg -> " " ^ short ~max:100 arg))
;;

let message_text index ({ message; at } : Timed_message.t) =
  let head =
    sprintf
      "[%d] %s%s:"
      index
      (role message)
      (match message, at with
       | User _, Some at -> sprintf " (%s)" (time at)
       | _ -> "")
  in
  let body =
    match message with
    | User u ->
      [ cut ~max:text_cut (user_text message u) ]
      @
        (match List.length u.images with
        | 0 -> []
        | n -> [ sprintf "[%d image%s]" n (if n = 1 then "" else "s") ])
    | Assistant a ->
      (match String.strip (Message.Assistant.text a) with
       | "" -> []
       | text -> [ cut ~max:text_cut text ])
      @ List.map (Message.Assistant.tool_calls a) ~f:call_line
      @
        (match a.stop_reason with
        | Error e -> [ "[error: " ^ short ~max:200 e ^ "]" ]
        | Aborted -> [ "[stopped]" ]
        | End_turn | Tool_use | Length -> [])
    | Tool_result r ->
      [ (if r.is_error then "(error) " else "") ^ cut ~max:result_cut r.text ]
  in
  match body with
  | [ line ]
    when (not (String.mem line '\n')) && not (String.is_prefix line ~prefix:" ")
    -> head ^ " " ^ line
  | lines -> String.concat ~sep:"\n" (head :: lines)
;;

let read ~current (summary : Session.Summary.t) ~from ~limit =
  let%bind.Or_error session = load summary in
  let messages = Session.identified_messages session in
  let count = List.length messages in
  let from = Option.value from ~default:(Int.max 0 (count - limit)) in
  if count = 0
  then Ok (session_line ~current summary ^ "\nIt has no messages.")
  else if from < 0 || from >= count
  then
    Or_error.errorf
      "from=%d is not a message of this session: it has %d, 0 to %d"
      from
      count
      (count - 1)
  else (
    let rec take acc size index = function
      | [] -> List.rev acc, index
      | _ when index - from >= limit -> List.rev acc, index
      | (_, message) :: rest ->
        let text = message_text index message in
        let size = size + String.length text in
        if size > read_budget && not (List.is_empty acc)
        then List.rev acc, index
        else take (text :: acc) size (index + 1) rest
    in
    let shown, next = take [] 0 from (List.drop messages from) in
    Ok
      (String.concat
         ~sep:"\n"
         ((session_line ~current summary
           :: sprintf "messages %d to %d of %d" from (next - 1) count
           :: shown)
          @
          if next < count
          then [ sprintf "(more: read with from=%d)" next ]
          else [])))
;;

let clamp ~default ~max limit =
  Int.clamp_exn (Option.value limit ~default) ~min:1 ~max
;;

let create ?(max_scan_bytes = 512 * 1024 * 1024) ~sessions_dir ~current () =
  let run (_ : Tool.Context.t) args =
    let dir = sessions_dir () in
    let session () =
      match Tool_args.string_opt args "session" with
      | Some key -> find ~dir key
      | None ->
        Or_error.error_string
          "session is required: the id of one that action search shows"
    in
    let limit = Tool_args.int_opt args "limit" in
    let result =
      match Tool_args.string args "action" with
      | "search" ->
        (match
           words (Option.value (Tool_args.string_opt args "query") ~default:"")
         with
         | [] ->
           Or_error.error_string "query is required: the words to look for"
         | words ->
           (match Tool_args.string_opt args "session" with
            | Some key ->
              let%bind.Or_error summary = find ~dir key in
              search_in ~current summary ~words
            | None ->
              Ok
                (search_all
                   ~max_scan_bytes
                   ~dir
                   ~current
                   ~words
                   ~limit:
                     (clamp
                        ~default:default_search_limit
                        ~max:max_search_limit
                        limit))))
      | "outline" ->
        let%bind.Or_error summary = session () in
        outline ~current summary ~before:(Tool_args.int_opt args "before")
      | "read" ->
        let%bind.Or_error summary = session () in
        read
          ~current
          summary
          ~from:(Tool_args.int_opt args "from")
          ~limit:(clamp ~default:default_read_limit ~max:max_read_limit limit)
      | other ->
        Or_error.errorf "unknown action %S: search, outline or read" other
    in
    match result with
    | Ok text -> Tool.Result.ok text
    | Error e -> Tool.Result.error (Error.to_string_hum e)
  in
  { Tool.spec; run }
;;
