open! Core
open! Import

let dim = Style.dim Style.plain
let gray = Style.fg Gray
let red = Style.fg Red
let green = Style.fg Green
let cyan = Style.fg Cyan

module Mark = struct
  type t =
    | Running
    | Done
    | Failed
    | Interrupted

  let span t : Content.Span.t =
    match t with
    | Running -> { text = "…"; style = Style.fg Yellow }
    | Done -> { text = "✓"; style = green }
    | Failed -> { text = "✕"; style = red }
    | Interrupted -> { text = "■"; style = dim }
  ;;
end

let chip ?(style = dim) text = { Content.Span.text; style }

let plural n word =
  if n = 1 then sprintf "1 %s" word else sprintf "%d %ss" n word
;;

let count_lines text = List.length (String.split_lines (String.rstrip text))
let lines_chip text = chip (plural (count_lines text) "line")

(* The first line of a multi-line argument, marked as continuing. *)
let one_line s =
  let s = String.strip s in
  match String.lsplit2 s ~on:'\n' with
  | Some (line, _) -> String.rstrip line ^ " …"
  | None -> s
;;

let header ~width ~mark ~name ?arg chips =
  let chips =
    List.concat_mapi chips ~f:(fun i (c : Content.Span.t) ->
      [ { Content.Span.text = (if i = 0 then "  " else " ")
        ; style = Style.plain
        }
      ; c
      ])
  in
  let name_width = Text_width.string name in
  let arg =
    match arg with
    | None -> []
    | Some arg ->
      let budget =
        width
        - Log_line.gutter_width
        - name_width
        - 1
        - Content.Line.width chips
      in
      [ { Content.Span.text =
            " " ^ Text_width.truncate arg ~width:(Int.max 12 budget)
        ; style = Style.plain
        }
      ]
  in
  Log_line.mark
    (Mark.span mark)
    (({ Content.Span.text = name; style = Style.bold Style.plain } :: arg)
     @ chips)
;;

let arg (call : P.Tool_call.t) =
  let args = Tool_args.of_call call in
  let str = Tool_args.string args in
  let ids () =
    match Tool_args.strings args "ids" @ Tool_args.strings args "id" with
    | [] -> None
    | ids -> Some (String.concat ~sep:" " ids)
  in
  let arg =
    match call.name with
    | "bash" -> Option.map (str "command") ~f:(fun c -> "$ " ^ one_line c)
    | "read" | "write" | "edit" -> str "path"
    | "ls" -> Some (Option.value (str "path") ~default:".")
    | "grep" | "find" -> str "pattern"
    | "subagent" -> str "task"
    | "job_wait" | "job_kill" | "subagent_wait" | "subagent_cancel" -> ids ()
    | _ ->
      (match
         List.find_map
           [ "command"; "path"; "pattern"; "task"; "url"; "query" ]
           ~f:str
       with
       | Some s -> Some s
       | None -> ids ())
  in
  Option.bind arg ~f:(fun s ->
    let s = one_line s in
    Option.some_if (not (String.is_empty s)) s)
;;

let summary call =
  match arg call with
  | Some arg -> call.name ^ " " ^ arg
  | None -> call.name
;;

let output ?(style = gray) ~head ?(tail = 0) text =
  let lines = String.split_lines (String.rstrip text) in
  let count = List.length lines in
  let line ?(style = style) text =
    Log_line.indent ~depth:2 (Content.Line.of_string ~style text)
  in
  if count - tail <= head
  then List.map lines ~f:line
  else
    List.map (List.take lines head) ~f:line
    @ [ line
          ~style:dim
          (sprintf "… %s" (plural (count - head - tail) "more line"))
      ]
    @ List.map (List.drop lines (count - tail)) ~f:line
;;

let all = Int.max_value
let live_tail_lines = 5

let rec pretty_json ?(indent = 0) (json : P.Json.t) : string list =
  let pad = String.make indent ' ' in
  match json with
  | `Object [] -> [ pad ^ "{}" ]
  | `Object fields ->
    ((pad ^ "{")
     :: List.concat_map fields ~f:(fun (key, value) ->
       match value with
       | `Object _ | `Array _ ->
         sprintf "%s  %s:" pad key :: pretty_json ~indent:(indent + 4) value
       | _ -> [ sprintf "%s  %s: %s" pad key (P.Json.to_string value) ]))
    @ [ pad ^ "}" ]
  | `Array [] -> [ pad ^ "[]" ]
  | `Array items ->
    ((pad ^ "[")
     :: List.concat_map items ~f:(fun item ->
       pretty_json ~indent:(indent + 2) item))
    @ [ pad ^ "]" ]
  | _ -> [ pad ^ P.Json.to_string json ]
;;

let arguments (call : P.Tool_call.t) =
  let lines =
    match P.Json.parse call.arguments with
    | Ok json -> pretty_json json
    | Error _ -> String.split_lines call.arguments
  in
  List.map lines ~f:(fun line ->
    Log_line.indent ~depth:2 (Content.Line.of_string ~style:dim line))
;;

(* [edit] returns a unified diff; its file header repeats the path. *)
let diff_lines text =
  match String.split_lines (String.rstrip text) with
  | old_file :: new_file :: rest
    when String.is_prefix old_file ~prefix:"--- a/"
         && String.is_prefix new_file ~prefix:"+++ b/" -> Some rest
  | _ -> None
;;

let diff_counts lines =
  List.fold lines ~init:(0, 0) ~f:(fun (added, removed) line ->
    if String.is_prefix line ~prefix:"+"
    then added + 1, removed
    else if String.is_prefix line ~prefix:"-"
    then added, removed + 1
    else added, removed)
;;

let is_diff text = String.is_prefix text ~prefix:"--- a/"

let diff_line line =
  let style =
    if
      String.is_prefix line ~prefix:"+++" || String.is_prefix line ~prefix:"---"
    then dim
    else if String.is_prefix line ~prefix:"@@"
    then cyan
    else if String.is_prefix line ~prefix:"+"
    then green
    else if String.is_prefix line ~prefix:"-"
    then red
    else gray
  in
  Log_line.indent ~depth:2 (Content.Line.of_string ~style line)
;;

let diff ~max_lines lines =
  let count = List.length lines in
  List.map (List.take lines max_lines) ~f:diff_line
  @
  if count > max_lines
  then
    [ Log_line.indent
        ~depth:2
        (Content.Line.of_string
           ~style:dim
           (sprintf "… %s" (plural (count - max_lines) "more line")))
    ]
  else []
;;

let images (images : P.Image.t list) =
  List.map images ~f:(fun image ->
    Log_line.indent
      ~depth:2
      (Content.Line.of_string ~style:cyan (P.Image.to_string_hum image)))
;;

let images_chip = function
  | [] -> []
  | images -> [ chip (plural (List.length images) "image") ]
;;

(* bash appends how a failed command ended as its last line. *)
let bash_outcome text =
  let lines = String.split_lines (String.rstrip text) in
  match List.last lines with
  | Some last
    when String.is_prefix last ~prefix:"["
         && String.is_suffix last ~suffix:"]"
         && List.exists
              [ "[exit code "; "[timed out after "; "[killed by " ]
              ~f:(fun prefix -> String.is_prefix last ~prefix) ->
    ( Some (String.sub last ~pos:1 ~len:(String.length last - 2))
    , String.concat ~sep:"\n" (List.drop_last_exn lines) )
  | _ -> None, text
;;

let job_started text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix:"started job " in
  let%map id, _ = String.lsplit2 rest ~on:':' in
  id
;;

let cancelled (r : P.Message.Tool_result.t) =
  r.is_error
  && List.exists (String.split_lines r.text) ~f:(fun line ->
    String.equal (String.strip line) "[cancelled]")
;;

let render
      ~(verbosity : Verbosity.t)
      ~width
      (call : P.Tool_call.t)
      result
      ~live_tail
  =
  let args = Tool_args.of_call call in
  let str key = Option.value (Tool_args.string args key) ~default:"" in
  let interrupted = Option.exists result ~f:cancelled in
  (* What a cancelled call managed to do is not an error. *)
  let result =
    Option.map result ~f:(fun (r : P.Message.Tool_result.t) ->
      if interrupted
      then
        { r with
          is_error = false
        ; text =
            String.concat
              ~sep:"\n"
              (List.filter (String.split_lines r.text) ~f:(fun line ->
                 not (String.equal (String.strip line) "[cancelled]")))
        }
      else r)
  in
  let mark : Mark.t =
    match result with
    | _ when interrupted -> Interrupted
    | Some { is_error = true; _ } -> Failed
    | Some _ -> Done
    | None -> Running
  in
  let quiet = Verbosity.equal verbosity Quiet in
  let error text =
    match verbosity with
    | Quiet -> output ~style:red ~head:3 text
    | Normal -> output ~style:red ~head:8 text
    | Verbose -> output ~style:red ~head:all text
  in
  let shown ?(head = 5) ?(tail = 0) text =
    match verbosity with
    | Quiet -> []
    | Normal -> output ~head ~tail text
    | Verbose -> output ~head:all text
  in
  let live () =
    let lines = String.split_lines (Option.value live_tail ~default:"") in
    let last n =
      List.map
        (List.drop lines (Int.max 0 (List.length lines - n)))
        ~f:(fun line ->
          Log_line.indent ~depth:2 (Content.Line.of_string ~style:gray line))
    in
    match verbosity with
    | Quiet -> []
    | Normal -> last 1
    | Verbose -> last live_tail_lines
  in
  (* Quiet mode shows no output, so says how much there was. *)
  let quiet_count text =
    if quiet && not (String.is_empty (String.strip text))
    then [ lines_chip text ]
    else []
  in
  let generic ?head ?tail (r : P.Message.Tool_result.t option) =
    match r with
    | None -> [], live ()
    | Some ({ is_error = true; _ } as r) -> images_chip r.images, error r.text
    | Some r ->
      let body =
        if is_diff r.text
        then (
          let lines = String.split_lines (String.rstrip r.text) in
          match verbosity with
          | Quiet -> []
          | Normal -> diff ~max_lines:8 lines
          | Verbose -> diff ~max_lines:all lines)
        else shown ?head ?tail r.text
      in
      ( quiet_count r.text @ images_chip r.images
      , body @ if quiet then [] else images r.images )
  in
  let chips, body =
    match call.name, result with
    | "bash", _ ->
      let background =
        Option.value (Tool_args.bool args "background") ~default:false
      in
      let job = Option.bind result ~f:(fun r -> job_started r.text) in
      let outcome, result =
        match result with
        | Some r ->
          let outcome, text = bash_outcome r.text in
          outcome, Some { r with text }
        | None -> None, None
      in
      let chips, body =
        match job with
        | Some _ -> [], []
        | None -> generic ~head:5 ~tail:3 result
      in
      ( List.filter_opt
          [ Option.some_if
              (background && Option.is_none job)
              (chip "background")
          ; Option.map job ~f:(fun id -> chip ("job " ^ id))
          ; Option.map outcome ~f:(chip ~style:red)
          ]
        @ chips
      , body )
    | "read", _ ->
      let range =
        match Tool_args.int args "offset", Tool_args.int args "limit" with
        | None, None -> []
        | Some o, None -> [ chip (sprintf "from line %d" o) ]
        | None, Some l -> [ chip (plural l "line") ]
        | Some o, Some l -> [ chip (sprintf "lines %d–%d" o (o + l - 1)) ]
      in
      (match result with
       | None -> range, []
       | Some ({ is_error = true; _ } as r) -> range, error r.text
       | Some ({ images = _ :: _; _ } as r) ->
         range @ images_chip r.images, if quiet then [] else images r.images
       | Some r ->
         ( range @ [ lines_chip r.text ]
         , (match verbosity with
            | Verbose -> output ~head:all r.text
            | Quiet | Normal -> []) ))
    | "write", _ ->
      let content = str "content" in
      let overwrote =
        match result with
        | Some { is_error = false; text; _ } ->
          String.is_prefix text ~prefix:"overwrote "
        | _ -> false
      in
      ( List.filter_opt
          [ Option.some_if (not (String.is_empty content)) (lines_chip content)
          ; Option.some_if overwrote (chip "overwrote")
          ]
      , (match result with
         | Some ({ is_error = true; _ } as r) -> error r.text
         | _ ->
           (match verbosity with
            | Verbose -> output ~head:all content
            | Quiet | Normal -> [])) )
    | "edit", Some ({ is_error = false; _ } as r) ->
      (match diff_lines r.text with
       | Some lines ->
         let added, removed = diff_counts lines in
         ( [ chip ~style:green (sprintf "+%d" added)
           ; chip ~style:red (sprintf "−%d" removed)
           ]
         , (match verbosity with
            | Quiet -> []
            | Normal -> diff ~max_lines:8 lines
            | Verbose -> diff ~max_lines:all lines) )
       | None -> generic result)
    | ("ls" | "grep" | "find"), _ ->
      let flags =
        List.filter_opt
          [ (if String.equal call.name "ls"
             then None
             else
               Option.map (Tool_args.string args "path") ~f:(fun p ->
                 chip ("in " ^ p)))
          ; Option.map (Tool_args.string args "glob") ~f:chip
          ; Option.bind (Tool_args.bool args "ignore_case") ~f:(fun b ->
              Option.some_if b (chip "ignore case"))
          ]
      in
      let chips, body = generic result in
      flags @ chips, body
    | _ -> generic result
  in
  let chips =
    match mark with
    | Interrupted -> chips @ [ chip "cancelled" ]
    | Running | Done | Failed -> chips
  in
  let arguments =
    match verbosity with
    | Verbose -> arguments call
    | Quiet | Normal -> []
  in
  (header ~width ~mark ~name:call.name ?arg:(arg call) chips :: arguments)
  @ body
;;
