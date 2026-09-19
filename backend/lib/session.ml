open! Core
open! Import

module Entry = struct
  type payload =
    | Message of Message.t
    | Model of
        { model : string
        ; thinking : Thinking.t
        }
    | Compaction of
        { summary : string
        ; kept_from : string
        }
    | Name of { name : string }
    | Cwd of { cwd : string }
  [@@deriving sexp, jsonaf]

  type t =
    { id : string
    ; parent : string option
    ; payload : payload
    }
  [@@deriving sexp, jsonaf]
end

module Line = struct
  type t =
    | Header of
        { id : string
        ; cwd : string
        ; created_at : string
        ; parent : string option [@jsonaf.option]
        }
    | Entry of Entry.t
    | Head of string
  [@@deriving sexp, jsonaf]
end

type t =
  { id : string
  ; path : string
  ; mutable cwd : string
  ; created_at : Time_float.t
  ; parent : string option
  ; mutable entries : Entry.t list (* reversed *)
  ; mutable head : string option
  ; by_id : Entry.t String.Table.t
  }

let id t = t.id
let path t = t.path
let cwd t = t.cwd
let parent t = t.parent
let head t = t.head
let entries t = List.rev t.entries
let created_at t = Time_float.to_string_utc t.created_at

let mtime t =
  Time_float.of_span_since_epoch
    (Time_float.Span.of_sec (Core_unix.stat t.path).st_mtime)
;;

let updated_at t = Time_float.to_string_utc (mtime t)

let duration_seconds t =
  let last = mtime t in
  let last =
    if Time_float.( > ) last t.created_at then last else Time_float.now ()
  in
  Time_float.diff last t.created_at |> Time_float.Span.to_sec
;;

let new_id () =
  String.concat (List.init 8 ~f:(fun _ -> sprintf "%02x" (Random.int 256)))
;;

let write_line t (line : Line.t) =
  Out_channel.with_file t.path ~append:true ~f:(fun oc ->
    Out_channel.output_string oc (Json.to_string (Line.jsonaf_of_t line));
    Out_channel.newline oc)
;;

let default_dir ~home = Filename.concat home ".prigh/sessions"

let stamp_of created_at =
  let date, ofday =
    Time_float.to_date_ofday created_at ~zone:Time_float.Zone.utc
  in
  let parts = Time_float.Ofday.to_parts ofday in
  sprintf
    "%s-%02d%02d%02d%03d"
    (Date.to_string_iso8601_basic date)
    parts.hr
    parts.min
    parts.sec
    parts.ms
;;

(* Session file names embed a millisecond stamp, so two sessions created in
   the same millisecond would collide. Remember the last stamp handed out and
   push the clock forward until the next one is strictly larger. *)
let last_created_at = ref None

let next_created_at () =
  let now = Time_float.now () in
  match !last_created_at with
  | None -> now
  | Some last ->
    let last_stamp = stamp_of last in
    if String.compare (stamp_of now) last_stamp > 0
    then now
    else (
      let rec bump t =
        let t = Time_float.add t (Time_float.Span.of_ms 1.) in
        if String.compare (stamp_of t) last_stamp > 0 then t else bump t
      in
      bump (Time_float.max now last))
;;

let create ~dir ~cwd ?parent () =
  Core_unix.mkdir_p dir;
  let id = new_id () in
  let created_at = next_created_at () in
  last_created_at := Some created_at;
  let stamp = stamp_of created_at in
  let path = Filename.concat dir (sprintf "%s_%s.jsonl" stamp id) in
  let t =
    { id
    ; path
    ; cwd
    ; created_at
    ; parent
    ; entries = []
    ; head = None
    ; by_id = String.Table.create ()
    }
  in
  write_line
    t
    (Header
       { id; cwd; created_at = Time_float.to_string_utc created_at; parent });
  t
;;

let add_entry t (entry : Entry.t) =
  t.entries <- entry :: t.entries;
  Hashtbl.set t.by_id ~key:entry.id ~data:entry;
  t.head <- Some entry.id;
  match entry.payload with
  | Cwd { cwd } -> t.cwd <- cwd
  | Message _ | Model _ | Compaction _ | Name _ -> ()
;;

let load path =
  Or_error.try_with (fun () ->
    let lines =
      In_channel.read_lines path
      |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
    in
    let parsed =
      List.map lines ~f:(fun l -> Line.t_of_jsonaf (Json.of_string l))
    in
    match parsed with
    | Header { id; cwd; created_at; parent } :: rest ->
      let created_at =
        match Time_float.of_string_with_utc_offset created_at with
        | t -> t
        | exception _ -> Time_float.epoch
      in
      let t =
        { id
        ; path
        ; cwd
        ; created_at
        ; parent
        ; entries = []
        ; head = None
        ; by_id = String.Table.create ()
        }
      in
      List.iter rest ~f:(function
        | Line.Header _ -> failwith "duplicate header"
        | Entry e -> add_entry t e
        | Head id -> t.head <- Some id);
      t
    | _ -> failwith "session file does not start with a header")
;;

let append t payload =
  let entry = { Entry.id = new_id (); parent = t.head; payload } in
  add_entry t entry;
  write_line t (Entry entry);
  entry
;;

let append_message t message = append t (Message message)
let set_model t ~model ~thinking = append t (Model { model; thinking })
let set_name t ~name = append t (Name { name })
let set_cwd t ~cwd = append t (Cwd { cwd })

let name t =
  List.find_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Name { name } -> Some name
    | Message _ | Model _ | Compaction _ | Cwd _ -> None)
;;

let append_compaction t ~summary ~kept_from =
  append t (Compaction { summary; kept_from })
;;

let active_path t =
  let rec go id acc =
    match Option.bind id ~f:(Hashtbl.find t.by_id) with
    | None -> acc
    | Some entry -> go entry.parent (entry :: acc)
  in
  go t.head []
;;

let messages t =
  let path = active_path t in
  let compaction =
    List.fold path ~init:None ~f:(fun acc (e : Entry.t) ->
      match e.payload with
      | Compaction { summary; kept_from } -> Some (summary, kept_from)
      | Message _ | Model _ | Name _ | Cwd _ -> acc)
  in
  let path =
    match compaction with
    | None -> path
    | Some (summary, kept_from) ->
      let kept =
        List.drop_while path ~f:(fun e -> not (String.equal e.id kept_from))
      in
      let kept =
        List.filter kept ~f:(fun e ->
          match e.payload with
          | Compaction _ | Name _ | Cwd _ -> false
          | Message _ | Model _ -> true)
      in
      { Entry.id = "summary"
      ; parent = None
      ; payload =
          Message
            (Message.user ("Summary of the conversation so far:\n" ^ summary))
      }
      :: kept
  in
  List.filter_map path ~f:(fun e ->
    match e.payload with
    | Message m -> Some m
    | Model _ | Compaction _ | Name _ | Cwd _ -> None)
;;

let model t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Model { model; thinking } -> Some (model, thinking)
    | Message _ | Compaction _ | Name _ | Cwd _ -> acc)
;;

let rewind t ~to_ =
  if Hashtbl.mem t.by_id to_
  then (
    t.head <- Some to_;
    write_line t (Head to_);
    Ok ())
  else Or_error.error_s [%message "no such entry" (to_ : string)]
;;

let fork ?at t ~dir =
  let at = Option.first_some at t.head in
  let path = active_path t in
  match at with
  | None -> Ok (create ~dir ~cwd:t.cwd ~parent:t.id ())
  | Some at ->
    if not (List.exists path ~f:(fun e -> String.equal e.id at))
    then
      Or_error.error_s [%message "entry not on the active path" (at : string)]
    else (
      let forked = create ~dir ~cwd:t.cwd ~parent:t.id () in
      let rec copy = function
        | [] -> ()
        | (e : Entry.t) :: rest ->
          ignore (append forked e.payload : Entry.t);
          if not (String.equal e.id at) then copy rest
      in
      copy path;
      Ok forked)
;;

module Summary = struct
  type t =
    { id : string
    ; path : string
    ; name : string option
    ; cwd : string
    ; created_at : string
    ; updated_at : string
    ; first_prompt : string option
    ; message_count : int
    ; parent : string option
    }
  [@@deriving sexp_of]
end

let list ~dir =
  match Sys_unix.is_directory dir with
  | `No | `Unknown -> []
  | `Yes ->
    Sys_unix.ls_dir dir
    |> List.filter ~f:(String.is_suffix ~suffix:".jsonl")
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun file ->
      let path = Filename.concat dir file in
      match load path with
      | Error _ -> None
      | Ok t ->
        let messages = messages t in
        Some
          { Summary.id = t.id
          ; path
          ; name = name t
          ; cwd = t.cwd
          ; created_at = created_at t
          ; updated_at = updated_at t
          ; first_prompt =
              List.find_map messages ~f:(function
                | Message.User u -> Some u.text
                | _ -> None)
          ; message_count = List.length messages
          ; parent = t.parent
          })
;;

let has_id ~dir id =
  match Sys_unix.is_directory dir with
  | `No | `Unknown -> false
  | `Yes ->
    Sys_unix.ls_dir dir
    |> List.filter ~f:(String.is_suffix ~suffix:".jsonl")
    |> List.exists ~f:(fun name ->
      match
        In_channel.with_file (Filename.concat dir name) ~f:In_channel.input_line
      with
      | None -> false
      | Some line ->
        (match Line.t_of_jsonaf (Json.of_string line) with
         | Header { id = other; _ } -> String.equal other id
         | _ -> false)
      | exception _ -> false)
;;

let import ~dir src_path =
  Or_error.bind (load src_path) ~f:(fun src ->
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p dir;
      let id = if has_id ~dir src.id then new_id () else src.id in
      let created_at = next_created_at () in
      last_created_at := Some created_at;
      let stamp = stamp_of created_at in
      let path = Filename.concat dir (sprintf "%s_%s.jsonl" stamp id) in
      let header =
        Json.to_string
          (Line.jsonaf_of_t
             (Header
                { id
                ; cwd = src.cwd
                ; created_at = Time_float.to_string_utc created_at
                ; parent = src.parent
                }))
      in
      let lines =
        In_channel.read_lines src_path
        |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
      in
      let rest = List.drop lines 1 in
      Out_channel.write_all
        path
        ~data:(String.concat (header :: rest) ~sep:"\n" ^ "\n");
      Or_error.ok_exn (load path)))
;;

module Export_format = struct
  type t =
    | Markdown
    | Jsonl
  [@@deriving sexp_of]

  let of_string = function
    | "markdown" -> Ok Markdown
    | "jsonl" -> Ok Jsonl
    | s ->
      Or_error.errorf "unknown export format %S (expected markdown or jsonl)" s
  ;;

  let extension = function
    | Markdown -> "md"
    | Jsonl -> "jsonl"
  ;;
end

let blockquote text =
  String.split_lines text
  |> List.map ~f:(fun line -> "> " ^ line)
  |> String.concat ~sep:"\n"
;;

let tool_call_block (call : Content.Tool_call.t) =
  let arguments =
    match Json.parse call.arguments with
    | Ok json -> Json.to_string_hum json
    | Error _ -> call.arguments
  in
  sprintf "### Tool: %s\n\n```json\n%s\n```" call.name (String.strip arguments)
;;

let to_markdown t =
  let block (e : Entry.t) =
    match e.payload with
    | Message (User u) -> Some (sprintf "## User\n\n%s" (String.strip u.text))
    | Message (Assistant a) ->
      let segments =
        List.filter_map a.content ~f:(function
          | Content.Text s when not (String.is_empty (String.strip s)) ->
            Some (String.strip s)
          | Content.Thinking th
            when not (String.is_empty (String.strip th.text)) ->
            Some (blockquote (String.strip th.text))
          | Content.Tool_call call -> Some (tool_call_block call)
          | Content.Text _ | Content.Thinking _ -> None)
      in
      if List.is_empty segments
      then None
      else Some (String.concat ("## Assistant" :: segments) ~sep:"\n\n")
    | Message (Tool_result r) ->
      Some
        (sprintf
           "### Tool: %s\n\n```\n%s\n```"
           r.tool_name
           (String.strip r.text))
    | Model _ | Compaction _ | Name _ | Cwd _ -> None
  in
  let blocks = List.filter_map (active_path t) ~f:block in
  String.concat (("# Session " ^ t.id) :: blocks) ~sep:"\n\n" ^ "\n"
;;
