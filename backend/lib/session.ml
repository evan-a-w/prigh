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
        }
    | Entry of Entry.t
    | Head of string
  [@@deriving sexp, jsonaf]
end

type t =
  { id : string
  ; path : string
  ; cwd : string
  ; mutable entries : Entry.t list (* reversed *)
  ; mutable head : string option
  ; by_id : Entry.t String.Table.t
  }

let id t = t.id
let path t = t.path
let cwd t = t.cwd
let head t = t.head
let entries t = List.rev t.entries

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

let create ~dir ~cwd =
  Core_unix.mkdir_p dir;
  let id = new_id () in
  let created_at = next_created_at () in
  last_created_at := Some created_at;
  let stamp = stamp_of created_at in
  let path = Filename.concat dir (sprintf "%s_%s.jsonl" stamp id) in
  let t =
    { id; path; cwd; entries = []; head = None; by_id = String.Table.create () }
  in
  write_line
    t
    (Header { id; cwd; created_at = Time_float.to_string_utc created_at });
  t
;;

let add_entry t (entry : Entry.t) =
  t.entries <- entry :: t.entries;
  Hashtbl.set t.by_id ~key:entry.id ~data:entry;
  t.head <- Some entry.id
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
    | Header { id; cwd; created_at = _ } :: rest ->
      let t =
        { id
        ; path
        ; cwd
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
      | Message _ | Model _ -> acc)
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
          | Compaction _ -> false
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
    | Model _ | Compaction _ -> None)
;;

let model t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Model { model; thinking } -> Some (model, thinking)
    | Message _ | Compaction _ -> acc)
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
  | None -> Ok (create ~dir ~cwd:t.cwd)
  | Some at ->
    if not (List.exists path ~f:(fun e -> String.equal e.id at))
    then
      Or_error.error_s [%message "entry not on the active path" (at : string)]
    else (
      let forked = create ~dir ~cwd:t.cwd in
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
    ; cwd : string
    ; created_at : string
    ; first_prompt : string option
    ; message_count : int
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
    |> List.filter_map ~f:(fun name ->
      let path = Filename.concat dir name in
      match load path with
      | Error _ -> None
      | Ok t ->
        let created_at =
          match In_channel.with_file path ~f:In_channel.input_line with
          | Some line ->
            (match Line.t_of_jsonaf (Json.of_string line) with
             | Header { created_at; _ } -> created_at
             | _ -> "")
          | None -> ""
        in
        let messages = messages t in
        Some
          { Summary.id = t.id
          ; path
          ; cwd = t.cwd
          ; created_at
          ; first_prompt =
              List.find_map messages ~f:(function
                | Message.User u -> Some u.text
                | _ -> None)
          ; message_count = List.length messages
          })
;;
