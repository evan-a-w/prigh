open! Core
open! Import

module Host = struct
  type t =
    { id : string
    ; name : string
    ; pinned : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

module Entry = struct
  module Payload = struct
    type t =
      | Message of Message.t
      | Model of
          { model : string
          ; thinking : Thinking.t
          }
      | Compaction of
          { summary : string
          ; kept_from : string
          ; pinned : string option [@jsonaf.option] [@sexp.option]
          }
      | Name of { name : string }
      | Description of { text : string }
      | Cwd of
          { cwd : string
          ; host : Host.t option [@jsonaf.option] [@sexp.option]
          }
      | System_prompt of { text : string }
      | Goal of { goal : Goal.t option }
      | Reminders of { reminders : Reminder.t list }
      | Budget of { usd : float option }
      | Approval of
          { mode : string
          ; plan : bool
          }
      | Allow_rule of { rule : string }
      | Archived of { archived : bool }
      | Spend of
          { usage : Usage.t
          ; cost_usd : float
          }
      | Resume_at of { at : float option }
      | Told_environment of Told_environment.t
      | Repo of
          { cwd : string
          ; repo : Git_location.t option
          }
      | Agent_worktree of Agent_worktree.t
      | Worktree_removed of { path : string }
    [@@deriving sexp, jsonaf]
  end

  type t =
    { id : string
    ; parent : string option
    ; at : float option [@jsonaf.option] [@sexp.option]
    ; payload : Payload.t
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
  ; mutable host : Host.t option
  ; host_cwds : string String.Table.t
  ; repos : Git_location.t option String.Table.t (** by cwd *)
  ; mutable agent_worktrees : Agent_worktree.t list (** newest first *)
  ; created_at : Time_float.t
  ; parent : string option
  ; mutable entries : Entry.t list (* reversed *)
  ; mutable head : string option
  ; by_id : Entry.t String.Table.t
  ; mutable persisted : bool
    (** whether the file exists: it is only written once the session has
        something worth keeping (a message or a name) *)
  ; unreadable : string option String.Table.t
    (** the parents of entries [load] could not read, by id, so that the
        entries after them stay on the path *)
  ; mutable skipped : string list
  ; mutable torn : bool (** the file does not end with a newline *)
  }

let id t = t.id
let path t = t.path
let cwd t = t.cwd
let host t = t.host
let host_cwds t = String.Map.of_hashtbl_exn t.host_cwds
let parent t = t.parent
let head t = t.head
let entries t = List.rev t.entries
let skipped t = t.skipped
let created_at t = Time_float.to_string_utc t.created_at

let mtime t =
  if t.persisted
  then
    Time_float.of_span_since_epoch
      (Time_float.Span.of_sec (Core_unix.stat t.path).st_mtime)
  else t.created_at
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

let output_line oc (line : Line.t) =
  Out_channel.output_string oc (Json.to_string (Line.jsonaf_of_t line));
  Out_channel.newline oc
;;

let header t =
  Line.Header
    { id = t.id
    ; cwd = t.cwd
    ; created_at = Time_float.to_string_utc t.created_at
    ; parent = t.parent
    }
;;

let worth_saving (line : Line.t) =
  match line with
  | Entry { payload = Message _ | Name _; _ } -> true
  | Entry
      { payload =
          ( Model _
          | Compaction _
          | Description _
          | Cwd _
          | System_prompt _
          | Goal _
          | Reminders _
          | Budget _
          | Approval _
          | Allow_rule _
          | Archived _
          | Spend _
          | Resume_at _
          | Told_environment _
          | Repo _
          | Agent_worktree _
          | Worktree_removed _ )
      ; _
      }
  | Header _ | Head _ -> false
;;

let persisted t = t.persisted
let gone t = t.persisted && not (Sys_unix.file_exists_exn t.path)

(* The file is created on the first line worth saving, with everything
   recorded in memory so far; before that nothing touches the disk. *)
(* Once the file exists, a line is only appended to it, never creates it: a
   session deleted while something still holds it (e.g. its description
   being written) stays deleted. *)
let write_line t (line : Line.t) =
  if t.persisted
  then (
    match Core_unix.openfile t.path ~mode:[ O_WRONLY; O_APPEND ] with
    | exception Core_unix.Unix_error (ENOENT, _, _) -> ()
    | fd ->
      let oc = Core_unix.out_channel_of_descr fd in
      Exn.protect
        ~f:(fun () ->
          if t.torn then Out_channel.newline oc;
          t.torn <- false;
          output_line oc line)
        ~finally:(fun () -> Out_channel.close oc))
  else if worth_saving line
  then (
    Core_unix.mkdir_p (Filename.dirname t.path);
    Out_channel.with_file t.path ~f:(fun oc ->
      output_line oc (header t);
      let entries = List.rev t.entries in
      List.iter entries ~f:(fun e -> output_line oc (Entry e));
      match t.head, List.last entries with
      | Some head, Some last when not (String.equal head last.id) ->
        output_line oc (Head head)
      | _ -> ());
    t.persisted <- true)
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
  let id = new_id () in
  let created_at = next_created_at () in
  last_created_at := Some created_at;
  let stamp = stamp_of created_at in
  let path = Filename.concat dir (sprintf "%s_%s.jsonl" stamp id) in
  let t =
    { id
    ; path
    ; cwd
    ; host = None
    ; host_cwds = String.Table.create ()
    ; repos = String.Table.create ()
    ; agent_worktrees = []
    ; created_at
    ; parent
    ; entries = []
    ; head = None
    ; by_id = String.Table.create ()
    ; persisted = false
    ; unreadable = String.Table.create ()
    ; skipped = []
    ; torn = false
    }
  in
  t
;;

let add_entry t (entry : Entry.t) =
  t.entries <- entry :: t.entries;
  Hashtbl.set t.by_id ~key:entry.id ~data:entry;
  t.head <- Some entry.id;
  match entry.payload with
  | Cwd { cwd; host } ->
    t.cwd <- cwd;
    Option.iter host ~f:(fun host ->
      t.host <- Some host;
      Hashtbl.set t.host_cwds ~key:host.id ~data:cwd)
  | Message _
  | Model _
  | Compaction _
  | Name _
  | Description _
  | System_prompt _
  | Goal _
  | Reminders _
  | Budget _
  | Approval _
  | Allow_rule _
  | Archived _
  | Spend _
  | Resume_at _
  | Told_environment _ -> ()
  | Repo { cwd; repo } -> Hashtbl.set t.repos ~key:cwd ~data:repo
  | Agent_worktree w ->
    t.agent_worktrees
    <- w
       :: List.filter t.agent_worktrees ~f:(fun (o : Agent_worktree.t) ->
         not (String.equal o.path w.path))
  | Worktree_removed { path } ->
    t.agent_worktrees
    <- List.filter t.agent_worktrees ~f:(fun (o : Agent_worktree.t) ->
         not (String.equal o.path path))
;;

let read_lines path =
  Utf8.sanitize (In_channel.read_all path)
  |> String.split_lines
  |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
;;

(* What can be known of an entry this version cannot read (e.g. one a newer
   prigh wrote): its id, its parent and the name of its kind. *)
let unreadable_entry (json : Json.t) =
  match json with
  | `Array [ `String "Entry"; (`Object _ as entry) ] ->
    (match Jsonaf.member "id" entry, Jsonaf.member "parent" entry with
     | Some (`String id), parent ->
       let parent =
         match parent with
         | Some (`String parent) -> Some parent
         | _ -> None
       in
       let kind =
         match Jsonaf.member "payload" entry with
         | Some (`Array (`String kind :: _)) -> kind
         | _ -> "unknown"
       in
       Some (id, parent, kind)
     | _ -> None)
  | _ -> None
;;

(* A line that cannot be read is skipped with a warning rather than making
   the whole session unreadable: a torn last line (prigh stopped while
   writing it) or a kind of line from a newer prigh. *)
let load ?(yield = fun () -> ()) path =
  Or_error.try_with (fun () ->
    let raw = In_channel.read_all path in
    yield ();
    let contents = Utf8.sanitize raw in
    yield ();
    let lines =
      String.split_lines contents
      |> List.mapi ~f:(fun i l -> i + 1, l)
      |> List.filter ~f:(fun (_, l) -> not (String.is_empty (String.strip l)))
    in
    let last = Option.value_map (List.last lines) ~default:0 ~f:fst in
    match lines with
    | [] -> failwith "session file is empty"
    | (_, first) :: rest ->
      (match Line.t_of_jsonaf (Json.of_string first) with
       | exception _ -> failwith "session file does not start with a header"
       | Entry _ | Head _ ->
         failwith "session file does not start with a header"
       | Header { id; cwd; created_at; parent } ->
         let created_at =
           match Time_float.of_string_with_utc_offset created_at with
           | t -> t
           | exception _ -> Time_float.epoch
         in
         let t =
           { id
           ; path
           ; cwd
           ; host = None
           ; host_cwds = String.Table.create ()
           ; repos = String.Table.create ()
           ; agent_worktrees = []
           ; created_at
           ; parent
           ; entries = []
           ; head = None
           ; by_id = String.Table.create ()
           ; persisted = true
           ; unreadable = String.Table.create ()
           ; skipped = []
           ; torn =
               (not (String.is_empty contents))
               && not (String.is_suffix contents ~suffix:"\n")
           }
         in
         let skip n reason =
           t.skipped <- sprintf "line %d: %s" n reason :: t.skipped
         in
         let parsed = ref 0 in
         List.iter rest ~f:(fun (n, l) ->
           parsed := !parsed + String.length l;
           if !parsed > 256 * 1024
           then (
             parsed := 0;
             yield ());
           match Json.of_string l with
           | exception _ ->
             skip
               n
               (if n = last
                then
                  "skipped an incomplete last line (prigh probably stopped \
                   while writing it)"
                else "skipped a line that is not JSON")
           | json ->
             (match Line.t_of_jsonaf json with
              | Header _ -> skip n "skipped a second header"
              | Entry e -> add_entry t e
              | Head id -> t.head <- Some id
              | exception _ ->
                (match unreadable_entry json with
                 | Some (id, parent, kind) ->
                   Hashtbl.set t.unreadable ~key:id ~data:parent;
                   t.head <- Some id;
                   skip
                     n
                     (sprintf
                        "skipped a %S entry this version of prigh cannot read \
                         (written by a newer one?)"
                        kind)
                 | None ->
                   skip
                     n
                     "skipped a line this version of prigh cannot read \
                      (written by a newer one?)")));
         t.skipped <- List.rev t.skipped;
         t))
;;

let append_at t ~at payload =
  let entry = { Entry.id = new_id (); parent = t.head; at; payload } in
  add_entry t entry;
  write_line t (Entry entry);
  entry
;;

let append ?(at = Core_unix.gettimeofday ()) t payload =
  append_at t ~at:(Some at) payload
;;

let append_message t ?at message = append ?at t (Message message)
let set_model t ~model ~thinking = append t (Model { model; thinking })
let set_name t ~name = append t (Name { name })
let set_description t ~text = append t (Description { text })
let set_cwd t ?host ~cwd () = append t (Cwd { cwd; host })
let set_system_prompt t ~text = append t (System_prompt { text })
let set_goal t goal = append t (Goal { goal })
let set_reminders t reminders = append t (Reminders { reminders })
let set_budget t ~usd = append t (Budget { usd })
let set_approval t ~mode ~plan = append t (Approval { mode; plan })
let add_allow_rule t ~rule = append t (Allow_rule { rule })
let set_archived t ~archived = append t (Archived { archived })
let add_spend t ~usage ~cost_usd = append t (Spend { usage; cost_usd })
let set_told_environment t told = append t (Told_environment told)
let repo t = Hashtbl.find t.repos t.cwd
let set_repo t ~cwd repo = append t (Repo { cwd; repo })
let agent_worktrees t = List.rev t.agent_worktrees
let add_agent_worktree t worktree = append t (Agent_worktree worktree)
let worktree_removed t ~path = append t (Worktree_removed { path })

let spend t =
  List.fold
    t.entries
    ~init:(Usage.zero, 0.)
    ~f:(fun (usage, cost) (e : Entry.t) ->
      match e.payload with
      | Spend s -> Usage.add usage s.usage, cost +. s.cost_usd
      | _ -> usage, cost)
;;

let resume_at t =
  List.find_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Resume_at { at } -> Some at
    | _ -> None)
  |> Option.join
;;

let set_resume_at t at =
  if not (Option.equal Float.equal (resume_at t) at)
  then ignore (append t (Resume_at { at }) : Entry.t)
;;

let name t =
  List.find_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Name { name } -> Some name
    | Message _
    | Model _
    | Compaction _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> None)
;;

let description t =
  List.find_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Description { text } -> Some text
    | Message _
    | Model _
    | Compaction _
    | Name _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> None)
;;

let append_compaction ?pinned t ~summary ~kept_from =
  append t (Compaction { summary; kept_from; pinned })
;;

let active_path t =
  let rec go id acc =
    match id with
    | None -> acc
    | Some id ->
      (match Hashtbl.find t.by_id id with
       | Some entry -> go entry.parent (entry :: acc)
       | None ->
         (match Hashtbl.find t.unreadable id with
          | Some parent -> go parent acc
          | None -> acc))
  in
  go t.head []
;;

let approval t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Approval { mode; plan } -> Some (mode, plan)
    | _ -> acc)
;;

let told_environment t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Told_environment told -> Some told
    | _ -> acc)
;;

let allow_rules t =
  List.filter_map (active_path t) ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Allow_rule { rule } -> Some rule
    | _ -> None)
;;

let summary_prefix = "Summary of the conversation so far:\n"

let summary_message summary =
  Message.user
    ~origin:{ Injected.empty with source = Some Summary }
    (summary_prefix ^ summary)
;;

let is_summary_message : Message.t -> bool = function
  | User { text; _ } -> String.is_prefix text ~prefix:summary_prefix
  | Assistant _ | Tool_result _ -> false
;;

(* The messages of the next request, each with the id of its entry (none for
   a compaction's summary): after a compaction, its pinned message, the
   summary, then the path from [kept_from]. *)
let context t =
  let path = active_path t in
  let compaction =
    List.fold path ~init:None ~f:(fun acc (e : Entry.t) ->
      match e.payload with
      | Compaction { summary; kept_from; pinned } ->
        Some (summary, kept_from, pinned)
      | Message _
      | Model _
      | Name _
      | Description _
      | Cwd _
      | System_prompt _
      | Goal _
      | Reminders _
      | Budget _
      | Approval _
      | Allow_rule _
      | Archived _
      | Spend _
      | Resume_at _
      | Told_environment _
      | Repo _
      | Agent_worktree _
      | Worktree_removed _ -> acc)
  in
  let messages entries =
    List.filter_map entries ~f:(fun (e : Entry.t) ->
      match e.payload with
      | Message message -> Some (Some e.id, { Timed_message.message; at = e.at })
      | Model _
      | Compaction _
      | Name _
      | Description _
      | Cwd _
      | System_prompt _
      | Goal _
      | Reminders _
      | Budget _
      | Approval _
      | Allow_rule _
      | Archived _
      | Spend _
      | Resume_at _
      | Told_environment _
      | Repo _
      | Agent_worktree _
      | Worktree_removed _ -> None)
  in
  match compaction with
  | None -> messages path
  | Some (summary, kept_from, pinned) ->
    let kept =
      List.drop_while path ~f:(fun e -> not (String.equal e.id kept_from))
    in
    let pinned =
      Option.bind pinned ~f:(Hashtbl.find t.by_id) |> Option.to_list
    in
    messages pinned
    @ [ None, { Timed_message.message = summary_message summary; at = None } ]
    @ messages kept
;;

let timed_messages t = List.map (context t) ~f:snd
let context_ids t = List.map (context t) ~f:fst
let identified_messages = context

let messages t =
  List.map (timed_messages t) ~f:(fun (m : Timed_message.t) -> m.message)
;;

let system_prompt t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | System_prompt { text } -> Some text
    | Message _
    | Model _
    | Compaction _
    | Name _
    | Description _
    | Cwd _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> acc)
;;

let goal t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Goal { goal } -> goal
    | Message _
    | Model _
    | Compaction _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> acc)
;;

let reminders t =
  List.fold (active_path t) ~init:[] ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Reminders { reminders } -> reminders
    | _ -> acc)
;;

let reminder_names t =
  List.concat_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Reminders { reminders } ->
      List.map reminders ~f:(fun (r : Reminder.t) -> r.name)
    | _ -> [])
;;

let budget t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Budget { usd } -> Some usd
    | Message _
    | Model _
    | Compaction _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> acc)
;;

let model t =
  List.fold (active_path t) ~init:None ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Model { model; thinking } -> Some (model, thinking)
    | Message _
    | Compaction _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> acc)
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
  let path = active_path t in
  let at =
    Option.first_some
      at
      (Option.map (List.last path) ~f:(fun (e : Entry.t) -> e.id))
  in
  match at with
  | None -> Ok (create ~dir ~cwd:t.cwd ~parent:t.id ())
  | Some at ->
    if not (List.exists path ~f:(fun e -> String.equal e.id at))
    then
      Or_error.error_s [%message "entry not on the active path" (at : string)]
    else (
      let forked = create ~dir ~cwd:t.cwd ~parent:t.id () in
      (* Copies get new ids, which a compaction's references follow. *)
      let ids = String.Table.create () in
      let renamed id = Option.value (Hashtbl.find ids id) ~default:id in
      let rec copy = function
        | [] -> ()
        | (e : Entry.t) :: rest ->
          let payload : Entry.Payload.t option =
            match e.payload with
            | Compaction { summary; kept_from; pinned } ->
              Some
                (Compaction
                   { summary
                   ; kept_from = renamed kept_from
                   ; pinned = Option.map pinned ~f:renamed
                   })
            | Archived _
            | Spend _
            | Resume_at _
            | Agent_worktree _
            | Worktree_removed _ -> None
            | payload -> Some payload
          in
          Option.iter payload ~f:(fun payload ->
            let copied = append_at forked ~at:e.at payload in
            Hashtbl.set ids ~key:e.id ~data:copied.id);
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
    ; description : string option
    ; cwd : string
    ; host : string option
    ; created_at : string
    ; updated_at : string
    ; first_prompt : string option
    ; message_count : int
    ; parent : string option
    ; archived : bool option
    ; last_activity : float
    ; resume_at : float option
    ; bytes : int
    ; repo : Git_location.t option option
    ; agent_worktrees : Agent_worktree.t list
    }
  [@@deriving sexp_of]

  let seconds_per_day = 86400.

  let is_archived t ~now ~archive_after_days ~active =
    (not active)
    &&
    match t.archived with
    | Some true -> true
    | Some false | None ->
      Float.(archive_after_days > 0.)
      && Float.(now - t.last_activity > archive_after_days * seconds_per_day)
  ;;
end

let data_dir path = String.chop_suffix_if_exists path ~suffix:".jsonl"

let is_directory path =
  match Core_unix.lstat path with
  | { st_kind = S_DIR; _ } -> true
  | _ | (exception _) -> false
;;

let files path =
  List.filter [ path; data_dir path ] ~f:(fun p -> Sys_unix.file_exists_exn p)
;;

let rec usage path =
  match Core_unix.lstat path with
  | exception _ -> 0
  | { st_kind = S_DIR; _ } ->
    List.sum
      (module Int)
      (Sys_unix.ls_dir path)
      ~f:(fun name -> usage (Filename.concat path name))
  | { st_size; _ } -> Int64.to_int_exn st_size
;;

let disk_usage path = List.sum (module Int) (files path) ~f:usage

let rec remove path =
  if is_directory path
  then (
    List.iter (Sys_unix.ls_dir path) ~f:(fun name ->
      remove (Filename.concat path name));
    Core_unix.rmdir path)
  else Core_unix.unlink path
;;

let delete_files path = List.iter (files path) ~f:remove

(* An [Archived] entry after the last message is a manual (un)archive; an
   unarchive also counts as activity. *)
let archived_and_activity t ~mtime =
  let last_message_at, marker =
    List.fold
      (List.rev t.entries)
      ~init:(None, None)
      ~f:(fun (last_message, marker) (e : Entry.t) ->
        match e.payload with
        | Message _ -> Some e.at, None
        | Archived { archived } -> last_message, Some (archived, e.at)
        | Model _
        | Compaction _
        | Name _
        | Description _
        | Cwd _
        | System_prompt _
        | Goal _
        | Reminders _
        | Budget _
        | Approval _
        | Allow_rule _
        | Spend _
        | Resume_at _
        | Told_environment _
        | Repo _
        | Agent_worktree _
        | Worktree_removed _ -> last_message, marker)
  in
  let activity = Option.value (Option.join last_message_at) ~default:mtime in
  let activity =
    match marker with
    | Some (false, Some at) -> Float.max activity at
    | Some _ | None -> activity
  in
  Option.map marker ~f:fst, activity
;;

let summarise t ~path ~mtime =
  let messages = messages t in
  let archived, last_activity = archived_and_activity t ~mtime in
  { Summary.id = t.id
  ; path
  ; name = name t
  ; description = description t
  ; cwd = t.cwd
  ; host = Option.map t.host ~f:(fun (h : Host.t) -> h.id)
  ; created_at = created_at t
  ; updated_at =
      Time_float.to_string_utc
        (Time_float.of_span_since_epoch (Time_float.Span.of_sec mtime))
  ; first_prompt =
      (* The user's words, not the notes the harness put before them; from
         the whole path, not what a compaction left (its summary first). *)
      List.find_map (active_path t) ~f:(fun (e : Entry.t) ->
        match e.payload with
        | Message (User u) when not (is_summary_message (User u)) ->
          let body = Message.User.body u in
          Option.some_if
            (not (String.is_empty (String.strip body)))
            (Skill.as_typed body)
        | _ -> None)
  ; message_count = List.length messages
  ; parent = t.parent
  ; archived
  ; last_activity
  ; resume_at = resume_at t
  ; bytes = 0
  ; repo = repo t
  ; agent_worktrees = agent_worktrees t
  }
;;

(* Summaries by path, valid while the file's size and mtime are unchanged,
   so listing does not parse every session again. *)
module Cache_entry = struct
  type t =
    { size : int64
    ; mtime : float
    ; summary : Summary.t option
    }
end

let summary_cache : Cache_entry.t String.Table.t = String.Table.create ()

let cached_summary ~live ~load path =
  match Core_unix.stat path with
  | exception _ -> None
  | { st_size = size; st_mtime = mtime; _ } ->
    let summary =
      match Hashtbl.find summary_cache path with
      | Some entry
        when Int64.equal entry.size size && Float.equal entry.mtime mtime ->
        entry.summary
      | Some _ | None ->
        let summary =
          (* A live session's file grows with every message: summing up
             what is in memory spares parsing it all again. *)
          (match live path with
           | Some t -> Some t
           | None -> Result.ok (load path))
          |> Option.map ~f:(fun t -> summarise t ~path ~mtime)
        in
        Hashtbl.set summary_cache ~key:path ~data:{ size; mtime; summary };
        summary
    in
    Option.map summary ~f:(fun s -> mtime, { s with bytes = disk_usage path })
;;

let list_with ~live ~load ~dir =
  match Sys_unix.is_directory dir with
  | `No | `Unknown -> []
  | `Yes ->
    let paths =
      Sys_unix.ls_dir dir
      |> List.filter ~f:(String.is_suffix ~suffix:".jsonl")
      |> List.sort ~compare:String.compare
      |> List.map ~f:(Filename.concat dir)
    in
    let present = String.Set.of_list paths in
    Hashtbl.filter_keys_inplace summary_cache ~f:(fun path ->
      (not (String.equal (Filename.dirname path) dir)) || Set.mem present path);
    List.filter_map paths ~f:(cached_summary ~live ~load)
    |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Float.compare b a)
    |> List.map ~f:snd
;;

let list ~dir = list_with ~live:(fun _ -> None) ~load ~dir

let find_summary summaries key =
  let key = String.strip key in
  match
    List.find summaries ~f:(fun (s : Summary.t) -> String.equal s.id key)
  with
  | Some s -> Ok s
  | None when String.is_empty key -> Or_error.error_string "no session id given"
  | None ->
    (match
       List.filter summaries ~f:(fun (s : Summary.t) ->
         String.is_prefix s.id ~prefix:key)
     with
     | [ s ] -> Ok s
     | [] -> Or_error.errorf "no session %S" key
     | many ->
       Or_error.errorf
         "session %S is ambiguous: %s"
         key
         (String.concat
            ~sep:", "
            (List.map many ~f:(fun (s : Summary.t) -> s.id))))
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
      let rest = List.drop (read_lines src_path) 1 in
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

let server_tool_block (tool : Content.Server_tool.t) =
  sprintf
    "### %s: %s%s"
    tool.name
    tool.input
    (Option.value_map tool.output ~default:"" ~f:(fun output ->
       "\n\n" ^ blockquote (String.strip output)))
;;

let heading title (at : float option) =
  match at with
  | None -> "## " ^ title
  | Some at ->
    let time = Time_float.of_span_since_epoch (Time_float.Span.of_sec at) in
    let date, ofday = Time_float.to_date_ofday time ~zone:Time_float.Zone.utc in
    let parts = Time_float.Ofday.to_parts ofday in
    sprintf
      "## %s · %s %02d:%02d UTC"
      title
      (Date.to_string date)
      parts.hr
      parts.min
;;

let to_markdown t =
  let images (images : Image.t list) =
    List.map images ~f:(fun i -> sprintf "\n\n*[image: %s]*" i.mime_type)
    |> String.concat
  in
  let block (e : Entry.t) =
    match e.payload with
    | Message (User u) ->
      Some
        (sprintf
           "%s\n\n%s%s"
           (heading "User" e.at)
           (String.strip u.text)
           (images u.images))
    | Message (Assistant a) ->
      let segments =
        List.filter_map a.content ~f:(function
          | Content.Text s when not (String.is_empty (String.strip s)) ->
            Some (String.strip s)
          | Content.Thinking th
            when not (String.is_empty (String.strip th.text)) ->
            Some (blockquote (String.strip th.text))
          | Content.Tool_call call -> Some (tool_call_block call)
          | Content.Server_tool tool -> Some (server_tool_block tool)
          | Content.Text _ | Content.Thinking _ -> None)
      in
      if List.is_empty segments
      then None
      else
        Some (String.concat (heading "Assistant" e.at :: segments) ~sep:"\n\n")
    | Message (Tool_result r) ->
      Some
        (sprintf
           "### Tool: %s\n\n```\n%s\n```%s"
           r.tool_name
           (String.strip r.text)
           (images r.images))
    | Model _
    | Compaction _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Archived _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree_removed _ -> None
  in
  let blocks = List.filter_map (active_path t) ~f:block in
  String.concat (("# Session " ^ t.id) :: blocks) ~sep:"\n\n" ^ "\n"
;;

module Filter = struct
  type t =
    { max_messages : int option
    ; cwd : string option
    ; older_than_days : float option
    ; prompt : string option
    }
  [@@deriving sexp_of]

  let matches t ~now (s : Summary.t) =
    Option.value_map t.max_messages ~default:true ~f:(fun n ->
      s.message_count <= n)
    && Option.value_map t.cwd ~default:true ~f:(String.equal s.cwd)
    && Option.value_map t.older_than_days ~default:true ~f:(fun days ->
      let updated = Time_float.of_string_with_utc_offset s.updated_at in
      Float.( >= ) (Time_float.Span.to_day (Time_float.diff now updated)) days)
    && Option.value_map t.prompt ~default:true ~f:(fun text ->
      Option.value_map s.first_prompt ~default:false ~f:(fun p ->
        String.is_substring p ~substring:text))
  ;;
end

let delete (s : Summary.t) = delete_files s.path
