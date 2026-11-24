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

module Home = struct
  type t =
    { cwd : string
    ; host : Host.t option [@jsonaf.option] [@sexp.option]
    ; repo : Git_location.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving sexp, jsonaf, equal]
end

module Fork_point = struct
  type t =
    { turn_id : string option [@jsonaf.option] [@sexp.option]
    ; mark : string option [@jsonaf.option] [@sexp.option]
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
          ; pinned : Pinned_messages.t
                [@jsonaf.default []]
                [@jsonaf_drop_default.equal]
                [@sexp.default []]
                [@sexp_drop_default.equal]
          }
      | Masked of { through : string }
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
          ; chat : bool
                [@sexp.bool]
                [@jsonaf.default false]
                [@jsonaf_drop_default.equal]
          }
      | Allow_rule of { rule : string }
      | Allowed_call of Allowed_call.t
      | Forget_allowed_call of
          { call : Allowed_call.Call.t option [@jsonaf.option] [@sexp.option] }
      | Archived of { archived : bool }
      | Pinned of { pinned : bool }
      | Spend of
          { usage : Usage.t
          ; cost_usd : float
          ; purpose : string option [@jsonaf.option] [@sexp.option]
          }
      | Resume_at of { at : float option }
      | Told_environment of Told_environment.t
      | Review of Review_state.Change.t
      | Repo of
          { cwd : string
          ; repo : Git_location.t option
          }
      | Agent_worktree of Agent_worktree.t
      | Worktree of Session_worktree.t
      | Worktree_removed of { path : string }
      | Home of { home : Home.t option }
      | Transcript_filter of { filter : string option }
      | Trim of { before : string }
      | Untrim
      | Trim_after of
          { after : string
          ; through : string
          }
      | Delete of { message : string }
      | Restore of { entry : string }
      | Engine of
          { engine : string
          ; adapter : string option [@jsonaf.option] [@sexp.option]
          }
      | Thread of
          { adapter : string
          ; host : string
          ; thread_id : string
          ; cwd : string
          ; fork_from : Fork_point.t option [@jsonaf.option] [@sexp.option]
          }
      | External_turn of
          { thread_id : string
          ; turn_id : string
          ; through : string
          ; first_turn_id : string option [@jsonaf.option] [@sexp.option]
          ; mark : string option [@jsonaf.option] [@sexp.option]
          ; before : string option [@jsonaf.option] [@sexp.option]
          ; after : string option [@jsonaf.option] [@sexp.option]
          ; cost_total : float option [@jsonaf.option] [@sexp.option]
          }
      | Handoff of
          { from_adapter : string option [@jsonaf.option] [@sexp.option]
          ; to_adapter : string
          ; text : string
          }
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

module Mode = struct
  type t =
    { approval : string
    ; plan : bool
    ; chat : bool
    }
  [@@deriving sexp_of, equal]
end

module Trim = struct
  type t =
    { before : string
    ; messages : int
    ; tokens : int
    ; at : float option
    }
  [@@deriving sexp_of]
end

module Hidden = struct
  module Kind = struct
    type t =
      | Trim_before of { before : string }
      | Trim_after of
          { after : string
          ; through : string
          }
      | Deleted of { message : string }
    [@@deriving sexp_of, equal]

    let is_trim = function
      | Trim_before _ | Trim_after _ -> true
      | Deleted _ -> false
    ;;
  end

  type t =
    { entry : string
    ; kind : Kind.t
    ; messages : int
    ; tokens : int
    ; at : float option
    ; divider : int option
    }
  [@@deriving sexp_of]
end

module Folded = struct
  type item = string option * Timed_message.t

  module Range = struct
    type t =
      { entry : Entry.t
      ; kind : Hidden.Kind.t
      ; items : item list (** what it took out of the context, oldest first *)
      ; hides : item -> bool
        (** whether it keeps the message out, when another range that took
            it out is cancelled *)
      }
  end

  type t =
    { context : item list
    ; hidden : Range.t list (** in effect, newest first *)
    ; position : int String.Table.t (** of the path's entries *)
    }
end

(* What a path says, folded entry by entry ([step]): the context and the
   ranges in effect, and the last of each setting on it. *)
module Path_state = struct
  module Reply = struct
    (* The latest reply: its request's input tokens, the context with it, and
       whether a trim, a deletion or their undoing came since. *)
    type t =
      { input : int
      ; rev_seen : Folded.item list
      ; edited : bool
      }
  end

  type t =
    { length : int
    ; rev_path : Entry.t list
    ; rev_context : Folded.item list
    ; hidden : Folded.Range.t list (** in effect, newest first *)
    ; reply : Reply.t option
    ; masked_through : string option
    ; approval : Mode.t option
    ; told_environment : Told_environment.t option
    ; system_prompt : string option
    ; goal : Goal.t option
    ; reminders : Reminder.t list
    ; budget : float option option
    ; model : (string * Thinking.t) option
    ; engine : (string * string option) option
    ; rev_allow_rules : string list
    ; rev_allowed_calls : Allowed_call.t list
    ; forgot_all : string option
      (** the latest [Forget_allowed_call] of them all *)
    }

  let empty =
    { length = 0
    ; rev_path = []
    ; rev_context = []
    ; hidden = []
    ; reply = None
    ; masked_through = None
    ; approval = None
    ; told_environment = None
    ; system_prompt = None
    ; goal = None
    ; reminders = []
    ; budget = None
    ; model = None
    ; engine = None
    ; rev_allow_rules = []
    ; rev_allowed_calls = []
    ; forgot_all = None
    }
  ;;
end

(* What the active path up to [head] makes of the conversation, worked out
   once per head: appending to the head advances it by the new entry, any
   other move of the head drops it. *)
module Derived = struct
  type t =
    { head : string option
    ; position : int String.Table.t
      (** of the path's entries; advancing adds to it *)
    ; state : Path_state.t
    ; path : Entry.t list Lazy.t
    ; context : Folded.item list Lazy.t
    ; messages : Message.t list Lazy.t
    ; hidden : Hidden.t list Lazy.t
    ; trim : Trim.t option Lazy.t
    ; context_tokens : int Lazy.t
    }
end

(* Totals of every branch, kept as entries are added. *)
module Totals = struct
  type t =
    { mutable spend : Usage.t * float
    ; spend_for : (int * float) String.Table.t (** by purpose *)
    ; mutable replies : (string * Usage.t) list
      (** each reply's model and usage, newest first *)
    ; mutable name : string option
    ; mutable description : string option
    ; mutable resume_at : float option
    }

  let create () =
    { spend = Usage.zero, 0.
    ; spend_for = String.Table.create ()
    ; replies = []
    ; name = None
    ; description = None
    ; resume_at = None
    }
  ;;

  let add t (payload : Entry.Payload.t) =
    match payload with
    | Spend { usage; cost_usd; purpose } ->
      let u, c = t.spend in
      t.spend <- Usage.add u usage, c +. cost_usd;
      Option.iter purpose ~f:(fun purpose ->
        Hashtbl.update t.spend_for purpose ~f:(function
          | None -> 1, cost_usd
          | Some (n, c) -> n + 1, c +. cost_usd))
    | Message (Assistant a) -> t.replies <- (a.model, a.usage) :: t.replies
    | Name { name } -> t.name <- Some name
    | Description { text } -> t.description <- Some text
    | Resume_at { at } -> t.resume_at <- at
    | _ -> ()
  ;;
end

type t =
  { id : string
  ; path : string
  ; initial_cwd : string (** the header's: before any [Cwd] entry *)
  ; mutable cwd : string
  ; mutable host : Host.t option
  ; host_cwds : string String.Table.t
  ; repos : Git_location.t option String.Table.t (** by cwd *)
  ; mutable worktrees : Session_worktree.t list (** newest first *)
  ; mutable home : Home.t option
  ; mutable transcript_filter : string option
  ; mutable pinned : bool
  ; created_at : Time_float.t
  ; parent : string option
  ; mutable entries : Entry.t list (* reversed *)
  ; mutable head : string option
  ; by_id : Entry.t String.Table.t
  ; mutable persisted : bool
    (** whether the file exists: it is only written once the session has
        something worth keeping (a message or a name) *)
  ; mutable temporary : bool (** never written, until {!keep} *)
  ; unreadable : string option String.Table.t
    (** the parents of entries [load] could not read, by id, so that the
        entries after them stay on the path *)
  ; mutable skipped : string list
  ; mutable torn : bool (** the file does not end with a newline *)
  ; masked : Message.t String.Table.t
    (** messages as {!Observation_mask.mask} makes them, by entry id *)
  ; offsets : int String.Table.t
    (** where each entry's line starts in the file, by entry id *)
  ; mutable derived : Derived.t option (** for [head], when worked out *)
  ; totals : Totals.t
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
  | Entry { payload = Message _ | Name _ | Review _; _ } -> true
  | Entry
      { payload =
          ( Model _
          | Compaction _
          | Masked _
          | Description _
          | Cwd _
          | System_prompt _
          | Goal _
          | Reminders _
          | Budget _
          | Approval _
          | Allow_rule _
          | Allowed_call _
          | Forget_allowed_call _
          | Archived _
          | Pinned _
          | Spend _
          | Resume_at _
          | Told_environment _
          | Repo _
          | Agent_worktree _
          | Worktree _
          | Worktree_removed _
          | Home _
          | Transcript_filter _
          | Engine _
          | Thread _
          | External_turn _
          | Handoff _
          | Trim _
          | Untrim
          | Trim_after _
          | Delete _
          | Restore _ )
      ; _
      }
  | Header _ | Head _ -> false
;;

let persisted t = t.persisted
let temporary t = t.temporary
let gone t = t.persisted && not (Sys_unix.file_exists_exn t.path)

let create_file t =
  Core_unix.mkdir_p (Filename.dirname t.path);
  Out_channel.with_file t.path ~f:(fun oc ->
    output_line oc (header t);
    let entries = List.rev t.entries in
    List.iter entries ~f:(fun e ->
      Hashtbl.set
        t.offsets
        ~key:e.id
        ~data:(Int64.to_int_exn (Out_channel.pos oc));
      output_line oc (Entry e));
    match t.head, List.last entries with
    | Some head, Some last when not (String.equal head last.id) ->
      output_line oc (Head head)
    | _ -> ());
  t.persisted <- true
;;

(* The file is created on the first line worth saving, with everything
   recorded in memory so far; before that nothing touches the disk. A
   temporary session never creates it. *)
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
          (match line with
           | Entry e ->
             (* The channel's position does not start at the end. *)
             let at = Int64.to_int_exn (Out_channel.length oc) in
             Hashtbl.set
               t.offsets
               ~key:e.id
               ~data:(if t.torn then at + 1 else at)
           | Header _ | Head _ -> ());
          if t.torn then Out_channel.newline oc;
          t.torn <- false;
          output_line oc line)
        ~finally:(fun () -> Out_channel.close oc))
  else if worth_saving line && not t.temporary
  then create_file t
;;

let keep t =
  if t.temporary
  then (
    t.temporary <- false;
    if List.exists t.entries ~f:(fun e -> worth_saving (Entry e))
    then create_file t)
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

let empty ~id ~path ~cwd ~created_at ~parent ~persisted ~temporary =
  { id
  ; path
  ; initial_cwd = cwd
  ; cwd
  ; host = None
  ; host_cwds = String.Table.create ()
  ; repos = String.Table.create ()
  ; worktrees = []
  ; home = None
  ; transcript_filter = None
  ; pinned = false
  ; created_at
  ; parent
  ; entries = []
  ; head = None
  ; by_id = String.Table.create ()
  ; persisted
  ; temporary
  ; unreadable = String.Table.create ()
  ; skipped = []
  ; torn = false
  ; masked = String.Table.create ()
  ; offsets = String.Table.create ()
  ; derived = None
  ; totals = Totals.create ()
  }
;;

let create ~dir ~cwd ?parent ?(temporary = false) () =
  let id = new_id () in
  let created_at = next_created_at () in
  last_created_at := Some created_at;
  let stamp = stamp_of created_at in
  let path = Filename.concat dir (sprintf "%s_%s.jsonl" stamp id) in
  empty ~id ~path ~cwd ~created_at ~parent ~persisted:false ~temporary
;;

(* The first way it mattered stays; its branch follows. *)
let track_worktree t (w : Session_worktree.t) =
  match
    List.find t.worktrees ~f:(fun (o : Session_worktree.t) ->
      String.equal o.path w.path)
  with
  | None -> t.worktrees <- w :: t.worktrees
  | Some _ ->
    t.worktrees
    <- List.map t.worktrees ~f:(fun (o : Session_worktree.t) ->
         if String.equal o.path w.path && Option.is_some w.branch
         then { o with branch = w.branch }
         else o)
;;

let summary_prefix = "Summary of the conversation so far:\n"

let summary_message summary =
  Message.user
    ~origin:{ Injected.empty with source = Some Summary }
    (summary_prefix ^ summary)
;;

let summary_of_message : Message.t -> string option = function
  | User { text; _ } -> String.chop_prefix text ~prefix:summary_prefix
  | Assistant _ | Tool_result _ -> None
;;

let is_summary_message m = Option.is_some (summary_of_message m)

(* A compaction's summary (an item without an id) sits where its [kept_from]
   is: after the pinned messages, before those it kept. *)
let summary_key = ""

let item_position position ((id, _) : Folded.item) =
  Hashtbl.find position (Option.value id ~default:summary_key)
;;

(* Before the first message that came after it on the path; at the end if
   none did. *)
let insert_by_position position context item =
  match item_position position item with
  | None -> item :: context
  | Some p ->
    let rec go = function
      | [] -> [ item ]
      | x :: rest ->
        (match item_position position x with
         | Some q when q > p -> item :: x :: rest
         | Some _ | None -> x :: go rest)
    in
    go context
;;

(* The range's messages go back into [context], but for those another range
   in effect hides, which go to it: in front for a trim before a message (a
   compaction since summarised only what came after them), else where they
   were. *)
let cancel position ~context ~hidden (r : Folded.Range.t) =
  let hidden =
    List.filter hidden ~f:(fun (h : Folded.Range.t) ->
      not (String.equal h.entry.id r.entry.id))
  in
  let by_position =
    List.stable_sort ~compare:(fun a b ->
      Option.compare
        Int.compare
        (item_position position a)
        (item_position position b))
  in
  let hidden, back =
    List.fold r.items ~init:(hidden, []) ~f:(fun (hidden, back) item ->
      match List.find hidden ~f:(fun (h : Folded.Range.t) -> h.hides item) with
      | Some h ->
        ( List.map hidden ~f:(fun (h' : Folded.Range.t) ->
            if String.equal h.entry.id h'.entry.id
            then { h' with items = by_position (h'.items @ [ item ]) }
            else h')
        , back )
      | None -> hidden, item :: back)
  in
  let back = List.rev back in
  let context =
    match r.kind with
    | Trim_before _ -> back @ context
    | Trim_after _ | Deleted _ ->
      List.fold back ~init:context ~f:(insert_by_position position)
  in
  context, hidden
;;

(* Message by message, as [Trim_point] sums them. *)
let estimate items =
  List.sum
    (module Int)
    items
    ~f:(fun (_, (m : Timed_message.t)) -> Message.estimate_tokens [ m.message ])
;;

let trim_of_folded ({ hidden; _ } : Folded.t) =
  let trims =
    List.filter hidden ~f:(fun (r : Folded.Range.t) ->
      match r.kind with
      | Trim_before _ -> true
      | Trim_after _ | Deleted _ -> false)
  in
  match trims with
  | [] -> None
  | latest :: _ ->
    let dropped = List.concat_map trims ~f:(fun r -> r.items) in
    let before =
      match latest.kind with
      | Trim_before { before } -> before
      | Trim_after _ | Deleted _ -> latest.entry.id
    in
    Some
      { Trim.before
      ; messages = List.length dropped
      ; tokens = estimate dropped
      ; at = latest.entry.at
      }
;;

let hidden_of_range ({ context; position; _ } : Folded.t) (r : Folded.Range.t) =
  let divider =
    match r.kind with
    | Trim_before _ | Deleted _ -> None
    | Trim_after { after; _ } ->
      let a = Option.value (Hashtbl.find position after) ~default:(-1) in
      Some
        (List.foldi context ~init:0 ~f:(fun i acc item ->
           match item_position position item with
           | Some p when p <= a -> i + 1
           | Some _ | None -> acc))
  in
  { Hidden.entry = r.entry.id
  ; kind = r.kind
  ; messages = List.length r.items
  ; tokens = estimate r.items
  ; at = r.entry.at
  ; divider
  }
;;

(* Folds the entry [e] into [s], the state of the path before it: a
   compaction's summary replaces what comes before its [kept_from] in the
   context so far, preceded by the compaction's pinned messages; a trim
   drops what comes before its message, a trim after a message what comes
   after it up to its [through], a deletion its message with the results of
   its calls; an untrim cancels the latest trim in effect, a restore the
   entry it names. Records [e]'s position in [position], which must have
   those of the entries before it. *)
let step ~find position (s : Path_state.t) (e : Entry.t) : Path_state.t =
  Hashtbl.set position ~key:e.id ~data:s.length;
  let pos = Hashtbl.find position in
  let message (e : Entry.t) =
    match e.payload with
    | Message message -> Some (Some e.id, { Timed_message.message; at = e.at })
    | _ -> None
  in
  let take (e : Entry.t) kind ~hides (rev_context, hidden) =
    let items, rest = List.partition_tf (List.rev rev_context) ~f:hides in
    List.rev rest, { Folded.Range.entry = e; kind; items; hides } :: hidden
  in
  let cancel_where (rev_context, hidden) ~f =
    match List.find hidden ~f with
    | None -> rev_context, hidden
    | Some r ->
      let context, hidden =
        cancel position ~context:(List.rev rev_context) ~hidden r
      in
      List.rev context, hidden
  in
  let rev_context, hidden =
    let ((rev_context, hidden) as acc) = s.rev_context, s.hidden in
    match e.payload with
    | Message _ -> Option.to_list (message e) @ rev_context, hidden
    | Compaction { summary; kept_from; pinned } ->
      let from = pos kept_from in
      Option.iter from ~f:(fun from ->
        Hashtbl.set position ~key:summary_key ~data:from);
      let kept =
        List.filter (List.rev rev_context) ~f:(fun (id, _) ->
          match Option.bind id ~f:pos, from with
          | Some p, Some from -> p >= from
          | _ -> false)
      in
      let pinned =
        List.filter_map pinned ~f:(fun (pin : Pinned_messages.Pin.t) ->
          Option.bind (find pin.id) ~f:message
          |> Option.map ~f:(fun (id, (m : Timed_message.t)) ->
            if pin.shortened
            then id, { m with message = Pinned_messages.shorten m.message }
            else id, m))
        |> List.filter ~f:(fun item ->
          not (List.exists hidden ~f:(fun (h : Folded.Range.t) -> h.hides item)))
      in
      let summary =
        None, { Timed_message.message = summary_message summary; at = None }
      in
      List.rev (pinned @ (summary :: kept)), hidden
    | Trim { before } ->
      let context = List.rev rev_context in
      let hides item =
        match item_position position item, pos before with
        | Some p, Some b -> p < b
        | _ -> false
      in
      let dropped, kept =
        match
          List.findi context ~f:(fun _ (id, _) ->
            Option.equal String.equal id (Some before))
        with
        | Some (i, _) -> List.take context i, List.drop context i
        | None -> [], context
      in
      ( List.rev kept
      , { Folded.Range.entry = e
        ; kind = Trim_before { before }
        ; items = dropped
        ; hides
        }
        :: hidden )
    | Trim_after { after; through } ->
      take
        e
        (Trim_after { after; through })
        ~hides:(fun item ->
          match item_position position item, pos after, pos through with
          | Some p, Some a, Some th -> a < p && p <= th
          | _ -> false)
        acc
    | Delete { message } ->
      let ids = Message_point.deleted_with (List.rev rev_context) ~message in
      take
        e
        (Deleted { message })
        ~hides:(fun (id, _) ->
          Option.value_map id ~default:false ~f:(Set.mem ids))
        acc
    | Untrim ->
      cancel_where acc ~f:(fun (r : Folded.Range.t) ->
        Hidden.Kind.is_trim r.kind)
    | Restore { entry } ->
      cancel_where acc ~f:(fun (r : Folded.Range.t) ->
        String.equal r.entry.id entry)
    | Model _
    | Masked _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Allowed_call _
    | Forget_allowed_call _
    | Archived _
    | Pinned _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree _
    | Worktree_removed _
    | Home _
    | Transcript_filter _
    | Engine _
    | Thread _
    | External_turn _
    | Handoff _
    | Review _ -> acc
  in
  let reply : Path_state.Reply.t option =
    match e.payload with
    | Message (Assistant a) ->
      Some { input = a.usage.input; rev_seen = rev_context; edited = false }
    | Trim _ | Untrim | Trim_after _ | Delete _ | Restore _ ->
      Option.map s.reply ~f:(fun r -> { r with edited = true })
    | _ -> s.reply
  in
  let s =
    { s with
      length = s.length + 1
    ; rev_path = e :: s.rev_path
    ; rev_context
    ; hidden
    ; reply
    }
  in
  match e.payload with
  | Masked { through } -> { s with masked_through = Some through }
  | Approval { mode; plan; chat } ->
    { s with approval = Some { approval = mode; plan; chat } }
  | Told_environment told -> { s with told_environment = Some told }
  | System_prompt { text } -> { s with system_prompt = Some text }
  | Goal { goal } -> { s with goal }
  | Reminders { reminders } -> { s with reminders }
  | Budget { usd } -> { s with budget = Some usd }
  | Model { model; thinking } -> { s with model = Some (model, thinking) }
  | Engine { engine; adapter } -> { s with engine = Some (engine, adapter) }
  | Allow_rule { rule } ->
    { s with rev_allow_rules = rule :: s.rev_allow_rules }
  | Allowed_call call ->
    { s with rev_allowed_calls = call :: s.rev_allowed_calls }
  | Forget_allowed_call { call = None } ->
    { s with rev_allowed_calls = []; forgot_all = Some e.id }
  | Forget_allowed_call { call = Some call } ->
    { s with
      rev_allowed_calls =
        List.filter s.rev_allowed_calls ~f:(fun (a : Allowed_call.t) ->
          not (Allowed_call.Call.equal a.call call))
    }
  | Message _
  | Compaction _
  | Name _
  | Description _
  | Cwd _
  | Archived _
  | Pinned _
  | Spend _
  | Resume_at _
  | Repo _
  | Agent_worktree _
  | Worktree _
  | Worktree_removed _
  | Home _
  | Transcript_filter _
  | Thread _
  | External_turn _
  | Handoff _
  | Trim _
  | Untrim
  | Trim_after _
  | Delete _
  | Restore _
  | Review _ -> s
;;

let derive_path ~find ~position path =
  List.fold path ~init:Path_state.empty ~f:(step ~find position)
;;

(* The messages of [context] to send: those up to the path's last [Masked]
   entry's [through] masked ({!Observation_mask.mask}, kept by id in
   [masked]). *)
let masked_messages ~masked ~position ~through context =
  let through = Option.bind through ~f:(Hashtbl.find position) in
  List.map context ~f:(fun (id, (m : Timed_message.t)) ->
    match id, m.message, through with
    (* Masking leaves the user's messages as they are; a pinned one may be
       shortened, which the cache by id would not tell apart. *)
    | _, User _, _ | None, _, _ | _, _, None -> m.message
    | Some id, (Assistant _ | Tool_result _), Some through ->
      (match Hashtbl.find position id with
       | Some p when p <= through ->
         Hashtbl.find_or_add masked id ~default:(fun () ->
           Observation_mask.mask m.message)
       | Some _ | None -> m.message))
;;

let make t ~head ~position (s : Path_state.t) : Derived.t =
  let context = lazy (List.rev s.rev_context) in
  let folded =
    lazy { Folded.context = force context; hidden = s.hidden; position }
  in
  { head
  ; position
  ; state = s
  ; path = lazy (List.rev s.rev_path)
  ; context
  ; messages =
      lazy
        (masked_messages
           ~masked:t.masked
           ~position
           ~through:s.masked_through
           (force context))
  ; hidden =
      lazy
        (let folded = force folded in
         List.rev_map folded.hidden ~f:(hidden_of_range folded))
  ; trim = lazy (trim_of_folded (force folded))
  ; context_tokens =
      lazy
        (match s.reply with
         | None -> 0
         | Some { input; edited = false; rev_seen = _ } -> input
         | Some { input; edited = true; rev_seen } ->
           Int.max 0 (input + estimate (force context) - estimate rev_seen))
  }
;;

(* The path from the root to [head], following the parents. *)
let walk t =
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

let derived t =
  match t.derived with
  | Some d -> d
  | None ->
    let position = String.Table.create () in
    let state = derive_path ~find:(Hashtbl.find t.by_id) ~position (walk t) in
    let d = make t ~head:t.head ~position state in
    t.derived <- Some d;
    d
;;

(* The head moves other than by appending to it. *)
let set_head t head =
  t.head <- head;
  t.derived <- None
;;

(* [entry] just became the head: what was derived for its parent advances
   by it. *)
let advance t (entry : Entry.t) =
  t.derived
  <- (match t.derived with
      | Some d when Option.equal String.equal d.head entry.parent ->
        let state =
          step ~find:(Hashtbl.find t.by_id) d.position d.state entry
        in
        Some (make t ~head:(Some entry.id) ~position:d.position state)
      | Some _ | None -> None)
;;

let add_entry t (entry : Entry.t) =
  t.entries <- entry :: t.entries;
  Hashtbl.set t.by_id ~key:entry.id ~data:entry;
  advance t entry;
  t.head <- Some entry.id;
  Totals.add t.totals entry.payload;
  match entry.payload with
  | Cwd { cwd; host } ->
    t.cwd <- cwd;
    Option.iter host ~f:(fun host ->
      t.host <- Some host;
      Hashtbl.set t.host_cwds ~key:host.id ~data:cwd)
  | Message _
  | Model _
  | Compaction _
  | Masked _
  | Name _
  | Description _
  | System_prompt _
  | Goal _
  | Reminders _
  | Budget _
  | Approval _
  | Allow_rule _
  | Allowed_call _
  | Forget_allowed_call _
  | Archived _
  | Spend _
  | Resume_at _
  | Told_environment _
  | Review _
  | Trim _
  | Untrim
  | Trim_after _
  | Delete _
  | Engine _
  | Thread _
  | External_turn _
  | Handoff _
  | Restore _ -> ()
  | Repo { cwd; repo } ->
    Hashtbl.set t.repos ~key:cwd ~data:repo;
    Option.iter
      (Option.bind repo ~f:Session_worktree.of_location)
      ~f:(track_worktree t)
  | Agent_worktree w -> track_worktree t (Session_worktree.of_agent w)
  | Worktree w -> track_worktree t w
  | Home { home } -> t.home <- home
  | Transcript_filter { filter } -> t.transcript_filter <- filter
  | Pinned { pinned } -> t.pinned <- pinned
  | Worktree_removed { path } ->
    t.worktrees
    <- List.filter t.worktrees ~f:(fun (o : Session_worktree.t) ->
         not (String.equal o.path path));
    (* Entering it again (made anew) records it again. *)
    Hashtbl.filter_keys_inplace t.repos ~f:(fun cwd ->
      not (Session_worktree.contains ~path ~dir:cwd))
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

(* Calls [f ~number ~offset line] for each line of [path], numbered from 1,
   with the byte offset where it starts, without its newline; reads a chunk
   at a time, so a large file is never in memory whole. [yield] every
   256 KB. Returns whether the file ends without a newline. *)
let iter_lines ?(yield = fun () -> ()) path ~f =
  In_channel.with_file path ~f:(fun ic ->
    let chunk = Bytes.create 65536 in
    let partial = Buffer.create 1024 in
    let partial_at = ref 0 in
    let number = ref 0 in
    let read = ref 0 in
    let since_yield = ref 0 in
    let rec go () =
      let n = In_channel.input ic ~buf:chunk ~pos:0 ~len:(Bytes.length chunk) in
      if n > 0
      then (
        let base = !read in
        let pos = ref 0 in
        while !pos < n do
          match Stdlib.Bytes.index_from_opt chunk !pos '\n' with
          | Some nl when nl < n ->
            incr number;
            let line, offset =
              if Buffer.length partial = 0
              then
                ( Bytes.To_string.sub chunk ~pos:!pos ~len:(nl - !pos)
                , base + !pos )
              else (
                Buffer.add_subbytes partial chunk ~pos:!pos ~len:(nl - !pos);
                let line = Buffer.contents partial in
                Buffer.clear partial;
                line, !partial_at)
            in
            pos := nl + 1;
            f ~number:!number ~offset (Utf8.sanitize line)
          | Some _ | None ->
            if Buffer.length partial = 0 then partial_at := base + !pos;
            Buffer.add_subbytes partial chunk ~pos:!pos ~len:(n - !pos);
            pos := n
        done;
        read := !read + n;
        since_yield := !since_yield + n;
        if !since_yield >= 256 * 1024
        then (
          since_yield := 0;
          yield ());
        go ())
    in
    go ();
    if Buffer.length partial > 0
    then (
      incr number;
      f
        ~number:!number
        ~offset:!partial_at
        (Utf8.sanitize (Buffer.contents partial));
      true)
    else false)
;;

let is_blank l = String.for_all l ~f:Char.is_whitespace

let skip t ~number reason =
  t.skipped <- sprintf "line %d: %s" number reason :: t.skipped
;;

(* A line after the header. *)
let read_json t ~number ~offset json =
  match Line.t_of_jsonaf json with
  | Header _ -> skip t ~number "skipped a second header"
  | Entry e ->
    Hashtbl.set t.offsets ~key:e.id ~data:offset;
    add_entry t e
  | Head id -> set_head t (Some id)
  | exception _ ->
    (match unreadable_entry json with
     | Some (id, parent, kind) ->
       Hashtbl.set t.unreadable ~key:id ~data:parent;
       set_head t (Some id);
       skip
         t
         ~number
         (sprintf
            "skipped a %S entry this version of prigh cannot read (written by \
             a newer one?)"
            kind)
     | None ->
       skip
         t
         ~number
         "skipped a line this version of prigh cannot read (written by a newer \
          one?)")
;;

(* A line that cannot be read is skipped with a warning rather than making
   the whole session unreadable: a torn last line (prigh stopped while
   writing it) or a kind of line from a newer prigh. The file is read a
   chunk at a time: what [load] holds beyond the session is one line. *)
let load ?yield path =
  Or_error.try_with (fun () ->
    let session = ref None in
    (* A line that is not JSON is only known to be the last one at the end;
       the others wait one line. *)
    let unparsed = ref None in
    let flush_unparsed ~is_last =
      Option.iter !unparsed ~f:(fun (t, number) ->
        unparsed := None;
        skip
          t
          ~number
          (if is_last
           then
             "skipped an incomplete last line (prigh probably stopped while \
              writing it)"
           else "skipped a line that is not JSON"))
    in
    let torn =
      iter_lines ?yield path ~f:(fun ~number ~offset l ->
        if not (is_blank l)
        then (
          match !session with
          | None ->
            (match Line.t_of_jsonaf (Json.of_string l) with
             | exception _ ->
               failwith "session file does not start with a header"
             | Entry _ | Head _ ->
               failwith "session file does not start with a header"
             | Header { id; cwd; created_at; parent } ->
               let created_at =
                 match Time_float.of_string_with_utc_offset created_at with
                 | t -> t
                 | exception _ -> Time_float.epoch
               in
               session
               := Some
                    (empty
                       ~id
                       ~path
                       ~cwd
                       ~created_at
                       ~parent
                       ~persisted:true
                       ~temporary:false))
          | Some t ->
            flush_unparsed ~is_last:false;
            (match Json.of_string l with
             | exception _ -> unparsed := Some (t, number)
             | json -> read_json t ~number ~offset json)))
    in
    match !session with
    | None -> failwith "session file is empty"
    | Some t ->
      flush_unparsed ~is_last:true;
      t.torn <- torn;
      t.skipped <- List.rev t.skipped;
      t)
;;

let entry_offset t id = Hashtbl.find t.offsets id

let read_entry_at path ~offset =
  match
    In_channel.with_file path ~f:(fun ic ->
      In_channel.seek ic (Int64.of_int offset);
      In_channel.input_line ic)
  with
  | exception _ -> None
  | None -> None
  | Some l ->
    (match Line.t_of_jsonaf (Json.of_string (Utf8.sanitize l)) with
     | Entry e -> Some e
     | Header _ | Head _ | (exception _) -> None)
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
let set_approval t ~mode ~plan ~chat = append t (Approval { mode; plan; chat })
let add_allow_rule t ~rule = append t (Allow_rule { rule })
let add_allowed_call t call = append t (Allowed_call call)
let forget_allowed_call t call = append t (Forget_allowed_call { call })

let set_archived t ~archived =
  if archived && t.pinned
  then ignore (append t (Pinned { pinned = false }) : Entry.t);
  append t (Archived { archived })
;;

let pinned t = t.pinned

let add_spend ?purpose t ~usage ~cost_usd =
  append t (Spend { usage; cost_usd; purpose })
;;

let set_told_environment t told = append t (Told_environment told)
let repo t = Hashtbl.find t.repos t.cwd
let set_repo t ~cwd repo = append t (Repo { cwd; repo })
let worktrees t = List.rev t.worktrees
let add_agent_worktree t worktree = append t (Agent_worktree worktree)
let add_worktree t worktree = append t (Worktree worktree)
let worktree_removed t ~path = append t (Worktree_removed { path })
let home t = t.home
let set_home t home = append t (Home { home })
let transcript_filter t = t.transcript_filter
let set_transcript_filter t filter = append t (Transcript_filter { filter })
let record_review t change = append t (Review change)

let review t =
  List.fold (entries t) ~init:Review_state.empty ~f:(fun acc (e : Entry.t) ->
    match e.payload with
    | Review change -> Review_state.apply acc change
    | _ -> acc)
;;

let spend t = t.totals.spend

let spend_for t ~purpose =
  Hashtbl.find t.totals.spend_for purpose |> Option.value ~default:(0, 0.)
;;

let replies t = List.rev t.totals.replies
let resume_at t = t.totals.resume_at

let set_resume_at t at =
  if not (Option.equal Float.equal (resume_at t) at)
  then ignore (append t (Resume_at { at }) : Entry.t)
;;

let name t = t.totals.name
let description t = t.totals.description

let append_compaction ?(pinned = []) t ~summary ~kept_from =
  append t (Compaction { summary; kept_from; pinned })
;;

let append_masked t ~through = append t (Masked { through })
let active_path t = force (derived t).path
let approval t = (derived t).state.approval
let told_environment t = (derived t).state.told_environment
let allow_rules t = List.rev (derived t).state.rev_allow_rules
let allowed_calls t = List.rev (derived t).state.rev_allowed_calls
let context t = force (derived t).context
let trim t = force (derived t).trim
let hidden t = force (derived t).hidden

let trimmed_messages t =
  List.concat_map (List.rev (derived t).state.hidden) ~f:(fun r ->
    match r.kind with
    | Trim_before _ -> r.items
    | Trim_after _ | Deleted _ -> [])
;;

let hidden_messages t ~entry =
  List.find_map (derived t).state.hidden ~f:(fun r ->
    Option.some_if (String.equal r.entry.id entry) r.items)
  |> Option.value ~default:[]
;;

let is_trim_point (m : Message.t) =
  match m with
  | User _ -> not (is_summary_message m)
  | Assistant _ -> true
  | Tool_result _ -> false
;;

let append_trim t ~before =
  let context = context t in
  match
    List.findi context ~f:(fun _ (id, _) ->
      Option.equal String.equal id (Some before))
  with
  | None ->
    Or_error.errorf
      "message %s is not in the model's context: trim before one of the \
       messages that /trim lists"
      before
  | Some (0, _) ->
    Or_error.error_string
      "nothing to trim: that message is the first one in the model's context"
  | Some (i, (_, m)) ->
    let dropped = List.take context i in
    let kept = List.drop context i in
    if not (is_trim_point m.message)
    then
      Or_error.error_string
        "a tool result goes with its call: trim before the reply that made the \
         call (/trim lists it)"
    else if Message_point.splits_a_call ~dropped ~kept
    then
      Or_error.error_string
        "trimming there would part a tool call from its result: pick an \
         earlier or later message"
    else (
      let entry = append t (Trim { before }) in
      Ok
        { Trim.before
        ; messages = List.length dropped
        ; tokens = estimate dropped
        ; at = entry.at
        })
;;

let find_hidden t ~entry ~what =
  List.find (hidden t) ~f:(fun h -> String.equal h.entry entry)
  |> Result.of_option
       ~error:
         (Error.of_string
            (sprintf "%s is not in effect: it was undone, or rewound past" what))
;;

let append_trim_after t ~after =
  let context = context t in
  match Message_point.trim_after_error context ~after, List.last context with
  | Some error, _ -> Or_error.error_string error
  | None, (None | Some (None, _)) ->
    Or_error.error_string "nothing to trim after that message"
  | None, Some (Some through, _) ->
    let entry = append t (Trim_after { after; through }) in
    find_hidden t ~entry:entry.id ~what:"the trim"
;;

let append_delete t ~message =
  match
    List.find (context t) ~f:(fun (id, _) ->
      Option.equal String.equal id (Some message))
  with
  | None ->
    Or_error.errorf
      "message %s is not in the model's context: delete one of the messages \
       that /delete lists"
      message
  | Some (_, { message = Tool_result _; _ }) ->
    Or_error.error_string
      "a tool result goes with its call: delete the assistant message that \
       made the call (/delete lists it)"
  | Some (_, { message = User _ | Assistant _; _ }) ->
    let entry = append t (Delete { message }) in
    find_hidden t ~entry:entry.id ~what:"the deletion"
;;

(* Cancels the latest range of [kind] in effect, or the one [entry] made. *)
let append_cancel ?entry t ~is_kind ~none ~what =
  let ranges =
    List.filter (hidden t) ~f:(fun (h : Hidden.t) -> is_kind h.kind)
  in
  match entry, List.last ranges with
  | None, None -> Or_error.error_string none
  | None, Some latest ->
    let (_ : Entry.t) =
      if Hidden.Kind.is_trim latest.kind
      then append t Untrim
      else append t (Restore { entry = latest.entry })
    in
    Ok latest
  | Some entry, _ ->
    (match List.find ranges ~f:(fun h -> String.equal h.entry entry) with
     | None ->
       Or_error.errorf
         "%s %s is not in effect: it was undone, or rewound past"
         what
         entry
     | Some h ->
       ignore (append t (Restore { entry }) : Entry.t);
       Ok h)
;;

let append_untrim ?entry t =
  append_cancel
    ?entry
    t
    ~is_kind:Hidden.Kind.is_trim
    ~none:"nothing is trimmed: /trim picks a point"
    ~what:"the trim"
;;

let append_undelete ?entry t =
  append_cancel
    ?entry
    t
    ~is_kind:(fun k -> not (Hidden.Kind.is_trim k))
    ~none:"nothing is deleted: /delete picks a message to delete"
    ~what:"the deletion"
;;

let context_tokens t = force (derived t).context_tokens
let timed_messages t = List.map (context t) ~f:snd
let context_ids t = List.map (context t) ~f:fst

let context_cwds t =
  let cwds = String.Table.create () in
  let (_ : string) =
    List.fold (entries t) ~init:t.initial_cwd ~f:(fun cwd (e : Entry.t) ->
      match e.payload with
      | Cwd { cwd; host = _ } -> cwd
      | Message _ ->
        Hashtbl.set cwds ~key:e.id ~data:cwd;
        cwd
      | Model _
      | Compaction _
      | Masked _
      | Name _
      | Description _
      | System_prompt _
      | Goal _
      | Reminders _
      | Budget _
      | Approval _
      | Allow_rule _
      | Allowed_call _
      | Forget_allowed_call _
      | Archived _
      | Pinned _
      | Spend _
      | Resume_at _
      | Told_environment _
      | Repo _
      | Agent_worktree _
      | Worktree _
      | Worktree_removed _
      | Home _
      | Transcript_filter _
      | Engine _
      | Thread _
      | External_turn _
      | Handoff _
      | Trim _
      | Untrim
      | Trim_after _
      | Delete _
      | Restore _
      | Review _ -> cwd)
  in
  List.map (context_ids t) ~f:(fun id ->
    Option.value (Option.bind id ~f:(Hashtbl.find cwds)) ~default:t.cwd)
;;

let identified_messages = context
let messages t = force (derived t).messages

let messages_since_forget_all t =
  let d = derived t in
  let after =
    Option.map d.state.forgot_all ~f:(fun id -> Hashtbl.find d.position id)
  in
  List.filter_map (force d.context) ~f:(fun (id, (m : Timed_message.t)) ->
    match after with
    | None -> Some m.message
    | Some after ->
      (match Option.bind id ~f:(Hashtbl.find d.position), after with
       | Some p, Some after when p > after -> Some m.message
       | (Some _ | None), _ -> None))
;;

let system_prompt t = (derived t).state.system_prompt
let goal t = (derived t).state.goal
let reminders t = (derived t).state.reminders

let reminder_names t =
  List.concat_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Reminders { reminders } ->
      List.map reminders ~f:(fun (r : Reminder.t) -> r.name)
    | _ -> [])
;;

let budget t = (derived t).state.budget
let model t = (derived t).state.model

let last_model t ~f =
  List.find_map t.entries ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Model { model; thinking } when f model -> Some (model, thinking)
    | _ -> None)
;;

module Thread = struct
  type t =
    { adapter : string
    ; host : string
    ; thread_id : string
    ; cwd : string
    ; fork_from : Fork_point.t option
    }
  [@@deriving sexp_of, equal]
end

module External_turn = struct
  type t =
    { thread_id : string
    ; turn_id : string
    ; through : string
    ; first_turn_id : string option
    ; mark : string option
    ; before : string option
    ; after : string option
    ; cost_total : float option
    }
  [@@deriving sexp_of, equal]
end

let engine t =
  match (derived t).state.engine with
  | None -> Ok Engine_choice.harness
  | Some (engine, adapter) -> Engine_choice.of_strings ~engine ~adapter
;;

let set_engine t (choice : Engine_choice.t) =
  append
    t
    (Engine
       { engine = Engine_id.to_string choice.engine
       ; adapter = Option.map choice.adapter ~f:Adapter_id.to_string
       })
;;

let threads t =
  List.rev_filter_map (active_path t) ~f:(fun (e : Entry.t) ->
    match e.payload with
    | Thread { adapter; host; thread_id; cwd; fork_from } ->
      Some { Thread.adapter; host; thread_id; cwd; fork_from }
    | _ -> None)
;;

let record_thread t ({ adapter; host; thread_id; cwd; fork_from } : Thread.t) =
  append t (Thread { adapter; host; thread_id; cwd; fork_from })
;;

let external_turns ?(all = false) t =
  List.filter_map
    (if all then entries t else active_path t)
    ~f:(fun (e : Entry.t) ->
      match e.payload with
      | External_turn
          { thread_id
          ; turn_id
          ; through
          ; first_turn_id
          ; mark
          ; before
          ; after
          ; cost_total
          } ->
        Some
          { External_turn.thread_id
          ; turn_id
          ; through
          ; first_turn_id
          ; mark
          ; before
          ; after
          ; cost_total
          }
      | _ -> None)
;;

let record_external_turn
      t
      ({ thread_id
       ; turn_id
       ; through
       ; first_turn_id
       ; mark
       ; before
       ; after
       ; cost_total
       } :
        External_turn.t)
  =
  append
    t
    (External_turn
       { thread_id
       ; turn_id
       ; through
       ; first_turn_id
       ; mark
       ; before
       ; after
       ; cost_total
       })
;;

let record_handoff t ~from_adapter ~to_adapter ~text =
  append t (Handoff { from_adapter; to_adapter; text })
;;

let in_context t id =
  List.exists (context t) ~f:(fun (i, _) ->
    Option.equal String.equal i (Some id))
;;

(* [kind] recorded again if its messages are in the context; a trim after a
   message reaches no further than its [through], or the path. *)
let record_again t (kind : Hidden.Kind.t) =
  let context = context t in
  match kind with
  | Trim_before { before } ->
    if
      List.exists (List.drop context 1) ~f:(fun (id, _) ->
        Option.equal String.equal id (Some before))
    then ignore (append t (Trim { before }) : Entry.t)
  | Deleted { message } ->
    if in_context t message
    then ignore (append t (Delete { message }) : Entry.t)
  | Trim_after { after; through } ->
    let position = String.Table.create () in
    List.iteri (active_path t) ~f:(fun i (e : Entry.t) ->
      Hashtbl.set position ~key:e.id ~data:i);
    let limit =
      Option.value (Hashtbl.find position through) ~default:Int.max_value
    in
    let in_range item =
      match item_position position item, Hashtbl.find position after with
      | Some p, Some a -> a < p && p <= limit
      | _ -> false
    in
    let after_it =
      List.drop_while context ~f:(fun (id, _) ->
        not (Option.equal String.equal id (Some after)))
      |> List.tl
      |> Option.value ~default:[]
    in
    (match List.last (List.filter context ~f:in_range) with
     | Some (Some through, _) when in_context t after ->
       let dropped, kept = List.partition_tf context ~f:in_range in
       if
         (not (List.exists after_it ~f:(fun (id, _) -> Option.is_none id)))
         && (not (Message_point.splits_a_call ~dropped:kept ~kept:dropped))
         && not (Message_point.splits_a_call ~dropped ~kept)
       then ignore (append t (Trim_after { after; through }) : Entry.t)
     | Some _ | None -> ())
;;

(* The ranges in effect stay while their messages do: when the path now
   (after a rewind, or in a copy) differs from the one [kinds] were in effect
   on (oldest first), those on it that were not are cancelled, and the others
   are recorded again. *)
let keep_hidden t (kinds : Hidden.Kind.t list) =
  List.iter (hidden t) ~f:(fun h ->
    if not (List.mem kinds h.kind ~equal:Hidden.Kind.equal)
    then ignore (append t (Restore { entry = h.entry }) : Entry.t));
  List.iter kinds ~f:(fun kind ->
    if not (List.exists (hidden t) ~f:(fun h -> Hidden.Kind.equal h.kind kind))
    then record_again t kind)
;;

let hidden_kinds t = List.map (hidden t) ~f:(fun (h : Hidden.t) -> h.kind)

let rewind t ~to_ =
  if Hashtbl.mem t.by_id to_
  then (
    let kinds = hidden_kinds t in
    set_head t (Some to_);
    write_line t (Head to_);
    keep_hidden t kinds;
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
      (* A snapshot's text is kept with the first snapshot of its content,
         which may be on another branch. *)
      let texts = String.Hash_set.create () in
      let review = lazy (review t) in
      let latest_thread = ref None in
      let last_turns = String.Table.create () in
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
                   ; pinned =
                       List.map pinned ~f:(fun pin ->
                         { pin with id = renamed pin.id })
                   })
            | System_prompt { text } ->
              (* It names the session it is in (to link it). *)
              Some
                (System_prompt
                   { text =
                       String.substr_replace_all
                         text
                         ~pattern:(Session_link.href t.id)
                         ~with_:(Session_link.href forked.id)
                   })
            | Trim { before } -> Some (Trim { before = renamed before })
            | Trim_after { after; through } ->
              Some
                (Trim_after { after = renamed after; through = renamed through })
            | Delete { message } -> Some (Delete { message = renamed message })
            | Restore { entry } -> Some (Restore { entry = renamed entry })
            | Masked { through } -> Some (Masked { through = renamed through })
            | Review
                (Snapshot
                   { snapshot = { content = Text { md5 }; _ } as s; text }) ->
              let text =
                match text with
                | Some _ -> text
                | None when Hash_set.mem texts md5 -> None
                | None -> Review_state.blob (force review) ~md5
              in
              if Option.is_some text then Hash_set.add texts md5;
              Some (Review (Snapshot { snapshot = s; text }))
            | Archived _
            | Pinned _
            | Spend _
            | Resume_at _
            | Agent_worktree _
            | Transcript_filter _ -> None
            | Thread { adapter; host; thread_id; cwd; fork_from = _ } ->
              latest_thread
              := Some { Thread.adapter; host; thread_id; cwd; fork_from = None };
              None
            | External_turn _ -> None
            | payload -> Some payload
          in
          Option.iter payload ~f:(fun payload ->
            let copied = append_at forked ~at:e.at payload in
            Hashtbl.set ids ~key:e.id ~data:copied.id);
          if not (String.equal e.id at) then copy rest
      in
      copy path;
      (* A run is recorded after its last entry: those whose entries were
         copied, wherever they are. *)
      List.iter path ~f:(fun (e : Entry.t) ->
        match e.payload with
        | External_turn { thread_id; turn_id; through; first_turn_id; mark; _ }
          when Hashtbl.mem ids through ->
          Hashtbl.set
            last_turns
            ~key:thread_id
            ~data:(turn_id, renamed through, first_turn_id, mark)
        | _ -> ());
      (* The fork continues a fork of the thread its agent was on, from its
         last turn in what was copied (agents that cannot fork one start
         afresh, handed the session). *)
      Option.iter !latest_thread ~f:(fun (thread : Thread.t) ->
        Option.iter
          (Hashtbl.find last_turns thread.thread_id)
          ~f:(fun (turn_id, through, first_turn_id, mark) ->
            ignore
              (append
                 forked
                 (Thread
                    { adapter = thread.adapter
                    ; host = thread.host
                    ; thread_id = thread.thread_id
                    ; cwd = thread.cwd
                    ; fork_from = Some { turn_id = Some turn_id; mark }
                    })
               : Entry.t);
            (* What it saw, for handoffs; its files' checkpoints stay the
               parent's. *)
            ignore
              (append
                 forked
                 (External_turn
                    { thread_id = thread.thread_id
                    ; turn_id
                    ; through
                    ; first_turn_id
                    ; mark
                    ; before = None
                    ; after = None
                    ; cost_total = None
                    })
               : Entry.t)));
      keep_hidden
        forked
        (List.map (hidden_kinds t) ~f:(fun (kind : Hidden.Kind.t) ->
           match kind with
           | Trim_before { before } ->
             Hidden.Kind.Trim_before { before = renamed before }
           | Trim_after { after; through } ->
             Trim_after { after = renamed after; through = renamed through }
           | Deleted { message } -> Deleted { message = renamed message }));
      (* The home follows the file, not the path: it may have been set on
         another branch. *)
      if not (Option.equal Home.equal forked.home t.home)
      then ignore (append forked (Home { home = t.home }) : Entry.t);
      Option.iter t.transcript_filter ~f:(fun filter ->
        ignore (set_transcript_filter forked (Some filter) : Entry.t));
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
    ; failed : bool
    ; parent : string option
    ; archived : bool option
    ; pinned : bool
    ; last_activity : float
    ; resume_at : float option
    ; bytes : int
    ; repo : Git_location.t option option
    ; worktrees : Session_worktree.t list
    ; home : Home.t option
    }
  [@@deriving sexp]

  let seconds_per_day = 86400.

  let is_archived t ~now ~archive_after_days ~active =
    (not active)
    && (not t.pinned)
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
  List.filter
    ([ path; data_dir path ] @ Trace_file.paths ~session_path:path)
    ~f:(fun p -> Sys_unix.file_exists_exn p)
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

(* What changes when [disk_usage] does, short of a walk: the sizes and
   mtimes of the trace files and of the data directory and the directories
   in it (where files are added). *)
let usage_stamp path =
  let stamp p =
    match Core_unix.lstat p with
    | exception _ -> None
    | { st_size; st_mtime; st_kind; _ } ->
      Some
        (st_kind, sprintf "%s %Ld %.6f" (Filename.basename p) st_size st_mtime)
  in
  List.concat_map
    (data_dir path :: Trace_file.paths ~session_path:path)
    ~f:(fun p ->
      match stamp p with
      | None -> []
      | Some (S_DIR, s) ->
        s
        :: List.filter_map
             (try Sys_unix.ls_dir p with
              | _ -> [])
             ~f:(fun name ->
               match stamp (Filename.concat p name) with
               | Some (S_DIR, s) -> Some s
               | Some _ | None -> None)
      | Some (_, s) -> [ s ])
  |> String.concat ~sep:"\n"
;;

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
   unarchive also counts as activity, as does an unpin (so that unpinning an
   idle session does not archive it at once). *)
let archived_and_activity t ~mtime =
  let (last_message_at, marker), unpinned_at =
    List.fold
      (List.rev t.entries)
      ~init:((None, None), None)
      ~f:(fun ((last_message, marker), unpinned_at) (e : Entry.t) ->
        match e.payload with
        | Pinned { pinned = false } -> (last_message, marker), Some e.at
        | Message _ -> (Some e.at, None), unpinned_at
        | Archived { archived } ->
          (last_message, Some (archived, e.at)), unpinned_at
        | Model _
        | Compaction _
        | Masked _
        | Name _
        | Description _
        | Cwd _
        | System_prompt _
        | Goal _
        | Reminders _
        | Budget _
        | Approval _
        | Allow_rule _
        | Allowed_call _
        | Forget_allowed_call _
        | Pinned { pinned = true }
        | Spend _
        | Resume_at _
        | Told_environment _
        | Repo _
        | Agent_worktree _
        | Worktree _
        | Worktree_removed _
        | Home _
        | Transcript_filter _
        | Engine _
        | Thread _
        | External_turn _
        | Handoff _
        | Trim _
        | Untrim
        | Trim_after _
        | Delete _
        | Restore _
        | Review _ -> (last_message, marker), unpinned_at)
  in
  let activity = Option.value (Option.join last_message_at) ~default:mtime in
  let activity =
    Option.fold (Option.join unpinned_at) ~init:activity ~f:Float.max
  in
  let activity =
    match marker with
    | Some (false, Some at) -> Float.max activity at
    | Some _ | None -> activity
  in
  Option.map marker ~f:fst, activity
;;

let set_pinned t ~pinned =
  if pinned
  then (
    match fst (archived_and_activity t ~mtime:0.) with
    | Some true -> ignore (append t (Archived { archived = false }) : Entry.t)
    | Some false | None -> ());
  append t (Pinned { pinned })
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
  ; failed =
      List.find_map (List.rev messages) ~f:(function
        | Message.Assistant { stop_reason; _ } ->
          Some
            (match stop_reason with
             | Error _ -> true
             | End_turn | Tool_use | Length | Aborted -> false)
        | User _ | Tool_result _ -> None)
      |> Option.value ~default:false
  ; parent = t.parent
  ; archived
  ; pinned = t.pinned
  ; last_activity
  ; resume_at = resume_at t
  ; bytes = 0
  ; repo = repo t
  ; worktrees = worktrees t
  ; home = t.home
  }
;;

(* Summaries by path, valid while the file's size and mtime are unchanged,
   so listing does not parse every session again; kept on disk too
   ([Summary_file]), so that a new process need not either. *)
module Cache_entry = struct
  type t =
    { size : Int64.t
    ; mtime : float
    ; usage : string (** {!usage_stamp} when [summary]'s bytes were counted *)
    ; summary : Summary.t option (** [None]: the file could not be read *)
    }
  [@@deriving sexp]

  let fresh t ~size ~mtime =
    Int64.equal t.size size && Float.equal t.mtime mtime
  ;;
end

let summary_cache : Cache_entry.t String.Table.t = String.Table.create ()

(* Directories whose [Summary_file] was read into [summary_cache]. *)
let summary_files_read = String.Hash_set.create ()
let cache_dir ~dir = Filename.concat dir ".cache"

(* Writes [path] whole or not at all, so that a reader never sees part of
   it; failing quietly (e.g. a read-only directory): it is only a cache. *)
let write_atomically path ~data =
  let tmp = sprintf "%s.%d.tmp" path (Core_unix.getpid () |> Pid.to_int) in
  try
    Core_unix.mkdir_p (Filename.dirname path);
    Out_channel.write_all tmp ~data;
    Core_unix.rename ~src:tmp ~dst:path
  with
  | _ ->
    (try Core_unix.unlink tmp with
     | _ -> ())
;;

(* The summaries of a directory's sessions by file name, as last listed.
   Each is checked against its file's size and mtime when used, so the file
   can only make listing faster: one that cannot be read (torn, corrupt or
   from another version: the version is in its name too, so that two
   versions sharing a directory keep apart) is ignored and written anew. *)
module Summary_file = struct
  let version = 1

  type t =
    { version : int
    ; summaries : (string * Cache_entry.t) list
    }
  [@@deriving sexp]

  let path ~dir =
    Filename.concat (cache_dir ~dir) (sprintf "summaries-v%d" version)
  ;;

  let read ~dir =
    match t_of_sexp (Sexp.load_sexp (path ~dir)) with
    | { version = v; summaries } when v = version -> summaries
    | { version = _; summaries = _ } | (exception _) -> []
  ;;

  let write ~dir summaries =
    write_atomically
      (path ~dir)
      ~data:(Sexp.to_string (sexp_of_t { version; summaries }))
  ;;
end

let read_summary_file ~dir =
  if not (Hash_set.mem summary_files_read dir)
  then (
    Hash_set.add summary_files_read dir;
    List.iter
      (Summary_file.read ~dir)
      ~f:(fun (name, (entry : Cache_entry.t)) ->
        let path = Filename.concat dir name in
        (* The directory may have moved since. *)
        let entry =
          { entry with
            summary = Option.map entry.summary ~f:(fun s -> { s with path })
          }
        in
        if not (Hashtbl.mem summary_cache path)
        then Hashtbl.set summary_cache ~key:path ~data:entry))
;;

let write_summary_file ~dir =
  Hashtbl.fold summary_cache ~init:[] ~f:(fun ~key:path ~data acc ->
    if String.equal (Filename.dirname path) dir
    then (Filename.basename path, data) :: acc
    else acc)
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> Summary_file.write ~dir
;;

(* The file's summary, and whether the cache changed. Its disk usage is
   cached with it, counted again when {!usage_stamp} changes. *)
let cached_summary ~live ~load path =
  match Core_unix.stat path with
  | exception _ -> None
  | { st_size = size; st_mtime = mtime; _ } ->
    let live = live path in
    let usage = usage_stamp path in
    let summary, parsed =
      match Hashtbl.find summary_cache path with
      | Some entry when Cache_entry.fresh entry ~size ~mtime ->
        if String.equal entry.usage usage
        then entry.summary, false
        else (
          let summary =
            Option.map entry.summary ~f:(fun s ->
              { s with bytes = disk_usage path })
          in
          Hashtbl.set
            summary_cache
            ~key:path
            ~data:{ entry with usage; summary };
          summary, Option.is_none live)
      | Some _ | None ->
        let summary =
          (* A live session's file grows with every message: summing up
             what is in memory spares parsing it all again. *)
          (match live with
           | Some t -> Some t
           | None -> Result.ok (load path))
          |> Option.map ~f:(fun t ->
            { (summarise t ~path ~mtime) with bytes = disk_usage path })
        in
        Hashtbl.set
          summary_cache
          ~key:path
          ~data:{ size; mtime; usage; summary };
        summary, Option.is_none live
    in
    Some (parsed, Option.map summary ~f:(fun s -> mtime, s))
;;

let is_session_file name =
  String.is_suffix name ~suffix:".jsonl" && not (Trace_file.is_trace_file name)
;;

let list_with ~yield ~live ~load ~dir =
  match Sys_unix.is_directory dir with
  | `No | `Unknown -> []
  | `Yes ->
    read_summary_file ~dir;
    let paths =
      Sys_unix.ls_dir dir
      |> List.filter ~f:is_session_file
      |> List.sort ~compare:String.compare
      |> List.map ~f:(Filename.concat dir)
    in
    let present = String.Set.of_list paths in
    let before = Hashtbl.length summary_cache in
    Hashtbl.filter_keys_inplace summary_cache ~f:(fun path ->
      (not (String.equal (Filename.dirname path) dir)) || Set.mem present path);
    let removed = Hashtbl.length summary_cache < before in
    let summaries =
      List.filter_map paths ~f:(fun path ->
        let summary = cached_summary ~live ~load path in
        yield ();
        summary)
    in
    if removed || List.exists summaries ~f:fst then write_summary_file ~dir;
    List.filter_map summaries ~f:snd
    |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Float.compare b a)
    |> List.map ~f:snd
;;

let summary_with ~live ~load path =
  let dir = Filename.dirname path in
  read_summary_file ~dir;
  match cached_summary ~live ~load path with
  | None -> None
  | Some (parsed, summary) ->
    if parsed then write_summary_file ~dir;
    Option.map summary ~f:snd
;;

let list ~dir = list_with ~yield:ignore ~live:(fun _ -> None) ~load ~dir

let forget_cached_summaries () =
  Hashtbl.clear summary_cache;
  Hash_set.clear summary_files_read
;;

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
    |> List.filter ~f:is_session_file
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
    | Masked _
    | Name _
    | Description _
    | Cwd _
    | System_prompt _
    | Goal _
    | Reminders _
    | Budget _
    | Approval _
    | Allow_rule _
    | Allowed_call _
    | Forget_allowed_call _
    | Archived _
    | Pinned _
    | Spend _
    | Resume_at _
    | Told_environment _
    | Repo _
    | Agent_worktree _
    | Worktree _
    | Worktree_removed _
    | Home _
    | Transcript_filter _
    | Engine _
    | Thread _
    | External_turn _
    | Handoff _
    | Trim _
    | Untrim
    | Trim_after _
    | Delete _
    | Restore _
    | Review _ -> None
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

module For_testing = struct
  let ids path = List.map path ~f:(fun (e : Entry.t) -> e.id)

  let view
        ~path
        ~context
        ~messages
        ~messages_since_forget_all
        ~hidden
        ~trim
        ~context_tokens
        ~approval
        ~told_environment
        ~system_prompt
        ~goal
        ~reminders
        ~budget
        ~model
        ~allow_rules
        ~allowed_calls
        ~spend
        ~spend_for
        ~replies
        ~name
        ~description
        ~resume_at
    =
    [%message
      ""
        ~path:(ids path : string list)
        (context : (string option * Timed_message.t) list)
        (messages : Message.t list)
        (messages_since_forget_all : Message.t list)
        (hidden : Hidden.t list)
        (trim : Trim.t option)
        (context_tokens : int)
        (approval : Mode.t option)
        (told_environment : Told_environment.t option)
        (system_prompt : string option)
        (goal : Goal.t option)
        (reminders : Reminder.t list)
        (budget : float option option)
        (model : (string * Thinking.t) option)
        (allow_rules : string list)
        (allowed_calls : Allowed_call.t list)
        (spend : Usage.t * float)
        (spend_for : (string * (int * float)) list)
        (replies : (string * Usage.t) list)
        (name : string option)
        (description : string option)
        (resume_at : float option)]
  ;;

  let cached t =
    let d = derived t in
    view
      ~path:(force d.path)
      ~context:(force d.context)
      ~messages:(force d.messages)
      ~messages_since_forget_all:(messages_since_forget_all t)
      ~hidden:(force d.hidden)
      ~trim:(force d.trim)
      ~context_tokens:(force d.context_tokens)
      ~approval:(approval t)
      ~told_environment:(told_environment t)
      ~system_prompt:(system_prompt t)
      ~goal:(goal t)
      ~reminders:(reminders t)
      ~budget:(budget t)
      ~model:(model t)
      ~allow_rules:(allow_rules t)
      ~allowed_calls:(allowed_calls t)
      ~spend:(spend t)
      ~spend_for:
        (Hashtbl.to_alist t.totals.spend_for
         |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b))
      ~replies:(replies t)
      ~name:(name t)
      ~description:(description t)
      ~resume_at:(resume_at t)
  ;;

  (* Everything again from the entries, the path folded with the positions
     of all its entries known from the start. *)
  let fold_path t path =
    let position = String.Table.create () in
    List.iteri path ~f:(fun i (e : Entry.t) ->
      Hashtbl.set position ~key:e.id ~data:i);
    let state = derive_path ~find:(Hashtbl.find t.by_id) ~position path in
    { Folded.context = List.rev state.rev_context
    ; hidden = state.hidden
    ; position
    }
  ;;

  let last path ~f =
    List.fold path ~init:None ~f:(fun acc (e : Entry.t) ->
      Option.first_some (f e.payload) acc)
  ;;

  let masked_ids path =
    match
      last path ~f:(function
        | Masked { through } -> Some through
        | _ -> None)
    with
    | None -> String.Set.empty
    | Some through ->
      let rec go acc = function
        | [] -> String.Set.empty
        | (e : Entry.t) :: rest ->
          let acc = Set.add acc e.id in
          if String.equal e.id through then acc else go acc rest
      in
      go String.Set.empty path
  ;;

  let context_tokens t path ~context =
    let last_reply =
      List.findi (List.rev path) ~f:(fun _ (e : Entry.t) ->
        match e.payload with
        | Message (Assistant _) -> true
        | _ -> false)
    in
    match last_reply with
    | None -> 0
    | Some (from_end, reply) ->
      let input =
        match reply.payload with
        | Message (Assistant a) -> a.usage.input
        | _ -> 0
      in
      let since = List.drop path (List.length path - from_end) in
      if
        not
          (List.exists since ~f:(fun (e : Entry.t) ->
             match e.payload with
             | Trim _ | Untrim | Trim_after _ | Delete _ | Restore _ -> true
             | _ -> false))
      then input
      else (
        let seen =
          (fold_path t (List.take path (List.length path - from_end))).context
        in
        Int.max 0 (input + estimate context - estimate seen))
  ;;

  let uncached t =
    let path = walk t in
    let folded = fold_path t path in
    let masked = masked_ids path in
    let entries = entries t in
    let spends =
      List.filter_map entries ~f:(fun (e : Entry.t) ->
        match e.payload with
        | Spend { usage; cost_usd; purpose } -> Some (usage, cost_usd, purpose)
        | _ -> None)
    in
    let path_last f = last path ~f in
    view
      ~path
      ~context:folded.context
      ~messages:
        (List.map folded.context ~f:(fun (id, (m : Timed_message.t)) ->
           match id, m.message with
           | _, User _ -> m.message
           | Some id, (Assistant _ | Tool_result _) when Set.mem masked id ->
             Observation_mask.mask m.message
           | (Some _ | None), (Assistant _ | Tool_result _) -> m.message))
      ~messages_since_forget_all:
        (let since =
           List.fold path ~init:None ~f:(fun since (e : Entry.t) ->
             match e.payload with
             | Forget_allowed_call { call = None } -> Some String.Set.empty
             | _ -> Option.map since ~f:(fun ids -> Set.add ids e.id))
         in
         List.filter_map folded.context ~f:(fun (id, (m : Timed_message.t)) ->
           match since, id with
           | None, _ -> Some m.message
           | Some ids, Some id when Set.mem ids id -> Some m.message
           | Some _, (Some _ | None) -> None))
      ~hidden:(List.rev_map folded.hidden ~f:(hidden_of_range folded))
      ~trim:(trim_of_folded folded)
      ~context_tokens:(context_tokens t path ~context:folded.context)
      ~approval:
        (path_last (function
           | Approval { mode; plan; chat } ->
             Some { Mode.approval = mode; plan; chat }
           | _ -> None))
      ~told_environment:
        (path_last (function
           | Told_environment told -> Some told
           | _ -> None))
      ~system_prompt:
        (path_last (function
           | System_prompt { text } -> Some text
           | _ -> None))
      ~goal:
        (path_last (function
           | Goal { goal } -> Some goal
           | _ -> None)
         |> Option.join)
      ~reminders:
        (path_last (function
           | Reminders { reminders } -> Some reminders
           | _ -> None)
         |> Option.value ~default:[])
      ~budget:
        (path_last (function
           | Budget { usd } -> Some usd
           | _ -> None))
      ~model:
        (path_last (function
           | Model { model; thinking } -> Some (model, thinking)
           | _ -> None))
      ~allow_rules:
        (List.filter_map path ~f:(fun (e : Entry.t) ->
           match e.payload with
           | Allow_rule { rule } -> Some rule
           | _ -> None))
      ~allowed_calls:
        (List.fold path ~init:[] ~f:(fun acc (e : Entry.t) ->
           match e.payload with
           | Allowed_call call -> call :: acc
           | Forget_allowed_call { call = None } -> []
           | Forget_allowed_call { call = Some call } ->
             List.filter acc ~f:(fun (a : Allowed_call.t) ->
               not (Allowed_call.Call.equal a.call call))
           | _ -> acc)
         |> List.rev)
      ~spend:
        (List.fold
           spends
           ~init:(Usage.zero, 0.)
           ~f:(fun (u, c) (usage, cost, _) -> Usage.add u usage, c +. cost))
      ~spend_for:
        (List.filter_map spends ~f:(fun (_, _, purpose) -> purpose)
         |> List.dedup_and_sort ~compare:String.compare
         |> List.map ~f:(fun purpose ->
           ( purpose
           , List.fold spends ~init:(0, 0.) ~f:(fun (n, c) (_, cost, p) ->
               if Option.equal String.equal p (Some purpose)
               then n + 1, c +. cost
               else n, c) )))
      ~replies:
        (List.filter_map entries ~f:(fun (e : Entry.t) ->
           match e.payload with
           | Message (Assistant a) -> Some (a.model, a.usage)
           | _ -> None))
      ~name:
        (last entries ~f:(function
           | Name { name } -> Some name
           | _ -> None))
      ~description:
        (last entries ~f:(function
           | Description { text } -> Some text
           | _ -> None))
      ~resume_at:
        (last entries ~f:(function
           | Resume_at { at } -> Some at
           | _ -> None)
         |> Option.join)
  ;;
end
