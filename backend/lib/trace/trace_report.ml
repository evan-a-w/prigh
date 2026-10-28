open! Core
open! Import

module Category = struct
  (* By priority: an instant covered by several counts as the last. *)
  type t =
    | Other
    | Tools
    | Model
    | Compaction
    | Retries
    | Waiting
  [@@deriving compare, enumerate]

  let to_string = function
    | Other -> "other"
    | Tools -> "tools"
    | Model -> "model"
    | Compaction -> "compaction"
    | Retries -> "retries"
    | Waiting -> "waiting on user"
  ;;

  let index t = List.findi_exn all ~f:(fun _ c -> compare c t = 0) |> fst

  let of_span (span : Trace.Span.t) =
    match span.name with
    | "approval_wait" | "ask_user_wait" -> Some Waiting
    | "retry_wait" -> Some Retries
    | "model_request" ->
      (match span.status with
       | Error -> Some Retries
       | Ok | Cancelled -> Some Model)
    | "compaction" | "mask" -> Some Compaction
    | "tool" -> Some Tools
    | _ -> None
  ;;
end

let duration_hum seconds =
  if Float.(seconds < 1.)
  then sprintf "%dms" (Float.iround_nearest_exn (seconds *. 1000.))
  else if Float.(seconds < 60.)
  then sprintf "%.1fs" seconds
  else (
    let total = Float.iround_nearest_exn seconds in
    if total < 3600
    then sprintf "%dm%02ds" (total / 60) (total % 60)
    else sprintf "%dh%02dm" (total / 3600) (total % 3600 / 60))
;;

let string_attr span name =
  match Trace.Span.attr span name with
  | Some v -> Trace.Value.to_string_hum v
  | None -> ""
;;

let label (span : Trace.Span.t) =
  let with_attr name =
    match string_attr span name with
    | "" -> span.name
    | v -> span.name ^ " " ^ v
  in
  match span.name with
  | "turn" -> with_attr "turn"
  | "model_request" -> with_attr "model"
  | "subagent" -> with_attr "agent_id"
  | "job" -> with_attr "job_id"
  | "tool" ->
    (match string_attr span "tool", string_attr span "summary" with
     | name, "" -> "tool " ^ name
     | name, summary -> sprintf "tool %s (%s)" name summary)
  | _ -> span.name
;;

module Tree = struct
  type t =
    { spans : Trace.Span.t list (** by start *)
    ; by_id : Trace.Span.t String.Table.t
    ; children : Trace.Span.t list String.Table.t
    ; roots : Trace.Span.t list
    }

  let create spans =
    let spans =
      List.stable_sort spans ~compare:(fun (a : Trace.Span.t) b ->
        Float.compare a.start b.start)
    in
    let by_id = String.Table.create () in
    List.iter spans ~f:(fun (s : Trace.Span.t) ->
      Hashtbl.set by_id ~key:s.id ~data:s);
    let children = String.Table.create () in
    let roots =
      List.filter spans ~f:(fun (s : Trace.Span.t) ->
        match Option.filter s.parent ~f:(Hashtbl.mem by_id) with
        | Some parent ->
          Hashtbl.add_multi children ~key:parent ~data:s;
          false
        | None -> true)
    in
    Hashtbl.map_inplace children ~f:List.rev;
    { spans; by_id; children; roots }
  ;;

  let children t (span : Trace.Span.t) =
    Hashtbl.find t.children span.id |> Option.value ~default:[]
  ;;

  let parent t (span : Trace.Span.t) = Option.bind span.parent ~f:(Hashtbl.find t.by_id)

  let rec ancestors t span =
    match parent t span with
    | None -> []
    | Some p -> p :: ancestors t p
  ;;

  let rec descendants t span =
    List.concat_map (children t span) ~f:(fun c -> c :: descendants t c)
  ;;
end

let attrs_hum ?(attrs = Fn.const true) (span : Trace.Span.t) =
  List.filter_map span.attrs ~f:(fun (k, v) ->
    if attrs k then Some (sprintf "%s=%s" k (Trace.Value.to_string_hum v)) else None)
  |> String.concat ~sep:" "
;;

let outline ?attrs spans =
  let tree = Tree.create spans in
  let buf = Buffer.create 1024 in
  let rec go depth (span : Trace.Span.t) =
    let status =
      match span.status with
      | Ok -> ""
      | status -> sprintf " [%s]" (Trace.Status.to_string status)
    in
    let attrs =
      match attrs_hum ?attrs span with
      | "" -> ""
      | a -> "  " ^ a
    in
    Buffer.add_string
      buf
      (sprintf
         "%s%s %s%s%s\n"
         (String.make (2 * depth) ' ')
         (label span)
         (duration_hum (Trace.Span.duration span))
         status
         attrs);
    List.iter (Tree.children tree span) ~f:(go (depth + 1))
  in
  List.iter tree.roots ~f:(go 0);
  Buffer.contents buf
;;

(* Rows of cells: the first column left-aligned, the others right. *)
let table rows =
  let widths =
    List.fold rows ~init:[] ~f:(fun widths row ->
      List.mapi row ~f:(fun i cell ->
        Int.max (String.length cell) (Option.value (List.nth widths i) ~default:0)))
  in
  List.map rows ~f:(fun row ->
    List.mapi row ~f:(fun i cell ->
      let pad = String.make (List.nth_exn widths i - String.length cell) ' ' in
      if i = 0 then cell ^ pad else pad ^ cell)
    |> String.concat ~sep:"  "
    |> String.rstrip
    |> sprintf "  %s")
;;

(* How [root]'s time on its track splits into categories. *)
let breakdown tree (root : Trace.Span.t) =
  let totals = Array.create ~len:(List.length Category.all) 0. in
  let clip x = Float.min root.stop (Float.max root.start x) in
  let events =
    List.filter_map
      (root :: Tree.descendants tree root)
      ~f:(fun (s : Trace.Span.t) ->
        if String.equal s.track root.track
        then (
          let category =
            if phys_equal s root then Some Category.Other else Category.of_span s
          in
          Option.map category ~f:(fun c -> clip s.start, clip s.stop, c))
        else None)
    |> List.concat_map ~f:(fun (start, stop, c) ->
      [ start, 1, Category.index c; stop, -1, Category.index c ])
    |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> Float.compare a b)
  in
  let counts = Array.create ~len:(List.length Category.all) 0 in
  let top () =
    let rec go i = if i < 0 then None else if counts.(i) > 0 then Some i else go (i - 1) in
    go (Array.length counts - 1)
  in
  ignore
    (List.fold events ~init:root.start ~f:(fun last (at, delta, c) ->
       Option.iter (top ()) ~f:(fun i -> totals.(i) <- totals.(i) +. (at -. last));
       counts.(c) <- counts.(c) + delta;
       at)
     : float);
  totals
;;

(* Nearest rank. *)
let percentile sorted p =
  let n = Array.length sorted in
  sorted.(Int.max 0 (Int.of_float (Float.round_up (p *. Float.of_int n)) - 1))
;;

let summary ?(slowest = 10) ?(bad = 0) spans =
  let tree = Tree.create spans in
  let named name = List.filter tree.spans ~f:(fun s -> String.equal s.name name) in
  let lines = Queue.create () in
  let add s = Queue.enqueue lines s in
  let section title rows =
    if not (List.is_empty rows)
    then (
      add "";
      add title;
      List.iter (table rows) ~f:add)
  in
  if List.is_empty spans
  then
    add
      "No trace yet: spans are recorded as prompts run, unless tracing is off \
       (PRIGH_TRACE=off, or trace \"off\" in the config)."
  else (
    let count name = List.length (named name) in
    add
      (sprintf
         "%d prompts, %d turns, %d model requests, %d tool calls (%d spans)"
         (count "prompt")
         (count "turn")
         (count "model_request")
         (count "tool")
         (List.length spans));
    let totals = Array.create ~len:(List.length Category.all) 0. in
    List.iter tree.roots ~f:(fun root ->
      Array.iteri (breakdown tree root) ~f:(fun i x -> totals.(i) <- totals.(i) +. x));
    let wall = Array.sum (module Float) totals ~f:Fn.id in
    add "";
    add (sprintf "Wall time %s (the main agent, prompt to idle):" (duration_hum wall));
    List.iter
      (table
         (List.filter_map
            (List.rev Category.all)
            ~f:(fun c ->
              let x = totals.(Category.index c) in
              if Float.(x <= 0.) && not ([%compare.equal: Category.t] c Other)
              then None
              else
                Some
                  [ Category.to_string c
                  ; duration_hum x
                  ; (if Float.(wall > 0.)
                     then sprintf "%.0f%%" (100. *. x /. wall)
                     else "-")
                  ])))
      ~f:add;
    let ttft =
      List.filter_map (named "model_request") ~f:(fun request ->
        match request.status with
        | Error | Cancelled -> None
        | Ok ->
          List.find (Tree.children tree request) ~f:(fun c ->
            String.equal c.name "first_token")
          |> Option.map ~f:(fun (first : Trace.Span.t) ->
            string_attr request "model", first.stop -. request.start))
      |> List.Assoc.sort_and_group ~compare:String.compare
    in
    section
      "Time to first token:"
      (if List.is_empty ttft
       then []
       else
         [ "model"; "requests"; "median"; "p90" ]
         :: List.map ttft ~f:(fun (model, times) ->
           let sorted = Array.of_list (List.sort times ~compare:Float.compare) in
           [ model
           ; Int.to_string (Array.length sorted)
           ; duration_hum (percentile sorted 0.5)
           ; duration_hum (percentile sorted 0.9)
           ]));
    let tools =
      List.map (named "tool") ~f:(fun s -> string_attr s "tool", s)
      |> List.Assoc.sort_and_group ~compare:String.compare
      |> List.map ~f:(fun (name, calls) ->
        let total = List.sum (module Float) calls ~f:Trace.Span.duration in
        let max =
          List.fold calls ~init:0. ~f:(fun m s -> Float.max m (Trace.Span.duration s))
        in
        let errors =
          List.count calls ~f:(fun s ->
            match s.status with
            | Error -> true
            | Ok | Cancelled -> false)
        in
        name, List.length calls, total, max, errors)
      |> List.sort ~compare:(fun (_, _, a, _, _) (_, _, b, _, _) -> Float.compare b a)
    in
    section
      "Tools (by total time, waits for approval and answers included):"
      (if List.is_empty tools
       then []
       else
         [ "tool"; "calls"; "total"; "max"; "errors" ]
         :: List.map tools ~f:(fun (name, calls, total, max, errors) ->
           [ name
           ; Int.to_string calls
           ; duration_hum total
           ; duration_hum max
           ; Int.to_string errors
           ]));
    let background_run (s : Trace.Span.t) =
      Option.equal Trace.Value.equal (Trace.Span.attr s "background") (Some (Bool true))
    in
    let background =
      List.filter_map
        [ ( "background subagent runs"
          , fun (s : Trace.Span.t) -> String.equal s.name "subagent" && background_run s )
        ; ( "sync subagent runs"
          , fun s -> String.equal s.name "subagent" && not (background_run s) )
        ; "background jobs", fun s -> String.equal s.name "job"
        ]
        ~f:(fun (what, f) ->
          match List.filter tree.spans ~f with
          | [] -> None
          | runs ->
            Some
              [ what
              ; Int.to_string (List.length runs)
              ; duration_hum (List.sum (module Float) runs ~f:Trace.Span.duration)
              ])
    in
    section "Subagents and jobs (alongside the main agent):" background;
    let candidates =
      List.filter tree.spans ~f:(fun s ->
        not
          (List.mem
             [ "prompt"; "turn"; "connect"; "first_token"; "thinking"; "stream" ]
             s.name
             ~equal:String.equal))
      |> List.sort ~compare:(fun a b ->
        Float.compare (Trace.Span.duration b) (Trace.Span.duration a))
      |> Fn.flip List.take slowest
    in
    let rows =
      List.map candidates ~f:(fun s ->
        let path =
          List.rev_map (Tree.ancestors tree s) ~f:label |> String.concat ~sep:" > "
        in
        let status =
          match s.status with
          | Ok -> ""
          | status -> sprintf " [%s]" (Trace.Status.to_string status)
        in
        ( duration_hum (Trace.Span.duration s)
        , label s ^ status ^ if String.is_empty path then "" else "  in " ^ path ))
    in
    let width =
      List.fold rows ~init:0 ~f:(fun w (d, _) -> Int.max w (String.length d))
    in
    if not (List.is_empty rows)
    then (
      add "";
      add "Slowest spans:";
      List.iter rows ~f:(fun (d, what) ->
        add (sprintf "  %*s  %s" width d what))));
  if bad > 0
  then (
    add "";
    add (sprintf "(%d unreadable lines skipped)" bad));
  String.concat ~sep:"\n" (Queue.to_list lines) ^ "\n"
;;

module Lane = struct
  type t =
    { tid : int
    ; mutable open_ : (string * int) list (** id and end, innermost first *)
    }
end

let perfetto spans =
  let tree = Tree.create spans in
  let base =
    List.fold spans ~init:Float.infinity ~f:(fun m (s : Trace.Span.t) ->
      Float.min m s.wall)
  in
  let us x = Float.iround_nearest_exn (x *. 1e6) in
  let timed =
    List.map tree.spans ~f:(fun (s : Trace.Span.t) ->
      let ts = us (s.wall -. base) in
      let end_ = us (s.wall -. base +. Trace.Span.duration s) in
      s, ts, end_, List.length (Tree.ancestors tree s))
    |> List.stable_sort ~compare:(fun (_, ts1, end1, d1) (_, ts2, end2, d2) ->
      match Int.compare ts1 ts2 with
      | 0 ->
        (match Int.compare d1 d2 with
         | 0 -> Int.compare end2 end1
         | c -> c)
      | c -> c)
  in
  let tracks = String.Table.create () in
  let track_order = Queue.create () in
  let next_tid = ref 1 in
  let lane_of = String.Table.create () in
  let threads = Queue.create () in
  let events =
    List.map timed ~f:(fun ((s : Trace.Span.t), ts, end_, _) ->
      let lanes =
        Hashtbl.find_or_add tracks s.track ~default:(fun () ->
          Queue.enqueue track_order s.track;
          ref [])
      in
      let ancestors = Tree.ancestors tree s |> List.map ~f:(fun (a : Trace.Span.t) -> a.id) in
      let fits (lane : Lane.t) =
        lane.open_ <- List.drop_while lane.open_ ~f:(fun (_, e) -> e <= ts);
        match lane.open_ with
        | [] -> true
        | (id, e) :: _ -> List.mem ancestors id ~equal:String.equal && end_ <= e
      in
      let preferred =
        Option.bind s.parent ~f:(Hashtbl.find lane_of)
        |> Option.filter ~f:(fun (l : Lane.t) ->
          List.exists !lanes ~f:(fun (x : Lane.t) -> x.tid = l.tid))
      in
      let lane =
        match Option.filter preferred ~f:fits with
        | Some lane -> lane
        | None ->
          (match List.find !lanes ~f:fits with
           | Some lane -> lane
           | None ->
             let lane = { Lane.tid = !next_tid; open_ = [] } in
             incr next_tid;
             let n = List.length !lanes in
             Queue.enqueue
               threads
               ( lane.tid
               , if n = 0 then s.track else sprintf "%s (%d)" s.track (n + 1) );
             lanes := !lanes @ [ lane ];
             lane)
      in
      lane.open_ <- (s.id, end_) :: lane.open_;
      Hashtbl.set lane_of ~key:s.id ~data:lane;
      `Object
        [ "name", `String (label s)
        ; "cat", `String s.name
        ; "ph", `String "X"
        ; "ts", `Number (Int.to_string ts)
        ; "dur", `Number (Int.to_string (end_ - ts))
        ; "pid", `Number "1"
        ; "tid", `Number (Int.to_string lane.tid)
        ; ( "args"
          , `Object
              (("status", `String (Trace.Status.to_string s.status))
               :: List.map s.attrs ~f:(fun (k, v) -> k, Trace.Value.to_json v)) )
        ])
  in
  let metadata =
    `Object
      [ "name", `String "process_name"
      ; "ph", `String "M"
      ; "pid", `Number "1"
      ; "args", `Object [ "name", `String "prigh" ]
      ]
    :: List.concat_map (Queue.to_list threads) ~f:(fun (tid, name) ->
      let meta name args =
        `Object
          [ "name", `String name
          ; "ph", `String "M"
          ; "pid", `Number "1"
          ; "tid", `Number (Int.to_string tid)
          ; "args", `Object args
          ]
      in
      [ meta "thread_name" [ "name", `String name ]
      ; meta "thread_sort_index" [ "sort_index", `Number (Int.to_string tid) ]
      ])
  in
  `Object
    [ "displayTimeUnit", `String "ms"; "traceEvents", `Array (metadata @ events) ]
;;

module Report = struct
  type t =
    { summary : string
    ; level : Trace.Level.t
    ; spans : Trace.Span.t list
    }
end
