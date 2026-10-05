open! Core
open! Import

module Status = struct
  type t =
    | Running
    | Complete
    | Failed
  [@@deriving sexp_of, equal]

  let to_string = function
    | Running -> "running"
    | Complete -> "complete"
    | Failed -> "failed"
  ;;

  let of_result = function
    | None -> Running
    | Some { Event.Subagent_result.is_error = true; _ } -> Failed
    | Some _ -> Complete
  ;;
end

(* Floats: milliseconds since the epoch overflow JavaScript's 32-bit ints. *)
let of_ms ms = Time_ns.of_span_since_epoch (Time_ns.Span.of_ms ms)

module Agent = struct
  type t =
    { id : string
    ; call_id : string
    ; parent : string option
    ; task : string
    ; model : string
    ; status : Status.t
    ; started_at : Time_ns.t
    ; ended_at : Time_ns.t option
    ; turns : int
    ; tool_calls : int
    ; current_tool : (string * Time_ns.t) option
    ; result : Event.Subagent_result.t option
    }
  [@@deriving sexp_of]

  let of_json json =
    let open Or_error.Let_syntax in
    let ms_opt name =
      match Json.field json name with
      | None | Some `Null -> Ok None
      | Some _ -> Json.float_field json name >>| fun ms -> Some (of_ms ms)
    in
    let%bind id = Json.string_field json "id" in
    let%bind call_id = Json.string_field json "call_id" in
    let%bind parent = Json.string_opt_field json "parent" in
    let%bind task = Json.string_field json "task" in
    let%bind model = Json.string_field json "model" in
    let%bind started_at = Json.float_field json "started_at_ms" >>| of_ms in
    let%bind ended_at = ms_opt "ended_at_ms" in
    let%bind turns = Json.int_field json "turns" in
    let%bind tool_calls = Json.int_field json "tool_calls" in
    let%bind current_tool = Json.string_opt_field json "current_tool" in
    let%bind tool_started = ms_opt "current_tool_started_at_ms" in
    let%bind stale = Json.bool_field json "stale" in
    let%map result =
      match Json.field json "result" with
      | None | Some `Null -> Ok None
      | Some r -> Event.Subagent_result.of_json r >>| Option.some
    in
    ( { id
      ; call_id
      ; parent
      ; task
      ; model
      ; status = Status.of_result result
      ; started_at
      ; ended_at
      ; turns
      ; tool_calls
      ; current_tool =
          Option.map current_tool ~f:(fun name ->
            name, Option.value tool_started ~default:started_at)
      ; result
      }
    , stale )
  ;;

  let running t = Status.equal t.status Running
end

module Job = struct
  type t =
    { info : Job_info.t
    ; seen_at : Time_ns.t
    }
  [@@deriving sexp_of]
end

module Item = struct
  module T = struct
    type t =
      | Agent of string
      | Job of string
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparable.Make_plain (T)
end

type t =
  { open_ : bool
  ; selected : Item.t option
  ; agents : Agent.t list
  ; jobs : Job.t list
  ; earlier : Item.Set.t
  ; stopping : Item.Set.t
  ; output : (string * string) option
  ; polled_at : Time_ns.t option
  }
[@@deriving sexp_of]

let empty =
  { open_ = false
  ; selected = None
  ; agents = []
  ; jobs = []
  ; earlier = Item.Set.empty
  ; stopping = Item.Set.empty
  ; output = None
  ; polled_at = None
  }
;;

let find_agent t id = List.find t.agents ~f:(fun a -> String.equal a.id id)
let find_job t id = List.find t.jobs ~f:(fun j -> String.equal j.info.id id)

let running t (item : Item.t) =
  match item with
  | Agent id -> Option.exists (find_agent t id) ~f:Agent.running
  | Job id -> Option.exists (find_job t id) ~f:(fun j -> j.info.running)
;;

let running_agents t = List.count t.agents ~f:Agent.running
let running_jobs t = List.count t.jobs ~f:(fun j -> j.info.running)

(* Whatever has stopped is no longer stopping. *)
let settle t =
  { t with stopping = Set.filter t.stopping ~f:(fun item -> running t item) }
;;

let update_agent t id ~f =
  { t with
    agents =
      List.map t.agents ~f:(fun a -> if String.equal a.id id then f a else a)
  }
;;

let rec record_in t ~now ~parent (event : Event.t) =
  match event with
  | Subagent_start { call_id; agent_id; task; model; tools = _ } ->
    if Option.is_some (find_agent t agent_id)
    then t
    else
      { t with
        agents =
          t.agents
          @ [ { Agent.id = agent_id
              ; call_id
              ; parent
              ; task
              ; model
              ; status = Running
              ; started_at = now
              ; ended_at = None
              ; turns = 0
              ; tool_calls = 0
              ; current_tool = None
              ; result = None
              }
            ]
      }
  | Subagent_end { agent_id; result; turns; _ } ->
    settle
      (update_agent t agent_id ~f:(fun a ->
         { a with
           status = Status.of_result (Some result)
         ; result = Some result
         ; ended_at = Some (Option.value a.ended_at ~default:now)
         ; current_tool = None
         ; turns
         }))
  | Subagent { agent_id; event = inner; call_id = _ } ->
    (match inner with
     | Subagent_start _ | Subagent_end _ | Subagent _ ->
       record_in t ~now ~parent:(Some agent_id) inner
     | Turn_start -> update_agent t agent_id ~f:(fun a -> { a with turns = a.turns + 1 })
     | Tool_start call ->
       update_agent t agent_id ~f:(fun a ->
         { a with
           tool_calls = a.tool_calls + 1
         ; current_tool = Some (call.name, now)
         })
     | Tool_end _ -> update_agent t agent_id ~f:(fun a -> { a with current_tool = None })
     | _ -> t)
  | _ -> t
;;

let record t ~now event = record_in t ~now ~parent:None event

let rec starts_or_ends (event : Event.t) =
  match event with
  | Subagent_start _ | Subagent_end _ -> true
  | Subagent { event; _ } -> starts_or_ends event
  | _ -> false
;;

let set_agents t agents =
  let earlier =
    List.fold agents ~init:t.earlier ~f:(fun earlier ((a : Agent.t), stale) ->
      if stale && Option.is_none (find_agent t a.id)
      then Set.add earlier (Agent a.id)
      else earlier)
  in
  settle { t with agents = List.map agents ~f:fst; earlier }
;;

let set_jobs t ~now jobs =
  let earlier =
    List.fold jobs ~init:t.earlier ~f:(fun earlier (j : Job_info.t) ->
      if j.delivered && Option.is_none (find_job t j.id)
      then Set.add earlier (Job j.id)
      else earlier)
  in
  settle
    { t with
      jobs = List.map jobs ~f:(fun info -> { Job.info; seen_at = now })
    ; earlier
    }
;;

let prompt_sent t =
  let finished =
    List.filter_map t.agents ~f:(fun a ->
      Option.some_if (not (Agent.running a)) (Item.Agent a.id))
    @ List.filter_map t.jobs ~f:(fun j ->
      Option.some_if (not j.info.running) (Item.Job j.info.id))
  in
  { t with earlier = Set.union t.earlier (Item.Set.of_list finished) }
;;

let children t parent =
  List.filter t.agents ~f:(fun a -> Option.equal String.equal a.parent parent)
;;

let lineage t id =
  let rec up id acc =
    match find_agent t id with
    | None -> acc
    | Some a ->
      (match a.parent with
       | Some parent -> up parent (a :: acc)
       | None -> a :: acc)
  in
  up id []
;;

(* Running first, each group in start order, children under their parent. *)
let tree t roots =
  let by_running (agents : Agent.t list) =
    let running, finished = List.partition_tf agents ~f:Agent.running in
    running @ finished
  in
  let rec walk depth (a : Agent.t) =
    (Item.Agent a.id, depth)
    :: List.concat_map (by_running (children t (Some a.id))) ~f:(walk (depth + 1))
  in
  List.concat_map (by_running roots) ~f:(walk 0)
;;

let is_root t (a : Agent.t) =
  match a.parent with
  | None -> true
  | Some parent -> Option.is_none (find_agent t parent)
;;

let jobs_where t ~f =
  let running, finished =
    List.filter t.jobs ~f:(fun j -> f (Item.Job j.info.id))
    |> List.partition_tf ~f:(fun j -> j.info.running)
  in
  List.map (running @ finished) ~f:(fun j -> Item.Job j.info.id, 0)
;;

let section t ~earlier =
  let in_section item = Bool.equal (Set.mem t.earlier item) earlier in
  tree
    t
    (List.filter t.agents ~f:(fun a -> is_root t a && in_section (Agent a.id)))
  @ jobs_where t ~f:in_section
;;

let listed t = section t ~earlier:false
let earlier t = section t ~earlier:true

let resolve t arg =
  let arg = String.strip arg in
  match Int.of_string_opt arg with
  | Some n -> Option.map (List.nth (listed t) (n - 1)) ~f:fst
  | None ->
    (match find_agent t arg, find_job t arg with
     | Some a, _ -> Some (Agent a.id)
     | None, Some j -> Some (Job j.info.id)
     | None, None ->
       List.find t.agents ~f:(fun a -> String.equal a.call_id arg)
       |> Option.map ~f:(fun a -> Item.Agent a.id))
;;

let cycle t delta =
  let items = List.map (listed t) ~f:fst in
  let n = List.length items in
  if n = 0
  then None
  else (
    let current =
      Option.bind t.selected ~f:(fun s ->
        List.findi items ~f:(fun _ i -> Item.equal i s) |> Option.map ~f:fst)
    in
    let next =
      match current with
      | None -> if delta > 0 then 0 else n - 1
      | Some i -> (i + delta) % n
    in
    List.nth items next)
;;

let span_between a b = Time_ns.Span.max Time_ns.Span.zero (Time_ns.diff b a)

let agent_elapsed (a : Agent.t) ~now =
  span_between a.started_at (Option.value a.ended_at ~default:now)
;;

let job_elapsed (j : Job.t) ~now =
  let at_reply = Time_ns.Span.of_sec j.info.elapsed in
  if j.info.running
  then Time_ns.Span.(at_reply + span_between j.seen_at now)
  else at_reply
;;

let format_span span =
  let s = Float.iround_down_exn (Time_ns.Span.to_sec span) in
  if s < 60
  then sprintf "%ds" s
  else if s < 3600
  then sprintf "%dm %02ds" (s / 60) (s % 60)
  else sprintf "%dh %02dm" (s / 3600) (s % 3600 / 60)
;;
