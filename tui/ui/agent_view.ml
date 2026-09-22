open! Core
open! Import

module Status = struct
  type t =
    | Running
    | Done of
        { turns : int
        ; cost_usd : float
        }
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; call_id : string
  ; task : string
  ; model : string
  ; status : Status.t
  ; turns : int
  ; transcript : Transcript.t
  ; children : t list
  }
[@@deriving sexp_of]

let create ~call_id ~agent_id ~task ~model =
  { id = agent_id
  ; call_id
  ; task
  ; model
  ; status = Running
  ; turns = 0
  ; transcript = Transcript.empty
  ; children = []
  }
;;

let find agents id = List.find agents ~f:(fun a -> String.equal a.id id)

let replace agents id f =
  List.map agents ~f:(fun a -> if String.equal a.id id then f a else a)
;;

let start agents ~call_id ~agent_id ~task ~model =
  agents @ [ create ~call_id ~agent_id ~task ~model ]
;;

let status_of_result ~turns ~cost_usd (result : P.Event.Subagent_result.t)
  : Status.t
  =
  if result.is_error then Failed result.text else Done { turns; cost_usd }
;;

let finish agents ~agent_id ~turns ~cost_usd ~result =
  replace agents agent_id (fun a ->
    { a with status = status_of_result ~turns ~cost_usd result; turns })
;;

let is_descendant t id = String.is_prefix id ~prefix:(t.id ^ "/")

let rec apply (t : t) (event : P.Event.t) : t =
  match event with
  | P.Event.Subagent { agent_id; event = inner; _ } ->
    if is_descendant t agent_id
    then (
      match find t.children agent_id with
      | None -> t
      | Some child ->
        { t with
          children = replace t.children agent_id (fun _ -> apply child inner)
        ; transcript = Transcript.apply t.transcript event
        })
    else if String.equal agent_id t.id
    then apply t inner
    else t
  | P.Event.Subagent_start { call_id; agent_id; task; model; _ } ->
    if is_descendant t agent_id
    then (
      let child = create ~call_id ~agent_id ~task ~model in
      { t with
        children = t.children @ [ child ]
      ; transcript = Transcript.apply t.transcript event
      })
    else t
  | P.Event.Subagent_end { agent_id; turns; cost_usd; result; _ } ->
    if is_descendant t agent_id
    then (
      let status = status_of_result ~turns ~cost_usd result in
      { t with
        children =
          replace t.children agent_id (fun c -> { c with status; turns })
      ; transcript = Transcript.apply t.transcript event
      })
    else t
  | P.Event.Turn_start ->
    { t with
      turns = t.turns + 1
    ; transcript = Transcript.apply t.transcript event
    }
  | _ -> { t with transcript = Transcript.apply t.transcript event }
;;

(** Applies a top-level event; [Subagent_start]/[Subagent_end] create and finish
    the depth-1 agents, [Subagent] routes to the matching one. *)
let apply_all agents (event : P.Event.t) =
  match event with
  | P.Event.Subagent_start { call_id; agent_id; task; model; _ } ->
    start agents ~call_id ~agent_id ~task ~model
  | P.Event.Subagent { agent_id; event = inner; _ } ->
    (match find agents agent_id with
     | None -> agents
     | Some agent -> replace agents agent_id (fun _ -> apply agent inner))
  | P.Event.Subagent_end { agent_id; turns; cost_usd; result; _ } ->
    finish agents ~agent_id ~turns ~cost_usd ~result
  | _ -> agents
;;
