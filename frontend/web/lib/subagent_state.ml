open! Core
open! Import

let model ~models ~(current : Llm.t) id =
  let named = List.filter models ~f:(fun (m : Llm.t) -> String.equal m.id id) in
  match
    List.find named ~f:(fun (m : Llm.t) ->
      String.equal m.provider current.provider)
  with
  | Some m -> m
  | None ->
    (match named with
     | m :: _ -> m
     | [] when String.equal current.id id -> current
     | [] ->
       { current with
         id
       ; key = id
       ; name = id
       ; context_window = 0
       ; thinking_options = []
       ; thinking_default = None
       ; cost = { input = 0.; output = 0.; cache_read = 0. }
       })
;;

let usd_of (model : Llm.t) (usage : Usage.t) =
  let per_m tokens price = Float.of_int tokens *. price /. 1e6 in
  per_m (usage.input - usage.cache_read) model.cost.input
  +. per_m usage.cache_read model.cost.cache_read
  +. per_m usage.output model.cost.output
;;

let replies transcript =
  Option.value_map transcript ~default:[] ~f:(fun chat ->
    List.filter_map (Chat.entries chat) ~f:(function
      | Chat.Entry.Assistant { message; _ } -> Some message.usage
      | User _ | Shell _ | Compaction _ -> None))
;;

(* By its name, which is all a subagent's event says of its host. *)
let host (s : State.t) (a : Agents.Agent.t) =
  match a.host with
  | None -> s.active_host, s.active_host_name
  | Some name ->
    (match List.find s.hosts ~f:(fun h -> String.equal h.name name) with
     | Some h -> h.id, Some name
     | None -> name, Some name)
;;

let of_agent (s : State.t) ~models (a : Agents.Agent.t) ~transcript ~cost_usd =
  let model = model ~models ~current:s.model a.model in
  let cwd = Option.value a.cwd ~default:s.cwd in
  let git_branch =
    match a.worktree with
    | Some w -> Some w.branch
    | None when Option.is_none a.host && String.equal cwd s.cwd -> s.git_branch
    | None -> None
  in
  let active_host, active_host_name = host s a in
  let usages = replies transcript in
  let usage =
    List.fold usages ~init:Usage.zero ~f:(fun (t : Usage.t) u ->
      { input = t.input + u.input
      ; output = t.output + u.output
      ; cache_read = t.cache_read + u.cache_read
      })
  in
  { s with
    cwd
  ; home = None
  ; git_branch
  ; model
  ; thinking = Option.value a.thinking ~default:"default"
  ; running = Agents.Status.equal a.status Running
  ; message_count =
      Option.value_map transcript ~default:0 ~f:(fun c ->
        List.length (Chat.entries c))
  ; usage
  ; cost_usd =
      Option.value_or_thunk cost_usd ~default:(fun () ->
        List.sum (module Float) usages ~f:(usd_of model))
  ; context_tokens =
      (match List.last usages with
       | Some u -> u.input + u.output
       | None -> 0)
  ; active_host
  ; active_host_name
  ; subagents = []
  ; jobs = []
  ; budget_usd = None
  ; goal = None
  ; reminders = []
  ; resume_at = None
  ; plan_mode = false
  ; chat_mode = false
  ; no_host = None
  ; plan = None
  }
;;

module Fixed = struct
  type t =
    | Model
    | Host
    | Folder
    | Branch
  [@@deriving sexp_of, equal]

  let explain t ~title ~parent =
    let what =
      match t with
      | Model -> "model and reasoning"
      | Host -> "tool host"
      | Folder -> "folder"
      | Branch -> "worktree and branch"
    in
    sprintf
      "%s keeps the %s it started with (a subagent's cannot change): back to \
       %s (Esc) to change the session's."
      title
      what
      parent
  ;;
end
