open! Core
open! Import

module Status = struct
  type t =
    | Idle
    | Running
    | Waiting
    | Failed
    | Finished
  [@@deriving sexp_of, equal]

  let label = function
    | Idle -> "Idle"
    | Running -> "Running"
    | Waiting -> "Waiting for you: a confirmation or a question"
    | Failed -> "Failed: its last reply ended in an error"
    | Finished -> "Done since you looked"
  ;;
end

module Out = struct
  type t =
    | Nothing
    | Leaving of string
    | Last of string
    | Blank of string
  [@@deriving sexp_of, equal]
end

type t =
  { enabled : bool
  ; ids : string list
  ; loaded : bool
  ; saved : string list
  ; known : String.Set.t option
  ; out : Out.t
  ; running : String.Set.t
  ; finished : String.Set.t
  ; dragging : string option
  }
[@@deriving sexp_of, equal]

let init =
  { enabled = false
  ; ids = []
  ; loaded = false
  ; saved = []
  ; known = None
  ; out = Nothing
  ; running = String.Set.empty
  ; finished = String.Set.empty
  ; dragging = None
  }
;;

let reset t = { init with enabled = t.enabled }
let set_enabled t enabled = { t with enabled }
let mem ids id = List.mem ids id ~equal:String.equal

let restore t ids =
  let ids =
    List.fold ids ~init:[] ~f:(fun seen id ->
      if mem seen id then seen else id :: seen)
    |> List.rev
    |> List.filter ~f:(fun id ->
      Option.for_all t.known ~f:(fun known -> Set.mem known id))
  in
  { t with ids; saved = ids }
;;

let load t = { t with loaded = true }

let to_save t =
  Option.some_if (t.loaded && not (List.equal String.equal t.ids t.saved)) t.ids
;;

let saved t = { t with saved = t.ids }

let follow t ~previous ~current =
  match current with
  | None -> t
  | Some id ->
    let changed = not (Option.equal String.equal previous current) in
    let out : Out.t =
      match t.out with
      | (Leaving b | Blank b) when changed && not (String.equal b id) -> Nothing
      | Last b when changed && not (String.equal b id) -> Blank id
      | out -> out
    in
    let kept_out =
      match out with
      | Leaving b | Last b | Blank b -> String.equal b id
      | Nothing -> false
    in
    { t with
      out
    ; ids = (if kept_out || mem t.ids id then t.ids else t.ids @ [ id ])
    ; finished = Set.remove t.finished id
    }
;;

let listed t (sessions : Session_summary.t list) ~current =
  let is_current id = Option.exists current ~f:(String.equal id) in
  let known =
    List.filter_map sessions ~f:(fun s -> Option.some_if (not s.archived) s.id)
    |> String.Set.of_list
  in
  let ids =
    List.filter t.ids ~f:(fun id -> is_current id || Set.mem known id)
  in
  let running =
    List.filter_map sessions ~f:(fun s -> Option.some_if s.running s.id)
    |> String.Set.of_list
  in
  let finished =
    Set.union
      t.finished
      (Set.filter (Set.diff t.running running) ~f:(Fn.non is_current))
    |> Set.filter ~f:(mem ids)
  in
  let out : Out.t =
    match t.out with
    | Blank b when Set.mem known b -> Nothing
    | out -> out
  in
  { t with ids; known = Some known; running; finished; out }
;;

let shown t (sessions : Session_summary.t list) ~current =
  List.filter_map t.ids ~f:(fun id ->
    match List.find sessions ~f:(fun s -> String.equal s.id id) with
    | Some s when not s.archived -> Some (id, Some s)
    | s when Option.exists current ~f:(String.equal id) -> Some (id, s)
    | Some _ | None -> None)
;;

let index ids id =
  List.findi ids ~f:(fun _ x -> String.equal x id) |> Option.map ~f:fst
;;

let neighbour ids id =
  match index ids id with
  | None -> None
  | Some i ->
    (match List.nth ids (i + 1) with
     | Some next -> Some next
     | None -> if i > 0 then List.nth ids (i - 1) else None)
;;

let close t id ~current =
  let ids = List.filter t.ids ~f:(Fn.non (String.equal id)) in
  let out : Out.t =
    if not (Option.exists current ~f:(String.equal id))
    then t.out
    else if List.is_empty ids
    then Last id
    else Leaving id
  in
  { t with ids; out; finished = Set.remove t.finished id }
;;

let move t id ~before =
  if not (mem t.ids id)
  then t
  else (
    let rest = List.filter t.ids ~f:(Fn.non (String.equal id)) in
    let ids =
      match before with
      | Some b when mem rest b ->
        List.concat_map rest ~f:(fun x ->
          if String.equal x b then [ id; x ] else [ x ])
      | Some _ | None -> rest @ [ id ]
    in
    { t with ids })
;;

let step ids ~from ~by =
  match ids with
  | [] -> None
  | _ ->
    let n = List.length ids in
    let at =
      match Option.bind from ~f:(index ids) with
      | Some i -> i + by
      | None -> if by > 0 then by - 1 else n + by
    in
    List.nth ids (((at mod n) + n) mod n)
;;

let nth ids n = if n >= 1 then List.nth ids (n - 1) else None

let status t (s : Session_summary.t option) ~current : Status.t =
  match s with
  | Some s when s.waiting > 0 -> Waiting
  | Some s when s.running -> Running
  | Some s when s.failed -> Failed
  | Some s when (not current) && Set.mem t.finished s.id -> Finished
  | Some _ | None -> Idle
;;

let to_json ids = `Array (List.map ids ~f:(fun id -> `String id))

let of_json : Json.t option -> string list = function
  | Some (`Array items) ->
    List.filter_map items ~f:(function
      | `String s -> Some s
      | _ -> None)
  | _ -> []
;;
