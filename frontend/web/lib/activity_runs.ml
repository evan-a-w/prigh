open! Core
open! Import
module Weak_map = Jsoo_weak_collections.Weak_map

module Step = struct
  type t =
    | Thinking of
        { n : int
        ; index : int
        ; text : string
        ; live : bool
        }
    | Call of
        { n : int
        ; call : Tool_call.t
        ; streaming : bool
        ; at : Time_ns.t option
        }
    | Server of
        { n : int
        ; index : int
        ; tool : Content.Server_tool.t
        ; streaming : bool
        }
    | Report of
        { n : int
        ; user : Message.User.t
        }

  let key = function
    | Thinking { n; index; _ } -> sprintf "think:%d:%d" n index
    | Call { call; _ } -> Tool_view.open_key call.id
    | Server { n; index; _ } -> sprintf "server:%d:%d" n index
    | Report { n; _ } -> sprintf "report:%d" n
  ;;

  let phrase : t -> Prigh_ui.Tool_phrase.Step.t = function
    | Thinking _ -> Thinking
    | Call { call; _ } -> Call call
    | Server { tool; _ } -> Call (fst (Tool_summary.as_call tool))
    | Report _ -> Report
  ;;

  let kind : t -> Transcript_filter.Kind.t = function
    | Thinking _ -> Thinking
    | Call { call; _ } -> Transcript_filter.Kind.of_tool call.name
    | Server _ -> Tools
    | Report _ -> Notes
  ;;
end

module Box = struct
  type t =
    { key : string
    ; start : int
    ; rev_steps : Step.t list
    ; since : Time_ns.t option
    }

  let key t = t.key
  let steps t = List.rev t.rev_steps
  let since t = t.since
end

module Segment = struct
  type t =
    | Whole
    | Blocks of int list
    | Box of Box.t
end

(* As kept: a box by its key, its newest version in [boxes]. *)
module Part = struct
  type t =
    | Whole
    | Blocks of int list (** newest first *)
    | Box of string
end

type state =
  { count : int
  ; parts : Part.t list Int.Map.t (** by entry, newest first *)
  ; boxes : Box.t String.Map.t
  ; open_ : string option
  ; last_at : Time_ns.t option
  }

let empty =
  { count = 0
  ; parts = Int.Map.empty
  ; boxes = String.Map.empty
  ; open_ = None
  ; last_at = None
  }
;;

(* Background work's reports and reminders: notices, not the user's words. *)
let report (u : Message.User.t) =
  match Message.User.source u with
  | Some { kind = Delivery | Reminder; _ } -> true
  | Some _ | None -> false
;;

let close t = { t with open_ = None }

let add_step (t, parts) ~n (step : Step.t) =
  match Option.bind t.open_ ~f:(Map.find t.boxes) with
  | Some box ->
    ( { t with
        boxes =
          Map.set
            t.boxes
            ~key:box.key
            ~data:{ box with rev_steps = step :: box.rev_steps }
      }
    , parts )
  | None ->
    let key = "run:" ^ Step.key step in
    let box = { Box.key; start = n; rev_steps = [ step ]; since = t.last_at } in
    ( { t with boxes = Map.set t.boxes ~key ~data:box; open_ = Some key }
    , Part.Box key :: parts )
;;

let add_block (t, (parts : Part.t list)) i =
  ( close t
  , match parts with
    | Blocks is :: rest -> Part.Blocks (i :: is) :: rest
    | parts -> Blocks [ i ] :: parts )
;;

let blank text = String.is_empty (String.strip text)

let add t (entry : Chat.Entry.t) =
  let n = t.count in
  let t, parts, at =
    match entry with
    | User u when report u ->
      let t, parts = add_step (t, []) ~n (Report { n; user = u }) in
      t, parts, u.at
    | User u -> close t, [ Part.Whole ], u.at
    | Shell _ | Compaction _ -> close t, [ Part.Whole ], None
    | Assistant { message; streaming } ->
      let count = List.length message.content in
      let t, parts =
        List.foldi message.content ~init:(t, []) ~f:(fun i acc content ->
          let live = streaming && i = count - 1 in
          match (content : Content.t) with
          | Text text when blank text -> acc
          | Text _ -> add_block acc i
          | Thinking text when blank text && not live -> acc
          | Thinking text ->
            add_step acc ~n (Thinking { n; index = i; text; live })
          | Tool_call call when String.equal call.name "render_ui" ->
            add_block acc i
          | Tool_call call ->
            add_step
              acc
              ~n
              (Call { n; call; streaming = live; at = message.at })
          | Server_tool tool ->
            add_step acc ~n (Server { n; index = i; tool; streaming }))
      in
      (* An error or an interruption ends the run: what follows is new. *)
      let t =
        match message.stop_reason with
        | Error _ | Aborted | Length -> close t
        | End_turn | Tool_use -> t
      in
      t, parts, message.at
  in
  { t with
    count = n + 1
  ; parts = Map.set t.parts ~key:n ~data:parts
  ; last_at = Option.first_some at t.last_at
  }
;;

(* Entries are numbered from 0 here, from the first loaded. *)
let memo : (Chat.Entry.t list, int * state) Weak_map.t = Weak_map.create ()

module Grouping = struct
  type t =
    { first : int
    ; t : state
    }
end

type t = Grouping.t

let of_chat chat =
  let first = Chat.first chat in
  (* Down to the newest cell already grouped, then up again. *)
  let rec down rev newer =
    match rev with
    | [] -> empty, newer
    | _ :: rest as cell ->
      (match Weak_map.get memo cell with
       | Some (f, t) when f = first -> t, newer
       | _ -> down rest (cell :: newer))
  in
  let t, newer = down (Chat.rev_entries chat) [] in
  let t =
    List.fold newer ~init:t ~f:(fun t cell ->
      match cell with
      | [] -> t
      | entry :: _ ->
        let t = add t entry in
        Weak_map.set memo cell (first, t);
        t)
  in
  { Grouping.first; t }
;;

let segments ({ first; t } : Grouping.t) n =
  Map.find t.parts (n - first)
  |> Option.value ~default:[]
  |> List.rev_map ~f:(fun (part : Part.t) : Segment.t ->
    match part with
    | Whole -> Whole
    | Blocks is -> Blocks (List.rev is)
    | Box key -> Box (Map.find_exn t.boxes key))
;;

let tail ({ first = _; t } : Grouping.t) =
  Option.bind t.open_ ~f:(Map.find t.boxes)
;;

let continues ({ first; t = _ } as g : Grouping.t) n =
  match tail g with
  | Some box -> box.start + first < n
  | None -> false
;;

let equal_segments =
  List.equal (fun (a : Segment.t) (b : Segment.t) ->
    match a, b with
    | Whole, Whole -> true
    | Blocks a, Blocks b -> List.equal Int.equal a b
    | Box a, Box b -> phys_equal a b
    | (Whole | Blocks _ | Box _), _ -> false)
;;
