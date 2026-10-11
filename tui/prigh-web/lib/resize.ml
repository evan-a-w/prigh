open! Core

module Handle = struct
  module T = struct
    type t =
      | Sidebar
      | Files
      | Files_split
      | Agents
      | Terminal
    [@@deriving sexp_of, equal, compare, enumerate]
  end

  include T
  include Comparator.Make (T)

  let name = function
    | Sidebar -> "sidebar"
    | Files -> "files"
    | Files_split -> "files-split"
    | Agents -> "agents"
    | Terminal -> "terminal"
  ;;

  let of_name s = List.find all ~f:(fun t -> String.equal (name t) s)

  let var = function
    | Sidebar -> "--sidebar-width"
    | Files -> "--files-width"
    | Files_split -> "--files-split"
    | Agents -> "--agents-width"
    | Terminal -> "--terminal-height"
  ;;

  let storage_key t = "prigh-web." ^ String.chop_prefix_exn (var t) ~prefix:"--"

  let label = function
    | Sidebar -> "Resize the sessions sidebar"
    | Files -> "Resize the file browser"
    | Files_split -> "Resize the file list and the preview"
    | Agents -> "Resize the agents panel"
    | Terminal -> "Resize the terminal"
  ;;

  let vertical = function
    | Sidebar | Files | Agents -> true
    | Files_split | Terminal -> false
  ;;

  let grows = function
    | Sidebar | Files_split -> 1
    | Files | Agents | Terminal -> -1
  ;;

  let unit = function
    | Files_split -> "%"
    | Sidebar | Files | Agents | Terminal -> "px"
  ;;

  let step = function
    | Files_split -> 5
    | Sidebar | Files | Agents | Terminal -> 16
  ;;
end

module Viewport = struct
  type t =
    { width : int
    ; height : int
    }
  [@@deriving sexp_of, equal]
end

module Shown = struct
  type t =
    { sidebar : bool
    ; files : bool
    ; agents : bool
    ; narrow : bool
    }
  [@@deriving sexp_of]
end

module Bounds = struct
  type t =
    { min : int
    ; max : int
    ; value : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  { sizes : int Map.M(Handle).t
  ; viewport : Viewport.t
  }
[@@deriving sexp_of]

let empty =
  { sizes = Map.empty (module Handle)
  ; viewport = { width = 1280; height = 800 }
  }
;;

let chat_min = 400
let files_floats_below = 1100
let agents_floats_below = 1180

(* Room under the terminal's top edge's highest position: the top bar, the
   composer and a few lines of chat. *)
let terminal_room = 260
let viewport t = t.viewport
let preference t h = Map.find t.sizes h
let clamp v ~min ~max = Int.max min (Int.min v max)

(* Without the window: the smallest and largest it may ever be, and the
   default. *)
let limits t (h : Handle.t) =
  match h with
  | Sidebar -> 200, 520, 280
  | Files -> 280, 1200, 400
  | Agents -> 300, 1200, 420
  | Files_split -> 20, 80, 45
  | Terminal ->
    let max = Int.max 120 (t.viewport.height - terminal_room) in
    120, max, clamp (t.viewport.height * 38 / 100) ~min:120 ~max
;;

let preferred t h =
  let min, max, default = limits t h in
  clamp (Option.value (preference t h) ~default) ~min ~max
;;

let docked t (shown : Shown.t) (h : Handle.t) =
  (not shown.narrow)
  &&
  match h with
  | Sidebar -> shown.sidebar
  | Files -> shown.files && t.viewport.width >= files_floats_below
  | Agents -> shown.agents && t.viewport.width >= agents_floats_below
  | Files_split | Terminal -> false
;;

let sides = [ Handle.Sidebar; Files; Agents ]
let budget t = t.viewport.width - chat_min

(* The docked side panels' widths: their preferences, shrunk in proportion
   to what they have above their minimums when the chat would get less than
   [chat_min]. *)
let docked_widths t shown =
  let panels =
    List.filter_map sides ~f:(fun h ->
      if docked t shown h
      then (
        let min, _, _ = limits t h in
        Some (h, min, preferred t h))
      else None)
  in
  let total = List.sum (module Int) panels ~f:(fun (_, _, w) -> w) in
  let excess = total - budget t in
  let slack = List.sum (module Int) panels ~f:(fun (_, min, w) -> w - min) in
  List.map panels ~f:(fun (h, min, w) ->
    if excess <= 0
    then h, w
    else if slack <= excess
    then h, min
    else h, w - (((excess * (w - min)) + slack - 1) / slack))
;;

let bounds t shown (h : Handle.t) : Bounds.t =
  let min, max, _ = limits t h in
  match h with
  | Files_split | Terminal -> { min; max; value = preferred t h }
  | Sidebar | Files | Agents ->
    let widths = docked_widths t shown in
    (match List.Assoc.find widths h ~equal:Handle.equal with
     | Some value ->
       let others =
         List.sum
           (module Int)
           widths
           ~f:(fun (h', w) -> if Handle.equal h h' then 0 else w)
       in
       { min; max = Int.max min (Int.min max (budget t - others)); value }
     | None ->
       (* Floating over the chat (or closed): at most most of the window. *)
       let max = Int.max min (Int.min max (t.viewport.width * 86 / 100)) in
       { min; max; value = clamp (preferred t h) ~min ~max })
;;

let vars t shown =
  List.map Handle.all ~f:(fun h ->
    Handle.var h, sprintf "%d%s" (bounds t shown h).value (Handle.unit h))
;;

module Action = struct
  type t =
    | Set of Handle.t * int
    | Nudge of Handle.t * int
    | Smallest of Handle.t
    | Largest of Handle.t
    | Reset of Handle.t
    | Viewport of Viewport.t
    | Restore of (Handle.t * int) list
  [@@deriving sexp_of]
end

module Save = struct
  type t =
    { key : string
    ; value : int option
    }
  [@@deriving sexp_of, equal]
end

let set t shown h value =
  let b = bounds t shown h in
  let value = clamp value ~min:b.min ~max:b.max in
  if Option.equal Int.equal (preference t h) (Some value)
  then t, []
  else
    ( { t with sizes = Map.set t.sizes ~key:h ~data:value }
    , [ { Save.key = Handle.storage_key h; value = Some value } ] )
;;

let update t shown (action : Action.t) =
  match action with
  | Set (h, value) -> set t shown h value
  | Nudge (h, steps) ->
    set t shown h ((bounds t shown h).value + (steps * Handle.step h))
  | Smallest h -> set t shown h (bounds t shown h).min
  | Largest h -> set t shown h (bounds t shown h).max
  | Reset h ->
    if Map.mem t.sizes h
    then
      ( { t with sizes = Map.remove t.sizes h }
      , [ { Save.key = Handle.storage_key h; value = None } ] )
    else t, []
  | Viewport viewport -> { t with viewport }, []
  | Restore sizes ->
    ( List.fold sizes ~init:t ~f:(fun t (h, value) ->
        (* The terminal's largest depends on the window, which may not be
           known yet. *)
        let min, max, _ = limits t h in
        let max = if Handle.equal h Terminal then Int.max_value else max in
        { t with sizes = Map.set t.sizes ~key:h ~data:(clamp value ~min ~max) })
    , [] )
;;
