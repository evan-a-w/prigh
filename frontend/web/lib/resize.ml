open! Core

module Handle = struct
  module T = struct
    type t =
      | Sidebar
      | Files
      | Files_preview
      | Agents
      | Terminal
      | Review_chat
      | Review_files
      | Review_margin
    [@@deriving sexp_of, equal, compare, enumerate]
  end

  include T
  include Comparator.Make (T)

  let var = function
    | Sidebar -> "--sidebar-width"
    | Files -> "--files-width"
    | Files_preview -> "--files-preview-width"
    | Agents -> "--agents-width"
    | Terminal -> "--terminal-height"
    | Review_chat -> "--review-chat-width"
    | Review_files -> "--review-files-width"
    | Review_margin -> "--review-margin-width"
  ;;

  let name = function
    | Sidebar -> "sidebar"
    | Files -> "files"
    | Files_preview -> "files-preview"
    | Agents -> "agents"
    | Terminal -> "terminal"
    | Review_chat -> "review-chat"
    | Review_files -> "review-files"
    | Review_margin -> "review-margin"
  ;;

  let of_name s = List.find all ~f:(fun t -> String.equal (name t) s)
  let pref_key t = "web." ^ String.chop_prefix_exn (var t) ~prefix:"--"

  let label = function
    | Sidebar -> "Resize the sessions sidebar"
    | Files -> "Resize the file browser"
    | Files_preview -> "Resize the file preview"
    | Agents -> "Resize the agents panel"
    | Terminal -> "Resize the terminal"
    | Review_chat -> "Resize the chat beside the review"
    | Review_files -> "Resize the review's file list"
    | Review_margin -> "Resize the review's comments"
  ;;

  let vertical = function
    | Terminal -> false
    | Sidebar
    | Files
    | Files_preview
    | Agents
    | Review_chat
    | Review_files
    | Review_margin -> true
  ;;

  let grows = function
    | Sidebar | Review_files -> 1
    | Files | Files_preview | Agents | Terminal | Review_chat | Review_margin
      -> -1
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
    ; preview : bool
    ; agents : bool
    ; reviewing : bool
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
let review_min = 480
let files_floats_below = 1100
let agents_floats_below = 1180

(* Room under the terminal's top edge's highest position: the session's bar,
   the composer and a few lines of chat. *)
let terminal_room = 260

(* What the review's code keeps beside its file list and its comments. *)
let review_code_min = 320
let step = 16
let viewport t = t.viewport
let preference t h = Map.find t.sizes h
let clamp v ~min ~max = Int.max min (Int.min v max)

(* Without the window: the smallest and largest it may ever be, and the
   default. *)
let limits t (h : Handle.t) =
  match h with
  | Sidebar -> 200, 520, 280
  | Files -> 280, 1200, 400
  | Files_preview -> 320, 1400, 480
  | Agents -> 300, 1200, 420
  | Review_chat -> 300, 1200, 400
  | Review_files -> 160, 520, 260
  | Review_margin -> 240, 640, 340
  | Terminal ->
    let max = Int.max 120 (t.viewport.height - terminal_room) in
    120, max, clamp (t.viewport.height * 38 / 100) ~min:120 ~max
;;

let minimum t h =
  let min, _, _ = limits t h in
  min
;;

let preferred t h =
  let min, max, default = limits t h in
  clamp (Option.value (preference t h) ~default) ~min ~max
;;

(* The width the flexible column in the middle keeps: the chat, or the
   review page with the chat docked beside it. *)
let budget t (shown : Shown.t) =
  t.viewport.width - if shown.reviewing then review_min else chat_min
;;

(* The side panels docked beside the middle column, but the preview. *)
let docked_sides t (shown : Shown.t) =
  if shown.narrow
  then []
  else
    List.filter
      [ Handle.Sidebar; Review_chat; Files; Agents ]
      ~f:(function
        | Sidebar -> shown.sidebar
        | Review_chat -> shown.reviewing
        | Files ->
          shown.files
          && (not shown.reviewing)
          && t.viewport.width >= files_floats_below
        | Agents -> shown.agents && t.viewport.width >= agents_floats_below
        | _ -> false)
;;

let preview_alone t (shown : Shown.t) =
  shown.files
  && shown.preview
  &&
  let sides = docked_sides t shown in
  (not (List.mem sides Files ~equal:Handle.equal))
  || List.sum (module Int) (Handle.Files_preview :: sides) ~f:(minimum t)
     > budget t shown
;;

let docked t shown =
  let sides = docked_sides t shown in
  if shown.preview
     && List.mem sides Files ~equal:Handle.equal
     && not (preview_alone t shown)
  then Handle.Files_preview :: sides
  else sides
;;

(* The docked side panels' widths: their preferences, shrunk in proportion
   to what they have above their minimums when the middle column would get
   less than its least. *)
let docked_widths t shown =
  let panels =
    List.map (docked t shown) ~f:(fun h -> h, minimum t h, preferred t h)
  in
  let total = List.sum (module Int) panels ~f:(fun (_, _, w) -> w) in
  let excess = total - budget t shown in
  let slack = List.sum (module Int) panels ~f:(fun (_, min, w) -> w - min) in
  List.map panels ~f:(fun (h, min, w) ->
    if excess <= 0
    then h, w
    else if slack <= excess
    then h, min
    else h, w - (((excess * (w - min)) + slack - 1) / slack))
;;

let rec bounds t (shown : Shown.t) (h : Handle.t) : Bounds.t =
  let min, max, _ = limits t h in
  let within cap = Int.max min (Int.min max cap) in
  match h with
  | Terminal -> { min; max; value = preferred t h }
  | Review_files | Review_margin ->
    let shown = { shown with reviewing = true } in
    let widths = docked_widths t shown in
    let page =
      t.viewport.width - List.sum (module Int) widths ~f:(fun (_, w) -> w)
    in
    let beside =
      match h with
      | Review_margin -> (bounds t shown Review_files).value
      | _ -> 0
    in
    let max = within (page - beside - review_code_min) in
    { min; max; value = clamp (preferred t h) ~min ~max }
  | Sidebar | Files | Files_preview | Agents | Review_chat ->
    let widths = docked_widths t shown in
    (match List.Assoc.find widths h ~equal:Handle.equal with
     | Some value ->
       let others =
         List.sum
           (module Int)
           widths
           ~f:(fun (h', w) -> if Handle.equal h h' then 0 else w)
       in
       { min; max = within (budget t shown - others); value }
     | None ->
       (* Floating over the chat (or closed): at most most of the window. *)
       let max = within (t.viewport.width * 86 / 100) in
       { min; max; value = clamp (preferred t h) ~min ~max })
;;

let vars t shown =
  List.map Handle.all ~f:(fun h ->
    Handle.var h, sprintf "%dpx" (bounds t shown h).value)
;;

module Action = struct
  type t =
    | Set of Handle.t * int
    | Nudge of Handle.t * int
    | Smallest of Handle.t
    | Largest of Handle.t
    | Reset of Handle.t
    | Viewport of Viewport.t
    | Restore of (Handle.t * int option) list
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
    , [ { Save.key = Handle.pref_key h; value = Some value } ] )
;;

let update t shown (action : Action.t) =
  match action with
  | Set (h, value) -> set t shown h value
  | Nudge (h, steps) -> set t shown h ((bounds t shown h).value + (steps * step))
  | Smallest h -> set t shown h (bounds t shown h).min
  | Largest h -> set t shown h (bounds t shown h).max
  | Reset h ->
    if Map.mem t.sizes h
    then
      ( { t with sizes = Map.remove t.sizes h }
      , [ { Save.key = Handle.pref_key h; value = None } ] )
    else t, []
  | Viewport viewport -> { t with viewport }, []
  | Restore sizes ->
    ( List.fold sizes ~init:t ~f:(fun t (h, value) ->
        match value with
        | None -> { t with sizes = Map.remove t.sizes h }
        | Some value ->
          (* The terminal's largest depends on the window, which may not be
             known yet. *)
          let min, max, _ = limits t h in
          let max = if Handle.equal h Terminal then Int.max_value else max in
          { t with
            sizes = Map.set t.sizes ~key:h ~data:(clamp value ~min ~max)
          })
    , [] )
;;
