open! Core

module Item = struct
  type t =
    { id : string
    ; label : string
    ; detail : string
    ; search : string
    ; marked : bool
    ; dimmed : bool
    }
  [@@deriving sexp_of, equal]

  let create
    ?(detail = "")
    ?search
    ?(marked = false)
    ?(dimmed = false)
    ~id
    label
    =
    { id
    ; label
    ; detail
    ; search = Option.value search ~default:(label ^ " " ^ detail ^ " " ^ id)
    ; marked
    ; dimmed
    }
  ;;
end

type t =
  { title : string
  ; items : Item.t list
  ; query : string
  ; visible : Item.t list
  ; selected : int
  ; multi : bool
  ; checked : String.Set.t
  }
[@@deriving sexp_of]

let refilter t =
  let visible =
    Fuzzy.rank ~query:t.query t.items ~key:(fun i -> i.Item.search)
  in
  let selected =
    if String.is_empty t.query
    then
      Option.value
        (List.findi visible ~f:(fun _ i -> i.marked) |> Option.map ~f:fst)
        ~default:0
    else 0
  in
  { t with visible; selected }
;;

let create
  ?(query = "")
  ?(multi = false)
  ?(checked = String.Set.empty)
  ~title
  items
  =
  refilter { title; items; query; visible = []; selected = 0; multi; checked }
;;

let title t = t.title
let query t = t.query
let visible t = t.visible
let selected t = t.selected
let selected_item t = List.nth t.visible t.selected
let multi t = t.multi
let checked t = t.checked

module Outcome = struct
  type nonrec t =
    | Continue of t
    | Selected of Item.t
    | Cancelled
end

let clamp t =
  let n = List.length t.visible in
  { t with selected = Int.max 0 (Int.min t.selected (n - 1)) }
;;

let handle t (intent : Intent.t) ~page : Outcome.t =
  let toggle id : Outcome.t =
    let checked =
      if Set.mem t.checked id
      then Set.remove t.checked id
      else Set.add t.checked id
    in
    Continue { t with checked }
  in
  match intent with
  | Cancel -> Cancelled
  | Submit ->
    (match selected_item t with
     | Some item -> Selected item
     | None -> Continue t)
  | Insert " " when t.multi ->
    (match selected_item t with
     | Some item -> toggle item.id
     | None -> Continue t)
  | Insert s -> Continue (refilter { t with query = t.query ^ s })
  | Home when t.multi ->
    (* Ctrl+A: check everything currently visible. *)
    let visible = String.Set.of_list (List.map t.visible ~f:(fun i -> i.id)) in
    Continue { t with checked = Set.union t.checked visible }
  | Copy_last when t.multi ->
    (* Ctrl+X: uncheck everything. *)
    Continue { t with checked = String.Set.empty }
  | Copy_last -> Continue t
  | Backspace ->
    if String.is_empty t.query
    then Continue t
    else (
      let chars = List.map (Text_width.uchars t.query) ~f:fst in
      Continue
        (refilter { t with query = String.concat (List.drop_last_exn chars) }))
  | Kill_to_start | Kill_to_end | Kill_word ->
    Continue (refilter { t with query = "" })
  | Up | Scroll_up -> Continue (clamp { t with selected = t.selected - 1 })
  | Down | Scroll_down -> Continue (clamp { t with selected = t.selected + 1 })
  | Page_up -> Continue (clamp { t with selected = t.selected - page })
  | Page_down -> Continue (clamp { t with selected = t.selected + page })
  | Home -> Continue { t with selected = 0 }
  | End -> Continue (clamp { t with selected = Int.max_value })
  | Newline
  | Paste _
  | Delete
  | Left
  | Right
  | Word_left
  | Word_right
  | Delete_word_forward
  | Yank
  | Yank_pop
  | Undo
  | Complete
  | Interrupt
  | Force_quit
  | Cycle_verbosity
  | Next_model
  | Prev_model
  | Next_thinking
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message -> Continue t
;;
