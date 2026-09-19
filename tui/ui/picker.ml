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

let create ?(query = "") ~title items =
  refilter { title; items; query; visible = []; selected = 0 }
;;

let title t = t.title
let query t = t.query
let visible t = t.visible
let selected t = t.selected
let selected_item t = List.nth t.visible t.selected

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
  match intent with
  | Cancel -> Cancelled
  | Submit ->
    (match selected_item t with
     | Some item -> Selected item
     | None -> Cancelled)
  | Insert s -> Continue (refilter { t with query = t.query ^ s })
  | Backspace ->
    if String.is_empty t.query
    then Continue t
    else (
      let chars = List.map (Text_width.uchars t.query) ~f:fst in
      Continue
        (refilter { t with query = String.concat (List.drop_last_exn chars) }))
  | Kill_line | Kill_to_end | Kill_word ->
    Continue (refilter { t with query = "" })
  | Up -> Continue (clamp { t with selected = t.selected - 1 })
  | Down -> Continue (clamp { t with selected = t.selected + 1 })
  | Page_up -> Continue (clamp { t with selected = t.selected - page })
  | Page_down -> Continue (clamp { t with selected = t.selected + page })
  | Home -> Continue { t with selected = 0 }
  | End -> Continue (clamp { t with selected = Int.max_value })
  | Newline
  | Delete
  | Left
  | Right
  | Complete
  | Interrupt
  | Force_quit
  | Clear_screen
  | Cycle_verbosity
  | Next_agent
  | Focus_agent _ -> Continue t
;;
