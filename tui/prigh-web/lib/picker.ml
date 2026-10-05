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
    ; search = Option.value search ~default:(label ^ " " ^ detail)
    ; marked
    ; dimmed
    }
  ;;
end

type t =
  { title : string
  ; query : string
  ; items : Item.t list
  ; visible : Item.t list
  ; selected : int
  }
[@@deriving sexp_of, equal]

let title t = t.title
let query t = t.query
let visible t = t.visible
let selected t = t.selected
let selected_item t = List.nth t.visible t.selected

let filter items ~query =
  Prigh_ui.Fuzzy.rank ~query items ~key:(fun (i : Item.t) -> i.search)
;;

let set_query t query =
  { t with query; visible = filter t.items ~query; selected = 0 }
;;

let create ?(query = "") ?highlight ~title items =
  let visible = filter items ~query in
  let selected =
    if String.is_empty query
    then
      Option.value_map
        (List.findi visible ~f:(fun _ (i : Item.t) ->
           match highlight with
           | Some id -> String.equal i.id id
           | None -> i.marked))
        ~default:0
        ~f:fst
    else 0
  in
  { title; query; items; visible; selected }
;;

let move t delta =
  let n = List.length t.visible in
  { t with selected = Int.max 0 (Int.min (n - 1) (t.selected + delta)) }
;;
