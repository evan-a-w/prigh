open! Core
open! Import

module Filter = struct
  type t =
    | Open
    | Closed
    | All
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Open -> "open"
    | Closed -> "closed"
    | All -> "all"
  ;;

  let of_string s =
    List.find all ~f:(fun f -> String.equal (to_string f) (String.lowercase s))
  ;;
end

module Status = struct
  type t =
    | Loading
    | Failed of string
    | Loaded of Forge_repo.t
  [@@deriving sexp_of, equal]
end

type t =
  { filter : Filter.t
  ; status : Status.t
  ; prs : Pull_request.t list
  ; picker : Picker.t
  }
[@@deriving sexp_of, equal]

let title = "Pull requests"

let create ?(query = "") filter =
  { filter
  ; status = Loading
  ; prs = []
  ; picker = Picker.create ~query ~title []
  }
;;

let state_label (pr : Pull_request.t) =
  match pr.state, pr.draft with
  | Open, true -> "Draft"
  | Open, false -> "Open"
  | Merged, _ -> "Merged"
  | Closed, _ -> "Closed"
;;

let branches (pr : Pull_request.t) = sprintf "%s → %s" pr.head pr.base

let detail (pr : Pull_request.t) =
  String.concat
    ~sep:" · "
    ([ pr.author; branches pr ]
     @
     match pr.state, pr.draft with
     | Open, true -> [ "draft" ]
     | Open, false -> []
     | (Merged | Closed), _ -> [ String.lowercase (state_label pr) ])
;;

let describe (pr : Pull_request.t) =
  sprintf "#%d %s · %s" pr.number pr.title (detail pr)
;;

(* The state shows as a badge beside it; the filter still matches it. *)
let item (pr : Pull_request.t) =
  Picker.Item.create
    ~id:(Int.to_string pr.number)
    ~detail:(sprintf "%s · %s" pr.author (branches pr))
    ~search:(describe pr)
    (sprintf "#%d %s" pr.number pr.title)
;;

let loaded t (l : Pr_list.t) =
  { t with
    status = Loaded l.repo
  ; prs = l.prs
  ; picker =
      Picker.create
        ~query:(Picker.query t.picker)
        ~title
        (List.map l.prs ~f:item)
  }
;;

let failed t e = { t with status = Failed e }

let find t number =
  List.find t.prs ~f:(fun (pr : Pull_request.t) -> pr.number = number)
;;

let selected t =
  Option.bind (Picker.selected_item t.picker) ~f:(fun item ->
    Option.bind (Int.of_string_opt item.id) ~f:(find t))
;;

module Command = struct
  type t =
    | List of Filter.t
    | View of int
    | New of string
    | Checkout of int
  [@@deriving sexp_of, equal]

  let usage = "/pr [list [open|closed|all] | view N | new [title] | checkout N]"

  let number word =
    Option.filter
      (Int.of_string_opt (String.chop_prefix_if_exists word ~prefix:"#"))
      ~f:(fun n -> n > 0)
  ;;

  let parse rest =
    let words =
      String.split (String.strip rest) ~on:' '
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    let needs_number name =
      Error
        (sprintf "/pr %s needs a number: /pr %s 24 (/pr lists them)" name name)
    in
    match words with
    | [] | [ "list" ] -> Ok (List Open)
    | ([ "list"; state ] | [ state ])
      when Option.is_some (Filter.of_string state) ->
      Ok (List (Option.value_exn (Filter.of_string state)))
    | "list" :: _ -> Error "/pr list takes open, closed or all: /pr list closed"
    | ([ "view"; n ] | [ n ]) when Option.is_some (number n) ->
      Ok (View (Option.value_exn (number n)))
    | "view" :: _ -> needs_number "view"
    | [ "checkout"; n ] when Option.is_some (number n) ->
      Ok (Checkout (Option.value_exn (number n)))
    | "checkout" :: _ -> needs_number "checkout"
    | "new" :: _ ->
      Ok
        (New
           (String.strip
              (String.chop_prefix_exn (String.strip rest) ~prefix:"new")))
    | word :: _ -> Error (sprintf "Unknown /pr argument “%s”: %s" word usage)
  ;;
end
