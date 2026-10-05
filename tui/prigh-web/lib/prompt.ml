open! Core
open! Import

module Action = struct
  type t =
    | Cd
    | Host_cwd of
        { host : string
        ; name : string
        }
    | Export
    | Import
  [@@deriving sexp_of, equal]
end

type t =
  { action : Action.t
  ; input : string
  ; suggestions : string list
  ; selected : int option
  ; error : string option
  ; busy : bool
  }
[@@deriving sexp_of, equal]

let create ?(input = "") action =
  { action
  ; input
  ; suggestions = []
  ; selected = None
  ; error = None
  ; busy = false
  }
;;

let set_input t input =
  { t with input; selected = None; error = None; busy = false }
;;

let listing t ~active_host =
  let prefix = "prefix", `String t.input in
  match t.action with
  | Cd -> "list_dirs", [ prefix; "host", `String active_host ]
  | Host_cwd { host; _ } -> "list_dirs", [ prefix; "host", `String host ]
  | Export | Import -> "list_paths", [ prefix ]
;;

let set_suggestions t ~prefix suggestions =
  if String.equal prefix t.input
  then
    { t with
      suggestions =
        List.filter suggestions ~f:(fun s ->
          not (String.equal s t.input || String.equal s (t.input ^ "/")))
    ; selected = None
    }
  else t
;;

let move t delta =
  match t.suggestions with
  | [] -> t
  | suggestions ->
    let last = List.length suggestions - 1 in
    let selected =
      match t.selected with
      | None -> if delta > 0 then 0 else last
      | Some i -> Int.max 0 (Int.min last (i + delta))
    in
    { t with selected = Some selected }
;;

let complete t =
  match List.nth t.suggestions (Option.value t.selected ~default:0) with
  | None -> t
  | Some path -> { (set_input t path) with suggestions = [] }
;;
