open! Core
open! Import

module Item = struct
  type t =
    { name : string
    ; state : string
    ; url : string option
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "name", `String t.name
      ; "state", `String t.state
      ; "url", Option.value_map t.url ~default:`Null ~f:(fun u -> `String u)
      ]
  ;;
end

type t =
  { state : string
  ; items : Item.t list
  }
[@@deriving sexp_of]

let str json name =
  match Json.member name json with
  | Some (`String s) when not (String.is_empty s) -> Some s
  | _ -> None
;;

let array json name =
  match Json.member name json with
  | Some (`Array items) -> items
  | _ -> []
;;

let of_github_check_runs json =
  List.map (array json "check_runs") ~f:(fun run ->
    let state =
      match str run "status", str run "conclusion" with
      | Some "completed", Some ("success" | "neutral") -> "success"
      | Some "completed", Some "skipped" -> "skipped"
      | Some "completed", _ -> "failure"
      | _ -> "pending"
    in
    { Item.name = Option.value (str run "name") ~default:"check"
    ; state
    ; url = str run "html_url"
    })
;;

let of_statuses json =
  List.map (array json "statuses") ~f:(fun status ->
    let state =
      match Option.first_some (str status "status") (str status "state") with
      | Some "success" -> "success"
      | Some ("failure" | "error") -> "failure"
      | Some "warning" -> "success"
      | Some "skipped" -> "skipped"
      | _ -> "pending"
    in
    { Item.name = Option.value (str status "context") ~default:"status"
    ; state
    ; url = str status "target_url"
    })
;;

let of_items items =
  let has state =
    List.exists items ~f:(fun (i : Item.t) -> String.equal i.state state)
  in
  let state =
    if List.is_empty items
    then "none"
    else if has "failure"
    then "failure"
    else if has "pending"
    then "pending"
    else "success"
  in
  { state; items }
;;

let to_json t =
  `Object
    [ "state", `String t.state
    ; "items", `Array (List.map t.items ~f:Item.to_json)
    ]
;;
