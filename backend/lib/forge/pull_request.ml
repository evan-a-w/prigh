open! Core
open! Import

module State = struct
  type t =
    | Open
    | Closed
    | Merged
  [@@deriving sexp_of, equal]

  let to_string = function
    | Open -> "open"
    | Closed -> "closed"
    | Merged -> "merged"
  ;;
end

type t =
  { number : int
  ; title : string
  ; state : State.t
  ; draft : bool
  ; author : string
  ; head : string
  ; head_sha : string
  ; base : string
  ; url : string
  ; body : string
  ; created_at : string
  ; updated_at : string
  }
[@@deriving sexp_of]

let path json names =
  List.fold names ~init:(Some json) ~f:(fun json name ->
    Option.bind json ~f:(Json.member name))
;;

let string_at json names =
  match path json names with
  | Some (`String s) -> Some s
  | _ -> None
;;

let forgejo_wip_prefixes = [ "WIP:"; "[WIP]" ]

let is_wip title =
  List.exists forgejo_wip_prefixes ~f:(fun prefix ->
    String.Caseless.is_prefix (String.lstrip title) ~prefix)
;;

let forgejo_draft_title title = if is_wip title then title else "WIP: " ^ title

let of_json (kind : Forge_kind.t) json =
  let open Or_error.Let_syntax in
  let%bind number =
    match path json [ "number" ] with
    | Some n ->
      Or_error.of_option (Json.int n) ~error:(Error.of_string "number")
    | None -> Or_error.error_string "pull request: missing number"
  in
  let str names = Option.value (string_at json names) ~default:"" in
  let title = str [ "title" ] in
  let merged =
    (match path json [ "merged" ] with
     | Some `True -> true
     | _ -> false)
    || Option.is_some (string_at json [ "merged_at" ])
  in
  let state : State.t =
    if merged
    then Merged
    else (
      match string_at json [ "state" ] with
      | Some "open" -> Open
      | _ -> Closed)
  in
  let draft =
    (match path json [ "draft" ] with
     | Some `True -> true
     | _ -> false)
    ||
    match kind with
    | Forgejo -> is_wip title
    | Github -> false
  in
  let head_ref = str [ "head"; "ref" ] in
  let head =
    let head_owner = string_at json [ "head"; "repo"; "owner"; "login" ] in
    let base_owner = string_at json [ "base"; "repo"; "owner"; "login" ] in
    match head_owner, base_owner with
    | Some h, Some b when not (String.equal h b) -> h ^ ":" ^ head_ref
    | _ -> head_ref
  in
  return
    { number
    ; title
    ; state
    ; draft
    ; author = str [ "user"; "login" ]
    ; head
    ; head_sha = str [ "head"; "sha" ]
    ; base = str [ "base"; "ref" ]
    ; url = str [ "html_url" ]
    ; body = str [ "body" ]
    ; created_at = str [ "created_at" ]
    ; updated_at = str [ "updated_at" ]
    }
;;

let to_json t =
  `Object
    [ "number", `Number (Int.to_string t.number)
    ; "title", `String t.title
    ; "state", `String (State.to_string t.state)
    ; ("draft", if t.draft then `True else `False)
    ; "author", `String t.author
    ; "head", `String t.head
    ; "base", `String t.base
    ; "url", `String t.url
    ; "body", `String t.body
    ; "created_at", `String t.created_at
    ; "updated_at", `String t.updated_at
    ]
;;
