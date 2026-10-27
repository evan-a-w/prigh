open! Core

module Kind = struct
  type t =
    | User
    | Assistant
    | Thinking
    | Tools
    | Subagents
    | Notes
  [@@deriving sexp_of, equal, compare, enumerate]

  let name = function
    | User -> "user"
    | Assistant -> "assistant"
    | Thinking -> "thinking"
    | Tools -> "tools"
    | Subagents -> "subagents"
    | Notes -> "notes"
  ;;

  let label = function
    | User -> "Your messages"
    | Assistant -> "Replies"
    | Thinking -> "Thinking"
    | Tools -> "Tool calls"
    | Subagents -> "Subagents"
    | Notes -> "Notices"
  ;;

  let detail = function
    | User -> "what you sent, and the skills you ran"
    | Assistant -> "the model's answers"
    | Thinking -> "the model's reasoning"
    | Tools -> "commands, edits, searches and their output"
    | Subagents -> "the agents it started, with their work"
    | Notes -> "errors, compactions, background reports"
  ;;

  let aliases =
    [ User, [ "you"; "me"; "mine"; "messages"; "prompts" ]
    ; Assistant, [ "replies"; "reply"; "answers"; "text" ]
    ; Thinking, [ "thoughts"; "reasoning" ]
    ; Tools, [ "tool"; "calls" ]
    ; Subagents, [ "subagent"; "agents"; "agent" ]
    ; Notes, [ "note"; "notices"; "notice"; "system"; "status" ]
    ]
  ;;

  let of_name word =
    let word = String.lowercase (String.strip word) in
    List.find_map aliases ~f:(fun (kind, words) ->
      Option.some_if
        (String.equal word (name kind)
         || List.mem words word ~equal:String.equal)
        kind)
  ;;

  let of_tool tool =
    if String.is_prefix tool ~prefix:"subagent" then Subagents else Tools
  ;;
end

type t = Kind.t list [@@deriving sexp_of, equal]

let of_hidden kinds = List.filter Kind.all ~f:(List.mem kinds ~equal:Kind.equal)
let all = []
let is_all = List.is_empty
let hidden t = t
let shows t kind = not (List.mem t kind ~equal:Kind.equal)

let toggle t kind =
  if shows t kind
  then of_hidden (kind :: t)
  else List.filter t ~f:(Fn.non (Kind.equal kind))
;;

let to_string t = String.concat ~sep:"," (List.map t ~f:Kind.name)

let of_string s =
  String.split s ~on:',' |> List.filter_map ~f:Kind.of_name |> of_hidden
;;

let summary t =
  match t with
  | [] -> None
  | t ->
    Some (String.concat ~sep:" " (List.map t ~f:(fun k -> "−" ^ Kind.name k)))
;;

let words = "all" :: List.map Kind.all ~f:Kind.name

let unknown word =
  let names = List.map Kind.all ~f:Kind.name in
  let closest =
    List.min_elt names ~compare:(fun a b ->
      Int.compare
        (Edit_distance.distance word a)
        (Edit_distance.distance word b))
  in
  let hint =
    match closest with
    | Some name when Edit_distance.distance word name <= 3 ->
      sprintf " Did you mean %s?" name
    | _ -> ""
  in
  sprintf
    "Unknown kind “%s”.%s Use all, or kinds (%s) to show only those, -KIND to \
     hide one, +KIND to show it."
    word
    hint
    (String.concat ~sep:", " names)
;;

let parse t args =
  let words =
    String.split args ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  let parsed =
    List.map words ~f:(fun word ->
      let sign, name =
        match
          List.find_map
            [ "-", `Hide; "−", `Hide; "+", `Show ]
            ~f:(fun (prefix, sign) ->
              Option.map (String.chop_prefix word ~prefix) ~f:(fun name ->
                sign, name))
        with
        | Some parsed -> parsed
        | None -> `Only, word
      in
      match sign, String.lowercase name with
      | `Only, "all" -> Ok `All
      | _ ->
        (match Kind.of_name name with
         | Some kind -> Ok (`Kind (sign, kind))
         | None -> Error (unknown name)))
  in
  match Result.all parsed with
  | Error e -> Error e
  | Ok [] -> Ok t
  | Ok parsed ->
    let only =
      List.filter_map parsed ~f:(function
        | `Kind (`Only, kind) -> Some kind
        | `All | `Kind ((`Hide | `Show), _) -> None)
    in
    let start =
      match only with
      | [] -> t
      | only ->
        of_hidden
          (List.filter Kind.all ~f:(Fn.non (List.mem only ~equal:Kind.equal)))
    in
    Ok
      (List.fold parsed ~init:start ~f:(fun t -> function
         | `All -> all
         | `Kind (`Only, _) -> t
         | `Kind (`Hide, kind) -> if shows t kind then toggle t kind else t
         | `Kind (`Show, kind) -> if shows t kind then t else toggle t kind))
;;
