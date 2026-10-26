open! Core
open! Import

module Action = struct
  type t =
    | List
    | New of
        { branch : string
        ; base : string option
        }
    | Open of
        { target : string
        ; new_session : bool
        }
    | Remove of
        { target : string
        ; force : bool
        }
    | Back of { main : bool }
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage =
  "/worktree [list|new BRANCH [BASE]|open [-n] TARGET|back [main]|rm [-f] \
   TARGET]"
;;

let no_home =
  "this session hasn't switched into a worktree; /worktree open TARGET switches"
;;

let subcommands =
  [ "list", "pick a worktree of this repository to work in"
  ; "new", "new BRANCH [BASE]: create a worktree and work in it"
  ; "open", "open [-n] TARGET: work in a worktree (-n: in a new session)"
  ; "back", "back [main]: back where the session switched into worktrees from"
  ; "rm", "rm [-f] TARGET: remove a worktree (-f: with uncommitted changes)"
  ]
;;

let words s =
  String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let with_flag ~flag ~usage args ~f : Action.t =
  let flags, rest = List.partition_tf args ~f:(String.equal flag) in
  match rest with
  | [ target ] -> f ~target ~flagged:(not (List.is_empty flags))
  | [] ->
    Error
      ("usage: "
       ^ usage
       ^ " (TARGET: a branch or path, as /worktree lists them)")
  | _ :: _ :: _ ->
    (match List.find rest ~f:(String.is_prefix ~prefix:"-") with
     | Some bad -> Error (sprintf "unknown option %s: usage: %s" bad usage)
     | None -> Error ("one worktree at a time: usage: " ^ usage))
;;

let parse s : Action.t =
  match words s with
  | [] | [ "list" ] -> List
  | "new" :: args ->
    (match args with
     | [ branch ] -> New { branch; base = None }
     | [ branch; base ] -> New { branch; base = Some base }
     | _ ->
       Error
         "usage: /worktree new BRANCH [BASE]: a new branch from BASE (default: \
          HEAD), or an existing branch")
  | "open" :: args ->
    with_flag
      ~flag:"-n"
      ~usage:"/worktree open [-n] TARGET"
      args
      ~f:(fun ~target ~flagged -> Open { target; new_session = flagged })
  | [ "back" ] -> Back { main = false }
  | [ "back"; "main" ] -> Back { main = true }
  | "back" :: _ ->
    Error
      "usage: /worktree back [main] (main: the repository's main checkout, \
       when the place it came from is gone)"
  | ("rm" | "remove") :: args ->
    with_flag
      ~flag:"-f"
      ~usage:"/worktree rm [-f] TARGET"
      args
      ~f:(fun ~target ~flagged -> Remove { target; force = flagged })
  | word :: _ -> Error (sprintf "unknown subcommand %S: %s" word usage)
;;

let target (w : P.Worktree.t) = Option.value w.branch ~default:w.path

let find (listing : P.Worktree.Listing.t) name =
  let name = String.rstrip name ~drop:(Char.equal '/') in
  let matches f = List.filter listing.worktrees ~f in
  let first_of candidates =
    List.find_map candidates ~f:(fun f ->
      match matches f with
      | [ w ] -> Some w
      | _ -> None)
  in
  match
    first_of
      [ (fun (w : P.Worktree.t) ->
          Option.exists w.branch ~f:(String.equal name))
      ; (fun w -> String.equal w.path name)
      ; (fun w -> String.is_suffix w.path ~suffix:("/" ^ name))
      ]
  with
  | Some w -> Ok w
  | None ->
    let names =
      List.concat_map listing.worktrees ~f:(fun w ->
        Option.to_list w.branch @ [ Filename.basename w.path ])
      |> List.dedup_and_sort ~compare:String.compare
    in
    let closest =
      List.map names ~f:(fun n -> Edit_distance.distance name n, n)
      |> List.sort ~compare:[%compare: int * string]
      |> Fn.flip List.take 3
      |> List.map ~f:snd
    in
    Error
      (sprintf
         "no worktree %S%s; /worktree lists them"
         name
         (if List.is_empty closest
          then ""
          else sprintf " (closest: %s)" (String.concat ~sep:", " closest)))
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let describe (w : P.Worktree.t) =
  let parts =
    [ Some (P.Worktree.label w)
    ; Some w.path
    ; Option.some_if w.main "main worktree"
    ; Option.some_if w.current "here"
    ; (match w.changes with
       | Some n when n > 0 -> Some (sprintf "%d changed" n)
       | _ -> None)
    ; Option.some_if w.prunable "gone"
    ; Option.some_if w.locked "locked"
    ; (if w.ahead > 0 || w.behind > 0
       then Some (sprintf "↑%d ↓%d" w.ahead w.behind)
       else None)
    ; (match w.sessions with
       | [] -> None
       | sessions -> Some (plural (List.length sessions) "session"))
    ]
  in
  String.concat ~sep:" · " (List.filter_opt parts)
;;

let marker (w : P.Worktree.t) =
  (if w.current then "+" else "") ^ if P.Worktree.dirty w then "*" else ""
;;

let session_name (s : P.Worktree.Session_ref.t) =
  Option.value s.name ~default:s.id
;;

let confirm_remove (w : P.Worktree.t) ~force =
  let changes =
    match w.changes with
    | Some n when n > 0 ->
      if force
      then sprintf "; its %s are lost" (plural n "uncommitted change")
      else sprintf "; it has %s" (plural n "uncommitted change")
    | _ -> ""
  in
  let sessions =
    match w.sessions with
    | [] -> ""
    | sessions ->
      sprintf
        "; sessions working there: %s"
        (String.concat ~sep:", " (List.map sessions ~f:session_name))
  in
  sprintf
    "Remove the worktree %s at %s%s%s? (the branch stays)"
    (P.Worktree.label w)
    w.path
    changes
    sessions
;;

let cannot_remove (w : P.Worktree.t) =
  if w.main
  then
    Some
      (sprintf
         "%s is the repository's main worktree: it cannot be removed"
         w.path)
  else if w.current
  then
    Some
      (sprintf
         "this session works in %s: /worktree open another one (or /cd \
          elsewhere) first"
         w.path)
  else None
;;

let created (c : P.Worktree.Created.t) =
  sprintf
    "worktree %s at %s (%s)"
    (P.Worktree.label c.worktree)
    c.worktree.path
    (if c.new_branch then "new branch" else "existing branch")
;;

let removed (w : P.Worktree.t) =
  sprintf
    "removed the worktree %s at %s (the branch stays)"
    (P.Worktree.label w)
    w.path
;;

let complete ~worktrees rest =
  let ends_word = String.is_suffix rest ~suffix:" " in
  let ws = words rest in
  let offset =
    if ends_word
    then String.length rest
    else (
      match String.rindex rest ' ' with
      | Some i -> i + 1
      | None -> 0)
  in
  let completed =
    if ends_word then ws else Option.value (List.drop_last ws) ~default:[]
  in
  let targets ~flag ~flag_help ~removable =
    (if List.mem completed flag ~equal:String.equal
     then []
     else [ flag, flag_help ])
    @ List.filter_map worktrees ~f:(fun (w : P.Worktree.t) ->
      if removable && Option.is_some (cannot_remove w)
      then None
      else Some (target w, describe w))
  in
  match completed with
  | [] -> Some (offset, subcommands)
  | [ "open" ] | [ "open"; "-n" ] ->
    Some
      (offset, targets ~flag:"-n" ~flag_help:"in a new session" ~removable:false)
  | [ ("rm" | "remove") ] | [ ("rm" | "remove"); "-f" ] ->
    Some
      ( offset
      , targets
          ~flag:"-f"
          ~flag_help:"also with uncommitted changes"
          ~removable:true )
  | [ "back" ] ->
    Some (offset, [ "main", "the repository's main checkout instead" ])
  | [ "new"; _ ] ->
    Some
      ( offset
      , List.filter_map worktrees ~f:(fun (w : P.Worktree.t) ->
          Option.map w.branch ~f:(fun b -> b, "base: " ^ describe w)) )
  | _ -> None
;;

let back_notice home ~cwd ~tilde =
  let cwd =
    Option.first_some
      cwd
      (Option.map home ~f:(fun (h : P.Session_home.t) -> h.cwd))
  in
  match home, cwd with
  | Some (home : P.Session_home.t), Some cwd
    when Option.exists home.repo ~f:(fun r -> Option.is_some r.branch)
         && String.equal home.cwd cwd ->
    sprintf "back in %s (%s)" (P.Session_home.label home ~tilde) (tilde cwd)
  | _, Some cwd -> "back in " ^ tilde cwd
  | _, None -> "back"
;;
