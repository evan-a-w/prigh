open! Core
open! Import

module Action = struct
  type t =
    | Show of
        { target : P.Git_diff.Target.t
        ; base : string option
        ; direct : bool
        }
    | Named of
        { name : string
        ; base : string option
        ; direct : bool
        ; model : string option
        }
    | Model of string
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage =
  "/review [WORKTREE|BRANCH] [--base REV] [--direct], or /review [BASE] \
   [instructions]"
;;

let words s =
  String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let path_like word =
  List.exists [ "/"; "~"; "." ] ~f:(fun prefix -> String.is_prefix word ~prefix)
;;

let flag word = List.mem [ "--base"; "--direct" ] word ~equal:String.equal

(* The positional words, the base and whether [--direct] is there. *)
let rec options acc ~base ~direct = function
  | [] -> Ok (List.rev acc, base, direct)
  | "--base" :: rev :: rest when not (flag rev) ->
    options acc ~base:(Option.first_some base (Some rev)) ~direct rest
  | "--base" :: _ -> Error "--base needs a revision: /review --base main"
  | "--direct" :: rest -> options acc ~base ~direct:true rest
  | w :: rest -> options (w :: acc) ~base ~direct rest
;;

let parse args : Action.t =
  let all = words args in
  if not (List.exists all ~f:flag)
  then (
    match all with
    | [] -> Show { target = Dir None; base = None; direct = false }
    | [ w ] when path_like w ->
      Show { target = Dir (Some w); base = None; direct = false }
    | [ w ] ->
      Named
        { name = w
        ; base = None
        ; direct = false
        ; model = Some (String.strip args)
        }
    | _ -> Model (String.strip args))
  else (
    match options [] ~base:None ~direct:false all with
    | Error e -> Error e
    | Ok ([], base, direct) -> Show { target = Dir None; base; direct }
    | Ok ([ w ], base, direct) when path_like w ->
      Show { target = Dir (Some w); base; direct }
    | Ok ([ w ], base, direct) -> Named { name = w; base; direct; model = None }
    | Ok (_, _, _) -> Error ("one worktree or branch at a time: " ^ usage))
;;

let resolve listing ~name ~base ~direct ~model : Action.t =
  match Worktree_command.find listing name, model with
  | Ok w, _ -> Show { target = Dir (Some w.path); base; direct }
  | Error _, Some args -> Model args
  | Error _, None -> Show { target = Rev name; base; direct }
;;

let command ?(target = P.Git_diff.Target.Dir None) ?base ?(direct = false) () =
  String.concat
    ~sep:" "
    ("/review"
     :: ((match target with
          | Dir dir -> Option.to_list dir
          | Rev rev -> [ rev ])
         @ Option.value_map base ~default:[] ~f:(fun b -> [ "--base"; b ])
         @ if direct then [ "--direct" ] else []))
;;

let complete ~worktrees ~(refs : P.Git_diff.Refs.t) rest =
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
  let has word = List.mem completed word ~equal:String.equal in
  let checked_out =
    List.filter_map worktrees ~f:(fun (w : P.Worktree.t) -> w.branch)
  in
  let unique names =
    List.fold names ~init:[] ~f:(fun acc ((name, _) as c) ->
      if List.exists acc ~f:(fun (n, _) -> String.equal n name)
      then acc
      else c :: acc)
    |> List.rev
  in
  match List.last completed with
  | Some "--base" ->
    Some
      ( offset
      , unique
          ((("HEAD", "the uncommitted changes only")
            :: List.map
                 (List.dedup_and_sort
                    ~compare:String.compare
                    (checked_out @ refs.branches))
                 ~f:(fun b -> b, "compare with " ^ b))
           @ List.map refs.remote_branches ~f:(fun b ->
             b, "compare with the remote branch " ^ b)
           @ List.map refs.tags ~f:(fun tag ->
             tag, "compare with the tag " ^ tag)) )
  | _ ->
    let positional =
      Result.ok (options [] ~base:None ~direct:false completed)
      |> Option.value_map ~default:[] ~f:(fun (ws, _, _) -> ws)
    in
    let flags =
      List.filter_opt
        [ Option.some_if
            (not (has "--base"))
            ("--base", "compare with another branch, tag or commit")
        ; Option.some_if
            (not (has "--direct"))
            ("--direct", "compare with the base itself, not the merge base")
        ]
    in
    (match positional with
     | [] ->
       let not_checked_out =
         List.filter (refs.branches @ refs.remote_branches) ~f:(fun b ->
           not (List.mem checked_out b ~equal:String.equal))
       in
       Some
         ( offset
         , flags
           @ List.map worktrees ~f:(fun (w : P.Worktree.t) ->
             Worktree_command.target w, Worktree_command.describe w)
           @ List.map not_checked_out ~f:(fun b ->
             b, "not checked out: review it read-only (needs --base)") )
     | _ :: _ -> if List.is_empty flags then None else Some (offset, flags))
;;
