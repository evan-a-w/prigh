open! Core
open! Import

module Action = struct
  type t =
    | Show of
        { dir : string option
        ; base : string option
        }
    | Named of
        { name : string
        ; base : string option
        ; model : string option
        }
    | Model of string
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "/review [WORKTREE] [--base REV], or /review [BASE] [instructions]"

let words s =
  String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let path_like word =
  List.exists [ "/"; "~"; "." ] ~f:(fun prefix ->
    String.is_prefix word ~prefix)
;;

let rec without_base acc = function
  | [] -> Ok (List.rev acc, None)
  | "--base" :: rev :: rest ->
    Result.map (without_base acc rest) ~f:(fun (ws, base) ->
      ws, Option.first_some base (Some rev))
  | [ "--base" ] -> Error "--base needs a revision: /review --base main"
  | w :: rest -> without_base (w :: acc) rest
;;

let parse args : Action.t =
  let all = words args in
  if not (List.mem all "--base" ~equal:String.equal)
  then (
    match all with
    | [] -> Show { dir = None; base = None }
    | [ w ] when path_like w -> Show { dir = Some w; base = None }
    | [ w ] -> Named { name = w; base = None; model = Some (String.strip args) }
    | _ -> Model (String.strip args))
  else (
    match without_base [] all with
    | Error e -> Error e
    | Ok ([], base) -> Show { dir = None; base }
    | Ok ([ w ], base) when path_like w -> Show { dir = Some w; base }
    | Ok ([ w ], base) -> Named { name = w; base; model = None }
    | Ok (_, _) -> Error ("one worktree at a time: " ^ usage))
;;

let resolve listing ~name ~base ~model : Action.t =
  match Worktree_command.find listing name, model with
  | Ok w, _ -> Show { dir = Some w.path; base }
  | Error _, Some args -> Model args
  | Error e, None -> Error e
;;

let command ?dir ?base () =
  String.concat
    ~sep:" "
    ("/review"
     :: (Option.to_list dir
         @ Option.value_map base ~default:[] ~f:(fun b -> [ "--base"; b ])))
;;

let complete ~worktrees ~branches rest =
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
  let has_base = List.mem completed "--base" ~equal:String.equal in
  match List.last completed with
  | Some "--base" ->
    let worktree_branches =
      List.filter_map worktrees ~f:(fun (w : P.Worktree.t) -> w.branch)
    in
    Some
      ( offset
      , List.map
          (List.dedup_and_sort
             ~compare:String.compare
             ("HEAD" :: worktree_branches @ branches))
          ~f:(fun b ->
            ( b
            , if String.equal b "HEAD"
              then "the uncommitted changes only"
              else "compare with " ^ b )) )
  | _ ->
    let positional =
      Result.ok (without_base [] completed)
      |> Option.value_map ~default:[] ~f:fst
    in
    let base_flag =
      if has_base then [] else [ "--base", "compare with another base" ]
    in
    (match positional with
     | [] ->
       Some
         ( offset
         , base_flag
           @ List.map worktrees ~f:(fun (w : P.Worktree.t) ->
             Worktree_command.target w, Worktree_command.describe w) )
     | _ :: _ -> if has_base then None else Some (offset, base_flag))
;;
