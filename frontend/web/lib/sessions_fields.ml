open! Core
open! Import

module Which = struct
  type t =
    | Worktree_hours
    | Archive_days
  [@@deriving sexp_of, equal, compare]
end

module Field = struct
  type t =
    { draft : string option
    ; error : string option
    }
  [@@deriving sexp_of, equal]

  let empty = { draft = None; error = None }
end

type t =
  { worktree_hours : Field.t
  ; archive_days : Field.t
  }
[@@deriving sexp_of, equal]

let empty = { worktree_hours = Field.empty; archive_days = Field.empty }

let get t : Which.t -> Field.t = function
  | Worktree_hours -> t.worktree_hours
  | Archive_days -> t.archive_days
;;

let set t (which : Which.t) field =
  match which with
  | Worktree_hours -> { t with worktree_hours = field }
  | Archive_days -> { t with archive_days = field }
;;

let draft t which = (get t which).draft
let error t which = (get t which).error
let input t which text = set t which { draft = Some text; error = None }

let parse (which : Which.t) text =
  match which with
  | Worktree_hours ->
    Result.map (Worktree_folds.parse_hours text) ~f:Float.of_int
  | Archive_days ->
    (match Prigh_ui.Auto_archive_command.parse text with
     | Set days -> Ok days
     | Show | Error _ ->
       Error
         (sprintf
            "“%s” isn't a number of days from 0 to %g: type one like 7, or off \
             to archive only by hand."
            text
            Prigh_ui.Auto_archive_command.max_days))
;;

let commit t which =
  match Option.map (get t which).draft ~f:String.strip with
  | None | Some "" -> set t which Field.empty, None
  | Some text ->
    (match parse which text with
     | Ok n -> set t which Field.empty, Some n
     | Error e -> set t which { (get t which) with error = Some e }, None)
;;

let drafted t =
  List.filter [ Which.Worktree_hours; Archive_days ] ~f:(fun w ->
    Option.is_some (draft t w))
;;
