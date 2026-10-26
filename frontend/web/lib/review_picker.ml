open! Core
open! Import

module Which = struct
  type t =
    | Target
    | Base
    | Since of string
  [@@deriving sexp_of, equal]
end

module Choice = struct
  type t =
    | Review of Git_diff.Target.t
    | Base of string
    | Since of
        { path : string
        ; index : int option
        }
  [@@deriving sexp_of, equal]
end

module Item = struct
  type t =
    { choice : Choice.t
    ; group : string
    ; label : string
    ; detail : string
    ; marked : bool
    }
  [@@deriving sexp_of, equal]

  let id t =
    match t.choice with
    | Review (Dir None) -> "own"
    | Review (Dir (Some dir)) -> "dir:" ^ dir
    | Review (Rev rev) -> "rev:" ^ rev
    | Base rev -> "base:" ^ rev
    | Since { index = None; _ } -> "since:latest"
    | Since { index = Some i; _ } -> sprintf "since:%d" i
  ;;

  let search t = t.label ^ " " ^ t.detail
end

type t =
  { which : Which.t
  ; items : Item.t list
  ; worktrees : Worktree.t list
  ; query : string
  ; selected : int
  }
[@@deriving sexp_of, equal]

let which t = t.which
let worktrees t = t.worktrees
let query t = t.query

let typed t =
  let q = String.strip t.query in
  match t.which with
  | Base
    when (not (String.is_empty q))
         && not (List.exists t.items ~f:(fun i -> String.equal i.label q)) ->
    [ { Item.choice = Base q
      ; group = "Typed"
      ; label = q
      ; detail = "compare with this branch, tag or commit"
      ; marked = false
      }
    ]
  | Base | Target | Since _ -> []
;;

let visible t =
  (if String.is_empty t.query
   then t.items
   else Prigh_ui.Fuzzy.rank ~query:t.query t.items ~key:Item.search)
  @ typed t
;;

let create ?(worktrees = []) which items =
  { which
  ; items
  ; worktrees
  ; query = ""
  ; selected =
      Option.value_map
        (List.findi items ~f:(fun _ i -> i.marked))
        ~default:0
        ~f:fst
  }
;;

let selected t = t.selected
let selected_item t = List.nth (visible t) t.selected
let set_query t query = { t with query; selected = 0 }

let move t delta =
  let n = List.length (visible t) in
  { t with selected = Int.max 0 (Int.min (n - 1) (t.selected + delta)) }
;;

let find t ~id = List.find (visible t) ~f:(fun i -> String.equal (Item.id i) id)

let item ?(detail = "") ~group ~marked choice label =
  { Item.choice; group; label; detail; marked }
;;

let targets ~worktrees ~(reviewed : Git_diff.Target.t) (d : Git_diff.t) =
  let worktrees =
    List.filter worktrees ~f:(fun (w : Worktree.t) -> not w.prunable)
  in
  let checked_out = List.filter_map worktrees ~f:(fun w -> w.branch) in
  let reviewing_root =
    match reviewed with
    | Rev _ -> None
    | Dir _ -> Some d.root
  in
  let worktree_items =
    List.map worktrees ~f:(fun (w : Worktree.t) ->
      let target : Git_diff.Target.t =
        if w.current then Dir None else Dir (Some w.path)
      in
      item
        ~group:"Worktrees"
        ~detail:(if w.current then w.path ^ " · this session's" else w.path)
        ~marked:(Option.exists reviewing_root ~f:(String.equal w.path))
        (Review target)
        (Worktree.label w))
  in
  (* The page's worktree, when the listing is not in yet. *)
  let worktree_items =
    match reviewing_root with
    | Some root when not (List.exists worktree_items ~f:(fun i -> i.marked)) ->
      item
        ~group:"Worktrees"
        ~detail:root
        ~marked:true
        (Review reviewed)
        (Option.value d.branch ~default:(Filename.basename root))
      :: worktree_items
    | Some _ | None -> worktree_items
  in
  let branch_items group names =
    List.filter names ~f:(fun b ->
      not (List.mem checked_out b ~equal:String.equal))
    |> List.map ~f:(fun b ->
      item
        ~group
        ~detail:"not checked out: read-only"
        ~marked:(Git_diff.Target.equal reviewed (Rev b))
        (Review (Rev b))
        b)
  in
  let reviewed_rev =
    match reviewed with
    | Rev rev
      when not
             (List.mem (d.branches @ d.remote_branches) rev ~equal:String.equal)
      ->
      [ item
          ~group:"Branches"
          ~detail:"not checked out: read-only"
          ~marked:true
          (Review reviewed)
          rev
      ]
    | Rev _ | Dir _ -> []
  in
  create
    ~worktrees
    Target
    (worktree_items
     @ reviewed_rev
     @ branch_items "Branches" d.branches
     @ branch_items "Remote branches" d.remote_branches)
;;

let bases (d : Git_diff.t) =
  let reviewed = Option.first_some d.rev d.branch in
  let base group ?detail name =
    item ~group ?detail ~marked:(String.equal d.base name) (Base name) name
  in
  let names group names ~detail =
    List.filter names ~f:(fun b ->
      not (Option.exists reviewed ~f:(String.equal b)))
    |> List.map ~f:(fun b -> base group ~detail b)
  in
  let known =
    List.concat [ d.branches; d.remote_branches; d.tags ]
    @ List.map d.commits ~f:(fun c -> c.sha)
  in
  (* A base typed before, or a commit not among the last few. *)
  let other =
    if String.equal d.base "HEAD" || List.mem known d.base ~equal:String.equal
    then []
    else
      [ base "Typed" ~detail:"compare with this branch, tag or commit" d.base ]
  in
  create
    Base
    ((match d.rev with
      | Some _ -> []
      | None ->
        [ base "Uncommitted" ~detail:"the uncommitted changes only" "HEAD" ])
     @ other
     @ names "Branches" d.branches ~detail:"local branch"
     @ names
         "Remote branches"
         d.remote_branches
         ~detail:"remote-tracking branch"
     @ names "Tags" d.tags ~detail:"tag"
     @ List.map d.commits ~f:(fun c ->
       item
         ~group:"Recent commits"
         ~detail:c.subject
         ~marked:(String.equal d.base c.sha)
         (Base c.sha)
         (Git_diff.Commit.short c)))
;;

let refresh t ~fresh =
  let t = { fresh with query = t.query; selected = t.selected } in
  move t 0
;;

let since ~path items = create (Since path) items
