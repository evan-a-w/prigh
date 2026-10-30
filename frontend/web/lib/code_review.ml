open! Core
open! Import
module Diff_files = Prigh_ui.Diff_files
module File_approval = Prigh_ui.File_approval

module Mode = struct
  type t =
    | Comment
    | Edit
  [@@deriving sexp_of, equal]
end

module Layout = struct
  type t =
    | Unified
    | Full
    | Since
  [@@deriving sexp_of, equal]
end

module Pop = struct
  type t =
    { left : int
    ; top : int
    }
  [@@deriving sexp_of, equal]
end

module Selection = struct
  type t =
    { path : string
    ; range : Review_comment.Range.t
    ; anchor : string
    ; from : Code_selection.Point.t option
    ; pop : Pop.t option
    }
  [@@deriving sexp_of, equal]
end

module Draft = struct
  type t =
    { path : string
    ; range : Review_comment.Range.t
    ; anchor : string
    ; text : string
    ; busy : bool
    }
  [@@deriving sexp_of, equal]
end

module Content = struct
  type t =
    | Loading
    | Loaded of
        { text : string
        ; md5 : string option
        }
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Editor = struct
  type t =
    { path : string
    ; text : string
    ; md5 : string option
    ; dirty : bool
    ; saving : bool
    ; conflict : bool
    ; error : string option
    }
  [@@deriving sexp_of, equal]
end

module Target = struct
  type t =
    { reviewed : Git_diff.Target.t
    ; base : string option
    ; direct : bool
    }
  [@@deriving sexp_of, equal]

  let default = { reviewed = Dir None; base = None; direct = false }
  let own t = Git_diff.Target.equal t.reviewed (Dir None)
end

type t =
  { session : string option
  ; target : Target.t
  ; fallback : Target.t option
  ; requested : Target.t
  ; own_root : string option
  ; all : Review.t
  ; review : Review.t
  ; diff : Git_diff.t option
  ; files : Diff_files.File.t list
  ; diff_error : string option
  ; loading : bool
  ; again : bool
  ; open_ : bool
  ; current : string option
  ; mode : Mode.t
  ; layout : Layout.t
  ; selection : Selection.t option
  ; draft : Draft.t option
  ; hover : string option
  ; replies : string String.Map.t
  ; busy : String.Set.t
  ; contents : Content.t String.Map.t
  ; editor : Editor.t option
  ; submitting : bool
  ; since : Review_since.File.t String.Map.t
  ; since_loading : bool
  ; since_again : bool
  ; picked : (int * Review_since.File.t option) String.Map.t
  ; views : Layout.t String.Map.t
  ; unfolded : String.Set.t
  ; marked : String.Set.t
  ; mark_from : string option
  }
[@@deriving sexp_of]

let empty =
  { session = None
  ; target = Target.default
  ; fallback = None
  ; requested = Target.default
  ; own_root = None
  ; all = Review.empty
  ; review = Review.empty
  ; diff = None
  ; files = []
  ; diff_error = None
  ; loading = false
  ; again = false
  ; open_ = false
  ; current = None
  ; mode = Comment
  ; layout = Unified
  ; selection = None
  ; draft = None
  ; hover = None
  ; replies = String.Map.empty
  ; busy = String.Set.empty
  ; contents = String.Map.empty
  ; editor = None
  ; submitting = false
  ; since = String.Map.empty
  ; since_loading = false
  ; since_again = false
  ; picked = String.Map.empty
  ; views = String.Map.empty
  ; unfolded = String.Set.empty
  ; marked = String.Set.empty
  ; mark_from = None
  }
;;

module Reply = struct
  type t =
    | Diff
    | Review
    | Comment_added
    | Changed of string option
    | Replied of string
    | Decided of string
    | Content of string
    | Shown of string
    | Saved of string
    | Submitted
    | Since of string
    | Since_picked of
        { root : string
        ; path : string
        ; index : int
        }
  [@@deriving sexp_of, equal]
end

module Out = struct
  type t =
    | Rpc of
        { method_ : Request.Method.t
        ; reply : Reply.t
        }
    | Get_diff of Target.t
    | Toast of
        { text : string
        ; error : bool
        }
    | Focus of string
    | Clear_text_selection
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Open of string option
    | Show of Target.t
    | Close
    | Toggle
    | Go of string
    | Step of int
    | Set_mode of Mode.t
    | Set_layout of Layout.t
    | Set_view of
        { path : string
        ; layout : Layout.t
        }
    | Pick_since of
        { path : string
        ; index : int
        }
    | Follow_latest of string
    | Unfold of string
    | Select_line of
        { path : string
        ; side : Review_comment.Side.t
        ; line : int
        ; extend : bool
        }
    | Select_text of
        { path : string
        ; start : Code_selection.Point.t
        ; end_ : Code_selection.Point.t
        ; pop : Pop.t option
        }
    | Clear_selection
    | Start_comment
    | Hover of string option
    | Draft_input of string
    | Draft_cancel
    | Draft_send
    | Reply_input of
        { id : string
        ; text : string
        }
    | Reply_send of string
    | Set_status of
        { id : string
        ; status : Review_comment.Status.t
        }
    | Delete of string
    | Decide of
        { id : string
        ; post : int
        ; accept : bool
        }
    | Toggle_approval of string
    | Approve_files of
        { paths : string list
        ; approve : bool
        }
    | Mark of
        { path : string
        ; range : bool
        }
    | Unmark_all
    | Submit
    | Edit_input of string
    | Save
    | Reload
    | Refresh
  [@@deriving sexp_of]
end

let rpc method_ reply = Out.Rpc { method_; reply }
let toast ?(error = false) text = Out.Toast { text; error }
let root t = Option.map t.diff ~f:(fun d -> d.root)

let read_only t =
  match t.target.reviewed with
  | Rev rev -> Some rev
  | Dir _ -> None
;;

let not_checked_out rev =
  sprintf
    "%s is not checked out, so it is read-only here: Check out into a worktree \
     to change it."
    rev
;;

(* What the session's review keys this worktree's comments and approvals
   by. *)
let review_root t = Option.map t.diff ~f:Git_diff.review_root

(* The reviewed worktree's part of [all]; before its diff is in, all of the
   session's own (as before worktrees were told apart), none of another. *)
let scoped t =
  let review =
    match review_root t with
    | Some root ->
      Review.scope
        t.all
        ~root
        ~own:
          (Target.own t.target
           || Option.equal String.equal t.own_root (Some root))
    | None -> if Target.own t.target then t.all else Review.empty
  in
  { t with review }
;;

let with_all t all = scoped { t with all }

(* Changes to one comment, made to every worktree's review. *)
let map_comments t ~f = with_all t { t.all with comments = f t.all.comments }

let relative t ~abs =
  Option.bind
    (Option.bind (root t) ~f:(fun root ->
       Option.some_if (Option.is_none (read_only t)) root))
    ~f:(fun root ->
      String.chop_prefix
        abs
        ~prefix:(if String.equal root "/" then "/" else root ^ "/"))
;;

let absolute t path =
  match root t with
  | Some root -> Filename.concat root path
  | None -> path
;;

let file t path =
  List.find t.files ~f:(fun (f : Diff_files.File.t) -> String.equal f.path path)
;;

let approval t f = File_approval.status t.review f
let history t path = Review.history t.review path

let since t path =
  match Map.find t.picked path with
  | Some (_, picked) -> picked
  | None -> Map.find t.since path
;;

let changed_since t path =
  Option.exists (Map.find t.since path) ~f:(fun s -> s.changed)
;;

let layout_of t path : Layout.t =
  let chosen =
    match Map.find t.views path with
    | Some layout -> layout
    | None -> if changed_since t path then Since else t.layout
  in
  match chosen with
  | Since when List.is_empty (history t path) -> Unified
  | layout -> layout
;;

let comment_path t id =
  List.find_map t.review.comments ~f:(fun (c : Review_comment.t) ->
    Option.some_if (String.equal c.id id) c.path)
;;

(* The user commented, replied or decided on [path]: it shows what changed
   since, as of its latest action. *)
let acted t path =
  { t with
    views = Map.set t.views ~key:path ~data:Layout.Since
  ; picked = Map.remove t.picked path
  }
;;

let get_since t =
  match root t with
  | None -> t, []
  | Some _ when List.is_empty t.review.snapshots ->
    { t with since = String.Map.empty }, []
  | Some _ when t.since_loading -> { t with since_again = true }, []
  | Some root ->
    ( { t with since_loading = true }
    , [ rpc (Review_since { root; path = None; index = None }) (Since root) ] )
;;

let get_picked t ~path ~index =
  match root t with
  | None -> t, []
  | Some root ->
    ( t
    , [ rpc
          (Review_since { root; path = Some path; index = Some index })
          (Since_picked { root; path; index })
      ] )
;;

(* New review actions on the reviewed worktree: what changed since them. *)
let with_all_since t all =
  let before = t.review.snapshots in
  let t = with_all t all in
  if List.equal Review.Snapshot.equal before t.review.snapshots
  then t, []
  else get_since t
;;

let approved t = File_approval.approved t.review t.files
let unsent t = Review.unsent t.review

let mark t ~abs =
  match relative t ~abs with
  | None -> `None
  | Some rel ->
    (match file t rel with
     | Some f -> `File (approval t f)
     | None ->
       let prefix = rel ^ "/" in
       (match
          List.count t.files ~f:(fun f -> String.is_prefix f.path ~prefix)
        with
        | 0 -> `None
        | n -> `Dir n))
;;

let get_diff t =
  if t.loading
  then { t with again = true }, []
  else
    { t with loading = true; requested = t.target }, [ Out.Get_diff t.target ]
;;

let refresh = get_diff
let changed t ~watching = if watching || t.open_ then get_diff t else t, []

(* What the page's file should read: its text, unless known or on its
   way. *)
let fetch_content t path =
  match Map.find t.contents path with
  | Some (Loading | Loaded _) -> t, []
  | Some (Failed _) | None ->
    ( { t with contents = Map.set t.contents ~key:path ~data:Content.Loading }
    , [ (match read_only t with
         | Some rev -> rpc (Git_show { dir = None; rev; path }) (Shown path)
         | None -> rpc (Fs_preview { path = absolute t path }) (Content path))
      ] )
;;

let refetch_content t path =
  fetch_content { t with contents = Map.remove t.contents path } path
;;

let paths t = List.map t.files ~f:(fun (f : Diff_files.File.t) -> f.path)

(* The page shows [path]: its text is read, the selection and draft of
   another file go. *)
let show t path =
  let t =
    { t with
      current = Some path
    ; selection =
        Option.filter t.selection ~f:(fun s -> String.equal s.path path)
    ; draft =
        Option.filter t.draft ~f:(fun (d : Draft.t) ->
          String.equal d.path path
          || not (String.is_empty (String.strip d.text)))
    }
  in
  let t, outs = fetch_content t path in
  let t =
    match t.mode, t.editor with
    | Edit, Some e when (not (String.equal e.path path)) && not e.dirty ->
      { t with editor = None }
    | _ -> t
  in
  t, outs
;;

(* The current file, or the first, as the diff changes under it. *)
let settle_current t =
  match t.current, t.files with
  | Some path, _ when List.mem (paths t) path ~equal:String.equal -> t, []
  | Some path, _
    when Option.exists t.editor ~f:(fun e ->
           e.dirty && String.equal e.path path) -> t, []
  | _, f :: _ when t.open_ -> show t f.path
  | _, [] -> { t with current = None }, []
  | _ -> t, []
;;

let open_editor t path =
  match Map.find t.contents path with
  | Some (Loaded { text; md5 = Some md5 }) ->
    { t with
      editor =
        Some
          { path
          ; text
          ; md5 = Some md5
          ; dirty = false
          ; saving = false
          ; conflict = false
          ; error = None
          }
    }
  | Some (Loaded { md5 = None; _ }) ->
    { t with
      editor =
        Some
          { path
          ; text = ""
          ; md5 = None
          ; dirty = false
          ; saving = false
          ; conflict = false
          ; error =
              Some "This file is too large to edit here: open it in an editor."
          }
    }
  | Some (Loading | Failed _) | None -> t
;;

let step t by =
  match paths t with
  | [] -> t, []
  | paths ->
    let n = List.length paths in
    let i =
      match t.current with
      | None -> if by > 0 then 0 else n - 1
      | Some p ->
        (match List.findi paths ~f:(fun _ q -> String.equal p q) with
         | Some (i, _) -> (((i + by) mod n) + n) mod n
         | None -> 0)
    in
    show t (List.nth_exn paths i)
;;

(* The whole file with the hunks' changes inline: the lines between hunks
   are the file's own. *)
let full_rows (hunk_rows : Diff_view.Row.t list) (lines : string array) =
  let out = ref [] in
  let old_n = ref 1 in
  let new_n = ref 1 in
  let fill_to last =
    while !new_n <= last && !new_n <= Array.length lines do
      out
      := Diff_view.Row.Line
           { kind = `Same
           ; old_line = Some !old_n
           ; new_line = Some !new_n
           ; text = lines.(!new_n - 1)
           }
         :: !out;
      incr old_n;
      incr new_n
    done
  in
  List.iter hunk_rows ~f:(fun (row : Diff_view.Row.t) ->
    match row with
    | Hunk header ->
      (match Diff_view.hunk_starts header with
       | Some (o, n) ->
         fill_to (n - 1);
         old_n := Int.max o 1;
         new_n := Int.max n 1
       | None -> ())
    | Line { kind; _ } ->
      out := row :: !out;
      (match kind with
       | `Same ->
         incr old_n;
         incr new_n
       | `Added -> incr new_n
       | `Removed -> incr old_n));
  fill_to (Array.length lines);
  List.rev !out
;;

let plain_rows lines =
  Array.to_list
    (Array.mapi lines ~f:(fun i text ->
       Diff_view.Row.Line
         { kind = `Same
         ; old_line = Some (i + 1)
         ; new_line = Some (i + 1)
         ; text
         }))
;;

let text_of t path =
  match Map.find t.contents path with
  | Some (Loaded { text; _ }) -> Some text
  | Some (Loading | Failed _) | None -> None
;;

module Rows = struct
  (* Made once per file version and layout, so that a key press or a
     streamed delta does not split a file again, and so that the rows stay
     the same values (the view's caches are keyed by them). *)
  type key =
    { path : string
    ; hash : string option
    ; text : string option
    ; full : bool
    ; since : string option
    }

  let memo : (string, key * Diff_view.Row.t list) Hashtbl.t =
    Hashtbl.create (module String)
  ;;

  let same a b =
    String.equal a.path b.path
    && Option.equal String.equal a.hash b.hash
    && Option.equal phys_equal a.text b.text
    && Bool.equal a.full b.full
    && Option.equal String.equal a.since b.since
  ;;

  let find key ~make =
    match Hashtbl.find memo key.path with
    | Some (k, rows) when same k key -> rows
    | Some _ | None ->
      let rows = make () in
      Hashtbl.set memo ~key:key.path ~data:(key, rows);
      rows
  ;;
end

let since_diff t path =
  match layout_of t path, since t path with
  | Since, Some { changed = true; diff; _ } when not (String.is_empty diff) ->
    Some diff
  | _ -> None
;;

let rows t ~path =
  let file = file t path in
  let text = text_of t path in
  let since = since_diff t path in
  (* Until something changed since the action: the changes as the page's
     layout shows them. *)
  let full =
    match layout_of t path with
    | Full -> true
    | Since -> Option.is_none since && Layout.equal t.layout Full
    | Unified -> false
  in
  Rows.find
    { path
    ; hash = Option.map file ~f:(fun f -> f.hash)
    ; text = (if full || Option.is_none file then text else None)
    ; full
    ; since
    }
    ~make:(fun () ->
      let lines () =
        Array.of_list (String.split_lines (Option.value text ~default:""))
      in
      match since, file with
      | Some diff, _ ->
        (match Diff_files.parse diff with
         | [] -> []
         | f :: _ -> Diff_view.unified_rows (String.concat ~sep:"\n" f.hunks))
      | None, None -> plain_rows (lines ())
      | None, Some f ->
        let hunk_rows =
          Diff_view.unified_rows (String.concat ~sep:"\n" f.hunks)
        in
        if full && Option.is_some text
        then full_rows hunk_rows (lines ())
        else hunk_rows)
;;

let lines t path =
  Option.map (text_of t path) ~f:(fun text ->
    Array.of_list (String.split_lines text))
;;

(* In the since-view the removed lines are the file as it was at the action,
   not what the change removed: they take no comments. *)
let on_snapshot t ~path (range : Review_comment.Range.t) =
  Option.is_some (since_diff t path) && Review_comment.Side.equal range.side Old
;;

let snapshot_lines =
  "The removed lines here are the file as it was at that action: comment on \
   the current lines, or on removed ones in Changes."
;;

let select t ~path ~start ~end_ ~pop =
  match
    Code_selection.resolve (rows t ~path) ~lines:(lines t path) ~start ~end_
  with
  | Some (range, _) when on_snapshot t ~path range ->
    None, [ toast snapshot_lines; Out.Clear_text_selection ]
  | Some (range, anchor) ->
    Some { Selection.path; range; anchor; from = None; pop }, []
  | None -> None, []
;;

(* Approving or taking back each file of [paths] that is not so already. *)
let approve_rpcs t paths ~approve =
  List.filter_map paths ~f:(fun path ->
    match file t path with
    | None -> None
    | Some f ->
      (match approval t f, Review.approval t.review path with
       | Approved, Some { root; _ } when not approve ->
         Some (rpc (Review_unapprove { root; path }) (Changed None))
       | (Changed | Unapproved), _ when approve ->
         Some
           (rpc
              (Review_approve { root = review_root t; path; hash = f.hash })
              (Changed None))
       | _ -> None))
;;

(* Approving [approving] moves on to the next file after it that is not
   approved, round the list; when there is none, it stays. *)
let advance t ~approving =
  let all = paths t in
  let unapproved path =
    (not (String.equal path approving))
    && Option.exists (file t path) ~f:(fun f ->
      not (File_approval.Status.equal (approval t f) Approved))
  in
  let after, before =
    match List.findi all ~f:(fun _ p -> String.equal p approving) with
    | Some (i, _) -> List.drop all (i + 1), List.take all i
    | None -> all, []
  in
  match List.find (after @ before) ~f:unapproved with
  | Some next -> show t next
  | None ->
    let n = List.length all in
    ( t
    , [ toast
          (if n = 1
           then "The file is approved: send your comments (s) or commit it (C)."
           else
             sprintf
               "All %d files are approved: send your comments (s) or commit \
                them (C)."
               n)
      ] )
;;

let file_dom_id path =
  "review-file-"
  ^ String.map path ~f:(fun c -> if Char.is_alphanum c then c else '-')
;;

let marked t = List.filter (paths t) ~f:(Set.mem t.marked)
let comment_busy t id = { t with busy = Set.add t.busy id }

let rec update t (action : Action.t) =
  match action with
  | Open path ->
    let t = { t with open_ = true } in
    let t, diff = get_diff t in
    let t, more =
      match path with
      | Some path -> show t path
      | None ->
        (match t.current with
         | Some path -> show t path
         | None -> settle_current t)
    in
    t, diff @ more @ [ Out.Focus "review-page" ]
  | Show target when Target.equal target t.target -> update t (Open None)
  | Show target ->
    (* Another worktree or base: its own files, from the first. *)
    let t =
      { t with
        target
      ; fallback =
          (match t.diff, t.diff_error with
           | Some _, None -> Some t.target
           | _ -> t.fallback)
      ; diff = None
      ; files = []
      ; diff_error = None
      ; current = None
      ; selection = None
      ; draft = None
      ; hover = None
      ; contents = String.Map.empty
      ; editor = None
      ; mode = Comment
      ; since = String.Map.empty
      ; picked = String.Map.empty
      ; views = String.Map.empty
      ; unfolded = String.Set.empty
      ; marked = String.Set.empty
      ; mark_from = None
      }
    in
    update (scoped t) (Open None)
  | Close -> { t with open_ = false; selection = None }, [ Out.Focus "editor" ]
  | Toggle ->
    if t.open_
    then { t with open_ = false; selection = None }, []
    else (
      let t = { t with open_ = true } in
      let t, diff = get_diff t in
      let t, more =
        match t.current with
        | Some path -> show t path
        | None -> settle_current t
      in
      t, diff @ more @ [ Out.Focus "review-page" ])
  | Go path ->
    let t, outs = show t path in
    let t, diff = get_diff t in
    t, outs @ diff
  | Step by -> step t by
  | Set_mode Comment -> { t with mode = Comment }, []
  | Set_mode Edit ->
    (match t.current, read_only t with
     | _, Some rev -> t, [ toast (not_checked_out rev) ]
     | None, None ->
       t, [ toast "No file to edit: the working tree has no changes." ]
     | Some path, None ->
       let t = { t with mode = Edit; selection = None } in
       let t, outs = fetch_content t path in
       let t =
         match t.editor with
         | Some e when String.equal e.path path -> t
         | Some _ | None -> open_editor t path
       in
       t, outs @ [ Out.Focus "review-editor" ])
  | Set_layout layout ->
    (match t.current with
     | Some path -> update t (Set_view { path; layout })
     | None -> { t with layout }, [])
  | Set_view { path; layout } ->
    let t =
      { t with
        layout
      ; views = Map.set t.views ~key:path ~data:layout
      ; selection = None
      }
    in
    (match layout with
     | Full -> fetch_content t path
     | Since when List.is_empty (history t path) ->
       ( t
       , [ toast
             (sprintf
                "No review actions on %s yet: approve it or comment on it \
                 first."
                path)
         ] )
     | Unified | Since -> t, [])
  | Follow_latest path ->
    ( { t with
        views = Map.set t.views ~key:path ~data:Since
      ; picked = Map.remove t.picked path
      ; selection = None
      }
    , [] )
  | Unfold key ->
    ( { t with
        unfolded =
          (if Set.mem t.unfolded key
           then Set.remove t.unfolded key
           else Set.add t.unfolded key)
      }
    , [] )
  | Pick_since { path; index } ->
    let t =
      { t with views = Map.set t.views ~key:path ~data:Since; selection = None }
    in
    if index >= List.length (history t path) - 1
    then { t with picked = Map.remove t.picked path }, []
    else
      get_picked
        { t with picked = Map.set t.picked ~key:path ~data:(index, None) }
        ~path
        ~index
  | Select_line { path; side; line; extend } ->
    let here = { Code_selection.Point.side; line; offset = 0 } in
    let from =
      match t.selection with
      | Some { path = p; from = Some from; _ }
        when extend && String.equal p path -> from
      | Some _ | None -> here
    in
    let t, outs = fetch_content t path in
    (match
       Code_selection.whole_lines
         (rows t ~path)
         ~lines:(lines t path)
         ~from
         ~to_:here
     with
     | Some (range, _) when on_snapshot t ~path range ->
       t, outs @ [ toast snapshot_lines ]
     | Some (range, anchor) ->
       ( { t with
           selection =
             Some { path; range; anchor; from = Some from; pop = None }
         }
       , outs @ [ Out.Clear_text_selection ] )
     | None -> t, outs @ [ toast "The file is still being read: try again." ])
  | Select_text { path; start; end_; pop } ->
    let selection, outs = select t ~path ~start ~end_ ~pop in
    { t with selection }, outs
  | Clear_selection ->
    if Option.is_some t.selection
    then { t with selection = None }, [ Out.Clear_text_selection ]
    else t, []
  | Hover hover -> { t with hover }, []
  | Start_comment ->
    (match t.selection with
     | None ->
       ( t
       , [ toast
             "Select the code to comment on first: drag across it, then \
              Comment (or c)."
         ] )
     | Some s ->
       ( { t with
           draft =
             Some
               { path = s.path
               ; range = s.range
               ; anchor = s.anchor
               ; text = ""
               ; busy = false
               }
         ; selection = None
         }
       , [ Out.Focus "review-draft" ] ))
  | Draft_input text ->
    { t with draft = Option.map t.draft ~f:(fun d -> { d with text }) }, []
  | Draft_cancel -> { t with draft = None }, []
  | Draft_send ->
    (match t.draft, review_root t with
     | Some d, _ when String.is_empty (String.strip d.text) ->
       t, [ toast "Write the comment first." ]
     | Some ({ busy = false; _ } as d), Some root ->
       ( { t with draft = Some { d with busy = true } }
       , [ rpc
             (Review_comment
                { root
                ; path = d.path
                ; range = d.range
                ; anchor = d.anchor
                ; text = d.text
                })
             Comment_added
         ] )
     | Some _, None ->
       t, [ toast ~error:true "The changes are still being read: try again." ]
     | Some { busy = true; _ }, _ | None, _ -> t, [])
  | Reply_input { id; text } ->
    { t with replies = Map.set t.replies ~key:id ~data:text }, []
  | Reply_send id ->
    (match Map.find t.replies id with
     | Some text when not (String.is_empty (String.strip text)) ->
       comment_busy t id, [ rpc (Review_reply { id; text }) (Replied id) ]
     | Some _ | None -> t, [ toast "Write the reply first." ])
  | Set_status { id; status } ->
    ( comment_busy t id
    , [ rpc (Review_set_status { id; status }) (Changed (Some id)) ] )
  | Delete id ->
    comment_busy t id, [ rpc (Review_delete { id }) (Changed (Some id)) ]
  | Decide { accept = true; _ } when Option.is_some (read_only t) ->
    t, [ toast (not_checked_out (Option.value (read_only t) ~default:"")) ]
  | Decide { id; post; accept } ->
    comment_busy t id, [ rpc (Review_decide { id; post; accept }) (Decided id) ]
  | Toggle_approval path ->
    (match file t path with
     | None -> t, [ toast (sprintf "%s has no changes to approve." path) ]
     | Some f ->
       (match approval t f with
        | Approved -> t, approve_rpcs t [ path ] ~approve:false
        | Changed | Unapproved ->
          let outs = approve_rpcs t [ path ] ~approve:true in
          if t.open_ && Option.exists t.current ~f:(String.equal path)
          then (
            let t, more = advance t ~approving:path in
            t, outs @ more)
          else t, outs))
  | Approve_files { paths; approve } ->
    (match approve_rpcs t paths ~approve with
     | [] ->
       ( t
       , [ toast
             (if approve
              then "Already approved, as they are now."
              else "None of them is approved.")
         ] )
     | outs -> t, outs)
  | Mark { path; range } ->
    let all = paths t in
    if not (List.mem all path ~equal:String.equal)
    then t, []
    else (
      let marked =
        match range, Option.first_some t.mark_from t.current with
        | true, Some from when List.mem all from ~equal:String.equal ->
          let index p =
            fst (List.findi_exn all ~f:(fun _ q -> String.equal p q))
          in
          let lo = Int.min (index from) (index path)
          and hi = Int.max (index from) (index path) in
          List.filteri all ~f:(fun i _ -> i >= lo && i <= hi)
          |> List.fold ~init:t.marked ~f:Set.add
        | _ ->
          if Set.mem t.marked path
          then Set.remove t.marked path
          else Set.add t.marked path
      in
      ( { t with marked; mark_from = (if range then t.mark_from else Some path) }
      , [] ))
  | Unmark_all -> { t with marked = String.Set.empty; mark_from = None }, []
  | Submit ->
    if unsent t = 0
    then
      t, [ toast "No new comments to send: select some code to comment on it." ]
    else if t.submitting
    then t, []
    else
      ( { t with submitting = true }
      , [ rpc (Review_submit { root = review_root t }) Submitted ] )
  | Edit_input text ->
    ( { t with
        editor =
          Option.map t.editor ~f:(fun e ->
            { e with text; dirty = true; error = None })
      }
    , [] )
  | Save ->
    (match t.editor with
     | None | Some { saving = true; _ } -> t, []
     | Some { md5 = None; _ } ->
       t, [ toast ~error:true "This file cannot be saved from here." ]
     | Some e ->
       ( { t with editor = Some { e with saving = true; error = None } }
       , [ rpc
             (Fs_write
                { path = absolute t e.path
                ; text = e.text
                ; expected = (if e.conflict then None else e.md5)
                ; review_root = root t
                })
             (Saved e.path)
         ] ))
  | Reload ->
    (match t.editor with
     | None -> t, []
     | Some e ->
       let t, outs = refetch_content { t with editor = None } e.path in
       t, outs)
  | Refresh -> get_diff t
;;

let set_review t review = with_all_since t review

let set_session t session ~watching =
  if Option.equal String.equal t.session session
  then t, []
  else (
    let t = { empty with session; layout = t.layout } in
    (* [empty]'s target: the session's own worktree. *)
    match session with
    | None -> t, []
    | Some _ ->
      let t, diff = if watching then get_diff t else t, [] in
      t, rpc Get_review Review :: diff)
;;

let preview_text t ~abs ~text ~md5 =
  match relative t ~abs with
  | None -> t
  | Some path ->
    { t with
      contents = Map.set t.contents ~key:path ~data:(Loaded { text; md5 })
    }
;;

(* The files whose change is different now: their texts are read again. *)
let refresh_contents t ~before =
  let stale =
    List.filter_map t.files ~f:(fun (f : Diff_files.File.t) ->
      match
        List.find before ~f:(fun (b : Diff_files.File.t) ->
          String.equal b.path f.path)
      with
      | Some b when String.equal b.hash f.hash -> None
      | Some _ | None -> Some f.path)
  in
  List.fold stale ~init:(t, []) ~f:(fun (t, outs) path ->
    if not (Map.mem t.contents path)
    then t, outs
    else (
      let t, more = refetch_content t path in
      t, outs @ more))
;;

let error_text e = String.capitalize e
let root_of = root

let rec reply t (why : Reply.t) (result : (Json.t, string) Result.t) =
  match why, result with
  | Diff, _ when not (Target.equal t.requested t.target) ->
    (* For the worktree or base reviewed before. *)
    get_diff { t with loading = false; again = false }
  | Diff, Error e
    when String.is_prefix e ~prefix:"unknown revision"
         && Option.is_some t.fallback ->
    (* A base or branch typed wrong: back to what was reviewed, saying which
       names are close. *)
    let back = Option.value_exn t.fallback in
    let t, outs =
      update
        { t with loading = false; again = false; fallback = None }
        (Show back)
    in
    t, toast ~error:true (error_text e) :: outs
  | Diff, Error e ->
    let t = { t with loading = false; diff_error = Some (error_text e) } in
    if t.again then get_diff { t with again = false } else t, []
  | Diff, Ok json ->
    (match Git_diff.of_json json with
     | Error e ->
       { t with loading = false; diff_error = Some (Error.to_string_hum e) }, []
     | Ok diff ->
       let before = t.files in
       let t =
         scoped
           { t with
             loading = false
           ; fallback = None
           ; diff = Some diff
           ; files = Diff_files.parse diff.diff
           ; diff_error = None
           ; own_root =
               (if Target.own t.target then Some diff.root else t.own_root)
           }
       in
       let t, contents = refresh_contents t ~before in
       let t, current = settle_current t in
       let t, since = get_since t in
       let t, again =
         if t.again then get_diff { t with again = false } else t, []
       in
       t, contents @ current @ since @ again)
  | Review, Error e -> t, [ toast ~error:true (error_text e) ]
  | Review, Ok json ->
    (match Review.of_json json with
     | Ok review -> with_all_since t review
     | Error e -> t, [ toast ~error:true (Error.to_string_hum e) ])
  | Comment_added, Error e ->
    ( { t with draft = Option.map t.draft ~f:(fun d -> { d with busy = false }) }
    , [ toast ~error:true (error_text e) ] )
  | Comment_added, Ok json ->
    let t = { t with draft = None; selection = None } in
    (match Review_comment.of_json json with
     | Ok c ->
       ( map_comments (acted t c.path) ~f:(fun comments ->
           if List.exists comments ~f:(fun o -> String.equal o.id c.id)
           then comments
           else comments @ [ c ])
       , [] )
     | Error _ -> t, [])
  | (Decided id | Replied id), result ->
    let t =
      match result, comment_path t id with
      | Ok _, Some path -> acted t path
      | _ -> t
    in
    let t, outs = reply t (Changed (Some id)) result in
    (match why with
     | Replied _ -> t, outs
     | _ ->
       (* An accepted change edited the file. *)
       let t, diff = get_diff t in
       t, outs @ diff)
  | Changed id, result ->
    let t =
      match id with
      | Some id -> { t with busy = Set.remove t.busy id }
      | None -> t
    in
    (match result with
     | Error e -> t, [ toast ~error:true (error_text e) ]
     | Ok json ->
       let t =
         match id with
         | Some id -> { t with replies = Map.remove t.replies id }
         | None -> t
       in
       (* An approval answers with the review; a comment with itself (the
          [review] event brings the rest, its snapshot too). *)
       (match Review.of_json json with
        | Ok review when Option.is_none id -> with_all_since t review
        | _ ->
          (match Review_comment.of_json json with
           | Ok c ->
             ( map_comments t ~f:(fun comments ->
                 List.map comments ~f:(fun o ->
                   if String.equal o.id c.id then c else o))
             , [] )
           | Error _ ->
             (* Deleted. *)
             ( map_comments t ~f:(fun comments ->
                 List.filter comments ~f:(fun o ->
                   not (Option.exists id ~f:(String.equal o.id))))
             , [] ))))
  | Content path, Error e ->
    ( { t with
        contents = Map.set t.contents ~key:path ~data:(Failed (error_text e))
      }
    , [] )
  | Shown path, Error e ->
    ( { t with
        contents = Map.set t.contents ~key:path ~data:(Failed (error_text e))
      }
    , [] )
  | Shown path, Ok json ->
    let content : Content.t =
      match Git_diff.shown_of_json json with
      | Ok (text, _) -> Loaded { text; md5 = None }
      | Error e -> Failed (Error.to_string_hum e)
    in
    { t with contents = Map.set t.contents ~key:path ~data:content }, []
  | Content path, Ok json ->
    let content : Content.t =
      match File_preview.of_json json with
      | Ok { content = Text { text; truncated = false; md5 }; _ } ->
        Loaded { text; md5 }
      | Ok { content = Text { text; truncated = true; _ }; _ } ->
        Loaded { text; md5 = None }
      | Ok { content = Image _ | Binary; _ } -> Failed "not a text file"
      | Error e -> Failed (Error.to_string_hum e)
    in
    let t = { t with contents = Map.set t.contents ~key:path ~data:content } in
    let t =
      match t.mode, t.editor, t.current with
      | Edit, None, Some current when String.equal current path ->
        open_editor t path
      | _ -> t
    in
    t, []
  | Saved path, Error e ->
    ( { t with
        editor =
          Option.map t.editor ~f:(fun e' ->
            if String.equal e'.path path
            then
              { e' with
                saving = false
              ; error = Some e
              ; conflict =
                  String.is_substring e ~substring:"changed since you opened it"
              }
            else e')
      }
    , [] )
  | Saved path, Ok json ->
    let md5 =
      match Json.string_field json "md5" with
      | Ok md5 -> Some md5
      | Error _ -> None
    in
    let t =
      { t with
        editor =
          Option.map t.editor ~f:(fun e ->
            if String.equal e.path path
            then
              { e with
                saving = false
              ; dirty = false
              ; conflict = false
              ; md5 = Option.first_some md5 e.md5
              }
            else e)
      ; contents =
          (match t.editor with
           | Some e when String.equal e.path path ->
             Map.set t.contents ~key:path ~data:(Loaded { text = e.text; md5 })
           | Some _ | None -> t.contents)
      }
    in
    let t, diff = get_diff t in
    t, toast (sprintf "Saved %s" path) :: diff
  | Since root, _ when not (Option.equal String.equal (Some root) (root_of t))
    ->
    (* Of the worktree reviewed before. *)
    get_since { t with since_loading = false; since_again = false }
  | Since _, result ->
    let t = { t with since_loading = false } in
    let t =
      match
        Result.bind result ~f:(fun j ->
          Result.map_error (Review_since.of_json j) ~f:Error.to_string_hum)
      with
      | Ok files ->
        { t with
          since =
            String.Map.of_alist_reduce
              (List.map files ~f:(fun (f : Review_since.File.t) -> f.path, f))
              ~f:(fun a _ -> a)
        }
      | Error _ ->
        (* An older backend, or a file not readable: no since-view. *)
        t
    in
    let t, picked =
      List.fold
        (Map.to_alist t.picked)
        ~init:(t, [])
        ~f:(fun (t, outs) (path, (index, _)) ->
          let t, more = get_picked t ~path ~index in
          t, outs @ more)
    in
    if t.since_again
    then (
      let t, again = get_since { t with since_again = false } in
      t, picked @ again)
    else t, picked
  | Since_picked { root; path; index }, result ->
    (match Map.find t.picked path with
     | Some (i, _)
       when i = index && Option.equal String.equal (Some root) (root_of t) ->
       (match
          Result.bind result ~f:(fun j ->
            Result.map_error (Review_since.of_json j) ~f:Error.to_string_hum)
        with
        | Ok [ f ] ->
          ( { t with picked = Map.set t.picked ~key:path ~data:(index, Some f) }
          , [] )
        | Ok _ -> { t with picked = Map.remove t.picked path }, []
        | Error e ->
          ( { t with picked = Map.remove t.picked path }
          , [ toast ~error:true (error_text e) ] ))
     | Some _ | None -> t, [])
  | Submitted, Error e ->
    { t with submitting = false }, [ toast ~error:true (error_text e) ]
  | Submitted, Ok json ->
    let count =
      Result.ok (Json.int_field json "count") |> Option.value ~default:0
    in
    ( { t with submitting = false }
    , [ toast
          (if count = 1
           then "Sent 1 comment to prigh"
           else sprintf "Sent %d comments to prigh" count)
      ] )
;;

let key t ~key ~shift ~ctrl ~alt : Action.t option =
  if ctrl || alt
  then None
  else (
    match key with
    | "]" | "n" | "j" | "ArrowRight" -> Some (Step 1)
    | "[" | "p" | "k" | "ArrowLeft" -> Some (Step (-1))
    | "a" | " " -> Option.map t.current ~f:(fun p -> Action.Toggle_approval p)
    | "x" ->
      Option.map t.current ~f:(fun path -> Action.Mark { path; range = false })
    | "X" when shift ->
      Option.map t.current ~f:(fun path -> Action.Mark { path; range = true })
    | "c" when Mode.equal t.mode Comment -> Some Start_comment
    | "e" -> Some (Set_mode (if Mode.equal t.mode Edit then Comment else Edit))
    | "v" ->
      let layout =
        Option.value_map t.current ~default:t.layout ~f:(layout_of t)
      in
      Some (Set_layout (if Layout.equal layout Full then Unified else Full))
    | "l" ->
      (match Option.map t.current ~f:(layout_of t) with
       | Some Since ->
         Some
           (Set_layout
              (match t.layout with
               | Since -> Unified
               | layout -> layout))
       | Some (Unified | Full) | None -> Some (Set_layout Since))
    | "s" when not shift -> Some Submit
    | "Escape" ->
      if Option.is_some t.draft
      then Some Draft_cancel
      else if Option.is_some t.selection
      then Some Clear_selection
      else if not (List.is_empty (marked t))
      then Some Unmark_all
      else Some Close
    | _ -> None)
;;

let toggle_key ~code ~shift ~ctrl ~alt ~meta =
  alt && (not (ctrl || shift || meta)) && String.equal code "KeyR"
;;

let field_key t ~field ~key ~shift ~ctrl ~meta : Action.t option =
  let command = ctrl || meta in
  match field, key with
  | `Draft, "Enter" when command || not shift -> Some Draft_send
  | `Draft, "Escape" -> Some Draft_cancel
  | `Reply id, "Enter" when command || not shift -> Some (Reply_send id)
  | `Editor, ("s" | "S") when command && Option.is_some t.editor -> Some Save
  | _ -> None
;;
