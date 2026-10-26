open! Core
open! Import

module Target = struct
  type t =
    | Session of string
    | Worktree of
        { session : string
        ; path : string
        }
  [@@deriving sexp_of, equal]
end

module Anchor = struct
  type t =
    | Point of
        { x : int
        ; y : int
        }
    | Element of string
  [@@deriving sexp_of, equal]
end

type t =
  { target : Target.t
  ; anchor : Anchor.t
  }
[@@deriving sexp_of, equal]

module Item = struct
  type t =
    | Open
    | Open_tab
    | Rename
    | Fork
    | New_worktree
    | Switch_back (** the session goes back to its home *)
    | Switch_here
    | New_session_here
    | Review
    | Files
    | Terminal
    | Create_pr
    | Copy_path
    | Copy_id
    | Archive
    | Unarchive
    | Delete
    | Remove
  [@@deriving sexp_of, equal, enumerate]

  let id t = String.lowercase (Sexp.to_string (sexp_of_t t))
  let of_id s = List.find all ~f:(fun t -> String.equal (id t) s)

  let danger = function
    | Delete | Remove -> true
    | Open
    | Open_tab
    | Rename
    | Fork
    | New_worktree
    | Switch_back
    | Switch_here
    | New_session_here
    | Review
    | Files
    | Terminal
    | Create_pr
    | Copy_path
    | Copy_id
    | Archive
    | Unarchive -> false
  ;;
end

let session_items ?host_home (s : Session_summary.t) ~current
  : (Item.t * string * string) list
  =
  List.filter_opt
    [ Option.map s.home ~f:(fun home ->
        ( Item.Switch_back
        , "Switch back to "
          ^ Session_home.label home ~tilde:(Tilde.of_path ?host_home)
        , "/worktree back" ))
    ; Option.some_if (not current) (Item.Open, "Open", "Enter")
    ; Some (Open_tab, "Open in new tab", "")
    ; Some (Rename, "Rename…", "/name")
    ; Some (Fork, "Fork…", "/fork")
    ; Option.some_if
        (Option.is_some s.repo)
        (Item.New_worktree, "New worktree…", "/worktree new")
    ; Some (Review, "Review changes", "/review")
    ; Some (Files, "Open in file browser", "/files")
    ; Some (Terminal, "Open terminal there", "/terminal")
    ; Some (Copy_path, "Copy path", "")
    ; Some (Copy_id, "Copy session id", "")
    ; (if s.archived
       then Some (Unarchive, "Unarchive", "A")
       else Some (Archive, "Archive", "A"))
    ; Some (Delete, "Delete…", "Delete")
    ]
;;

let worktree_items (s : Session_summary.t) ~path
  : (Item.t * string * string) list
  =
  let target =
    match List.find s.worktrees ~f:(fun w -> String.equal w.path path) with
    | Some { branch = Some branch; _ } -> branch
    | Some { branch = None; _ } | None -> path
  in
  [ Item.Switch_here, "Switch session here", "/worktree open " ^ target
  ; New_session_here, "New session here", "/worktree open -n " ^ target
  ; Review, "Review changes", "/review " ^ target
  ; Files, "Open in file browser", "/files " ^ path
  ; Terminal, "Open terminal there", "/terminal " ^ path
  ; Create_pr, "Create pull request…", "/pr new"
  ; Copy_path, "Copy path", ""
  ; Remove, "Remove worktree…", "/worktree rm " ^ target
  ]
;;

let items ?host_home (target : Target.t) s ~current =
  match target with
  | Session _ -> session_items ?host_home s ~current
  | Worktree { path; _ } -> worktree_items s ~path
;;

let picker ?host_home target (s : Session_summary.t) ~current =
  let title =
    match target with
    | Target.Session _ -> "Session"
    | Worktree { path; _ } ->
      (match List.find s.worktrees ~f:(fun w -> String.equal w.path path) with
       | Some w -> "Worktree " ^ Session_worktree.label w
       | None -> "Worktree")
  in
  Picker.create
    ~title
    (List.map
       (items ?host_home target s ~current)
       ~f:(fun (item, label, detail) ->
         Picker.Item.create ~id:(Item.id item) ~detail label))
;;

let worktree t =
  match t.target with
  | Session _ -> None
  | Worktree { path; _ } -> Some path
;;

let session t =
  match t.target with
  | Session path | Worktree { session = path; _ } -> path
;;

let worktree_dom_id ~session path =
  "worktree-"
  ^ session
  ^ "-"
  ^ String.map path ~f:(fun c -> if Char.is_alphanum c then c else '-')
;;

module Plan = struct
  type t =
    { switch : string option
    ; enter : string option
    ; run : string option
    ; notice : string option
    }
  [@@deriving sexp_of, equal]
end
