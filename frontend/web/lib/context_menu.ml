open! Core
open! Import

module Target = struct
  type t =
    | Session of string
    | Worktree of
        { session : string
        ; path : string
        }
    | Message of int
    | Review_files of string list
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
    | Copy_message
    | Trim_before
    | Trim_after
    | Approve_files
    | Unapprove_files
    | Stage_files
    | Unstage_files
    | Commit_files
    | Select_files
    | Unselect_files
    | Copy_paths
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
    | Unarchive
    | Copy_message
    | Trim_before
    | Trim_after
    | Approve_files
    | Unapprove_files
    | Stage_files
    | Unstage_files
    | Commit_files
    | Select_files
    | Unselect_files
    | Copy_paths -> false
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
  | Session _ | Message _ | Review_files _ ->
    session_items ?host_home s ~current
  | Worktree { path; _ } -> worktree_items s ~path
;;

let picker ?host_home target (s : Session_summary.t) ~current =
  let title =
    match target with
    | Target.Session _ | Message _ | Review_files _ -> "Session"
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

module Message = struct
  type t =
    { reply : bool
    ; calls : int
    ; text : string
    ; first : bool
    ; newest : bool
    ; running : bool
    }
  [@@deriving sexp_of, equal]

  let title t =
    match t.reply, t.calls with
    | false, _ -> "Your message"
    | true, 0 -> "Reply"
    | true, 1 -> "Reply with a tool call"
    | true, n -> sprintf "Reply with %d tool calls" n
  ;;
end

let message_picker (msg : Message.t) =
  let running = "Wait for the run to end, or stop it (Esc)" in
  let item (kind : Item.t) label ~command ~unless:(disabled, why) =
    Picker.Item.create
      ~id:(Item.id kind)
      ~detail:(if disabled then why else command)
      ~dimmed:disabled
      label
  in
  Picker.create
    ~title:(Message.title msg)
    [ item
        Copy_message
        "Copy"
        ~command:""
        ~unless:
          (String.is_empty (String.strip msg.text), "No text: only tool calls")
    ; item
        Trim_before
        "Trim everything before this"
        ~command:"/trim"
        ~unless:
          (if msg.running
           then true, running
           else msg.first, "The context starts here")
    ; item
        Trim_after
        "Trim everything after this"
        ~command:"/trim after"
        ~unless:
          (if msg.running
           then true, running
           else msg.newest, "Nothing after it: the newest message")
    ; item Delete "Delete" ~command:"/delete" ~unless:(msg.running, running)
    ]
;;

module Files = struct
  type t =
    { paths : string list
    ; approved : int
    ; staged : int
    ; unstaged : int
    ; selected : bool
    ; composing : bool
    ; git : string option
    }
  [@@deriving sexp_of, equal]
end

let files_picker (f : Files.t) =
  let n = List.length f.paths in
  let them = if n = 1 then "it" else "them" in
  let item (kind : Item.t) label ~key ~unless:(disabled, why) =
    Picker.Item.create
      ~id:(Item.id kind)
      ~detail:(if disabled then why else key)
      ~dimmed:disabled
      label
  in
  let git ~unless:(disabled, why) =
    match f.git with
    | Some why -> true, why
    | None -> disabled, why
  in
  Picker.create
    ~title:
      (match f.paths with
       | [ path ] -> Filename.basename path
       | _ -> sprintf "%d selected files" n)
    [ item
        Approve_files
        "Approve"
        ~key:(if n = 1 then "Space" else "")
        ~unless:(f.approved = n, sprintf "Approved as %s are now" them)
    ; item
        Unapprove_files
        "Take back the approval"
        ~key:""
        ~unless:(f.approved = 0, "Not approved")
    ; item
        Stage_files
        "Stage"
        ~key:""
        ~unless:(git ~unless:(f.unstaged = 0, "Git has all of the changes"))
    ; item
        Unstage_files
        "Unstage"
        ~key:""
        ~unless:(git ~unless:(f.staged = 0, "Nothing staged"))
    ; item
        Commit_files
        (if f.composing then "Add to the commit" else "Commit…")
        ~key:"Shift+C"
        ~unless:(git ~unless:(f.staged + f.unstaged = 0, "Nothing to commit"))
    ; (if f.selected
       then item Unselect_files "Unselect" ~key:"x" ~unless:(false, "")
       else item Select_files "Select" ~key:"x" ~unless:(false, ""))
    ; item
        Copy_paths
        (if n = 1 then "Copy path" else "Copy paths")
        ~key:""
        ~unless:(false, "")
    ]
;;

let worktree t =
  match t.target with
  | Session _ | Message _ | Review_files _ -> None
  | Worktree { path; _ } -> Some path
;;

let session t =
  match t.target with
  | Session path | Worktree { session = path; _ } -> Some path
  | Message _ | Review_files _ -> None
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
