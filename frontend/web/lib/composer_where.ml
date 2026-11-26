open! Core
open! Import
open Html

type t =
  { path : string
  ; branch : string option
  ; fixed : bool (** a subagent's: it keeps them *)
  }

(* Off the backend, [git_branch] is the backend's guess; the session's listed
   repository is the tool host's. *)
let branch (m : App.Model.t) (s : State.t) =
  match s.git_branch with
  | Some _ as branch -> branch
  | None when String.equal s.active_host Host.backend_id -> None
  | None ->
    List.find_map m.sessions ~f:(fun (summary : Session_summary.t) ->
      if String.equal summary.id s.session_id
      then Option.bind summary.repo ~f:(fun (r : Repo.t) -> r.branch)
      else None)
;;

let of_model (m : App.Model.t) =
  let fixed = Option.is_some (App.Model.shown_subagent m) in
  match App.Model.shown_state m with
  | None -> None
  | Some s when s.temporary || App.temporary_shown m || String.is_empty s.cwd ->
    None
  | Some s ->
    Some
      { path = Tilde.of_path ?host_home:(Tilde.active_host_home s) s.cwd
      ; branch = (if fixed then s.git_branch else branch m s)
      ; fixed
      }
;;

(* A button of the line: its icon, then its text, cut short with an ellipsis
   when the line is too narrow. *)
let item ~cls ~title ~label ~on_click ~glyph text =
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_
          (cls
           ^ " flex min-w-0 items-center gap-1.5 rounded-sm px-1 py-0.5 \
              hover:text-foreground focus-visible:outline-1 \
              focus-visible:outline-ring narrow:min-h-11")
      ; Attr.title title
      ; Attr.create "aria-label" label
      ; Attr.on_click (fun _ -> on_click)
      ]
    [ icon ~cls:"size-3.5 shrink-0" glyph; text ]
;;

let view t ~inject =
  let where = t.path in
  Node.div
    ~attrs:
      [ Attr.class_
          "composer-where loon flex min-w-0 items-center gap-4 px-2 pt-1.5 \
           font-mono text-xs text-muted-foreground"
      ]
    [ item
        ~cls:"where-path"
        ~title:
          (if t.fixed
           then t.path ^ ": the subagent's folder"
           else t.path ^ ". Start in another folder (/cd)")
        ~label:
          (if t.fixed
           then sprintf "The subagent's folder: %s" where
           else sprintf "Folder: %s. Change it" where)
        ~on_click:
          (inject
             (if t.fixed
              then App.Action.Subagent_fixed Folder
              else App.Action.Run "/cd"))
        ~glyph:Folder
        (Node.span ~attrs:[ Attr.class_ "truncate" ] [ Node.text where ])
    ; (match t.branch with
       | None -> Node.none
       | Some branch ->
         item
           ~cls:"where-branch ml-auto shrink-0"
           ~title:
             (if t.fixed
              then branch ^ ": the subagent's branch"
              else "Work in another worktree (/worktree)")
           ~label:
             (if t.fixed
              then sprintf "The subagent's branch: %s" branch
              else sprintf "Branch: %s. Change it" branch)
           ~on_click:
             (inject
                (if t.fixed
                 then App.Action.Subagent_fixed Branch
                 else App.Action.Run "/worktree"))
           ~glyph:Branch
           (Node.span
              ~attrs:[ Attr.class_ "max-w-[24ch] truncate" ]
              [ Node.text branch ]))
    ]
;;
