open! Core
open! Import
open Html

type t =
  { host : string option
  ; path : string
  ; branch : string option
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

(* Named only when it is not where tools run unless asked otherwise: the
   default host for new sessions (the backend's without one), or this
   browser's own. *)
let host (m : App.Model.t) (s : State.t) =
  let default =
    match Option.bind m.config ~f:(fun (c : Config.t) -> c.default_host) with
    | Some id when List.exists s.hosts ~f:(fun h -> String.equal h.id id) -> id
    | Some _ | None -> Host.backend_id
  in
  let here =
    Option.exists m.hello ~f:(fun (hello : Hello_reply.t) ->
      String.equal hello.client_id s.active_host)
  in
  if here || String.equal s.active_host default
  then None
  else (
    match Active_host.of_state s with
    | Connected h -> Some h.name
    | Disconnected { name; _ } -> Some name
    | Unset -> None)
;;

let of_model (m : App.Model.t) =
  match m.state with
  | None -> None
  | Some s when s.temporary || App.temporary_shown m || String.is_empty s.cwd ->
    None
  | Some s ->
    Some
      { host = host m s
      ; path = Tilde.of_path ?host_home:(Tilde.active_host_home s) s.cwd
      ; branch = branch m s
      }
;;

(* A button of the line: its icon, then its text cut short (a path from its
   start, so that its end shows). *)
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
  let where =
    Option.value_map t.host ~default:"" ~f:(sprintf "%s: ") ^ t.path
  in
  Node.div
    ~attrs:
      [ Attr.class_
          "composer-where loon flex min-w-0 items-center gap-4 px-2 pt-1.5 \
           font-mono text-xs text-muted-foreground"
      ]
    [ item
        ~cls:"where-path"
        ~title:
          (Option.value_map t.host ~default:"" ~f:(sprintf "On %s, in ")
           ^ t.path
           ^ ". Start in another folder (/cd)")
        ~label:(sprintf "Folder: %s. Change it" where)
        ~on_click:(inject (App.Action.Run "/cd"))
        ~glyph:Folder
        (Node.span
           ~attrs:[ Attr.class_ "truncate [direction:rtl]" ]
           [ Node.span ~attrs:[ Attr.create "dir" "ltr" ] [ Node.text where ] ])
    ; (match t.branch with
       | None -> Node.none
       | Some branch ->
         item
           ~cls:"where-branch ml-auto shrink-0"
           ~title:"Work in another worktree (/worktree)"
           ~label:(sprintf "Branch: %s. Change it" branch)
           ~on_click:(inject (App.Action.Run "/worktree"))
           ~glyph:Branch
           (Node.span
              ~attrs:[ Attr.class_ "max-w-[24ch] truncate" ]
              [ Node.text branch ]))
    ]
;;
