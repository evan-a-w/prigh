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

let ask = "Ask prigh anything…"

let sentence t =
  String.concat
    [ "Working "
    ; Option.value_map t.host ~default:"" ~f:(sprintf "on %s ")
    ; "in "
    ; t.path
    ; Option.value_map t.branch ~default:"" ~f:(sprintf " on %s")
    ; ". "
    ; ask
    ]
;;

(* [path] cut to its last [n] components, when that shortens it. *)
let last_components path n =
  let parts =
    String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty)
  in
  let kept = String.concat ~sep:"/" (List.drop parts (List.length parts - n)) in
  if List.length parts > n && String.length path - String.length kept > 2
  then Some ("…/" ^ kept)
  else None
;;

let line t ~path ~ask:with_ask =
  List.concat
    [ [ Node.text "Working " ]
    ; (match t.host with
       | Some host ->
         [ Node.text "on "; span ~cls:"cwd-host" host; Node.text " " ]
       | None -> [])
    ; [ Node.text "in "; span ~cls:"cwd-path" path ]
    ; (match t.branch with
       | Some branch -> [ Node.text " on "; span ~cls:"cwd-branch" branch ]
       | None -> [])
    ; [ Node.text (if with_ask then ". " ^ ask else ".") ]
    ]
;;

(* Layers in one place, the longest on top: each shows only when it fits on
   the line (style.css) and covers those under it; the bottom one, the
   shortest, is cut with an ellipsis. The line takes a little over half the
   editor's width, or what the bottom one needs if more. *)
let view t =
  let paths =
    t.path :: List.filter_map [ 2; 1 ] ~f:(last_components t.path)
    |> List.remove_consecutive_duplicates ~equal:String.equal
  in
  let shortest = List.last_exn paths in
  Node.div
    ~attrs:[ Attr.class_ "cwd-hint"; Attr.create "aria-hidden" "true" ]
    [ Node.span
        ~attrs:[ Attr.class_ "cwd-hint-stack" ]
        (Node.span
           ~attrs:[ Attr.class_ "cwd-hint-last" ]
           [ Node.span
               ~attrs:[ Attr.class_ "cwd-hint-cut" ]
               (line t ~path:shortest ~ask:false)
           ]
         :: List.rev_map paths ~f:(fun path ->
           Node.span
             ~attrs:[ Attr.class_ "cwd-hint-fit" ]
             [ Node.span
                 ~attrs:[ Attr.class_ "cwd-hint-line" ]
                 (line t ~path ~ask:true)
             ]))
    ]
;;
