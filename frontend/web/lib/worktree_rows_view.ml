open! Core
open! Import
open Html
module Action = App.Action
module Session_worktrees = Prigh_ui.Session_worktrees

(* A context menu's event: at the pointer, or (from the keyboard, which
   gives no position) under the row. *)
let open_menu
      ~inject
      target
      ~row
      (ev : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
  =
  let x = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientX)
  and y = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientY) in
  let anchor : Context_menu.Anchor.t =
    if x = 0 && y = 0 then Element row else Point { x; y }
  in
  Js_of_ocaml.Dom.preventDefault ev;
  Effect.Many
    [ Effect.Stop_propagation; inject (Action.Open_menu { target; anchor }) ]
;;

let changes (w : Worktree.t) =
  match w.changes with
  | Some n when n > 0 ->
    Node.span
      ~attrs:
        [ Attr.class_ "worktree-changes"
        ; Attr.title
            (sprintf
               "%d file%s with uncommitted changes"
               n
               (if n = 1 then "" else "s"))
        ]
      [ Node.textf "● %d" n ]
  | _ -> Node.none
;;

let sync (w : Worktree.t) =
  match
    List.filter_opt
      [ Option.some_if (w.ahead > 0) (sprintf "↑%d" w.ahead)
      ; Option.some_if (w.behind > 0) (sprintf "↓%d" w.behind)
      ]
  with
  | [] -> Node.none
  | parts ->
    Node.span
      ~attrs:
        [ Attr.class_ "worktree-sync"
        ; Attr.title
            (sprintf
               "%d ahead of and %d behind %s"
               w.ahead
               w.behind
               (Option.value w.upstream ~default:"its upstream"))
        ]
      [ Node.text (String.concat ~sep:" " parts) ]
;;

let tag ?(cls = "") text ~title =
  Node.span
    ~attrs:[ Attr.class_ ("worktree-tag " ^ cls); Attr.title title ]
    [ Node.text text ]
;;

(* A worktree under its session: its branch, what is in it, why it is
   there. Clicking (or right-clicking) opens its menu. *)
let row
      ?host_home
      ~inject
      ~entering
      (s : Session_summary.t)
      (n : Session_worktrees.Node.t)
  =
  let w = n.worktree in
  let label = Session_worktrees.Node.label n in
  let id = Context_menu.worktree_dom_id ~session:s.id w.path in
  let target =
    Context_menu.Target.Worktree { session = s.path; path = w.path }
  in
  let gone = Option.exists n.listed ~f:(fun l -> l.prunable) in
  let opening = Option.equal String.equal entering (Some w.path) in
  Node.button
    ~attrs:
      [ classes
          [ "worktree" ]
          [ "here", n.here
          ; "agent", Session_worktrees.Node.agent n
          ; "dirty", Option.exists n.listed ~f:Worktree.dirty
          ; "gone", gone
          ; "opening", opening
          ]
      ; Attr.type_ "button"
      ; Attr.id id
      ; Attr.create "data-worktree" w.path
      ; Attr.create "data-owner" s.id
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.title (Tilde.of_path ?host_home w.path)
      ; Attr.on_click (fun _ ->
          inject (Action.Open_menu { target; anchor = Element id }))
      ; Attr.on_contextmenu (open_menu ~inject target ~row:id)
      ]
    [ (if opening
       then lamp ~title:"Opening…" ()
       else icon ~cls:"worktree-icon" Branch)
    ; span ~cls:"worktree-branch" label
    ; (match w.source with
       | Agent { agent; task } ->
         tag
           ~cls:"agent"
           "subagent"
           ~title:(sprintf "Made for subagent %s: %s" agent task)
       | Pull_request number ->
         tag
           (sprintf "#%d" number)
           ~title:(sprintf "Pull request #%d, checked out" number)
       | Created | Entered -> Node.none)
    ; (if n.here
       then tag ~cls:"here" "here" ~title:"The session works in it"
       else Node.none)
    ; Option.value_map n.listed ~default:Node.none ~f:changes
    ; Option.value_map n.listed ~default:Node.none ~f:sync
    ; (if Option.exists n.listed ~f:(fun l -> l.locked)
       then tag "locked" ~title:"git worktree unlock to remove it"
       else Node.none)
    ; (if gone then tag "gone" ~title:"Its directory was deleted" else Node.none)
    ]
;;

let rows (m : App.Model.t) ~inject (s : Session_summary.t) =
  let cwd =
    match m.state with
    | Some state when String.equal state.session_id s.id -> state.cwd
    | _ -> s.cwd
  in
  Session_worktrees.of_session
    ~listings:
      (Option.to_list (Worktrees.listing m.worktrees)
       @ Repo_listings.listings m.repo_worktrees)
    ~unsettled:(Repo_listings.unsettled m.repo_worktrees)
    ~cwd
    s
  |> List.map
       ~f:
         (row
            ?host_home:(Tilde.session_host_home m.state s)
            ~inject
            ~entering:(Worktrees.entering m.worktrees)
            s)
;;
