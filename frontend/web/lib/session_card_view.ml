open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action
module Icon = Loon.Icon

(* The chat on show's own subagents: the session's, or the subagent's. *)
let agents (m : Model.t) =
  Agents.children m.agents m.subagent_chat |> List.map ~f:(fun a -> a, 0)
;;

(* In a subagent's chat, the chats above it: the session's, then its
   ancestors, the nearest last. *)
let parents (m : Model.t) =
  match m.subagent_chat with
  | None -> []
  | Some id ->
    (match List.rev (Agents.lineage m.agents id) with
     | [] | [ _ ] -> []
     | _ :: ancestors -> List.rev ancestors)
;;

(* The worktrees that matter to the session (the ones the sidebar showed
   under it): where it works, its subagents', its pull requests'. *)
let worktrees (m : Model.t) =
  match m.state with
  | None -> []
  | Some state ->
    (match
       List.find m.sessions ~f:(fun s -> String.equal s.id state.session_id)
     with
     | None -> []
     | Some s ->
       List.map (Session_worktree_nodes.of_session m s) ~f:(fun n -> s, n))
;;

let something (m : Model.t) =
  Option.is_some m.subagent_chat
  || not (List.is_empty (agents m) && List.is_empty (worktrees m))
;;

let inline (m : Model.t) = m.session_card_open && (not m.narrow) && something m

(* The card is 208px wide, 12px in from the pane's right edge, with 12px
   between it and the log's column: the column clears it when its right edge
   is 232px in. Centred ([--column] wide), it does from a pane of
   [--column] + 464px; in a narrower one [--push] (which chat.css takes
   off the column's left padding) moves it left by what the margin lacks, and
   the log's right padding keeps it out from under the card once it has
   reached the left edge. The composer moves left with it but keeps its
   width (the card is above it).
   Below an 820px pane the chat would get too narrow: the card covers its
   corner instead. The
   variables are set on the pane's children, so that [cqw] is the pane's. *)
let main_attr (m : Model.t) =
  if inline m
  then
    Attr.class_
      "session-card-push @container \
       *:[--push:max(0px,calc(var(--column)/2+232px-50cqw))] \
       [&_.chat]:pr-[232px] @max-[820px]:*:[--push:0px] \
       @max-[820px]:[&_.chat]:pr-(--edge) [&>.composer]:mr-auto \
       [&>.composer]:ml-[max(0px,calc((100%-var(--column))/2-var(--edge)-var(--push)))]"
  else Attr.empty
;;

let section
      (m : Model.t)
      ~inject
      ~(which : Session_card.Section.t)
      ?count
      ~glyph
      ~name
      items
  =
  let count = Option.value count ~default:(List.length items) in
  let open_ = Session_card.is_open m.session_card which in
  let id =
    match which with
    | Parents -> "session-card-parents"
    | Subagents -> "session-card-subagents"
    | Ended -> "session-card-ended"
    | Worktrees -> "session-card-worktrees"
  in
  Loon.Collapsible.view
    ~id
    ~class_:"session-card-section"
    ~open_
    ~on_change:(fun _ -> inject (Action.Session_card (Fold which)))
    (fun c ->
       [ Loon.Collapsible.trigger
           c
           ~class_:
             "flex h-7 w-full items-center gap-2 rounded-md px-2 text-left \
              text-[12.5px] text-foreground/90 hover:bg-accent narrow:h-11 \
              [&_svg]:size-3.5 [&_svg]:shrink-0 [&_svg]:text-muted-foreground"
           [ Icon.view glyph
           ; Node.span
               ~attrs:[ Attr.class_ "min-w-0 flex-1 truncate" ]
               [ Node.text name ]
           ; Node.span
               ~attrs:
                 [ Attr.class_
                     "flex items-center gap-1 text-[11px] tabular-nums \
                      text-muted-foreground"
                 ]
               [ Node.text (Int.to_string count)
               ; Icon.view
                   ~class_:
                     (if open_
                      then "rotate-90 transition-transform"
                      else "transition-transform")
                   Chevron_right
               ]
           ]
       ; Loon.Collapsible.content
           c
           ~class_:"flex flex-col pb-0.5 pl-5.5"
           (if open_ then items else [])
       ])
;;

let subagent_row (m : Model.t) ~inject ((a : Agents.Agent.t), depth) =
  let now = Option.value m.now ~default:Time_ns.epoch in
  let activity = Session_card.Activity.of_agent a ~now in
  let failed =
    match activity with
    | Finished { failed; _ } -> failed
    | Thinking | Working -> false
  in
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.classes
          ([ "session-card-agent"
           ; "group/sub flex h-7 w-full items-center gap-1.5 rounded-md px-2 \
              text-left text-[12px] text-foreground/85 hover:bg-accent \
              hover:text-foreground narrow:h-11"
           ]
           @
           if Session_card.Activity.running activity then [ "running" ] else []
          )
      ; Attr.title (String.strip a.task)
      ; (if depth = 0
         then Attr.empty
         else Attr.style (Css_gen.padding_left (`Px (8 + (12 * depth)))))
      ; Attr.on_click (fun _ -> inject (Action.Open_subagent_chat a.id))
      ]
    [ Node.span
        ~attrs:[ Attr.class_ "min-w-0 truncate" ]
        [ Node.text (Agents.Agent.title a) ]
    ; Icon.view
        ~class_:
          "size-3! shrink-0 text-muted-foreground opacity-0 \
           group-hover/sub:opacity-100 group-focus-visible/sub:opacity-100"
        Arrow_up_right
    ; Node.span
        ~attrs:
          [ Attr.class_
              (if failed
               then "ml-auto shrink-0 text-[11px] text-destructive"
               else "ml-auto shrink-0 text-[11px] text-muted-foreground")
          ]
        [ Node.text (Session_card.Activity.to_string activity) ]
    ]
;;

(* The subagents that ended, folded into one row by default: "24 finished
   · 3 failed"; unfolded, they follow it, the latest first. *)
let ended_row (m : Model.t) ~inject ended =
  let failed =
    List.count ended ~f:(fun ((a : Agents.Agent.t), _) ->
      Agents.Status.equal a.status Failed)
  in
  let finished = List.length ended - failed in
  let open_ = Session_card.is_open m.session_card Ended in
  let part n what cls =
    Node.span ~attrs:[ Attr.class_ cls ] [ Node.text (sprintf "%d %s" n what) ]
  in
  let parts =
    List.filter_opt
      [ (if finished > 0 then Some (part finished "finished" "") else None)
      ; (if failed > 0
         then Some (part failed "failed" "text-destructive")
         else None)
      ]
    |> List.intersperse ~sep:(Node.span [ Node.text " · " ])
  in
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_
          "session-card-ended flex h-7 w-full items-center gap-1.5 rounded-md \
           px-2 text-left text-[12px] text-muted-foreground hover:bg-accent \
           hover:text-foreground narrow:h-11"
      ; Attr.create "aria-expanded" (Bool.to_string open_)
      ; Attr.on_click (fun _ -> inject (Action.Session_card (Fold Ended)))
      ]
    [ Node.span ~attrs:[ Attr.class_ "min-w-0 truncate" ] parts
    ; Icon.view
        ~class_:
          (if open_
           then "ml-auto size-3! shrink-0 rotate-90 transition-transform"
           else "ml-auto size-3! shrink-0 transition-transform")
        Chevron_right
    ]
;;

(* Running subagents, then those that ended: one row, unfolding to them. *)
let subagent_rows (m : Model.t) ~inject agents =
  let running, ended =
    List.partition_tf agents ~f:(fun ((a : Agents.Agent.t), _) ->
      Agents.Status.equal a.status Running)
  in
  let ended_at (a : Agents.Agent.t) =
    Option.value a.ended_at ~default:a.started_at
  in
  let ended =
    List.stable_sort ended ~compare:(fun ((a : Agents.Agent.t), _) (b, _) ->
      Time_ns.compare (ended_at b) (ended_at a))
  in
  List.map running ~f:(subagent_row m ~inject)
  @
  match ended with
  | [] -> []
  | ended ->
    ended_row m ~inject ended
    ::
    (if Session_card.is_open m.session_card Ended
     then List.map ended ~f:(subagent_row m ~inject)
     else [])
;;

(* A chat above the one on show: the session's ([None]) or a subagent's. *)
let parent_row ~inject ~label ~title (target : string option) =
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.class_
          "session-card-parent flex h-7 w-full items-center gap-1.5 rounded-md \
           px-2 text-left text-[12px] text-foreground/85 hover:bg-accent \
           hover:text-foreground narrow:h-11"
      ; Attr.title title
      ; Attr.on_click (fun _ ->
          inject
            (match target with
             | None -> Action.Close_subagent_chat
             | Some id -> Action.Open_subagent_chat id))
      ]
    [ Node.span ~attrs:[ Attr.class_ "min-w-0 truncate" ] [ Node.text label ] ]
;;

(* A click (or a right-click) opens the worktree's menu: open it here or in
   a new session, its pull request, remove it. *)
let worktree_row
      (m : Model.t)
      ~inject
      ((s : Session_summary.t), (n : Prigh_ui.Session_worktrees.Node.t))
  =
  let w = n.worktree in
  let id = Context_menu.worktree_dom_id ~session:s.id w.path in
  let target =
    Context_menu.Target.Worktree { session = s.path; path = w.path }
  in
  let opening =
    Option.equal String.equal (Worktrees.entering m.worktrees) (Some w.path)
  in
  let menu_at (ev : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) =
    let x = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientX)
    and y = Float.to_int (Js_of_ocaml.Js.to_float ev##.clientY) in
    let anchor : Context_menu.Anchor.t =
      if x = 0 && y = 0 then Element id else Point { x; y }
    in
    Js_of_ocaml.Dom.preventDefault ev;
    Effect.Many
      [ Effect.Stop_propagation; inject (Action.Open_menu { target; anchor }) ]
  in
  let changes =
    Option.bind n.listed ~f:(fun l -> l.changes)
    |> Option.filter ~f:(fun c -> c > 0)
  in
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.classes
          ([ "session-card-worktree"
           ; "flex h-7 w-full items-center gap-2 rounded-md px-2 text-left \
              text-[12px] hover:bg-accent narrow:h-11"
           ]
           @ (if n.here then [ "current" ] else [])
           @ if opening then [ "opening" ] else [])
      ; Attr.id id
      ; Attr.create "data-worktree" w.path
      ; Attr.create "data-owner" s.id
      ; Attr.create "aria-haspopup" "menu"
      ; Attr.title
          (Tilde.of_path ?host_home:(Tilde.session_host_home m.state s) w.path)
      ; (if n.here then Attr.create "aria-current" "true" else Attr.empty)
      ; Attr.on_click (fun _ ->
          inject (Action.Open_menu { target; anchor = Element id }))
      ; Attr.on_contextmenu menu_at
      ]
    [ Node.span
        ~attrs:
          [ Attr.class_
              (if n.here
               then "min-w-0 truncate font-mono text-foreground"
               else "min-w-0 truncate font-mono text-foreground/85")
          ]
        [ Node.text (Prigh_ui.Session_worktrees.Node.label n) ]
    ; (match changes with
       | Some c ->
         Node.span
           ~attrs:
             [ Attr.class_
                 "ml-auto shrink-0 text-[11px] tabular-nums \
                  text-muted-foreground"
             ; Attr.title
                 (sprintf "%d changed file%s" c (if c = 1 then "" else "s"))
             ]
           [ Node.text (sprintf "%d changed" c) ]
       | None -> Node.none)
    ]
;;

let sections (m : Model.t) ~inject =
  let agents = agents m in
  let worktrees = worktrees m in
  let in_subagent = Option.is_some m.subagent_chat in
  let session =
    Option.value_map
      m.state
      ~default:"The session's own chat"
      ~f:(Session_title.of_state m)
  in
  [ (if not in_subagent
     then Node.none
     else
       section
         m
         ~inject
         ~which:Parents
         ~glyph:Arrow_up
         ~name:"Parents"
         (parent_row ~inject ~label:"Root" ~title:session None
          :: List.map (parents m) ~f:(fun (a : Agents.Agent.t) ->
            parent_row
              ~inject
              ~label:(Agents.Agent.title a)
              ~title:(String.strip a.task)
              (Some a.id))))
  ; (if List.is_empty agents
     then Node.none
     else
       section
         m
         ~inject
         ~which:Subagents
         ~glyph:Bot
         ~name:"Subagents"
         ~count:(List.length agents)
         (subagent_rows m ~inject agents))
  ; (if List.is_empty worktrees || in_subagent
     then Node.none
     else
       section
         m
         ~inject
         ~which:Worktrees
         ~glyph:Git_branch
         ~name:"Worktrees"
         (List.map worktrees ~f:(worktree_row m ~inject)))
  ]
;;

let view (m : Model.t) ~inject =
  if not (something m)
  then Node.none
  else if m.narrow
  then
    Loon.Drawer.view
      ~id:"session-card-sheet"
      ~open_:m.session_card.sheet
      ~on_open_change:(fun open_ -> inject (Action.Session_card (Sheet open_)))
      (fun d ->
         [ Loon.Drawer.content
             d
             ~class_:"loon session-card sheet"
             [ Loon.Drawer.header
                 [ Loon.Drawer.title d [ Node.text "This session" ] ]
             ; div
                 ~cls:
                   "flex flex-col gap-1 px-3 \
                    pb-[max(12px,env(safe-area-inset-bottom))]"
                 (sections m ~inject)
             ]
         ])
  else if not m.session_card_open
  then Node.none
  else
    div
      ~cls:"loon session-card-anchor relative z-[5] h-0"
      [ Node.aside
          ~attrs:
            [ Attr.class_
                "session-card absolute top-13 right-3 flex w-[208px] flex-col \
                 rounded-xl border bg-card p-1.5 text-card-foreground"
            ; Attr.create "aria-label" "This session"
            ]
          (sections m ~inject)
      ]
;;
