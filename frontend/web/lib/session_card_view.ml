open! Core
open! Import
open Html
module Model = App.Model
module Action = App.Action
module Icon = Loon.Icon

let agents (m : Model.t) =
  Agents.listed m.agents @ Agents.earlier m.agents
  |> List.filter_map ~f:(fun ((item : Agents.Item.t), depth) ->
    match item with
    | Agent id ->
      Agents.find_agent m.agents id |> Option.map ~f:(fun a -> a, depth)
    | Job _ -> None)
;;

let worktrees (m : Model.t) =
  match Worktrees.listing m.worktrees with
  | Some l -> l.worktrees
  | None -> []
;;

let something (m : Model.t) =
  not (List.is_empty (agents m) && List.is_empty (worktrees m))
;;

let inline (m : Model.t) = m.session_card_open && (not m.narrow) && something m

(* The card is 208px wide, 12px in from the pane's right edge, with 12px
   between it and the log's column: the column clears it when its right edge
   is 232px in. Centred ([--log-column] wide), it does from a pane of
   [--log-column] + 464px; in a narrower one [--push] (which chat.css takes
   off the column's left padding) moves it left by what the margin lacks, and
   the log's right padding keeps it out from under the card once it has
   reached the left edge. The composer moves left with it but stays as
   wide (narrower, its chips would lose their labels; the card is above it).
   Below an 820px pane the chat would get too narrow: the card covers its
   corner instead. The
   variables are set on the pane's children, so that [cqw] is the pane's. *)
let main_attr (m : Model.t) =
  if inline m
  then
    Attr.class_
      "session-card-push @container \
       *:[--push:max(0px,calc(var(--log-column)/2+232px-50cqw))] \
       [&_.chat]:pr-[232px] @max-[820px]:*:[--push:0px] \
       @max-[820px]:[&_.chat]:pr-(--edge) [&>.composer]:mr-auto \
       [&>.composer]:ml-[max(0px,calc((100%-var(--column))/2-var(--edge)-var(--push)))]"
  else Attr.empty
;;

let section
      (m : Model.t)
      ~inject
      ~(which : Session_card.Section.t)
      ~glyph
      ~name
      items
  =
  let open_ = Session_card.is_open m.session_card which in
  let id =
    match which with
    | Subagents -> "session-card-subagents"
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
               [ Node.text (Int.to_string (List.length items))
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

let worktree_row ~inject (w : Worktree.t) =
  Node.button
    ~attrs:
      [ Attr.type_ "button"
      ; Attr.classes
          ([ "session-card-worktree"
           ; "flex h-7 w-full items-center gap-2 rounded-md px-2 text-left \
              text-[12px] hover:bg-accent narrow:h-11"
           ]
           @ if w.current then [ "current" ] else [])
      ; Attr.title w.path
      ; (if w.current then Attr.create "aria-current" "true" else Attr.empty)
      ; Attr.on_click (fun _ ->
          if w.current
          then Effect.Ignore
          else
            inject (Action.Open_worktree { path = w.path; new_session = false }))
      ]
    [ Node.span
        ~attrs:
          [ Attr.class_
              (if w.current
               then "min-w-0 truncate font-mono text-foreground"
               else "min-w-0 truncate font-mono text-foreground/85")
          ]
        [ Node.text (Worktree.label w) ]
    ; (match w.changes with
       | Some n when n > 0 ->
         Node.span
           ~attrs:
             [ Attr.class_
                 "ml-auto shrink-0 text-[11px] tabular-nums \
                  text-muted-foreground"
             ; Attr.title
                 (sprintf "%d changed file%s" n (if n = 1 then "" else "s"))
             ]
           [ Node.text (sprintf "%d changed" n) ]
       | _ -> Node.none)
    ]
;;

let sections (m : Model.t) ~inject =
  let agents = agents m in
  let worktrees = worktrees m in
  [ (if List.is_empty agents
     then Node.none
     else
       section
         m
         ~inject
         ~which:Subagents
         ~glyph:Bot
         ~name:"Subagents"
         (List.map agents ~f:(subagent_row m ~inject)))
  ; (if List.is_empty worktrees
     then Node.none
     else
       section
         m
         ~inject
         ~which:Worktrees
         ~glyph:Git_branch
         ~name:"Worktrees"
         (List.map worktrees ~f:(worktree_row ~inject)))
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

let toggle (m : Model.t) ~inject =
  if not (something m)
  then Node.none
  else (
    let on = if m.narrow then m.session_card.sheet else m.session_card_open in
    button
      ~cls:
        (if on
         then "icon ghost session-card-toggle open"
         else "icon ghost session-card-toggle")
      ~title:"Subagents and worktrees"
      ~label:"Subagents and worktrees"
      ~attrs:[ Attr.create "aria-pressed" (Bool.to_string on) ]
      ~on_click:(inject Action.Toggle_session_card)
      [ Icon.view ~class_:"size-4 -scale-x-100" Panel_left ])
;;
