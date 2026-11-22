open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children
let t s = Node.text s

let row ?(class_ = "") ?right ?on_click icon label =
  Node.button
    ~attrs:
      ([ Attr.class_
           (Cn.cn
              [ "group/row flex h-7 w-full items-center gap-2 rounded-md px-2 text-left \
                 text-[13px] text-foreground/90 hover:bg-accent [&_svg]:size-3.5 \
                 [&_svg]:shrink-0 [&_svg]:text-muted-foreground"
              ; class_
              ])
       ]
       @ Option.to_list (Option.map on_click ~f:(fun e -> Attr.on_click (fun _ -> e))))
    [ icon
    ; span "min-w-0 flex-1 truncate" [ t label ]
    ; Option.value right ~default:Node.none
    ]
;;

let chevron = Icon.view ~class_:"size-3.5!" Chevron_down

let counts ~added ~removed =
  span
    "shrink-0 font-mono text-[11px] tabular-nums"
    [ span "text-emerald-400" [ Node.textf "+%d" added ]
    ; t " "
    ; span "text-red-400" [ Node.textf "−%d" removed ]
    ]
;;

let section ~open_ ~toggle ~count icon label items =
  Node.div
    [ row
        icon
        label
        ~on_click:toggle
        ~right:
          (span
             "flex items-center gap-1 text-[11px] tabular-nums text-muted-foreground"
             [ Node.textf "%d" count
             ; Icon.view
                 ~class_:
                   (if open_
                    then "size-3.5! rotate-90 transition-transform"
                    else "size-3.5! transition-transform")
                 Chevron_right
             ])
    ; (if open_ then div "flex flex-col pb-0.5 pl-5.5" items else Node.none)
    ]
;;

let subagent (a : Fake.Subagent.t) =
  Node.a
    ~attrs:
      [ Attr.href "#"
      ; Attr.title a.task
      ; Attr.class_
          "group/sub flex h-7 items-center gap-1.5 rounded-md px-2 text-[12.5px] \
           text-foreground/85 hover:bg-accent hover:text-foreground"
      ]
    [ span "min-w-0 truncate" [ t a.name ]
    ; Icons.view
        ~class_:"size-3 shrink-0 text-muted-foreground opacity-0 group-hover/sub:opacity-100"
        Arrow_up_right
    ; Activity.view ~class_:"ml-auto shrink-0 text-[11px] text-muted-foreground" a.status ~age:a.age
    ]
;;

let worktree (w : Fake.Worktree.t) =
  Node.a
    ~attrs:
      [ Attr.href "#"
      ; Attr.title w.path
      ; Attr.class_
          "flex h-7 items-center gap-2 rounded-md px-2 text-[12px] hover:bg-accent"
      ]
    [ span "min-w-0 truncate font-mono text-foreground/85" [ t w.branch ]
    ; div "ml-auto" [ counts ~added:w.added ~removed:w.removed ]
    ]
;;

let view ?(class_ = "") ~subagents_open ~worktrees_open ~toggle_subagents ~toggle_worktrees () =
  Node.aside
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "flex w-[260px] shrink-0 flex-col rounded-xl border bg-card p-1.5 \
                text-card-foreground"
             ; class_
             ])
      ]
    [ div
        "flex h-7 items-center gap-2 px-2 text-[13px] text-muted-foreground"
        [ Icon.view ~class_:"size-3.5 shrink-0" Monitor
        ; span "text-foreground/90" [ t Fake.User.host ]
        ; span "min-w-0 truncate font-mono text-[11px]" [ t "~/projects/prigh" ]
        ; Button.view
            ~variant:Ghost
            ~size:Icon_xs
            ~class_:"ml-auto -mr-1 text-muted-foreground"
            ~attrs:[ Attr.create "aria-label" "Open folder" ]
            [ Icon.view Folder ]
        ]
    ; row (Icon.view Folder) "Open in Files" ~right:chevron
    ; row (Icon.view Plus) "Add project script"
    ; Separator.view ~class_:"my-1" ()
    ; row (Icon.view Git_branch) Fake.User.branch ~class_:"font-mono text-[12px]" ~right:chevron
    ; row (Icon.view Git_commit_vertical) "Commit" ~right:chevron
    ; row (Icons.view File_diff) "Changes" ~right:(counts ~added:12 ~removed:3)
    ; Separator.view ~class_:"my-1" ()
    ; section
        ~open_:subagents_open
        ~toggle:toggle_subagents
        ~count:(List.length Fake.Subagent.all)
        (Icon.view Bot)
        "Subagents"
        (List.map Fake.Subagent.all ~f:subagent)
    ; section
        ~open_:worktrees_open
        ~toggle:toggle_worktrees
        ~count:(List.length Fake.Worktree.all)
        (Icon.view Git_branch)
        "Worktrees"
        (List.map Fake.Worktree.all ~f:worktree)
    ]
;;
