open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children
let t s = Node.text s

let row ?(class_ = "") ?right icon label =
  Node.button
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "flex h-8 w-full items-center gap-2.5 rounded-md px-2 text-left text-[13px] \
                text-foreground/90 hover:bg-accent [&_svg]:size-4 [&_svg]:shrink-0 \
                [&_svg]:text-muted-foreground"
             ; class_
             ])
      ]
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

let heading ?count label =
  div
    "flex items-center gap-1.5 px-2 pt-3 pb-1 text-[11px] font-medium uppercase \
     tracking-wider text-muted-foreground/80"
    [ t label
    ; (match count with
       | None -> Node.none
       | Some n -> span "tabular-nums text-muted-foreground/60" [ Node.textf "%d" n ])
    ]
;;

let subagent (a : Fake.Subagent.t) =
  Node.button
    ~attrs:
      [ Attr.class_
          "flex w-full flex-col gap-0.5 rounded-md px-2 py-1.5 text-left hover:bg-accent"
      ]
    [ div
        "flex items-center gap-2 text-[13px]"
        [ span "truncate text-foreground/90" [ t a.name ]
        ; Activity.view ~class_:"ml-auto text-xs text-muted-foreground" a.status ~age:a.age
        ]
    ; span "truncate text-xs text-muted-foreground" [ t a.task ]
    ]
;;

let worktree (w : Fake.Worktree.t) =
  Node.button
    ~attrs:
      [ Attr.class_
          "flex w-full flex-col gap-0.5 rounded-md px-2 py-1.5 text-left hover:bg-accent"
      ]
    [ div
        "flex items-center gap-2 text-[13px]"
        [ Icon.view ~class_:"size-3.5 shrink-0 text-muted-foreground" Git_branch
        ; span "truncate font-mono text-[12px] text-foreground/90" [ t w.branch ]
        ; div "ml-auto" [ counts ~added:w.added ~removed:w.removed ]
        ]
    ; div
        "flex items-center gap-2 pl-5.5 text-xs text-muted-foreground"
        [ span "truncate font-mono text-[11px]" [ t w.path ]
        ; span
            "ml-auto shrink-0 tabular-nums"
            [ Node.textf "%d session%s" w.sessions (if w.sessions = 1 then "" else "s") ]
        ]
    ]
;;

let view () =
  let running, finished =
    List.partition_tf Fake.Subagent.all ~f:(fun a ->
      match a.status with
      | Thinking | Working -> true
      | Idle -> false)
  in
  Node.aside
    ~attrs:
      [ Attr.class_
          "flex h-full w-full shrink-0 flex-col overflow-y-auto border-l bg-background \
           px-2 pb-3 md:w-[280px]"
      ]
    [ div
        "flex h-9 items-center gap-2 px-2 text-[13px] text-muted-foreground"
        [ Icon.view ~class_:"size-4 shrink-0" Monitor
        ; span "text-foreground/90" [ t Fake.User.host ]
        ; span "min-w-0 truncate font-mono text-[11px]" [ t "~/projects/prigh" ]
        ; Button.view
            ~variant:Ghost
            ~size:Icon_xs
            ~class_:"ml-auto text-muted-foreground"
            ~attrs:[ Attr.create "aria-label" "Open folder" ]
            [ Icon.view Folder ]
        ]
    ; row (Icon.view Folder) "Open in Files" ~right:chevron
    ; row (Icon.view Plus) "Add project script"
    ; Separator.view ~class_:"my-1.5" ()
    ; row (Icon.view Git_branch) Fake.User.branch ~class_:"font-mono text-[12px]" ~right:chevron
    ; row (Icon.view Git_commit_vertical) "Commit" ~right:chevron
    ; row (Icons.view File_diff) "Changes" ~right:(counts ~added:12 ~removed:3)
    ; Separator.view ~class_:"my-1.5" ()
    ; heading "Subagents" ~count:(List.length Fake.Subagent.all)
    ; div "flex flex-col" (List.map running ~f:subagent)
    ; div "flex flex-col opacity-80" (List.map finished ~f:subagent)
    ; heading "Worktrees" ~count:(List.length Fake.Worktree.all)
    ; div "flex flex-col" (List.map Fake.Worktree.all ~f:worktree)
    ; row ~class_:"mt-0.5 text-muted-foreground" (Icon.view Plus) "New worktree…"
    ]
;;
