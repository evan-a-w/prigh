open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

module File = struct
  type t =
    { dir : string
    ; name : string
    ; added : int
    ; removed : int
    ; status : [ `Modified | `Added | `Deleted ]
    }
end

let files : File.t list =
  [ { dir = "frontend/web/mockup/"; name = "composer.ml"; added = 48; removed = 21; status = `Modified }
  ; { dir = "frontend/web/mockup/"; name = "chip.ml"; added = 32; removed = 0; status = `Added }
  ; { dir = "frontend/web/mockup/"; name = "chip.mli"; added = 9; removed = 0; status = `Added }
  ; { dir = "frontend/web/e2e/"; name = "mobile.mjs"; added = 6; removed = 2; status = `Modified }
  ; { dir = "frontend/web/bin/"; name = "phone.css"; added = 0; removed = 37; status = `Deleted }
  ]
;;

module Line = struct
  type t =
    | Hunk of string
    | Ctx of int * int * string
    | Add of int * string
    | Del of int * string
    | Thread
end

let diff : Line.t list =
  [ Hunk "@@ -88,14 +88,19 @@ let view ?(queued = []) ~tip () ="
  ; Ctx (88, 88, "        ; Input_group.addon")
  ; Ctx (89, 89, "            ~align:Block_end")
  ; Del (90, "            ~class_:\"gap-0.5 px-2 pb-2\"")
  ; Add (90, "            ~class_:\"gap-0.5 px-2 pb-2 max-sm:gap-0\"")
  ; Ctx (91, 91, "            [ attach ~tip")
  ; Del (92, "            ; chip ~tip ~id:\"model\" \"Model\" (Icon.view Sparkles)")
  ; Del (93, "                Fake.User.model")
  ; Add (92, "            ; Chip.model ~tip Fake.User.model")
  ; Add (93, "            ; Chip.thinking ~tip Fake.User.thinking")
  ; Thread
  ; Add (94, "            ; Chip.mode ~tip ~class_:\"max-sm:hidden\" Fake.User.mode")
  ; Add (95, "            ; Chip.host ~tip ~class_:\"max-sm:hidden\" Fake.User.host")
  ; Ctx (94, 96, "            ; div \"ml-auto\" []")
  ; Ctx (95, 97, "            ; status ~tip ~context:0.42 ~cost:1.84")
  ; Hunk "@@ -131,9 +133,6 @@ let view ?(queued = []) ~tip () ="
  ; Ctx (131, 133, "    ; div")
  ; Ctx (132, 134, "        \"flex items-center justify-between gap-4 px-3 pt-2\"")
  ; Del (133, "        [ (* the checkout, then the branch, on one row *)")
  ; Del (134, "          span")
  ; Del (135, "            \"flex min-w-0 items-center gap-1.5\"")
  ; Add (135, "        [ checkout Fake.User.cwd")
  ; Add (136, "        ; branch Fake.User.branch")
  ; Ctx (136, 137, "        ]")
  ]
;;

let file_icon (f : File.t) =
  match f.status with
  | `Added -> span "text-[10px] font-semibold text-emerald-500" [ Node.text "A" ]
  | `Deleted -> span "text-[10px] font-semibold text-red-500" [ Node.text "D" ]
  | `Modified -> span "text-[10px] font-semibold text-amber-500" [ Node.text "M" ]
;;

let file_row ~selected (f : File.t) =
  Node.button
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "flex w-full items-center gap-2 rounded-md px-2 py-1 text-left text-xs \
                hover:bg-accent/50"
             ; (if selected then "bg-accent text-accent-foreground" else "text-muted-foreground")
             ])
      ]
    [ span "flex w-3 justify-center" [ file_icon f ]
    ; span
        "min-w-0 flex-1 truncate"
        [ span "text-foreground/90" [ Node.text f.name ]
        ; span "ml-1.5 text-muted-foreground/70" [ Node.text f.dir ]
        ]
    ; Panel.counts f.added f.removed
    ]
;;

let num n = span "w-9 shrink-0 select-none pr-2 text-right text-muted-foreground/50" [ Node.text n ]

let code_line ~bg ~sign ~old_n ~new_n text =
  div
    (Cn.cn [ "flex min-w-max leading-[20px]"; bg ])
    [ num old_n
    ; num new_n
    ; span "w-4 shrink-0 select-none text-center text-muted-foreground/60" [ Node.text sign ]
    ; span "whitespace-pre pr-4" [ Node.text text ]
    ]
;;

let avatar ~class_ initials =
  span
    (Cn.cn
       [ "flex size-5 shrink-0 items-center justify-center rounded-full text-[9px] \
          font-semibold"
       ; class_
       ])
    [ Node.text initials ]
;;

let comment ~who ~initials ~avatar_class ~age text =
  div
    "flex gap-2"
    [ avatar ~class_:avatar_class initials
    ; div
        "flex min-w-0 flex-col gap-0.5"
        [ div
            "flex items-center gap-1.5 text-xs"
            [ span "font-medium text-foreground" [ Node.text who ]
            ; span "text-muted-foreground/70" [ Node.text age ]
            ]
        ; div "text-[13px] leading-relaxed text-foreground/90" text
        ]
    ]
;;

let inline_code s =
  Node.code
    ~attrs:[ Attr.class_ "rounded bg-muted px-1 font-mono text-[0.9em]" ]
    [ Node.text s ]
;;

let thread =
  div
    "my-1.5 ml-[5.5rem] mr-4 flex max-w-[520px] flex-col gap-3 rounded-lg border bg-card \
     p-3 font-sans whitespace-normal"
    [ comment
        ~who:Fake.User.name
        ~initials:Fake.User.initials
        ~avatar_class:"bg-accent text-foreground"
        ~age:"4m"
        [ Node.text "Should thinking hide on 320px too? The row still wraps there." ]
    ; comment
        ~who:"prigh"
        ~initials:"P"
        ~avatar_class:"bg-primary text-primary-foreground"
        ~age:"3m"
        [ Node.text "At 320px the row fits with "
        ; inline_code "gap-0"
        ; Node.text "; the e2e's mobile check passes. I'd keep thinking visible."
        ]
    ; div
        "flex items-center gap-2"
        [ Input.view ~class_:"h-7 flex-1 text-xs" ~placeholder:"Reply…" ()
        ; Button.view ~variant:Ghost ~size:Xs [ Icon.view Check; Node.text "Resolve" ]
        ]
    ]
;;

let diff_view =
  let rows =
    List.map diff ~f:(function
      | Line.Hunk h ->
        div
          "mt-1 bg-sky-500/8 px-3 leading-[22px] text-sky-300/80 first:mt-0"
          [ Node.text h ]
      | Ctx (o, n, t) ->
        code_line ~bg:"" ~sign:"" ~old_n:(Int.to_string o) ~new_n:(Int.to_string n) t
      | Add (n, t) ->
        code_line
          ~bg:"bg-emerald-500/12 text-emerald-50/95 [&>span:nth-child(3)]:text-emerald-400"
          ~sign:"+"
          ~old_n:""
          ~new_n:(Int.to_string n)
          t
      | Del (o, t) ->
        code_line
          ~bg:"bg-red-500/12 text-red-50/90 [&>span:nth-child(3)]:text-red-400"
          ~sign:"−"
          ~old_n:(Int.to_string o)
          ~new_n:""
          t
      | Thread -> thread)
  in
  let f = List.hd_exn files in
  div
    "m-3 overflow-hidden rounded-lg border"
    [ div
        "flex items-center gap-2 border-b bg-muted/40 px-3 py-1.5 text-xs"
        [ Icon.view ~class_:"size-3.5 text-muted-foreground" Chevron_down
        ; span "font-mono text-foreground/90" [ Node.text (f.dir ^ f.name) ]
        ; Panel.counts f.added f.removed
        ; div
            "ml-auto flex items-center gap-1.5 text-muted-foreground"
            [ Checkbox.view
                ~id:"viewed"
                ~class_:"size-3.5"
                ~checked:Unchecked
                ~on_change:(fun _ -> Effect.Ignore)
                ()
            ; Label.view ~for_:"viewed" ~class_:"text-xs font-normal" [ Node.text "Viewed" ]
            ]
        ]
    ; div "overflow-x-auto py-1 font-mono text-[12px]" rows
    ]
;;

let changes ~tip =
  div
    "flex min-h-0 flex-1 flex-col"
    [ div
        "flex items-center gap-2 px-3 pt-3 pb-1 text-xs text-muted-foreground"
        [ Icon.view ~class_:"size-3.5" Git_branch
        ; span "font-mono text-foreground/90" [ Node.text "feat/composer-chips" ]
        ; Icon.view ~class_:"size-3" Arrow_right
        ; span "font-mono" [ Node.text "main" ]
        ; span "ml-auto" [ Node.textf "%d files" (List.length files) ]
        ; Panel.counts 95 60
        ]
    ; div
        "flex flex-col gap-px border-b px-1.5 pb-2"
        (List.mapi files ~f:(fun i f -> file_row ~selected:(i = 0) f))
    ; div
        "min-h-0 flex-1 overflow-y-auto"
        [ diff_view
        ; div
            "flex items-center justify-center gap-1.5 pb-4 text-xs text-muted-foreground/70"
            [ Panel.icon_button tip ~id:"prev-file" "Previous file" (Icon.view Chevron_up)
            ; span "" [ Node.text "1 of 5 files · 1 open thread" ]
            ; Panel.icon_button tip ~id:"next-file" "Next file" (Icon.view Chevron_down)
            ]
        ]
    ]
;;

let panel ~tip ~tab ~set_tab =
  Tabs.view ~id:"review-tabs" ~class_:"h-full gap-0" ~value:tab ~on_change:set_tab (fun t ->
    [ Panel.view
        ~title:
          [ Tabs.list
              ~variant:Line
              ~class_:"h-10 min-w-0 gap-1 overflow-x-auto"
              t
              [ Tabs.trigger ~value:"changes" [ Node.text "Changes"; Badge.view ~variant:Secondary ~class_:"h-4 px-1.5 text-[10px]" [ Node.text "5" ] ]
              ; Tabs.trigger ~value:"pr" [ Node.text "Pull request" ]
              ; Tabs.trigger ~value:"files" [ Node.text "Files" ]
              ]
          ]
        ~actions:
          [ Button.view
              ~variant:Outline
              ~size:Xs
              ~class_:"mr-1 gap-1"
              [ Icon.view Git_commit_vertical; span "max-md:hidden" [ Node.text "Commit" ] ]
          ; Button.view
              ~variant:Default
              ~size:Xs
              ~class_:"mr-1 gap-1"
              [ Icons.view Git_pull_request; span "max-md:hidden" [ Node.text "Open PR" ] ]
          ; Panel.icon_button tip ~id:"review-more" "More" (Icon.view Ellipsis)
          ]
        [ Tabs.content t ~value:"changes" ~class_:"flex min-h-0 flex-1 flex-col" [ changes ~tip ]
        ; Tabs.content
            t
            ~value:"pr"
            ~class_:"p-6 text-muted-foreground"
            [ Node.text "No pull request for this branch yet." ]
        ; Tabs.content t ~value:"files" ~class_:"p-6 text-muted-foreground" [ Node.text "Files" ]
        ]
    ])
;;

let component ~tip (local_ graph) =
  let tab, set_tab = Bonsai.state "changes" graph in
  let right =
    let%arr tab and set_tab and tip in
    panel ~tip ~tab ~set_tab
  in
  Shell.component
    ~right
    ~right_size:52.
    ~right_title:"Review changes"
    ~pressed:[ "review" ]
    ~tip
    ~main:(Screen_chat.component ~tip graph)
    graph
;;
