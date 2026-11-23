open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let icon_button ?(class_ = "") ?on_click ?(active = false) tip ~id label icon =
  Tip.wrap
    tip
    ~id
    label
    (Button.view
       ~variant:Ghost
       ~size:Icon_sm
       ~attrs:
         ([ Attr.create "aria-label" label ]
          @ if active then [ Attr.create "aria-pressed" "true" ] else [])
       ~class_:
         (Cn.cn
            [ "text-muted-foreground hover:text-foreground"
            ; (if active then "bg-accent text-accent-foreground" else "")
            ; class_
            ])
       ?on_click
       [ icon ])
;;

let session_item ~active (s : Fake.Session.t) =
  Sidebar.menu_item
    [ Sidebar.menu_button
        ~is_active:active
        ~size:Lg
        ~class_:"h-auto flex-col items-stretch gap-0.5 px-2.5 py-1.5 data-active:font-normal"
        [ div
            "flex items-center gap-2 text-xs text-muted-foreground"
            [ span "truncate" [ Node.text s.project.name ]
            ; Activity.view ~class_:"ml-auto" s.status ~age:s.age
            ]
        ; span
            (if active then "truncate text-[13px] text-foreground" else "truncate text-[13px]")
            [ Node.text s.title ]
        ]
    ]
;;

let header ~tip sidebar =
  Sidebar.header
    ~class_:"gap-2 px-2 pt-2"
    [ div
        "flex h-8 items-center gap-1"
        [ Sidebar.trigger ~class_:"text-muted-foreground" sidebar
        ; span "px-1 text-sm font-semibold tracking-tight" [ Node.text "prigh" ]
        ]
    ; div
        "flex items-center gap-0.5"
        [ Button.view
            ~variant:Ghost
            ~size:Sm
            ~class_:"flex-1 justify-start gap-2 px-2 font-normal text-muted-foreground"
            [ Icon.view Search
            ; Node.text "Search"
            ; Kbd.view ~class_:"ml-auto max-md:hidden" [ Node.text "Ctrl K" ]
            ]
        ; icon_button tip ~id:"new-session" "New session" (Icons.view Square_pen)
        ]
    ]
;;

let footer ~tip =
  Sidebar.footer
    ~class_:"gap-1 px-2 pb-2"
    [ Sidebar.menu
        [ Sidebar.menu_item
            [ Sidebar.menu_button
                ~size:Sm
                ~class_:"text-muted-foreground"
                [ Icon.view Archive
                ; span "" [ Node.text "Archive" ]
                ; span
                    "ml-auto flex items-center gap-1 text-xs tabular-nums"
                    [ Node.textf "%d" Fake.Session.settled
                    ; Icon.view ~class_:"size-3.5!" Chevron_right
                    ]
                ]
            ]
        ]
    ; div
        "flex items-center gap-0.5"
        [ icon_button tip ~id:"settings" "Settings" (Icon.view Settings)
        ; icon_button tip ~id:"prs" "Pull requests" (Icons.view Git_pull_request)
        ; icon_button tip ~id:"usage" "Usage" (Icon.view Chart_column)
        ; div "ml-auto" []
        ; Tip.wrap
            tip
            ~id:"user"
            (Fake.User.name ^ " · account")
            (Button.view
               ~variant:Ghost
               ~size:Icon_sm
               ~class_:"rounded-full"
               [ Avatar.view
                   ~size:Sm
                   ~status:Loading
                   ~on_status_change:(fun _ -> Effect.Ignore)
                   (fun a ->
                     [ Avatar.fallback
                         a
                         ~class_:"bg-accent text-[10px] font-medium"
                         [ Node.text Fake.User.initials ]
                     ])
               ])
        ]
    ]
;;

let top_bar ?more ~tip ~sidebar ~pressed ~has_right ~right_open ~set_right_open () =
  let current = Fake.Session.current in
  Node.header
    ~attrs:
      [ Attr.class_
          "flex h-12 shrink-0 items-center gap-2 px-3 pt-[env(safe-area-inset-top)]"
      ]
    [ (if Sidebar.open_ sidebar && not (Sidebar.is_mobile sidebar)
       then Node.none
       else Sidebar.trigger ~class_:"text-muted-foreground" sidebar)
    ; Breadcrumb.root
        ~class_:"min-w-0"
        [ Breadcrumb.list
            ~class_:"flex-nowrap text-sm"
            [ Breadcrumb.item
                ~class_:"max-md:hidden"
                [ Breadcrumb.link
                    ~href:"#"
                    ~class_:"flex items-center gap-1.5"
                    [ Node.text current.project.name ]
                ]
            ; Breadcrumb.separator
                ~class_:"text-muted-foreground/50 max-md:hidden"
                ~children:[ Node.text "/" ]
                ()
            ; Breadcrumb.item
                ~class_:"min-w-0"
                [ Breadcrumb.page ~class_:"truncate" [ Node.text current.title ] ]
            ]
        ]
    ; div
        "ml-auto flex items-center gap-0.5"
        [ icon_button
            tip
            ~id:"terminal"
            ~active:(List.mem pressed "terminal" ~equal:String.equal)
            "Terminal"
            (Icons.view Panel_bottom)
        ; icon_button
            tip
            ~id:"review"
            ~active:(List.mem pressed "review" ~equal:String.equal)
            "Review changes"
            (Icons.view Git_compare)
        ; (if has_right
           then
             icon_button
               tip
               ~id:"panel"
               "Session panel"
               ~active:right_open
               ~on_click:(set_right_open (not right_open))
               (Icons.view Panel_right)
           else Node.none)
        ; (match more with
           | None -> icon_button tip ~id:"more" "More" ~class_:"md:hidden" (Icon.view Ellipsis)
           | Some wrap ->
             wrap
               (Button.view
                  ~variant:Ghost
                  ~size:Icon_sm
                  ~attrs:[ Attr.create "aria-label" "Session" ]
                  ~class_:
                    "text-muted-foreground hover:text-foreground \
                     aria-expanded:bg-accent aria-expanded:text-accent-foreground"
                  [ Icon.view Ellipsis ]))
        ]
    ]
;;

let mobile_sheet ~close ~title content =
  div
    "fixed inset-0 z-40 flex flex-col bg-background pt-[env(safe-area-inset-top)] \
     pb-[env(safe-area-inset-bottom)]"
    [ div
        "flex h-12 shrink-0 items-center gap-1 border-b px-2"
        [ Button.view
            ~variant:Ghost
            ~size:Sm
            ~class_:"h-11 gap-1 px-2 text-muted-foreground"
            ~on_click:close
            [ Icon.view Chevron_left; Node.text "Chat" ]
        ; span "truncate text-sm font-medium" [ Node.text title ]
        ]
    ; div "flex min-h-0 flex-1 flex-col" [ content ]
    ]
;;

let component
  ?right
  ?right_size
  ?(right_title = "Session")
  ?bottom
  ?(bottom_size = 38.)
  ?overlay
  ?(pressed = [])
  ?more
  ~tip
  ~main
  (local_ graph)
  =
  let sidebar = Sidebar.component ~persist:false graph in
  (* None: as the width suits, open on desktop and closed on phones. *)
  let right_choice, set_right_choice = Bonsai.state (None : bool option) graph in
  let use_card = Option.is_none right in
  let right =
    match right with
    | None -> Bonsai.return None
    | Some r -> r >>| Option.some
  in
  let subagents_open, set_subagents_open = Bonsai.state true graph in
  let worktrees_open, set_worktrees_open = Bonsai.state false graph in
  let bottom =
    match bottom with
    | None -> Bonsai.return None
    | Some b -> b >>| Option.some
  in
  let right_layout, set_right_layout = Bonsai.state ([] : float list) graph in
  let bottom_layout, set_bottom_layout = Bonsai.state ([] : float list) graph in
  let overlay = Option.value overlay ~default:(Bonsai.return Node.none) in
  let more =
    match more with
    | None -> Bonsai.return None
    | Some m -> m >>| Option.some
  in
  let%arr sidebar
  and more
  and tip
  and main
  and right
  and bottom
  and overlay
  and right_choice
  and set_right_choice
  and subagents_open
  and set_subagents_open
  and worktrees_open
  and set_worktrees_open
  and right_layout
  and set_right_layout
  and bottom_layout
  and set_bottom_layout in
  let current = Fake.Session.current in
  let mobile = Sidebar.is_mobile sidebar in
  let right_open = Option.value right_choice ~default:(not mobile) in
  let set_right_open b = set_right_choice (Some b) in
  let main_column = div "flex h-full min-w-0 flex-1 flex-col" [ main ] in
  let main_and_bottom =
    match bottom with
    | Some b when not mobile ->
      Resizable.view
        ~id:"bottom-split"
        ~orientation:Vertical
        ~layout:bottom_layout
        ~on_layout_change:set_bottom_layout
        [ Resizable.panel ~min_size:25. [ main_column ]
        ; Resizable.handle ~class_:"bg-border" ()
        ; Resizable.panel ~default_size:bottom_size ~min_size:15. [ b ]
        ]
    | _ -> main_column
  in
  let card ?class_ () =
    Session_panel.view
      ?class_
      ~subagents_open
      ~worktrees_open
      ~toggle_subagents:(set_subagents_open (not subagents_open))
      ~toggle_worktrees:(set_worktrees_open (not worktrees_open))
      ()
  in
  let body =
    match right, right_size with
    | _ when use_card && mobile ->
      [ main_and_bottom
      ; Drawer.view
          ~id:"session-drawer"
          ~open_:right_open
          ~on_open_change:set_right_open
          (fun d ->
            [ Drawer.content
                d
                ~class_:"pb-[env(safe-area-inset-bottom)]"
                [ Drawer.header
                    ~class_:"sr-only"
                    [ Drawer.title d [ Node.text "Session" ] ]
                ; card
                    ~class_:
                      "w-full border-0 bg-transparent px-2 pb-3 [&_a]:h-11 \
                       [&_button]:h-11 [&_button]:text-sm"
                    ()
                ]
            ])
      ]
    | _ when use_card ->
      (* The card sits in the chat's right margin; [--push] shifts the chat left
         only by what the margin lacks: the column is 760px with 24px of padding,
         the card 208px with 12px either side of it. Below an 820px main area it
         overlaps instead, rather than squeezing the chat. *)
      [ div
          "@container relative flex min-w-0 flex-1"
          [ div
              (if right_open
               then "flex min-w-0 flex-1 [--push:max(0px,calc(1176px-100cqw))] \
                     @max-[820px]:[--push:0px]"
               else "flex min-w-0 flex-1")
              [ main_and_bottom ]
          ; (if right_open then div "absolute top-1 right-3 z-10" [ card () ] else Node.none)
          ]
      ]
    | Some r, _ when right_open && mobile ->
      [ main_and_bottom
      ; mobile_sheet ~close:(set_right_open false) ~title:right_title r
      ]
    | Some r, Some size when right_open ->
      [ Resizable.view
          ~id:"right-split"
          ~orientation:Horizontal
          ~layout:right_layout
          ~on_layout_change:set_right_layout
          [ Resizable.panel ~min_size:30. [ main_and_bottom ]
          ; Resizable.handle ~class_:"bg-border" ()
          ; Resizable.panel ~default_size:size ~min_size:22. [ r ]
          ]
      ]
    | Some r, None when right_open -> [ main_and_bottom; r ]
    | _ -> [ main_and_bottom ]
  in
  Node.div
    ~attrs:[ Attr.class_ "bg-background text-foreground font-sans antialiased" ]
    [ Sidebar.provider
        ~class_:"h-svh min-h-0"
        sidebar
        [ Sidebar.root
            sidebar
            ~class_:"border-r-0"
            [ header ~tip sidebar
            ; Sidebar.content
                ~class_:"gap-0 px-2"
                [ Sidebar.group
                    ~class_:"px-0"
                    [ Sidebar.menu
                        ~class_:"gap-0.5"
                        (List.map Fake.Session.recent ~f:(fun s ->
                           session_item ~active:(phys_equal s current) s))
                    ]
                ]
            ; footer ~tip
            ]
        ; Sidebar.inset
            ~class_:"min-h-0 overflow-hidden"
            [ top_bar
                ?more
                ~tip
                ~sidebar
                ~pressed
                ~has_right:(use_card || Option.is_some right)
                ~right_open
                ~set_right_open
                ()
            ; div "relative flex min-h-0 flex-1" body
            ]
        ]
    ; overlay
    ]
;;
