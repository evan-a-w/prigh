open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

module Section = struct
  type t =
    | General
    | Appearance
    | Models
    | Hosts
    | Git_hosting
    | Mcp
    | Memory
    | About_you
    | Instructions
    | Keys
    | Account
  [@@deriving equal]

  let title = function
    | General -> "General"
    | Appearance -> "Appearance"
    | Models -> "Models & providers"
    | Hosts -> "Hosts"
    | Git_hosting -> "Git hosting"
    | Mcp -> "MCP servers"
    | Memory -> "Memory"
    | About_you -> "About you"
    | Instructions -> "Custom instructions"
    | Keys -> "Keyboard shortcuts"
    | Account -> "Account"
  ;;

  let icon = function
    | General -> Icon.view Settings_2
    | Appearance -> Icon.view Paintbrush
    | Models -> Icon.view Sparkles
    | Hosts -> Icon.view Server
    | Git_hosting -> Icon.view Github
    | Mcp -> Icon.view Blocks
    | Memory -> Icon.view Brain
    | About_you -> Icon.view Circle_user
    | Instructions -> Icon.view Message_square
    | Keys -> Icon.view Keyboard
    | Account -> Icon.view User
  ;;

  (* The mockups that exist; the other sections are inert. *)
  let href = function
    | Appearance -> Some "#settings"
    | Mcp -> Some "#settings-mcp"
    | Keys -> Some "#settings-keys"
    | _ -> None
  ;;

  let groups =
    [ "App", [ General; Appearance; Keys ]
    ; "Agent", [ Models; Hosts; Mcp; Git_hosting ]
    ; "You", [ About_you; Instructions; Memory; Account ]
    ]
  ;;
end

let nav_item ~current ~show_section (s : Section.t) =
  let active = Section.equal s current in
  let attrs =
    match Section.href s with
    | Some _ when active -> [ Attr.on_click (fun _ -> show_section) ]
    | Some href -> [ Attr.href href ]
    | None -> []
  in
  Node.a
    ~attrs:
      ([ Attr.class_
           (Cn.cn
              [ "flex h-8 items-center gap-2.5 rounded-md px-2 text-sm \
                 text-sidebar-foreground/80 outline-none select-none \
                 hover:bg-sidebar-accent/60 hover:text-sidebar-foreground \
                 focus-visible:ring-2 focus-visible:ring-sidebar-ring \
                 max-md:h-12 max-md:gap-3 max-md:rounded-none max-md:px-4 \
                 max-md:text-[15px] [&_svg]:size-4 [&_svg]:shrink-0 \
                 [&_svg]:text-muted-foreground"
              ; (if active
                 then
                   "md:bg-sidebar-accent md:text-sidebar-accent-foreground \
                    md:[&_svg]:text-sidebar-accent-foreground"
                 else "")
              ; (if Option.is_none (Section.href s) then "cursor-default" else "cursor-pointer")
              ])
       ]
       @ (if active then [ Attr.create "aria-current" "page" ] else [])
       @ attrs)
    [ Section.icon s
    ; span "truncate" [ Node.text (Section.title s) ]
    ; Icon.view ~class_:"ml-auto size-4 text-muted-foreground/60 md:hidden" Chevron_right
    ]
;;

let nav ~current ~show_section ~show_list =
  Node.nav
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "flex w-60 shrink-0 flex-col gap-4 overflow-y-auto border-r \
                border-sidebar-border bg-sidebar px-2 py-2 \
                max-md:w-full max-md:border-r-0 max-md:bg-background max-md:px-0 \
                max-md:pt-[env(safe-area-inset-top)] \
                max-md:pb-[env(safe-area-inset-bottom)]"
             ; (if show_list then "" else "max-md:hidden")
             ])
      ]
    [ div
        "flex h-8 items-center gap-1 max-md:h-12 max-md:px-2"
        [ Button.link
            ~href:"#chat"
            ~variant:Ghost
            ~size:Sm
            ~class_:
              "gap-1.5 px-2 font-normal text-muted-foreground hover:text-foreground \
               max-md:h-11 max-md:text-[15px]"
            [ Icon.view Arrow_left; Node.text "Back to chat" ]
        ]
    ; div
        "px-2 text-[15px] font-semibold max-md:px-4 max-md:text-2xl md:hidden"
        [ Node.text "Settings" ]
    ; div
        "flex flex-col gap-4 max-md:gap-6"
        (List.map Section.groups ~f:(fun (label, sections) ->
           div
             "flex flex-col gap-px"
             [ div
                 "px-2 pb-1 text-xs font-medium text-muted-foreground max-md:px-4"
                 [ Node.text label ]
             ; div
                 "flex flex-col gap-px max-md:mx-3 max-md:overflow-hidden \
                  max-md:rounded-xl max-md:border max-md:bg-card \
                  max-md:divide-y max-md:divide-border max-md:gap-0"
                 (List.map sections ~f:(nav_item ~current ~show_section))
             ]))
    ; div
        "mt-auto flex items-center gap-2 px-2 pt-2 text-xs text-muted-foreground \
         max-md:px-4 max-md:pb-2"
        [ Node.text "prigh 0.9.2"
        ; span "text-muted-foreground/50" [ Node.text "·" ]
        ; Node.text Fake.User.host
        ]
    ]
;;

let component ?(start_on_list = false) ?(actions = []) ~section ~description ~body
  (local_ graph)
  =
  let show_list, set_show_list = Bonsai.state start_on_list graph in
  let%arr show_list and set_show_list and body in
  div
    "flex h-svh min-h-0 bg-background font-sans text-foreground antialiased"
    [ nav ~current:section ~show_section:(set_show_list false) ~show_list
    ; Node.main
        ~attrs:
          [ Attr.class_
              (Cn.cn
                 [ "flex min-w-0 flex-1 flex-col"
                 ; (if show_list then "max-md:hidden" else "")
                 ])
          ]
        [ div
            "flex h-12 shrink-0 items-center gap-1 border-b px-2 \
             pt-[env(safe-area-inset-top)] md:hidden"
            [ Button.view
                ~variant:Ghost
                ~class_:"h-11 gap-1 px-2 text-[15px] font-normal text-muted-foreground"
                ~on_click:(set_show_list true)
                [ Icon.view Chevron_left; Node.text "Settings" ]
            ]
        ; div
            "min-h-0 flex-1 overflow-y-auto"
            [ div
                "mx-auto flex w-full max-w-2xl flex-col gap-8 px-8 pt-12 pb-16 \
                 max-md:px-4 max-md:pt-5 max-md:pb-[max(4rem,env(safe-area-inset-bottom))]"
                [ div
                    "flex items-end gap-4"
                    [ div
                        "min-w-0 flex-1"
                        [ Node.h1
                            ~attrs:[ Attr.class_ "text-xl font-semibold tracking-tight" ]
                            [ Node.text (Section.title section) ]
                        ; Node.p
                            ~attrs:[ Attr.class_ "mt-1 text-sm text-muted-foreground" ]
                            [ Node.text description ]
                        ]
                    ; div "flex shrink-0 items-center gap-2" actions
                    ]
                ; body
                ]
            ]
        ]
    ]
;;

let group ?title ?description rows =
  div
    "flex flex-col gap-2.5"
    [ (match title with
       | None -> Node.none
       | Some title ->
         div
           "px-1"
           [ Node.h2 ~attrs:[ Attr.class_ "text-sm font-medium" ] [ Node.text title ]
           ; (match description with
              | None -> Node.none
              | Some d ->
                Node.p
                  ~attrs:[ Attr.class_ "mt-0.5 text-xs text-muted-foreground" ]
                  [ Node.text d ])
           ])
    ; div
        "overflow-hidden rounded-xl border bg-card divide-y divide-border"
        rows
    ]
;;

let row ?(stack = false) ?for_ ~title ?description control =
  let text =
    div
      "min-w-0 flex-1"
      [ Label.view ?for_ ~class_:"text-sm font-normal" [ Node.text title ]
      ; (match description with
         | None -> Node.none
         | Some d ->
           Node.p
             ~attrs:[ Attr.class_ "mt-0.5 text-xs leading-relaxed text-muted-foreground" ]
             [ Node.text d ])
      ]
  in
  if stack
  then div "flex flex-col gap-3 px-4 py-3.5" [ text; control ]
  else
    div
      "flex min-h-14 items-center gap-6 px-4 py-3 max-md:gap-4"
      [ text; div "flex shrink-0 items-center" [ control ] ]
;;
