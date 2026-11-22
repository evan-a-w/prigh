open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

module Status = struct
  type t =
    | Connected of int (** its tools *)
    | Connecting
    | Needs_sign_in
    | Failed of string
    | Disabled
end

module Server = struct
  type t =
    { name : string
    ; transport : string
    ; target : string
    ; status : Status.t
    }
end

let yours : Server.t list =
  [ { name = "github"
    ; transport = "http"
    ; target = "https://api.githubcopilot.com/mcp/"
    ; status = Connected 38
    }
  ; { name = "linear"
    ; transport = "http"
    ; target = "https://mcp.linear.app/sse"
    ; status = Needs_sign_in
    }
  ; { name = "sentry"
    ; transport = "http"
    ; target = "https://mcp.sentry.dev/mcp"
    ; status = Disabled
    }
  ]
;;

let project : Server.t list =
  [ { name = "playwright"
    ; transport = "stdio"
    ; target = "npx @playwright/mcp@latest --headless"
    ; status = Connected 21
    }
  ; { name = "postgres"
    ; transport = "stdio"
    ; target = "uvx mcp-server-postgres postgresql://localhost/prigh"
    ; status = Failed "exited with code 1: connection refused (127.0.0.1:5432)"
    }
  ; { name = "docs"; transport = "stdio"; target = "./tools/docs-mcp"; status = Connecting }
  ]
;;

let dot cls = span (Cn.cn [ "size-1.5 rounded-full"; cls ]) []

let status_badge : Status.t -> Node.t = function
  | Connected n ->
    Badge.view
      ~variant:Outline
      ~class_:"gap-1.5 font-normal text-muted-foreground"
      [ dot "bg-emerald-400"; Node.textf "%d tools" n ]
  | Connecting ->
    Badge.view
      ~variant:Outline
      ~class_:"gap-1.5 font-normal text-muted-foreground"
      [ Spinner.view ~class_:"size-3" (); Node.text "Connecting" ]
  | Needs_sign_in ->
    Badge.view
      ~variant:Outline
      ~class_:"gap-1.5 border-amber-400/30 font-normal text-amber-300"
      [ dot "bg-amber-400"; Node.text "Needs sign-in" ]
  | Failed _ ->
    Badge.view
      ~variant:Outline
      ~class_:"gap-1.5 border-destructive/40 font-normal text-destructive"
      [ dot "bg-destructive"; Node.text "Failed" ]
  | Disabled ->
    Badge.view
      ~variant:Outline
      ~class_:"font-normal text-muted-foreground/70"
      [ Node.text "Off" ]
;;

let row_menu (local_ graph) =
  Dropdown_menu.component
    (Bonsai.return (fun menu ->
       [ Dropdown_menu.trigger
           menu
           (Button.view
              ~variant:Ghost
              ~size:Icon_sm
              ~class_:"text-muted-foreground max-md:size-10"
              ~attrs:[ Attr.create "aria-label" "Server actions" ]
              [ Icon.view Ellipsis ])
       ; Dropdown_menu.content
           menu
           ~align:End
           ~class_:"w-44"
           [ Dropdown_menu.item [ Icon.view Refresh_cw; Node.text "Reconnect" ]
           ; Dropdown_menu.item [ Icon.view Pencil; Node.text "Edit" ]
           ; Dropdown_menu.item [ Icon.view File_text; Node.text "View log" ]
           ; Dropdown_menu.item [ Icon.view Copy; Node.text "Copy command" ]
           ; Dropdown_menu.separator ()
           ; Dropdown_menu.item
               ~variant:Destructive
               [ Icon.view Trash_2; Node.text "Remove" ]
           ]
       ]))
    graph
;;

let server_row (s : Server.t) (local_ graph) =
  let enabled, set_enabled =
    Bonsai.state
      (match s.status with
       | Disabled -> false
       | _ -> true)
      graph
  in
  let menu = row_menu graph in
  let%arr enabled and set_enabled and menu in
  let icon =
    if String.equal s.transport "http" then Icon.view Globe else Icon.view Terminal
  in
  div
    "flex flex-col gap-2 px-4 py-3"
    [ div
        "flex items-center gap-3"
        [ div
            "flex size-8 shrink-0 items-center justify-center rounded-md border \
             bg-background text-muted-foreground [&_svg]:size-4"
            [ icon ]
        ; div
            "min-w-0 flex-1"
            [ div
                "flex items-center gap-2"
                [ span "font-mono text-[13px] font-medium" [ Node.text s.name ]
                ; span
                    "rounded bg-muted px-1.5 py-px text-[10px] uppercase tracking-wide \
                     text-muted-foreground"
                    [ Node.text s.transport ]
                ]
            ; div
                "mt-0.5 truncate font-mono text-xs text-muted-foreground"
                [ Node.text s.target ]
            ]
        ; div "shrink-0 max-sm:hidden" [ status_badge s.status ]
        ; (match s.status with
           | Needs_sign_in ->
             Button.view ~variant:Outline ~size:Xs ~class_:"max-sm:hidden" [ Node.text "Sign in" ]
           | _ -> Node.none)
        ; Switch.view
            ~size:Sm
            ~attrs:[ Attr.create "aria-label" ("Enable " ^ s.name) ]
            ~checked:enabled
            ~on_change:set_enabled
            ()
        ; menu
        ]
    ; div "flex items-center gap-2 pl-11 sm:hidden" [ status_badge s.status ]
    ; (match s.status with
       | Failed e ->
         div
           "ml-11 flex items-start gap-2 rounded-md border border-destructive/25 \
            bg-destructive/10 px-2.5 py-2 text-xs text-destructive"
           [ Icon.view ~class_:"mt-px size-3.5 shrink-0" Circle_alert
           ; span "min-w-0 flex-1 font-mono break-words" [ Node.text e ]
           ; Button.view
               ~variant:Link
               ~size:Xs
               ~class_:"h-auto p-0 text-destructive"
               [ Node.text "View log" ]
           ]
       | _ -> Node.none)
    ]
;;

let group ~title ~description ~path servers (local_ graph) =
  let rows = List.map servers ~f:(fun s -> server_row s graph) |> Bonsai.all in
  let%arr rows in
  div
    "flex flex-col gap-2.5"
    [ div
        "flex items-end gap-2 px-1"
        [ div
            "min-w-0 flex-1"
            [ Node.h2 ~attrs:[ Attr.class_ "text-sm font-medium" ] [ Node.text title ]
            ; Node.p
                ~attrs:[ Attr.class_ "mt-0.5 text-xs text-muted-foreground" ]
                [ Node.text description
                ; Node.code
                    ~attrs:[ Attr.class_ "ml-1 font-mono text-[11px] text-foreground/80" ]
                    [ Node.text path ]
                ]
            ]
        ]
    ; div "overflow-hidden rounded-xl border bg-card divide-y divide-border" rows
    ]
;;

let component ~tip:_ (local_ graph) =
  let yours =
    group
      ~title:"Yours"
      ~description:"In every project, from"
      ~path:"~/.prigh/mcp.json"
      yours
      graph
  in
  let project =
    group
      ~title:"This project"
      ~description:"prigh's, from"
      ~path:".mcp.json"
      project
      graph
  in
  let body =
    let%arr yours and project in
    div
      "flex flex-col gap-8"
      [ yours
      ; project
      ; div
          "flex items-center gap-2 px-1 text-xs text-muted-foreground"
          [ Icon.view ~class_:"size-3.5" Info
          ; Node.text "Sessions pick up changes when they next start a turn."
          ]
      ]
  in
  Settings_page.component
    ~section:Mcp
    ~description:"Tools from Model Context Protocol servers, for every session."
    ~actions:
      [ Button.view
          ~size:Sm
          ~class_:"gap-1.5"
          [ Icon.view Plus; Node.text "Add server" ]
      ]
    ~body
    graph
;;
