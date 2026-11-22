open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let prompt cmd =
  div
    ""
    [ span "text-emerald-400" [ Node.text "lloyd@desktop" ]
    ; span "text-muted-foreground" [ Node.text ":" ]
    ; span "text-sky-400" [ Node.text "~/p/prigh" ]
    ; span "text-fuchsia-400" [ Node.text " (feat/new-ui)" ]
    ; span "text-muted-foreground" [ Node.text " $ " ]
    ; Node.text cmd
    ]
;;

let line ?(cls = "") s = div cls [ Node.text s ]

let terminal =
  div
    "min-h-0 flex-1 overflow-y-auto bg-[#0d1319] px-3 py-2 font-mono text-[12px] \
     leading-[18px] text-foreground/90"
    [ prompt "dune build @runtest 2>&1 | tail -n 6"
    ; line ~cls:"text-muted-foreground" "Done: 1873/1902 (jobs: 16)"
    ; line "File \"web/test/test_composer.ml\", line 42, characters 0-1:"
    ; line ~cls:"text-red-400" "-|  [+] [Opus 4.5] [High] [Auto] [desktop]"
    ; line ~cls:"text-emerald-400" "+|  [+] [Opus 4.5] [High]   42% $1.84"
    ; line ~cls:"text-amber-300" "Error: 1 test diff (run `dune promote`)"
    ; prompt "dune promote && dune build @runtest"
    ; line ~cls:"text-muted-foreground" "Promoting _build/default/web/test/test_composer.ml.corrected"
    ; line ~cls:"text-emerald-400" "✓ 412 tests passed in 38.2s"
    ; div "[&>div]:inline" [ prompt ""; span "inline-block h-[14px] w-[7px] translate-y-[2px] bg-foreground/80 animate-pulse" [] ]
    ]
;;

let tab ~active name icon =
  Node.button
    ~attrs:
      [ Attr.class_
          (Cn.cn
             [ "flex h-7 items-center gap-1.5 rounded-md px-2 text-xs"
             ; (if active
                then "bg-accent text-foreground"
                else "text-muted-foreground hover:text-foreground")
             ])
      ]
    [ icon; Node.text name ]
;;

let panel ~tip =
  div
    "flex h-full min-h-0 flex-col bg-[#0d1319]"
    [ div
        "flex h-9 shrink-0 items-center gap-1 border-b bg-background px-2"
        [ tab ~active:true "zsh" (Icon.view ~class_:"size-3.5" Terminal)
        ; tab ~active:false "dune build -w" (Spinner.view ~class_:"size-3 text-sky-400" ())
        ; tab ~active:false "serve :7790" (span "size-1.5 rounded-full bg-emerald-400" [])
        ; Panel.icon_button tip ~id:"term-new" "New terminal" (Icon.view Plus)
        ; div "ml-auto flex items-center gap-0.5 text-xs text-muted-foreground" [ span "mr-2" [ Node.text "desktop" ] ]
        ; Panel.icon_button tip ~id:"term-split" "Split" (Icon.view Columns_2)
        ; Panel.icon_button tip ~id:"term-max" "Maximise" (Icon.view Fullscreen)
        ; Panel.icon_button tip ~id:"term-close" "Close panel" (Icon.view X)
        ]
    ; terminal
    ]
;;

let component ~tip (local_ graph) =
  Shell.component
    ~bottom:(Bonsai.map tip ~f:(fun tip -> panel ~tip))
    ~pressed:[ "terminal" ]
    ~tip
    ~main:(Screen_chat.component ~tip graph)
    graph
;;
