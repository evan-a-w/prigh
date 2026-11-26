open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

(* A pie, not a ring: a ring reads as a spinner. *)
let ring fraction =
  let svg name attrs = Node.create_svg name ~attrs [] in
  let pie_c = 2. *. Float.pi *. 3.5 in
  Node.create_svg
    "svg"
    ~attrs:[ Attr.create "viewBox" "0 0 18 18"; Attr.class_ "size-4 -rotate-90" ]
    [ svg
        "circle"
        [ Attr.create "cx" "9"
        ; Attr.create "cy" "9"
        ; Attr.create "r" "8"
        ; Attr.create "fill" "none"
        ; Attr.create "stroke" "currentColor"
        ; Attr.create "stroke-width" "1.5"
        ]
    ; svg
        "circle"
        [ Attr.create "cx" "9"
        ; Attr.create "cy" "9"
        ; Attr.create "r" "3.5"
        ; Attr.create "fill" "none"
        ; Attr.create "stroke" "currentColor"
        ; Attr.create "stroke-width" "7"
        ; Attr.create "stroke-dasharray" (sprintf "%.2f %.2f" (pie_c *. fraction) pie_c)
        ]
    ]
;;

let chip ?(class_ = "") ?(wrap = fun _ -> None) ~tip ~id label icon text =
  let with_tip button =
    match wrap id with
    | Some f -> f button
    | None -> Tip.wrap ~side:Top tip ~id label button
  in
  with_tip
    (Input_group.button
       ~variant:Ghost
       ~size:Xs
       ~class_:
         (Cn.cn
            [ "gap-1 px-1.5 font-normal text-muted-foreground \
               hover:text-foreground [&_svg]:size-3.5"
            ; class_
            ])
       [ icon; span "" [ Node.text text ] ])
;;

let queued_pill text =
  div
    "ml-auto mb-2 flex w-fit max-w-full items-center gap-2 rounded-full border \
     bg-card py-1 pr-1 pl-3 text-xs text-muted-foreground"
    [ Icon.view ~class_:"size-3.5" Clock
    ; span "font-medium text-foreground" [ Node.text "Queued" ]
    ; span "truncate" [ Node.text text ]
    ; Button.view
        ~variant:Ghost
        ~size:Icon_xs
        ~class_:"rounded-full"
        ~attrs:[ Attr.create "aria-label" "Unqueue" ]
        [ Icon.view X ]
    ]
;;

let view ?(queued = []) ?wrap_chip ?draft ?above ~tip () =
  let chip = chip ?wrap:wrap_chip in
  div
    "relative mx-auto w-full max-w-[760px] px-4 \
     pb-[max(0.75rem,env(safe-area-inset-bottom))]"
    [ div "" (List.map queued ~f:queued_pill)
    ; Option.value above ~default:Node.none
    ; Input_group.root
        ~class_:
          "h-auto flex-col rounded-2xl border-border bg-card shadow-none \
           dark:bg-card has-[textarea:focus-visible]:ring-0 \
           has-[textarea:focus-visible]:border-ring/70"
        [ Input_group.textarea
            ~class_:"min-h-12 px-4 pt-3 text-base md:text-[13.5px]"
            ~rows:2
            ?value:draft
            ~placeholder:"Ask anything. @ files, $ skills, / commands"
            ()
        ; Input_group.addon
            ~align:Block_end
            ~class_:"gap-0.5 px-2 pb-2"
            [ Tip.wrap
                ~side:Top
                tip
                ~id:"attach"
                "Attach files and images"
                (Input_group.button
                   ~variant:Ghost
                   ~size:Icon_xs
                   ~class_:"text-muted-foreground"
                   ~attrs:[ Attr.create "aria-label" "Attach" ]
                   [ Icon.view Plus ])
            ; Tip.wrap
                ~side:Top
                tip
                ~id:"status"
                "Context 42% of 200k · $1.84 this session"
                (Input_group.button
                   ~variant:Ghost
                   ~size:Icon_xs
                   ~class_:"text-muted-foreground"
                   ~attrs:[ Attr.create "aria-label" "Context and cost" ]
                   [ ring 0.42 ])
            ; div "ml-auto" []
            ; chip ~tip ~id:"model" "Model, reasoning and mode" (Icon.view Sparkles) Fake.User.model
            ; chip ~tip ~id:"host" "Tool host" (Icon.view Server) Fake.User.host
            ; Tip.wrap
                ~side:Top
                tip
                ~id:"send"
                "Send (Enter)"
                (Input_group.button
                   ~variant:Default
                   ~size:Icon_xs
                   ~class_:"ml-1.5 size-7 rounded-full"
                   ~attrs:[ Attr.create "aria-label" "Send" ]
                   [ Icon.view Arrow_up ])
            ]
        ]
    ; div
        "flex items-center justify-between gap-4 px-3 pt-2 text-xs \
         text-muted-foreground/80"
        [ span
            "flex min-w-0 items-center gap-1.5"
            [ Icon.view ~class_:"size-3.5 shrink-0" Folder
            ; span "truncate" [ Node.text Fake.User.cwd ]
            ]
        ; span
            "flex shrink-0 items-center gap-1.5"
            [ Icon.view ~class_:"size-3.5" Git_branch
            ; span "font-mono" [ Node.text Fake.User.branch ]
            ]
        ]
    ]
;;
