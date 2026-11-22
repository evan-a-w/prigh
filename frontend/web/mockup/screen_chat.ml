open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children
let p children = Node.p ~attrs:[ Attr.class_ "leading-relaxed" ] children
let code s = Node.code ~attrs:[ Attr.class_ "rounded bg-muted px-1 py-0.5 font-mono text-[0.85em] text-foreground" ] [ Node.text s ]

let user_message text =
  div
    "flex justify-end"
    [ div
        "max-w-[80%] rounded-2xl rounded-br-md bg-bubble px-3.5 py-2 text-bubble-foreground"
        [ Node.text text ]
    ]
;;

let tool_icon = function
  | "read" -> Icon.view File_text
  | "grep" -> Icons.view File_search
  | "edit" -> Icon.view Pencil
  | "bash" -> Icon.view Terminal
  | "subagent" -> Icon.view Bot
  | _ -> Icons.view Wrench
;;

let tool_row ~open_ids ~toggle i (call : Fake.Tool_call.t) =
  let id = sprintf "%s-%d" call.tool i in
  let running = String.equal call.duration "running" in
  let status =
    if running
    then Node.span ~attrs:[ Attr.class_ "thinking-shimmer" ] [ Node.text "working…" ]
    else if call.ok
    then Icon.view ~class_:"size-3.5 text-emerald-400/80" Check
    else Icon.view ~class_:"size-3.5 text-red-400" X
  in
  let row_classes =
    "group/tool flex w-full items-center gap-2 rounded-md px-2 py-1 text-left text-xs \
     text-muted-foreground hover:bg-accent/40 hover:text-foreground"
  in
  let row extra =
    [ span "flex size-4 items-center justify-center [&_svg]:size-3.5" [ tool_icon call.tool ]
    ; span "w-16 shrink-0 font-medium text-foreground/90" [ Node.text call.tool ]
    ; span "min-w-0 flex-1 truncate font-mono text-[11.5px]" [ Node.text call.summary ]
    ; (if running
       then span "shrink-0" [ status ]
       else
         Node.fragment
           [ span "shrink-0 tabular-nums" [ Node.text call.duration ]
           ; span "flex size-4 shrink-0 items-center justify-center" [ status ]
           ])
    ]
    @ extra
  in
  match call.output with
  | _ when String.equal call.tool "subagent" ->
    let name =
      match List.hd Fake.Subagent.all with
      | Some a -> a.name
      | None -> call.summary
    in
    Node.a
      ~attrs:[ Attr.href "#"; Attr.title "Open the subagent's chat"; Attr.class_ row_classes ]
      [ span "flex size-4 items-center justify-center [&_svg]:size-3.5" [ tool_icon call.tool ]
      ; span "w-16 shrink-0 font-medium text-foreground/90" [ Node.text call.tool ]
      ; span
          "min-w-0 flex-1 truncate"
          [ span
              "text-foreground/90 underline decoration-muted-foreground/40 \
               underline-offset-2 group-hover/tool:decoration-foreground"
              [ Node.text name ]
          ; Icons.view ~class_:"mx-1 inline size-3 align-[-1px]" Arrow_up_right
          ; span "font-mono text-[11.5px]" [ Node.text call.summary ]
          ]
      ; span "shrink-0" [ status ]
      ]
  | None -> div row_classes (row [])
  | Some output ->
    Collapsible.view
      ~id:("tool-" ^ id)
      ~open_:(Set.mem open_ids id)
      ~on_change:(fun _ -> toggle id)
      (fun c ->
        [ Collapsible.trigger c ~class_:row_classes (row [])
        ; Collapsible.content
            c
            [ Scroll_area.root
                ~class_:"mx-2 mt-1 mb-2 max-h-40 rounded-lg border bg-muted/40"
                [ Node.pre
                    ~attrs:
                      [ Attr.class_
                          "p-3 font-mono text-[11.5px] leading-relaxed text-foreground/85"
                      ]
                    [ Node.text output ]
                ]
            ]
        ])
;;

let worked ~open_ids ~toggle ~id ~duration calls =
  Collapsible.view
    ~id
    ~open_:(Set.mem open_ids id)
    ~on_change:(fun _ -> toggle id)
    (fun c ->
      [ Collapsible.trigger
          c
          ~class_:
            "group/worked flex w-full items-center gap-1 border-b pb-2 text-left text-sm \
             text-muted-foreground hover:text-foreground"
          [ Node.textf "Worked for %s" duration
          ; span
              "text-xs text-muted-foreground/70"
              [ Node.textf "· %d tool calls" (List.length calls) ]
          ; Icon.view
              ~class_:
                "size-3.5 transition-transform group-data-[state=open]/worked:rotate-90"
              Chevron_right
          ]
      ; Collapsible.content
          c
          ~class_:"pt-2"
          [ div "flex flex-col" (List.mapi calls ~f:(tool_row ~open_ids ~toggle)) ]
      ])
;;

let code_block lines =
  div
    "overflow-hidden rounded-lg border bg-muted/40"
    [ div
        "flex items-center justify-between border-b px-3 py-1.5 text-xs text-muted-foreground"
        [ span "font-mono" [ Node.text "ocaml" ]
        ; Button.view
            ~variant:Ghost
            ~size:Icon_xs
            ~attrs:[ Attr.create "aria-label" "Copy" ]
            [ Icon.view Copy ]
        ]
    ; Node.pre
        ~attrs:[ Attr.class_ "overflow-x-auto p-3 font-mono text-[12px] leading-relaxed" ]
        [ Node.text (String.concat ~sep:"\n" lines) ]
    ]
;;

let assistant_answer =
  div
    "flex flex-col gap-3"
    [ p
        [ Node.text
            "The composer has five chips, and on a 320px phone they wrap onto two \
             lines. Here is what each one does and what I would do with it:"
        ]
    ; Node.h3 ~attrs:[ Attr.class_ "pt-1 text-sm font-semibold" ] [ Node.text "The chips" ]
    ; Node.ul
        ~attrs:[ Attr.class_ "ml-5 flex list-disc flex-col gap-1 marker:text-muted-foreground" ]
        [ Node.li [ Node.text "Model and thinking: used every session; keep them." ]
        ; Node.li
            [ Node.text "Mode: rarely changed; move it under "
            ; code "/mode"
            ; Node.text " and the ⋯ menu on phones."
            ]
        ; Node.li
            [ Node.text "Host: only shown when more than one host is connected." ]
        ]
    ; p
        [ Node.text "The row becomes one "
        ; code "Input_group.addon ~align:Block_end"
        ; Node.text ":"
        ]
    ; code_block
        [ "Input_group.root"
        ; "  [ Input_group.textarea ~placeholder ()"
        ; "  ; Input_group.addon ~align:Block_end"
        ; "      [ attach; model_chip; thinking_chip"
        ; "      ; spacer; status; mic; send ]"
        ; "  ]"
        ]
    ]
;;

let thinking =
  div
    "flex items-center gap-2 text-sm text-muted-foreground"
    [ span "thinking-shimmer font-medium" [ Node.text "Thinking…" ]
    ; span "text-xs tabular-nums text-muted-foreground/70" [ Node.text "12s · Esc to interrupt" ]
    ]
;;

let component
  ?(banner = Bonsai.return Node.none)
  ?(above_composer = Bonsai.return Node.none)
  ?(queued = [ "also check the ⋯ sheet on 320px" ])
  ?composer
  ~tip
  (local_ graph)
  =
  let composer =
    match composer with
    | Some c -> c
    | None -> Bonsai.return (fun tip -> Composer.view ~queued ~tip ())
  in
  let open_ids, set_open_ids =
    Bonsai.state (String.Set.of_list [ "turn-2"; "bash-2" ]) graph
  in
  let%arr open_ids and set_open_ids and tip and banner and above_composer and composer in
  let toggle id =
    set_open_ids (if Set.mem open_ids id then Set.remove open_ids id else Set.add open_ids id)
  in
  Node.fragment
    [ banner
    ; (* column-reverse keeps the view at the bottom, as a chat opens. *)
      div
        "flex min-h-0 flex-1 flex-col-reverse overflow-y-auto"
        [ div
            "mx-auto flex w-full max-w-[760px] flex-col gap-6 px-6 pt-6 pb-10 text-sm"
            [ user_message
                "The web UI is getting cluttered. Look at the composer chips and tell me \
                 what we could drop."
            ; worked
                ~open_ids
                ~toggle
                ~id:"turn-1"
                ~duration:"23s"
                Fake.Tool_call.first_turn
            ; assistant_answer
            ; user_message
                "Do it with loon's Input group, then run the tests and check 320px."
            ; worked
                ~open_ids
                ~toggle
                ~id:"turn-2"
                ~duration:"1m 48s"
                Fake.Tool_call.second_turn
            ; p
                [ Node.text
                    "The chips are in one row now. The e2e caught one problem: on a \
                     320px phone the chips are 38px tall, under the 44px minimum, so a \
                     subagent is fixing their touch targets."
                ]
            ; thinking
            ]
        ]
    ; above_composer
    ; composer tip
    ]
;;
