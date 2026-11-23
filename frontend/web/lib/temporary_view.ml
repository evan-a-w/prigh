open! Core
open! Import
open Html
module Action = App.Action

let hero (m : App.Model.t) =
  let temporary = App.temporary_shown m in
  Node.div
    ~attrs:
      [ classes
          [ "hero px-3 text-center [&_h2]:m-0 [&_h2]:inline-block \
             [&_h2]:border-0 [&_h2]:p-0 [&_h2]:text-[30px] \
             [&_h2]:leading-tight [&_h2]:font-[650] [&_h2]:tracking-[-0.01em] \
             [&_h2]:text-balance [&_h2]:text-foreground \
             narrow:[&_h2]:text-[25px]"
          ]
          [ "temporary", temporary ]
      ]
    [ Node.h2
        [ Node.text
            (if temporary then "Temporary session" else "What are we building?")
        ; Node.span
            ~attrs:
              [ Attr.class_
                  "hero-caret ml-2 inline-block h-[0.9em] w-[0.45em] \
                   rounded-xs bg-primary align-[-0.1em] transition-colors \
                   in-[.temporary]:bg-border motion-reduce:transition-none"
              ; Attr.create "aria-hidden" "true"
              ]
            []
        ]
    ; (if temporary
       then
         Node.p
           ~attrs:
             [ Attr.class_
                 "hero-sub mt-2.5 mb-0 animate-[appear_0.16s_ease-out] text-sm \
                  text-muted-foreground motion-reduce:animate-none"
             ]
           [ Node.text "Not saved and not listed. Deleted when you leave." ]
       else
         Node.p
           ~attrs:
             [ Attr.class_
                 "hero-sub mt-2.5 mb-0 animate-[appear_0.16s_ease-out] text-sm \
                  text-muted-foreground motion-reduce:animate-none"
             ]
           [ Loon.Kbd.view [ Node.text "/" ]
           ; Node.text " for commands, "
           ; Loon.Kbd.view [ Node.text "@" ]
           ; Node.text " for files"
           ])
    ]
;;

let chip (m : App.Model.t) ~inject =
  let on = App.temporary_shown m in
  Node.button
    ~attrs:
      [ classes
          [ "btn"
          ; "temporary-chip ml-auto h-[30px] flex-none gap-[7px] rounded-full \
             border-transparent bg-transparent pr-3 pl-2.5 text-[13px] \
             font-medium text-muted-foreground hover:bg-accent \
             hover:text-foreground focus-visible:outline-2 \
             focus-visible:outline-offset-1 focus-visible:outline-ring \
             narrow:h-9 max-[359px]:px-2.5 [&_.icon]:size-4 \
             [&.on]:border-foreground/30 [&.on]:bg-foreground/10 \
             [&.on]:text-foreground"
          ]
          [ "on", on ]
      ; Attr.type_ "button"
      ; Attr.role "switch"
      ; Attr.create "aria-checked" (Bool.to_string on)
      ; Attr.title
          (if on
           then "Temporary: not saved, deleted when you leave (Ctrl+Shift+O)"
           else "Make it temporary: not saved or listed (Ctrl+Shift+O)")
      ; Attr.on_click (fun _ -> inject (Action.Set_temporary (not on)))
      ]
    [ icon Ghost; Node.text "Temporary" ]
;;

(* What the new session runs on, when the backend runs several engines: a
   picker of them. *)
let engine_chip (m : App.Model.t) ~inject =
  match App.current_choice m with
  | Some current when Deployed_engines.has_choice (App.deployed m) ->
    let label =
      match current.adapter with
      | None -> String.capitalize current.engine
      | Some _ -> Deployed_engines.Choice.label current
    in
    button
      ~cls:
        "ghost tray-engine h-[30px] min-w-0 gap-1.5 border-transparent \
         bg-transparent px-2 text-[12.5px] font-normal text-muted-foreground \
         hover:bg-accent hover:text-foreground focus-visible:outline-2 \
         focus-visible:outline-offset-1 focus-visible:outline-ring narrow:h-9 \
         [&_.icon]:size-[15px] [&_.icon]:flex-none \
         max-[359px]:[&_.icon]:hidden [&>span]:truncate"
      ~title:
        (sprintf "Runs on %s: start it on another engine (/new ENGINE)" label)
      ~label:(sprintf "Engine: %s. Change it" label)
      ~on_click:(inject Action.Pick_engine)
      [ icon Bot; span ~cls:"tray-engine-name" label ]
  | _ -> Node.none
;;

let tray (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    let folder =
      Sidebar_view.home_relative
        ?host_home:(Sidebar_view.state_host_home m)
        state.cwd
    in
    div
      ~cls:
        "tray in-[.app:not(.narrow)]:relative in-[.app:not(.narrow)]:mx-3.5 \
         in-[.app:not(.narrow)]:-mt-1.5 in-[.app:not(.narrow)]:min-h-[34px] \
         in-[.app:not(.narrow)]:rounded-b-[14px] in-[.app:not(.narrow)]:border \
         in-[.app:not(.narrow)]:border-t-0 \
         in-[.app:not(.narrow)]:border-border \
         in-[.app:not(.narrow)]:bg-card/50 in-[.app:not(.narrow)]:py-0.5 \
         in-[.app:not(.narrow)]:pr-1.5 in-[.app:not(.narrow)]:pl-2 \
         narrow:in-[.fresh]:order-first narrow:in-[.fresh]:px-0.5 \
         narrow:in-[.fresh]:pb-1 flex min-h-11 min-w-0 items-center gap-0.5 \
         text-[13px] text-muted-foreground"
      [ (if state.chat_mode
         then
           div
             ~cls:
               "tray-chat flex h-[30px] min-w-0 items-center gap-1.5 px-2 \
                font-mono text-[12.5px] text-primary [&_.icon]:size-[15px] \
                [&_.icon]:flex-none [&>span]:truncate"
             ~attrs:[ Attr.title "Chat mode: no tool host, folder or branch" ]
             [ icon Message; span "Chat: no tool host" ]
         else Node.none)
      ; (if state.chat_mode
         then Node.none
         else
           button
             ~cls:
               "ghost tray-where h-[30px] min-w-0 gap-1.5 border-transparent \
                bg-transparent px-2 font-mono text-[12.5px] font-normal \
                text-muted-foreground first:flex-initial hover:bg-accent \
                hover:text-foreground focus-visible:outline-2 \
                focus-visible:outline-offset-1 focus-visible:outline-ring \
                narrow:h-9 [&_.icon]:size-[15px] [&_.icon]:flex-none \
                [&_.icon.chevron]:-ml-0.5 [&_.icon.chevron]:size-[13px] \
                max-[359px]:[&_.icon.chevron]:hidden [&>span]:truncate"
             ~title:"Start in another folder (/cd)"
             ~label:(sprintf "Folder: %s. Change it" folder)
             ~on_click:(inject (Action.Run "/cd"))
             [ icon Folder
             ; span ~cls:"tray-folder narrow:hidden" folder
             ; span
                 ~cls:"tray-folder-short hidden narrow:inline narrow:truncate"
                 (Filename.basename folder)
             ])
      ; (match state.git_branch with
         | Some _ when state.chat_mode -> Node.none
         | Some branch ->
           button
             ~cls:
               "ghost tray-where h-[30px] min-w-0 gap-1.5 border-transparent \
                bg-transparent px-2 font-mono text-[12.5px] font-normal \
                text-muted-foreground first:flex-initial hover:bg-accent \
                hover:text-foreground focus-visible:outline-2 \
                focus-visible:outline-offset-1 focus-visible:outline-ring \
                narrow:h-9 [&_.icon]:size-[15px] [&_.icon]:flex-none \
                [&_.icon.chevron]:-ml-0.5 [&_.icon.chevron]:size-[13px] \
                max-[359px]:[&_.icon.chevron]:hidden [&>span]:truncate"
             ~title:"Work in another worktree (/worktree)"
             ~label:(sprintf "Branch: %s. Change it" branch)
             ~on_click:(inject (Action.Run "/worktree"))
             [ icon Branch; span ~cls:"tray-branch" branch; icon Chevron ]
         | None -> Node.none)
      ; engine_chip m ~inject
      ; chip m ~inject
      ]
;;

let pill (m : App.Model.t) ~inject =
  Node.button
    ~attrs:
      [ classes
          [ "btn"
          ; "temporary-pill h-[30px] gap-1.5 rounded-full border-foreground/20 \
             bg-popover/85 px-2.5 text-[13px] font-[550] text-foreground \
             backdrop-blur-sm hover:bg-accent [&.open]:bg-accent \
             [&_.icon]:size-[15px] [&_.icon]:text-muted-foreground \
             [&_.icon.chevron]:-mr-0.5 [&_.icon.chevron]:size-[13px] \
             narrow:in-[.strip]:h-11 narrow:in-[.strip]:max-w-full \
             narrow:in-[.strip]:min-w-0 narrow:in-[.strip]:border-transparent \
             narrow:in-[.strip]:bg-transparent \
             narrow:in-[.strip]:backdrop-blur-none \
             narrow:in-[.strip]:text-[15px] narrow:in-[.strip]:font-semibold"
          ]
          [ "open", Option.is_some m.dialog ]
      ; Attr.type_ "button"
      ; Attr.create "aria-haspopup" "dialog"
      ; Attr.title "Not saved: deleted when you leave (/temporary)"
      ; Attr.on_click (fun _ -> inject Action.Temporary_menu)
      ]
    [ icon Ghost
    ; span
        ~cls:"pill-label narrow:truncate"
        (if m.narrow then "Temporary session" else "Temporary")
    ; icon Chevron
    ]
;;

let menu ~inject =
  Modal.view
    ~cls:
      "temporary-menu in-[.app:not(.narrow)]:w-[300px] \
       in-[.app:not(.narrow)]:[&_.modal-head]:hidden [&_.modal-body]:p-1.5 \
       [&_p]:mx-2.5 [&_p]:mt-2.5 [&_p]:mb-2 [&_p]:text-xs [&_p]:leading-normal \
       [&_p]:text-muted-foreground \
       narrow:[&_.modal-body]:px-[max(10px,var(--safe-l))] \
       narrow:[&_.modal-body]:pt-1 \
       narrow:[&_.modal-body]:pb-[max(14px,var(--safe-b))] narrow:[&_p]:mx-1.5 \
       narrow:[&_p]:mt-0 narrow:[&_p]:text-sm"
    ~title:"Temporary session"
    ~on_close:(inject Action.Close_dialog)
    [ Node.p
        [ Node.text
            "This session isn't saved. It's deleted, with its subagents, when \
             you leave it."
        ]
    ; Node.div
        ~attrs:[ Attr.class_ "temporary-actions flex flex-col gap-0.5" ]
        [ button
            ~cls:
              "temporary-keep h-auto min-h-9 w-full justify-start gap-2.5 \
               border-transparent bg-transparent px-2 text-[13.5px] \
               font-medium text-foreground hover:bg-accent \
               focus-visible:bg-accent narrow:min-h-11 narrow:text-[15px] \
               [&_.icon]:text-muted-foreground"
            ~on_click:(inject Action.Keep_session)
            [ icon Keep; Node.text "Keep this session" ]
        ; button
            ~cls:
              "temporary-delete h-auto min-h-9 w-full justify-start gap-2.5 \
               border-transparent bg-transparent px-2 text-[13.5px] \
               font-medium text-destructive hover:bg-accent \
               focus-visible:bg-accent narrow:min-h-11 narrow:text-[15px] \
               [&_.icon]:text-destructive"
            ~on_click:(inject Action.Delete_temporary)
            [ icon Trash; Node.text "Delete it now" ]
        ]
    ]
;;

let leave (m : App.Model.t) ~inject =
  let working =
    match m.state with
    | None -> []
    | Some s ->
      let count what n =
        match n with
        | 0 -> None
        | 1 -> Some (sprintf "a %s" what)
        | n -> Some (sprintf "%d %ss" n what)
      in
      List.filter_opt
        [ Option.some_if s.running "its turn"
        ; count "subagent" (List.count s.subagents ~f:(fun a -> a.running))
        ; count "job" (List.count s.jobs ~f:(fun j -> j.running))
        ]
  in
  let them =
    match working with
    | [] | [ "its turn" ] -> "stops it"
    | _ -> "stops them"
  in
  Modal.view
    ~cls:
      "leave-temporary w-[min(440px,100%)] [&_.modal-head_h2]:text-base \
       [&_.modal-body_p]:text-sm [&_.modal-body_p]:leading-normal \
       [&_.modal-body_p]:text-muted-foreground [&_.modal-buttons]:flex-wrap \
       [&_.modal-buttons]:border-t-0 \
       narrow:[&_.modal-buttons]:flex-col-reverse \
       narrow:[&_.modal-buttons]:items-stretch \
       narrow:[&_.modal-buttons_.btn]:m-0 \
       narrow:[&_.modal-buttons_.btn]:w-full"
    ~title:"Delete this temporary session?"
    ~on_close:(inject Action.Close_dialog)
    ~footer:
      [ button
          ~cls:
            "ghost leave-keep mr-auto border-transparent bg-transparent pl-1 \
             text-muted-foreground hover:text-foreground"
          ~on_click:(inject Action.Leave_keep)
          [ Node.text "Keep it instead" ]
      ; button ~on_click:(inject Action.Close_dialog) [ Node.text "Stay" ]
      ; button
          ~cls:"danger"
          ~on_click:(inject Action.Leave_delete)
          [ Node.text "Delete and leave" ]
      ]
    [ Node.p
        [ Node.textf
            "It's still working%s. Leaving deletes the transcript and %s."
            (match working with
             | [] -> ""
             | working -> sprintf " (%s)" (String.concat ~sep:", " working))
            them
        ]
    ]
;;
