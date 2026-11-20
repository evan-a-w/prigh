open! Core
open! Import
open Html
module Action = App.Action

let compacts_at (m : App.Model.t) (state : State.t) =
  let window = state.model.context_window in
  let at_tokens =
    Option.value_map m.config ~default:0 ~f:(fun (c : Config.t) ->
      c.compact_at_tokens)
  in
  if window <= 0 || at_tokens <= 0
  then 80.
  else Float.min 80. (100. *. Float.of_int at_tokens /. Float.of_int window)
;;

let level ~compacts_at percent =
  if Float.(percent >= compacts_at)
  then "hot"
  else if Float.(percent >= compacts_at - 15.)
  then "warm"
  else ""
;;

let usd_of (model : Llm.t) (usage : Usage.t) =
  let per_m tokens price = Float.of_int tokens *. price /. 1e6 in
  per_m (usage.input - usage.cache_read) model.cost.input
  +. per_m usage.cache_read model.cost.cache_read
  +. per_m usage.output model.cost.output
;;

let last_turn_usd (m : App.Model.t) (state : State.t) =
  let rec since_user acc = function
    | [] | Chat.Entry.User _ :: _ -> acc
    | Assistant { message; _ } :: rest -> since_user (message.usage :: acc) rest
    | (Shell _ | Compaction _) :: rest -> since_user acc rest
  in
  match since_user [] (Chat.rev_entries m.chat) with
  | [] -> None
  | usages -> Some (List.sum (module Float) usages ~f:(usd_of state.model))
;;

let card_id = "context-card"
let tokens = Status_view.tokens

let card (m : App.Model.t) (state : State.t) ~inject =
  let line ?(cls = "") k v =
    div
      ~cls:(String.strip ("status-detail " ^ cls))
      [ span ~cls:"status-detail-key" k; span ~cls:"status-detail-value" v ]
  in
  let at = compacts_at m state in
  let percent = Status_view.context_percent state in
  let mode =
    Option.value m.pending.mode ~default:(Session_mode.of_state state)
  in
  let thinking = Option.value m.pending.thinking ~default:state.thinking in
  let cache_rate =
    if state.usage.input > 0
    then
      Some
        (100.
         *. Float.of_int state.usage.cache_read
         /. Float.of_int state.usage.input)
    else None
  in
  Node.div
    ~attrs:
      [ Attr.class_ "status-details context-card"
      ; Attr.role "dialog"
      ; Attr.create "aria-label" "Context and cost"
      ; Attr.id card_id
      ]
    [ div
        ~cls:"context-card-head"
        [ span ~cls:"context-card-title" "Context"
        ; span
            ~cls:"context-card-of"
            (sprintf
               "%s / %s"
               (tokens state.context_tokens)
               (tokens state.model.context_window))
        ]
    ; (match percent with
       | Some p ->
         Node.div
           ~attrs:
             [ Attr.class_
                 (String.strip ("context-bar " ^ level ~compacts_at:at p))
             ; Attr.create "aria-hidden" "true"
             ]
           [ Node.span
               ~attrs:
                 [ Attr.class_ "context-bar-fill"
                 ; Attr.style (Status_view.fill p)
                 ]
               []
           ; Node.span
               ~attrs:
                 [ Attr.class_ "context-bar-mark"
                 ; Attr.style
                     (Css_gen.left
                        (`Percent (Percent.of_percentage (Float.min at 100.))))
                 ]
               []
           ]
       | None -> Node.none)
    ; div
        ~cls:"context-card-sub"
        [ Node.text
            (match percent with
             | Some p ->
               (* The tokens too: [compact_at_tokens] makes a large
                  window's percentage small. *)
               sprintf
                 "%.0f%% used · compacts at %s (%.0f%%)"
                 p
                 (tokens
                    (Float.iround_nearest_exn
                       (at /. 100. *. Float.of_int state.model.context_window)))
                 at
             | None -> sprintf "Compacts at %.0f%%" at)
        ]
    ; div ~cls:"context-card-rule" []
    ; (match Prigh_ui.Loop_status.budget_part state with
       | None -> line "This session" (sprintf "$%.2f" state.cost_usd)
       | Some (text, lvl) ->
         line
           ~cls:("cost " ^ Status_view.level_class lvl)
           "This session"
           (sprintf "$%.2f · %s" state.cost_usd text))
    ; (match last_turn_usd m state with
       | Some usd -> line "Last turn" (sprintf "$%.3f" usd)
       | None -> Node.none)
    ; (match cache_rate with
       | Some r -> line "Cache hits" (sprintf "%.0f%%" r)
       | None -> Node.none)
    ; line
        "Tokens"
        (sprintf
           "%s in · %s out"
           (tokens state.usage.input)
           (tokens state.usage.output))
    ; div ~cls:"context-card-rule" []
    ; line ~cls:"status-line" "Status" (Status_view.activity_word m)
    ; line
        ~cls:"reasoning-line"
        "Reasoning"
        (Option.value
           (Reasoning.chip_label state.model ~current:thinking)
           ~default:"none")
    ; line ~cls:"mode-line" "Mode" (Session_mode.title mode)
    ; line "Model" state.model.name
    ; (match state.git_branch with
       | Some branch -> line ~cls:"branch-line" "Branch" branch
       | None -> Node.none)
    ; div
        ~cls:"context-card-foot"
        [ (if Option.exists percent ~f:(fun p -> Float.(p >= 70.))
           then
             button
               ~cls:"context-compact"
               ~title:
                 "Summarise the conversation so far to free context (/compact)"
               ~on_click:
                 (Effect.Many
                    [ inject Action.Toggle_context_card
                    ; inject (Action.Run "/compact")
                    ])
               [ Node.text "Compact" ]
           else span ~cls:"status-detail-hint" "/compact to free context")
        ; button
            ~cls:"link context-details"
            ~title:"The session's details (/session)"
            ~on_click:
              (Effect.Many
                 [ inject Action.Toggle_context_card
                 ; inject (Action.Run "/session")
                 ])
            [ Node.text "Details" ]
        ]
    ]
;;

(* A circle's stroke, dashed to the fraction. *)
let radius = 12.
let circumference = 2. *. Float.pi *. radius

let ring_svg percent =
  let on = circumference *. Float.clamp_exn percent ~min:0. ~max:100. /. 100. in
  Ui_svg.svg
    ~cls:"context-ring-svg"
    ~keep_ratio:true
    ~width:30.
    ~height:30.
    [ Ui_svg.el
        "circle"
        [ "class", "context-ring-track"
        ; "cx", "15"
        ; "cy", "15"
        ; "r", Ui_svg.num radius
        ]
        []
    ; (* Under half a unit, the round caps alone would draw a dot. *)
      (if Float.(on < 0.5)
       then Node.none
       else
         Ui_svg.el
           "circle"
           [ "class", "context-ring-fill"
           ; "cx", "15"
           ; "cy", "15"
           ; "r", Ui_svg.num radius
           ; ( "stroke-dasharray"
             , sprintf "%s %s" (Ui_svg.num on) (Ui_svg.num circumference) )
           ; "transform", "rotate(-90 15 15)"
           ]
           [])
    ; Ui_svg.el
        "circle"
        [ "class", "context-ring-spin"
        ; "cx", "15"
        ; "cy", "15"
        ; "r", Ui_svg.num radius
        ; ( "stroke-dasharray"
          , sprintf "%s %s" (Ui_svg.num 9.) (Ui_svg.num circumference) )
        ]
        []
    ]
;;

let view (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    let at = compacts_at m state in
    let percent = Status_view.context_percent state in
    let p = Option.value percent ~default:0. in
    let busy = Status_view.busy m in
    let label =
      match percent with
      | Some p -> sprintf "Context %.0f%% full, $%.2f: details" p state.cost_usd
      | None -> sprintf "Context and cost ($%.2f): details" state.cost_usd
    in
    Node.div
      ~attrs:
        [ classes
            [ "context-ring-anchor" ]
            [ "open", m.context_card; "busy", busy ]
        ]
      [ (if m.context_card
         then
           Node.div
             ~attrs:
               [ Attr.class_ "menu-backdrop context-backdrop"
               ; Attr.on_click (fun _ -> inject Action.Toggle_context_card)
               ]
             []
         else Node.none)
      ; Node.button
          ~attrs:
            [ Attr.class_
                (String.strip ("context-ring " ^ level ~compacts_at:at p))
            ; Attr.type_ "button"
            ; Attr.create "aria-label" label
            ; Attr.create "aria-haspopup" "dialog"
            ; Attr.create "aria-expanded" (Bool.to_string m.context_card)
            ; Attr.create "aria-describedby" card_id
            ; Attr.on_click (fun _ -> inject Action.Toggle_context_card)
            ]
          [ ring_svg p ]
      ; (* What the agent is doing, for screen readers: the ring turns. *)
        Node.span
          ~attrs:
            [ Attr.class_ "sr-only"
            ; Attr.role "status"
            ; Attr.create "aria-atomic" "true"
            ]
          [ Node.text (Status_view.activity_word m) ]
      ; card m state ~inject
      ]
;;
