open! Core
open! Import
open Html
module Action = App.Action

let tokens n =
  if n >= 1_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else if n >= 10_000
  then sprintf "%dk" (n / 1000)
  else if n >= 1000
  then sprintf "%.1fk" (Float.of_int n /. 1000.)
  else Int.to_string n
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let item ?(cls = "") ?title children =
  Node.span
    ~attrs:
      ([ Attr.class_ (String.strip ("status-item " ^ cls)) ]
       @ Option.value_map title ~default:[] ~f:(fun t -> [ Attr.title t ]))
    children
;;

let context (state : State.t) =
  if state.model.context_window <= 0
  then Node.none
  else (
    let percent =
      100.
      *. Float.of_int state.context_tokens
      /. Float.of_int state.model.context_window
    in
    let level =
      if Float.(percent >= 85.)
      then "hot"
      else if Float.(percent >= 60.)
      then "warm"
      else ""
    in
    item
      ~cls:("context " ^ level)
      ~title:
        (sprintf
           "Context: %s of %s tokens (/compact frees some)"
           (tokens state.context_tokens)
           (tokens state.model.context_window))
      [ Node.span
          ~attrs:[ Attr.class_ "meter" ]
          [ Node.span
              ~attrs:
                [ Attr.class_ "meter-fill"
                ; Attr.style
                    (Css_gen.width
                       (`Percent
                           (Percent.of_percentage (Float.min 100. percent))))
                ]
              []
          ]
      ; Node.textf "%.0f%%" percent
      ])
;;

let background (state : State.t) ~inject =
  let running_agents = List.count state.subagents ~f:(fun s -> s.running) in
  let running_jobs = List.count state.jobs ~f:(fun j -> j.running) in
  let parts =
    List.filter_opt
      [ Option.some_if
          (not (List.is_empty state.subagents))
          (plural (List.length state.subagents) "subagent")
      ; Option.some_if
          (not (List.is_empty state.jobs))
          (plural (List.length state.jobs) "job")
      ]
  in
  match parts with
  | [] -> Node.none
  | parts ->
    Node.button
      ~attrs:
        [ classes
            [ "status-item"; "link"; "background" ]
            [ "active", running_agents + running_jobs > 0 ]
        ; Attr.type_ "button"
        ; Attr.title "Background work (/agents)"
        ; Attr.on_click (fun _ -> inject Action.Open_agents)
        ]
      [ icon Cpu; Node.text (String.concat ~sep:" · " parts) ]
;;

let host (state : State.t) =
  if String.equal state.active_host Host.backend_id
  then Node.none
  else (
    let name =
      List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host)
      |> Option.value_map ~default:state.active_host ~f:(fun h -> h.name)
    in
    item ~cls:"host" ~title:"Tools run here" [ icon Server; Node.text name ])
;;

let user (m : App.Model.t) =
  match m.hello with
  | Some ({ user = Some user; _ } as hello) ->
    let text =
      match Hello_reply.acting_as hello with
      | Some ns -> sprintf "%s as %s" user ns
      | None -> user
    in
    item ~cls:"user" ~title:"Signed in" [ Node.text text ]
  | _ -> Node.none
;;

let view (m : App.Model.t) ~inject =
  match m.state with
  | None -> div ~cls:"status" []
  | Some state ->
    let steer, follow_up = m.queue in
    div
      ~cls:"status"
      [ (if state.running
         then
           item
             ~cls:"running"
             [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
             ; Node.text "Working"
             ]
         else
           item
             ~cls:"idle"
             [ Node.span ~attrs:[ Attr.class_ "idle-dot" ] []
             ; Node.text "Ready"
             ])
      ; context state
      ; item
          ~cls:"tokens"
          ~title:
            (sprintf
               "Tokens: %d in, %d out, %d cache read"
               state.usage.input
               state.usage.output
               state.usage.cache_read)
          [ Node.textf
              "↑%s ↓%s"
              (tokens state.usage.input)
              (tokens state.usage.output)
          ]
      ; item
          ~cls:"cost"
          ~title:"Session cost"
          [ Node.textf "$%.4f" state.cost_usd ]
      ; (if steer + follow_up > 0
         then
           Node.button
             ~attrs:
               [ Attr.class_ "status-item link queued"
               ; Attr.type_ "button"
               ; Attr.title "Take the last queued message back into the editor"
               ; Attr.on_click (fun _ -> inject Action.Dequeue)
               ]
             [ icon Undo; Node.text (sprintf "%d queued" (steer + follow_up)) ]
         else Node.none)
      ; background state ~inject
      ; host state
      ; div ~cls:"status-spacer" []
      ; user m
      ]
;;
