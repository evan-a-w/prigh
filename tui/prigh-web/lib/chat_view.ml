open! Core
open! Import
open Chat_html

let image_src = Image_view.src

let tokens n =
  if n < 1000
  then Int.to_string n
  else if n < 100_000
  then sprintf "%.1fk" (Float.of_int n /. 1000.)
  else sprintf "%dk" (n / 1000)
;;

let user ({ text; images } : Message.User.t) =
  div
    "msg user"
    [ Image_view.thumbs images
    ; (if String.is_empty (String.strip text)
       then Node.none
       else div "bubble" [ Node.text text ])
    ]
;;

let thinking ~live text =
  if live
  then
    div
      "thinking live"
      [ div
          "thinking-head"
          [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
          ; span "label" "Thinking…"
          ]
      ; div "thinking-text" [ Node.text (String.strip text) ]
      ]
  else
    folded
      ~cls:"thinking"
      ~label:"Thought"
      ~preview:(Markdown.preview text)
      (div "thinking-text" [ Markdown_view.render text ])
;;

let stopped (message : Message.Assistant.t) =
  match message.stop_reason with
  | Error e ->
    div
      "stop error"
      [ span "title" "The model returned an error"
      ; div "detail" [ Node.text e ]
      ; span "hint" "Send a message to retry, or switch model."
      ]
  | Aborted -> div "stop aborted" [ Node.text "Interrupted" ]
  | Length ->
    div
      "stop length"
      [ Node.text "Stopped at the output token limit — ask it to continue." ]
  | End_turn | Tool_use -> Node.none
;;

let footer (message : Message.Assistant.t) =
  match message.stop_reason with
  | End_turn when message.usage.output > 0 ->
    let { Usage.input; output; cache_read } = message.usage in
    div
      "meta"
      [ Node.text
          (String.concat
             ~sep:" · "
             ([ message.model; tokens (input + cache_read) ^ " in" ]
              @ (if cache_read > 0
                 then [ tokens cache_read ^ " cached" ]
                 else [])
              @ [ tokens output ^ " out" ]))
      ]
  | _ -> Node.none
;;

let rec assistant chat (message : Message.Assistant.t) ~streaming =
  let count = List.length message.content in
  let blocks =
    List.mapi message.content ~f:(fun i content ->
      let last = i = count - 1 in
      match (content : Content.t) with
      | Text text when String.is_empty (String.strip text) -> Node.none
      | Text text -> Markdown_view.render ~streaming:(streaming && last) text
      | Thinking text when String.is_empty (String.strip text) -> Node.none
      | Thinking text -> thinking ~live:(streaming && last) text
      | Tool_call call ->
        Tool_view.view
          ~nested:view
          ~streaming:(streaming && last)
          ~running:(Chat.running chat)
          call
          (Chat.tool chat call.id))
  in
  let blocks = present blocks in
  let pending =
    if streaming && count = 0
    then div "pending" (List.init 3 ~f:(fun _ -> Node.span []))
    else Node.none
  in
  div
    (if streaming then "msg assistant streaming" else "msg assistant")
    ((pending :: blocks)
     @ [ stopped message; (if streaming then Node.none else footer message) ])

and entry chat (entry : Chat.Entry.t) =
  match entry with
  | User u ->
    (match Prigh_ui.Delivery.parse u.text with
     | Some sections -> div "msg" [ Delivery_view.view sections ]
     | None -> user u)
  | Notice text -> div "msg notice" [ Node.text text ]
  | Compaction summary ->
    folded
      ~cls:"msg compaction"
      ~label:"Context compacted"
      ~preview:(Markdown.preview summary)
      (Markdown_view.render summary)
  | Assistant { message; streaming } -> assistant chat message ~streaming

and view chat = div "entries" (List.map (Chat.entries chat) ~f:(entry chat))
